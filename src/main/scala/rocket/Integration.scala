package saturn.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.diplomacy._

import saturn.common._
import saturn.backend.{VectorBackend}
import saturn.mem.{TLSplitInterface, VectorMemUnit}
import saturn.frontend.{VectorDispatcher}
import eventtrace.EventTrace

class SaturnRocketUnit(implicit p: Parameters) extends RocketVectorUnit()(p) with HasVectorParams with HasCoreParameters {

  if (vParams.useScalarFPFMA) {
    require(coreParams.fpu.isDefined)
    if (vParams.useScalarFPFMA) {
      require(coreParams.fpu.get.sfmaLatency == vParams.fmaPipeDepth - 1)
      require(coreParams.fpu.get.dfmaLatency == vParams.fmaPipeDepth - 1)
    }
  }

  val tl_if = LazyModule(new TLSplitInterface)
  atlNode := TLBuffer(vParams.tlBuffer) := TLWidthWidget(mLenB) := tl_if.node

  override lazy val module = new SaturnRocketImpl
  class SaturnRocketImpl extends RocketVectorUnitModuleImp(this) with HasVectorParams with HasCoreParameters {

    val useL1DCache = mLen == vMemDataBits

    // EventTrace, in configs that trace: Saturn's micro-ops, each from Rocket's instruction; see SaturnTrace.
    def vuop = new SaturnTrace.Vuop(new RocketTrace.Instruction(vaddrBitsExtended))
    val vuops = EventTrace.collection(vuop)

    val dis = Module(new VectorDispatcher)
    val vfu = Module(new SaturnRocketFrontend(tl_if.edge))
    val vu = Module(new VectorBackend()(p.alterPartial { case SaturnTraceVuops => Some(vuops) }))
    val vmu = Module(new VectorMemUnit)

    val hella_if = Module(new HellaCacheInterface)
    val scalar_arb = Module(new Arbiter(new ScalarWrite, 2))

    dis.io.issue <> vfu.io.issue
    vfu.io.issue_refusal := dis.io.issue_refusal

    // EventTrace: a micro-op is created when the dispatcher sends it to the backend, which gives it its age tag. Its
    // instruction is the one in Rocket's Check stage when the iterative fault checker sends it, or else the one in
    // Writeback, where the pipelined fault checker's last stage is.
    {
      val instructions = p(RocketTraceInstructions)()
      def at(stage: RocketTrace.Stage.Type) = instructions.where(_.stage === stage)
      val schema = new EventTrace.Schema[SaturnTrace.Vuop](vuop) {}
      def dispatch(instruction: RocketTrace.Instruction) = schema.create(event = "dispatch") { u =>
        u.instruction := instruction
        u.pc := dis.io.dis.bits.pc
        u.bits := dis.io.dis.bits.bits
        u.vat := dis.io.dis.bits.vat
        u.vstart := dis.io.dis.bits.vstart
        u.vl := dis.io.dis.bits.vconfig.vl
        u.stage := SaturnTrace.VuopStage.Dispatch
        vuops.add(u)
      }
      when (dis.io.dis.fire) {
        EventTrace.test(at(RocketTrace.Stage.Check).size === 1) {
          dispatch(at(RocketTrace.Stage.Check).only)
        }.otherwise {
          dispatch(at(RocketTrace.Stage.Writeback).only)
        }
      }
    }

    vfu.io.core <> io.core
    vfu.io.tlb <> io.tlb

    vu.io.index_access <> vfu.io.index_access
    vu.io.mask_access <> vfu.io.mask_access
    vu.io.vmu <> vmu.io.vu
    vu.io.vat_tail := dis.io.vat_tail
    vu.io.vat_head := dis.io.vat_head
    vu.io.dis <> dis.io.dis
    dis.io.vat_release := vu.io.vat_release
    vmu.io.enq <> dis.io.mem

    vmu.io.scalar_check <> vfu.io.scalar_check

    io.core.backend_busy   := vu.io.busy || tl_if.module.io.mem_busy || hella_if.io.mem_busy || vmu.io.busy
    io.core.set_vxsat      := vu.io.set_vxsat
    io.core.set_fflags     := vu.io.set_fflags

    scalar_arb.io.in(0) <> vu.io.scalar_resp
    scalar_arb.io.in(1) <> dis.io.scalar_resp
    io.core.resp <> Queue(scalar_arb.io.out)

    io.fp_req <> vu.io.fp_req
    vu.io.fp_resp.valid := io.fp_resp.valid
    vu.io.fp_resp.bits := io.fp_resp.bits
    io.fp_resp.ready := true.B

    io.dmem <> hella_if.io.dmem
    hella_if.io.vec_busy := vu.io.busy || vmu.io.busy
    hella_if.io.status := io.core.status

    def block[T <: Data](in: DecoupledIO[T], block: Bool): DecoupledIO[T] = {
      val out = Wire(Decoupled(in.bits.cloneType))
      out.bits := in.bits
      out.valid := in.valid && !block
      in.ready := out.ready && !block
      out
    }

    val load_use_tl_reg = RegInit(true.B)
    val store_use_tl_reg = RegInit(true.B)

    // virtually-addressed requests must go through L1
    val load_use_tl = load_use_tl_reg || !useL1DCache.B
    val store_use_tl = store_use_tl_reg || !useL1DCache.B

    vmu.io.dmem.load_resp.valid := tl_if.module.io.vec.load_resp.valid || hella_if.io.vec.load_resp.valid
    vmu.io.dmem.load_resp.bits := Mux1H(
      Seq(tl_if.module.io.vec.load_resp.valid, hella_if.io.vec.load_resp.valid),
      Seq(tl_if.module.io.vec.load_resp.bits , hella_if.io.vec.load_resp.bits))
    vmu.io.dmem.store_ack.valid := tl_if.module.io.vec.store_ack.valid || hella_if.io.vec.store_ack.valid
    vmu.io.dmem.store_ack.bits := Mux1H(
      Seq(tl_if.module.io.vec.store_ack.valid, hella_if.io.vec.store_ack.valid),
      Seq(tl_if.module.io.vec.store_ack.bits , hella_if.io.vec.store_ack.bits))

    when (load_use_tl) {
      tl_if.module.io.vec.load_req <> block(vmu.io.dmem.load_req, hella_if.io.mem_busy)
      hella_if.io.vec.load_req.valid := false.B
      hella_if.io.vec.load_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.load_req <> block(vmu.io.dmem.load_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.load_req.valid := false.B
      tl_if.module.io.vec.load_req.bits := DontCare
    }
    when (store_use_tl) {
      tl_if.module.io.vec.store_req <> block(vmu.io.dmem.store_req, hella_if.io.mem_busy)
      hella_if.io.vec.store_req.valid := false.B
      hella_if.io.vec.store_req.bits := DontCare
    } .otherwise {
      hella_if.io.vec.store_req <> block(vmu.io.dmem.store_req, tl_if.module.io.mem_busy)
      tl_if.module.io.vec.store_req.valid := false.B
      tl_if.module.io.vec.store_req.bits := DontCare
    }
  }
}

