package saturn.common

import chisel3._
import org.chipsalliance.cde.config._
import eventtrace.{Collection, EventTrace}

/** What EventTrace records of Saturn, in configs that trace: each micro-op, the manual's unit of dispatch, from its
  * dispatch until the backend frees its age tag. An instruction becomes one micro-op, one per page when its access
  * crosses pages, or one per element in the iterative fault checker.
  */
object SaturnTrace {

  /** Where a micro-op is, by the manual's stages. */
  object VuopStage extends ChiselEnum {
    val Dispatch = Value // the dispatch queue
    val Issue    = Value // its issue queues
    val Sequence = Value // the load, store, or execute sequencer that frees its age tag
  }

  /** Why the micro-op at the head of the dispatch queue waits: a sequencer it needs holds an older micro-op, and that
    * sequencer's issue queue, if it has entries, is full.
    */
  object VuopStallCause extends ChiselEnum {
    val load    = Value // the load sequencer
    val store   = Value // the store sequencer
    val special = Value // the special sequencer
    val execute = Value // an execute sequencer
  }

  /** One micro-op. */
  class Vuop(host: EventTrace.ObjectType)(implicit p: Parameters) extends EventTrace.ObjectType("vuop") {
    private val inst = new VectorIssueInst
    val instruction = const(Id(host))                 // the host core's instruction it comes from
    val pc          = const(inst.pc.cloneType)
    val bits        = const(inst.bits.cloneType)
    val vat         = const(inst.vat.cloneType)       // its age tag, unique among live micro-ops
    val vstart      = const(inst.vstart.cloneType)    // with vl, bounds the elements it covers
    val vl          = const(inst.vconfig.vl.cloneType)
    val stage       = mutable(VuopStage())
  }
}

/** EventTrace: the micro-ops, which Saturn's glue for a host core declares and binds for the modules it builds; `None`
  * where the glue binds none, as Shuttle's.
  */
case object SaturnTraceVuops extends Field[Option[Collection[SaturnTrace.Vuop]]](None)
