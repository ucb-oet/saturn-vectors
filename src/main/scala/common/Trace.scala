package saturn.common

import chisel3._
import org.chipsalliance.cde.config._
import eventtrace.{Collection, EventTrace}

/** What EventTrace records of Saturn, in configs that trace: each micro-op the dispatcher sends to the backend, as the
  * manual calls the unit of dispatch, from its dispatch until the backend frees its age tag. An instruction becomes
  * one micro-op, one per page when its access crosses pages, or one per element in the iterative fault checker.
  */
object SaturnTrace {

  /** Where a micro-op is, by the manual's stages: Dispatch is the dispatch queue, Issue its issue queues, and Sequence
    * the load, store, or execute sequencer that frees its age tag, even when the special sequencer works on it too.
    */
  object VuopStage extends ChiselEnum {
    val Dispatch, Issue, Sequence = Value
  }

  /** One micro-op. `instruction` is the host core's instruction it comes from, an object of type `host`; `vat` is its
    * age tag, unique among live micro-ops; `vstart` and `vl` bound the elements it covers.
    */
  class Vuop(host: EventTrace.ObjectType)(implicit p: Parameters) extends EventTrace.ObjectType("vuop") {
    private val inst = new VectorIssueInst
    val instruction = const(Id(host))
    val pc     = const(inst.pc.cloneType)
    val bits   = const(inst.bits.cloneType)
    val vat    = const(inst.vat.cloneType)
    val vstart = const(inst.vstart.cloneType)
    val vl     = const(inst.vconfig.vl.cloneType)
    val stage  = mutable(VuopStage())
  }
}

/** EventTrace: the micro-ops, which Saturn's glue for a host core declares and binds for the modules it builds, so that
  * they can trace them; `None` where the glue passes none, as Shuttle's does for now.
  */
case object SaturnTraceVuops extends Field[Option[Collection[SaturnTrace.Vuop]]](None)
