//> using target.scope test

package voodoo

import spinal.core._
import spinal.core.sim._
import org.scalatest.funsuite.AnyFunSuite
import voodoo.raster._

class SpanWalkerClipDisabledRegressionTest extends AnyFunSuite {
  test("SpanWalker emits spans when clipping is disabled and clip bounds are zero") {
    val c = Config
      .voodoo1(TraceConfig(enabled = true))
      .copy(
        vertexFormat = QFormat(5, 1, true),
        vColorFormat = QFormat(4, 1, true),
        vDepthFormat = QFormat(4, 1, true),
        wAccumFormat = QFormat(4, 1, true),
        coefficientFormat = QFormat(6, 1, true),
        texCoordsFormat = QFormat(4, 1, true),
        texCoordsAccumFormat = QFormat(5, 1, true),
        texCoordsHiFormat = QFormat(5, 1, true),
        addressWidth = 4 bits,
        maxFbDims = (4, 4)
      )

    SimConfig.withIVerilog.withWave.compile(SpanWalker(c)).doSim { dut =>
      dut.clockDomain.forkStimulus(period = 10)
      dut.i.valid #= false
      dut.o.ready #= true
      dut.enableClipping #= false
      dut.clipLeft #= 0
      dut.clipRight #= 0
      dut.clipLowY #= 0
      dut.clipHighY #= 0

      dut.i.payload.xrange(0) #= 0.0
      dut.i.payload.xrange(1) #= 2.0
      dut.i.payload.yrange(0) #= 0.0
      dut.i.payload.yrange(1) #= 2.0
      for (idx <- 0 until 3) {
        dut.i.payload.coeffs(idx).a #= 0.0
        dut.i.payload.coeffs(idx).b #= 0.0
        dut.i.payload.coeffs(idx).c #= 1.0
        dut.i.payload.edgeStart(idx) #= 1.0
      }
      dut.i.payload.grads.all.foreach { grad =>
        grad.start #= 0.0
        grad.d(0) #= 0.0
        grad.d(1) #= 0.0
      }
      dut.i.payload.texHi.sStart #= 0.0
      dut.i.payload.texHi.tStart #= 0.0
      dut.i.payload.texHi.dSdX #= 0.0
      dut.i.payload.texHi.dTdX #= 0.0
      dut.i.payload.texHi.dSdY #= 0.0
      dut.i.payload.texHi.dTdY #= 0.0
      dut.i.payload.hiAlpha.start #= 0.0
      dut.i.payload.hiAlpha.dAdX #= 0.0
      dut.i.payload.hiAlpha.dAdY #= 0.0
      dut.i.payload.config.fbzColorPath
        .assignFromBits(B(0, dut.i.payload.config.fbzColorPath.getBitsWidth bits))
      dut.i.payload.config.fogMode
        .assignFromBits(B(0, dut.i.payload.config.fogMode.getBitsWidth bits))
      dut.i.payload.config.alphaMode
        .assignFromBits(B(0, dut.i.payload.config.alphaMode.getBitsWidth bits))
      dut.i.payload.config.enableClipping #= false
      dut.i.payload.config.clipLeft #= 0
      dut.i.payload.config.clipRight #= 0
      dut.i.payload.config.clipLowY #= 0
      dut.i.payload.config.clipHighY #= 0
      dut.i.payload.config.tmuTextureMode #= 0
      dut.i.payload.config.tmuTexBaseAddr #= 0
      dut.i.payload.config.tmuTexBaseAddr1 #= 0
      dut.i.payload.config.tmuTexBaseAddr2 #= 0
      dut.i.payload.config.tmuTexBaseAddr38 #= 0
      dut.i.payload.config.tmuTLOD #= 0
      dut.i.payload.trace.valid #= true
      dut.i.payload.trace.origin #= Trace.Origin.triangle
      dut.i.payload.trace.drawId #= 0
      dut.i.payload.trace.primitiveId #= 0

      dut.clockDomain.waitSampling()
      dut.i.valid #= true
      dut.clockDomain.waitSamplingWhere(dut.i.ready.toBoolean)
      dut.clockDomain.waitSampling()
      dut.i.valid #= false

      val spans = scala.collection.mutable.ArrayBuffer.empty[(Int, Int, Int)]
      var cycles = 0
      while (cycles < 40 && spans.length < 2) {
        dut.clockDomain.waitSampling()
        cycles += 1
        println(
          f"cycle=$cycles valid=${dut.o.valid.toBoolean}%s xStart=${dut.o.payload.xStart.toDouble}%.1f xEnd=${dut.o.payload.xEnd.toDouble}%.1f y=${dut.o.payload.y.toDouble}%.1f"
        )
        if (dut.o.valid.toBoolean && dut.o.ready.toBoolean) {
          spans += ((
            dut.o.payload.xStart.toDouble.toInt,
            dut.o.payload.xEnd.toDouble.toInt,
            dut.o.payload.y.toDouble.toInt
          ))
        }
      }

      assert(spans.nonEmpty, "expected at least one emitted span with clipping disabled")
      assert(spans.forall(_._2 >= 0), s"expected non-negative xEnd, got $spans")
      assert(spans.head == ((0, 1, 0)), s"unexpected first span: ${spans.headOption}")
    }
  }
}
