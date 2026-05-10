//> using target.scope test

package voodoo.de10

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._
import scala.language.reflectiveCalls

class H2fLwToBmbBridgeTest extends AnyFunSuite {
  private val statusByteAddress = 0x254
  private val debugLiveByteAddress = 0x258
  private val capturedAddressByteAddress = 0x25c
  private val capturedDataByteAddress = 0x260
  private val capturedMetaByteAddress = 0x264

  private def init(dut: H2fLwToBmbBridge): Unit = {
    dut.io.h2fLw.address #= 0
    dut.io.h2fLw.read #= false
    dut.io.h2fLw.write #= false
    dut.io.h2fLw.byteenable #= 0xf
    dut.io.h2fLw.writedata #= 0

    dut.io.cpuBus.cmd.ready #= false
    dut.io.cpuBus.rsp.valid #= false
    dut.io.cpuBus.rsp.last #= true
    dut.io.cpuBus.rsp.source #= 0
    dut.io.cpuBus.rsp.opcode #= 0
    dut.io.cpuBus.rsp.data #= 0
  }

  private def waitUntil(dut: H2fLwToBmbBridge, maxCycles: Int, clue: String)(
      cond: => Boolean
  ): Unit = {
    var cycles = 0
    while (!cond && cycles < maxCycles) {
      dut.clockDomain.waitSampling()
      cycles += 1
    }
    assert(cond, clue)
  }

  private def mmioRead(dut: H2fLwToBmbBridge, byteAddress: Int, maxWait: Int = 64): Long = {
    dut.io.h2fLw.address #= (byteAddress >> 2)
    dut.io.h2fLw.byteenable #= 0xf
    dut.io.h2fLw.read #= true
    dut.clockDomain.waitSampling()
    waitUntil(dut, maxWait, f"read 0x$byteAddress%06x did not get accepted") {
      !dut.io.h2fLw.waitrequest.toBoolean
    }
    dut.io.h2fLw.read #= false
    waitUntil(dut, maxWait, f"read 0x$byteAddress%06x did not return data") {
      dut.io.h2fLw.readdatavalid.toBoolean
    }
    val value = dut.io.h2fLw.readdata.toLong & 0xffffffffL
    dut.clockDomain.waitSampling()
    value
  }

  private def mmioWrite(
      dut: H2fLwToBmbBridge,
      byteAddress: Int,
      value: Long,
      maxWait: Int = 64
  ): Unit = {
    dut.io.h2fLw.address #= (byteAddress >> 2)
    dut.io.h2fLw.writedata #= value
    dut.io.h2fLw.byteenable #= 0xf
    dut.io.h2fLw.write #= true
    dut.clockDomain.waitSampling()
    waitUntil(dut, maxWait, f"write 0x$byteAddress%06x did not get accepted") {
      !dut.io.h2fLw.waitrequest.toBoolean
    }
    dut.io.h2fLw.write #= false
    dut.clockDomain.waitSampling()
  }

  private def bridge() = H2fLwToBmbBridge(24, voodoo.Core.cpuBmbParams, timeoutLog2 = 3)

  test("read command that is never accepted returns a timeout response") {
    SimConfig.withIVerilog.compile(bridge()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      init(dut)
      dut.clockDomain.waitSampling()

      val value = mmioRead(dut, 0x400000, maxWait = 32)
      assert(value == 0L)

      val status = mmioRead(dut, statusByteAddress, maxWait = 16)
      assert((status & 0x1L) != 0, f"expected wedged bit in status 0x$status%08x")
      assert((status & 0x2L) != 0, f"expected request-timeout bit in status 0x$status%08x")
    }
  }

  test("accepted read command that never responds returns a timeout response") {
    SimConfig.withIVerilog.compile(bridge()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      init(dut)
      dut.io.cpuBus.cmd.ready #= true
      dut.clockDomain.waitSampling()

      val value = mmioRead(dut, 0x400000, maxWait = 32)
      assert(value == 0L)

      val status = mmioRead(dut, statusByteAddress, maxWait = 16)
      assert((status & 0x1L) != 0, f"expected wedged bit in status 0x$status%08x")
      assert((status & 0x4L) != 0, f"expected response-timeout bit in status 0x$status%08x")
    }
  }

  test("status read is local while a write response is stuck") {
    SimConfig.withIVerilog.compile(bridge()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      init(dut)
      dut.io.cpuBus.cmd.ready #= true
      dut.clockDomain.waitSampling()

      mmioWrite(dut, 0x100, 0xdeadbeefL, maxWait = 8)

      val status = mmioRead(dut, statusByteAddress, maxWait = 6)
      assert((status & 0x1L) == 0, f"status read should beat timeout, got 0x$status%08x")
      assert((status & 0x10L) != 0, f"expected in-flight bit in status 0x$status%08x")

      val live = mmioRead(dut, debugLiveByteAddress, maxWait = 6)
      assert((live & (1L << 14)) != 0, f"expected blocked bit in live debug 0x$live%08x")
      assert((live & (1L << 15)) != 0, f"expected in-flight bit in live debug 0x$live%08x")
    }
  }

  test("debug capture records a stalled write timeout") {
    SimConfig.withIVerilog.compile(bridge()).doSim { dut =>
      dut.clockDomain.forkStimulus(10)
      init(dut)
      dut.clockDomain.waitSampling()

      mmioWrite(dut, 0x110, 0x00000300L, maxWait = 32)

      val status = mmioRead(dut, statusByteAddress, maxWait = 16)
      assert((status & 0x1L) != 0, f"expected wedged bit in status 0x$status%08x")
      assert((status & 0x2L) != 0, f"expected request-timeout bit in status 0x$status%08x")

      assert(mmioRead(dut, capturedAddressByteAddress, maxWait = 16) == 0x110L)
      assert(mmioRead(dut, capturedDataByteAddress, maxWait = 16) == 0x00000300L)
      val meta = mmioRead(dut, capturedMetaByteAddress, maxWait = 16)
      assert((meta & 0x1L) != 0, f"expected valid capture metadata 0x$meta%08x")
      assert((meta & 0x2L) != 0, f"expected captured write metadata 0x$meta%08x")
      assert((meta & 0x8L) != 0, f"expected captured stalled metadata 0x$meta%08x")
      assert((meta & 0x20L) != 0, f"expected captured timeout metadata 0x$meta%08x")
      assert(((meta >> 12) & 0xfL) == 0xfL, f"expected captured byteenable metadata 0x$meta%08x")
    }
  }
}
