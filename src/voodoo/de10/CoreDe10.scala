package voodoo.de10

import spinal.core._
import spinal.core.formal._
import spinal.lib._
import spinal.lib.bus.avalon._
import spinal.lib.bus.bmb._
import voodoo.{Config, Core}
import voodoo.hdmi.HdmiScanoutPort

/** DE10-oriented wrapper around Core.
  *
  * The MMIO bundle name is kept for compatibility with the host-sim harness, but the board path
  * uses the full HPS-to-FPGA aperture while framebuffer and texture traffic still exit through the
  * FPGA-to-SDRAM Avalon-MM ports.
  */
case class H2fLwMmio(addressWidth: Int) extends Bundle with IMasterSlave {
  val address = UInt(addressWidth bits)
  val read = Bool()
  val write = Bool()
  val byteenable = Bits(4 bits)
  val writedata = Bits(32 bits)
  val waitrequest = Bool()
  val readdata = Bits(32 bits)
  val readdatavalid = Bool()

  override def asMaster(): Unit = {
    out(address, read, write, byteenable, writedata)
    in(waitrequest, readdata, readdatavalid)
  }
}

case class H2fLwToBmbBridge(
    addressWidth: Int,
    bmbParams: BmbParameter,
    timeoutLog2: Int = 20
) extends Component {
  val io = new Bundle {
    val h2fLw = slave(H2fLwMmio(addressWidth - 2))
    val cpuBus = master(Bmb(bmbParams))
  }

  private val timeoutLimit = 1 << timeoutLog2
  private val wedgedStatusAddr = U(0x254, addressWidth bits)
  private val debugLiveAddr = U(0x258, addressWidth bits)
  private val debugCapturedAddressAddr = U(0x25c, addressWidth bits)
  private val debugCapturedDataAddr = U(0x260, addressWidth bits)
  private val debugCapturedMetaAddr = U(0x264, addressWidth bits)

  val cmdAddress = (io.h2fLw.address.resize(addressWidth) |<< 2)
  val writeReq = io.h2fLw.write
  val readReq = io.h2fLw.read
  val illegalReq = writeReq && readReq
  val selectedReadReq = !writeReq && readReq
  val hasReq = writeReq || readReq
  val statusReadReq = selectedReadReq && cmdAddress === wedgedStatusAddr
  val debugLiveReadReq = selectedReadReq && cmdAddress === debugLiveAddr
  val debugCapturedAddressReadReq = selectedReadReq && cmdAddress === debugCapturedAddressAddr
  val debugCapturedDataReadReq = selectedReadReq && cmdAddress === debugCapturedDataAddr
  val debugCapturedMetaReadReq = selectedReadReq && cmdAddress === debugCapturedMetaAddr
  val debugReadReq = statusReadReq || debugLiveReadReq || debugCapturedAddressReadReq ||
    debugCapturedDataReadReq || debugCapturedMetaReadReq

  io.cpuBus.cmd.last := True
  io.cpuBus.cmd.source := 0
  io.cpuBus.cmd.opcode := Bmb.Cmd.Opcode.READ
  io.cpuBus.cmd.address := cmdAddress
  io.cpuBus.cmd.length := 3
  io.cpuBus.cmd.data := io.h2fLw.writedata
  io.cpuBus.cmd.mask := io.h2fLw.byteenable
  val cmdInFlight = RegInit(False)
  val readRspPending = RegInit(False)
  val readData = Reg(Bits(32 bits)) init (0)
  val readDataValid = RegInit(False)
  val readDataPending = RegInit(False)
  val wedged = RegInit(False)
  val requestStallCycles = Reg(UInt((timeoutLog2 + 1) bits)) init (0)
  val responseStallCycles = Reg(UInt((timeoutLog2 + 1) bits)) init (0)
  val wedgedWriteStallCycles = Reg(UInt((timeoutLog2 + 1) bits)) init (0)
  val droppedWrites = Reg(UInt(8 bits)) init (0)
  val requestTimeoutSeen = RegInit(False)
  val responseTimeoutSeen = RegInit(False)
  val capturedValid = RegInit(False)
  val capturedAddress = Reg(UInt(addressWidth bits)) init (0)
  val capturedData = Reg(Bits(32 bits)) init (0)
  val capturedMeta = Reg(Bits(32 bits)) init (0)

  val cmdBlocked = cmdInFlight || readRspPending || readDataPending || readDataValid
  val wedgedReadReq = selectedReadReq && wedged && !debugReadReq
  val localReadReq = debugReadReq || wedgedReadReq
  val localWriteReq = writeReq && wedged
  val locallyHandledReq = localReadReq || localWriteReq
  val readResponseSlotAvailable = !readDataPending && !readDataValid
  val localReadAccept = localReadReq && readResponseSlotAvailable
  val localWriteAccept = localWriteReq && !illegalReq
  val localAccept = hasReq && !illegalReq && (localReadAccept || localWriteAccept)
  val canIssueReq = hasReq && !illegalReq && !cmdBlocked && !locallyHandledReq
  val acceptWindow = canIssueReq && io.cpuBus.cmd.ready
  val requestStalled =
    hasReq && !illegalReq && !locallyHandledReq && !acceptWindow && (cmdBlocked || !io.cpuBus.cmd.ready)
  val requestStallLimitReached = requestStalled && requestStallCycles === U(
    timeoutLimit - 1,
    requestStallCycles.getWidth bits
  )
  val requestTimeoutAccept = requestStallLimitReached && (writeReq || readResponseSlotAvailable)
  val requestTimeoutRead = requestTimeoutAccept && selectedReadReq
  val requestTimeoutWrite = requestTimeoutAccept && writeReq
  val localOrRequestTimeoutRead = localReadAccept || requestTimeoutRead

  io.cpuBus.rsp.ready := readResponseSlotAvailable && !localOrRequestTimeoutRead
  val rspFire = io.cpuBus.rsp.valid && io.cpuBus.rsp.ready
  val cpuCmdFire = io.cpuBus.cmd.valid && io.cpuBus.cmd.ready
  val rspCompletesCurrent = rspFire && (cmdInFlight || acceptWindow)
  val rspIsRead = (cmdInFlight && readRspPending) || acceptWindow && selectedReadReq
  val responseStallLimitReached = cmdInFlight && !rspFire && responseStallCycles === U(
    timeoutLimit - 1,
    responseStallCycles.getWidth bits
  )
  val responseTimeoutRead =
    responseStallLimitReached && readRspPending && readResponseSlotAvailable && !localOrRequestTimeoutRead
  val responseTimeoutWrite = responseStallLimitReached && !readRspPending
  val responseTimeout = responseTimeoutRead || responseTimeoutWrite
  val wedgeNow = requestTimeoutAccept || responseTimeout

  val statusReadData = B(32 bits, default -> False)
  statusReadData(0) := wedged || wedgeNow
  statusReadData(1) := requestTimeoutSeen || requestTimeoutAccept
  statusReadData(2) := responseTimeoutSeen || responseTimeout
  statusReadData(4) := cmdInFlight
  statusReadData(5) := readRspPending
  statusReadData(8, 8 bits) := droppedWrites.asBits
  statusReadData(16, 16 bits) := wedgedWriteStallCycles.resize(16 bits).asBits

  val debugLiveData = B(32 bits, default -> False)
  debugLiveData(0) := hasReq
  debugLiveData(1) := writeReq
  debugLiveData(2) := readReq
  debugLiveData(3) := illegalReq
  debugLiveData(4) := debugReadReq
  debugLiveData(5) := localWriteReq
  debugLiveData(6) := localAccept
  debugLiveData(7) := acceptWindow
  debugLiveData(8) := io.cpuBus.cmd.valid
  debugLiveData(9) := io.cpuBus.cmd.ready
  debugLiveData(10) := cpuCmdFire
  debugLiveData(11) := io.cpuBus.rsp.valid
  debugLiveData(12) := io.cpuBus.rsp.ready
  debugLiveData(13) := rspFire
  debugLiveData(14) := cmdBlocked
  debugLiveData(15) := cmdInFlight
  debugLiveData(16) := readRspPending
  debugLiveData(17) := readDataPending
  debugLiveData(18) := readDataValid
  debugLiveData(19) := requestStalled
  debugLiveData(20) := requestTimeoutAccept
  debugLiveData(21) := responseTimeout
  debugLiveData(22) := wedged
  debugLiveData(23) := wedgeNow
  debugLiveData(24) := readResponseSlotAvailable
  debugLiveData(25) := locallyHandledReq
  debugLiveData(26) := canIssueReq
  debugLiveData(27) := localReadAccept
  debugLiveData(28) := localWriteAccept
  debugLiveData(29) := rspCompletesCurrent
  debugLiveData(30) := selectedReadReq
  debugLiveData(31) := capturedValid

  val capturedAddressData = B(32 bits, default -> False)
  capturedAddressData(addressWidth - 1 downto 0) := capturedAddress.asBits

  val localReadData = B(32 bits, default -> False)
  when(statusReadReq) {
    localReadData := statusReadData
  }
  when(debugLiveReadReq) {
    localReadData := debugLiveData
  }
  when(debugCapturedAddressReadReq) {
    localReadData := capturedAddressData
  }
  when(debugCapturedDataReadReq) {
    localReadData := capturedData
  }
  when(debugCapturedMetaReadReq) {
    localReadData := capturedMeta
  }

  io.h2fLw.waitrequest := hasReq && !(localAccept || requestTimeoutAccept || acceptWindow)
  io.h2fLw.readdata := readData
  io.h2fLw.readdatavalid := readDataValid

  readDataValid := False

  when(requestStalled) {
    when(requestTimeoutAccept) {
      requestStallCycles := 0
    } elsewhen (requestStallCycles =/= U(timeoutLimit - 1, requestStallCycles.getWidth bits)) {
      requestStallCycles := requestStallCycles + 1
    }
  } otherwise {
    requestStallCycles := 0
  }

  when(cmdInFlight && !rspFire) {
    when(responseTimeout) {
      responseStallCycles := 0
    } elsewhen (responseStallCycles =/= U(timeoutLimit - 1, responseStallCycles.getWidth bits)) {
      responseStallCycles := responseStallCycles + 1
    }
  } otherwise {
    responseStallCycles := 0
  }

  when(requestTimeoutAccept) {
    requestTimeoutSeen := True
  }

  when(responseTimeout) {
    responseTimeoutSeen := True
  }

  val captureNow = hasReq && !illegalReq && !debugReadReq &&
    (requestStalled || acceptWindow || localWriteAccept || requestTimeoutAccept)
  val captureMetaNow = B(32 bits, default -> False)
  captureMetaNow(0) := True
  captureMetaNow(1) := writeReq
  captureMetaNow(2) := selectedReadReq
  captureMetaNow(3) := requestStalled
  captureMetaNow(4) := acceptWindow
  captureMetaNow(5) := requestTimeoutAccept
  captureMetaNow(6) := responseTimeout
  captureMetaNow(7) := localWriteAccept
  captureMetaNow(8) := io.cpuBus.cmd.ready
  captureMetaNow(9) := cmdBlocked
  captureMetaNow(10) := wedged
  captureMetaNow(12, 4 bits) := io.h2fLw.byteenable
  captureMetaNow(16, 16 bits) := requestStallCycles.resize(16 bits).asBits

  when(captureNow) {
    capturedValid := True
    capturedAddress := cmdAddress
    capturedData := io.h2fLw.writedata
    capturedMeta := captureMetaNow
  }

  when(wedgeNow) {
    wedged := True
    wedgedWriteStallCycles := U(timeoutLimit, wedgedWriteStallCycles.getWidth bits)
  }

  io.cpuBus.cmd.valid := canIssueReq
  when(writeReq) {
    io.cpuBus.cmd.opcode := Bmb.Cmd.Opcode.WRITE
  } otherwise {
    io.cpuBus.cmd.opcode := Bmb.Cmd.Opcode.READ
  }

  when(localReadAccept) {
    readData := localReadData
    readDataPending := True
  }

  when(localWriteAccept || requestTimeoutWrite) {
    when(droppedWrites =/= droppedWrites.maxValue) {
      droppedWrites := droppedWrites + 1
    }
  }

  when(acceptWindow && !rspCompletesCurrent) {
    cmdInFlight := True
    readRspPending := selectedReadReq
  }

  when(rspCompletesCurrent) {
    cmdInFlight := False
    when(rspIsRead) {
      readData := io.cpuBus.rsp.data
      readDataPending := True
    }
    readRspPending := False
  }

  when(requestTimeoutRead || responseTimeoutRead) {
    readData := B(32 bits, default -> False)
    readDataPending := True
  }

  when(responseTimeout) {
    cmdInFlight := False
    readRspPending := False
  }

  when(readDataPending) {
    readDataPending := False
    readDataValid := True
  }

  GenerationFlags.formal {
    when(acceptWindow) {
      assert(io.cpuBus.cmd.valid)
      assert(io.cpuBus.cmd.address === cmdAddress)
      assert(io.cpuBus.cmd.length === 3)
      assert(io.cpuBus.cmd.mask === io.h2fLw.byteenable)
      assert(io.cpuBus.cmd.data === io.h2fLw.writedata)
      assert(io.cpuBus.cmd.source === 0)
      assert(io.cpuBus.cmd.last)
      when(writeReq) {
        assert(io.cpuBus.cmd.opcode === Bmb.Cmd.Opcode.WRITE)
      } otherwise {
        assert(io.cpuBus.cmd.opcode === Bmb.Cmd.Opcode.READ)
      }
    }

    when(illegalReq) {
      assert(io.h2fLw.waitrequest)
      assert(!io.cpuBus.cmd.valid)
    }

    when(cmdBlocked && hasReq && !locallyHandledReq && !requestTimeoutAccept) {
      assert(io.h2fLw.waitrequest)
    }

    when(canIssueReq && !io.cpuBus.cmd.ready && !requestTimeoutAccept) {
      assert(io.h2fLw.waitrequest)
      assert(io.cpuBus.cmd.valid)
    }

    when(localReadAccept) {
      assert(!io.h2fLw.waitrequest)
      assert(!io.cpuBus.cmd.valid)
    }

    when(localWriteAccept) {
      assert(!io.h2fLw.waitrequest)
      assert(!io.cpuBus.cmd.valid)
    }

    when(requestTimeoutAccept) {
      assert(!io.h2fLw.waitrequest)
      assert(!io.cpuBus.cmd.fire)
      assert(wedgeNow)
    }

    when(acceptWindow && selectedReadReq && rspFire) {
      assert(!cmdInFlight)
      assert(!readRspPending)
    }

    when(rspCompletesCurrent) {
      assert(io.cpuBus.rsp.last)
      assert(io.cpuBus.rsp.source === 0)
    }

    when(readDataValid) {
      assert(io.h2fLw.readdatavalid)
      assert(io.h2fLw.readdata === readData)
    }

    cover(io.cpuBus.cmd.fire && io.cpuBus.cmd.opcode === Bmb.Cmd.Opcode.WRITE)
    cover(io.cpuBus.cmd.fire && io.cpuBus.cmd.opcode === Bmb.Cmd.Opcode.READ)
    cover(acceptWindow && rspFire && selectedReadReq)
    cover(requestTimeoutAccept)
    cover(responseTimeout)
  }
}

case class CoreDe10(c: Config) extends Component {
  private val cpuAddressWidth = Core.cpuBmbParams.access.addressWidth

  val io = new Bundle {
    val h2fLw = slave(H2fLwMmio(cpuAddressWidth - 2))
    val memFbWrite = master(
      AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth))
    )
    val memFbColorWrite = master(
      AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth))
    )
    val memFbAuxWrite = master(
      AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth))
    )
    val memFbColorRead = master(
      AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth))
    )
    val memFbAuxRead = master(
      AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth))
    )
    val memTex = master(AvalonMM(De10MemBackend.avalonConfig(De10MemBackend.physicalAddressWidth)))
    val hdmi = master(HdmiScanoutPort(c))
  }

  val core = Core(c)
  val memBackend = De10MemBackend(c)
  val h2fBridge = H2fLwToBmbBridge(cpuAddressWidth, Core.cpuBmbParams)

  io.h2fLw <> h2fBridge.io.h2fLw
  core.io.cpuBus <> h2fBridge.io.cpuBus

  Seq(
    core.io.fbMemWrite <> memBackend.io.fbMemWrite,
    core.io.fbColorWriteMem <> memBackend.io.fbColorWriteMem,
    core.io.fbAuxWriteMem <> memBackend.io.fbAuxWriteMem,
    core.io.fbColorReadMem <> memBackend.io.fbColorReadMem,
    core.io.fbAuxReadMem <> memBackend.io.fbAuxReadMem,
    core.io.texMem <> memBackend.io.texMem,
    io.memFbWrite <> memBackend.io.memFbWrite,
    io.memFbColorWrite <> memBackend.io.memFbColorWrite,
    io.memFbAuxWrite <> memBackend.io.memFbAuxWrite,
    io.memFbColorRead <> memBackend.io.memFbColorRead,
    io.memFbAuxRead <> memBackend.io.memFbAuxRead,
    io.memTex <> memBackend.io.memTex
  )

  core.io.statusInputs.vRetrace := core.io.hdmi.status.newFrame
  core.io.statusInputs.memFifoFree := 0xffff
  core.io.statusInputs.pciInterrupt := False
  core.io.hdmi.clock := io.hdmi.clock
  core.io.hdmi.reset := io.hdmi.reset
  io.hdmi.video := core.io.hdmi.video
  io.hdmi.status := core.io.hdmi.status
  io.hdmi.underflow := core.io.hdmi.underflow
  io.hdmi.fifoLevel := core.io.hdmi.fifoLevel

  core.io.fbBaseAddr := 0
  core.io.flushFbCaches := False
}
