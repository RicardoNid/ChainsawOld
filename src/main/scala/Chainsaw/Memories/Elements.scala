package Chainsaw.Memories

import Chainsaw._
import spinal.core._
import spinal.core.sim._
import spinal.lib._

case class XILINX_BRAM_PORT(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val dataIn = Bits(dataWidth bits)
  val addr = UInt(addressWidth bits)
  val dataOut = Bits(dataWidth bits)
  val en = Bool
  val we = Bool

  override def asMaster(): Unit = {
    in(dataOut)
    out(dataIn, addr, we, en)
  }

  def >>(readPort: XILINX_BRAM_PORT_READ): Unit = {
    addr := readPort.addr
    en := readPort.en
    we := False
    dataIn := dataIn.getZero // TODO: don't care
    readPort.dataOut := dataOut
  }

  def <<(writePort: XILINX_BRAM_PORT_WRITE): Unit = {
    addr := writePort.addr
    en := writePort.en
    we := writePort.we
    dataIn := writePort.dataIn
  }

  def read(addrIn: UInt) = {
    addr := addrIn
    en := True
    we := False
    dataIn := dataIn.getZero // TODO: don't care
    dataOut
  }

  def write(addrIn: UInt, data: Bits) = {
    addr := addrIn
    en := True
    we := True
    dataIn := data
  }

  def preAssign(): Unit = {
    addr.clearAll()
    addr.allowOverride
    en.clear()
    en.allowOverride
    we.clear()
    we.allowOverride
    dataIn.clearAll()
    dataIn.allowOverride
  }
}

case class XILINX_BRAM_PORT_READ(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val addr = UInt(addressWidth bits)
  val dataOut = Bits(dataWidth bits)
  val en = Bool
  override def asMaster(): Unit = {
    in(dataOut)
    out(addr, en)
  }
}

case class XILINX_BRAM_PORT_WRITE(dataWidth: Int, addressWidth: Int) extends Bundle with IMasterSlave {
  val addr = UInt(addressWidth bits)
  val dataIn = Bits(dataWidth bits)
  val en = Bool
  val we = Bool
  override def asMaster(): Unit = {
    out(dataIn, addr, en, we)
  }
}

case class XILINX_BRAM18E2() extends Component {

  val dataWidth = 2 * (8 + 1) // core width = 36
  val addressWidth = 10

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)
}

case class XILINX_BRAM36E2() extends Component {

  val dataWidth = 4 * (8 + 1) // core width = 36
  val addressWidth = 10

  val ioA = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val ioB = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  val bram = Mem(Bits(dataWidth bits), BigInt(1) << addressWidth)

  ioA.dataOut := bram.readWriteSync(ioA.addr, ioA.dataIn, ioA.en, ioA.we)
  ioB.dataOut := bram.readWriteSync(ioB.addr, ioB.dataIn, ioB.en, ioB.we)
}

case class mWnRRAM(m: Int, n: Int) extends Component {

  //  // replication
  //  val io = new Bundle {
  //    val reads = Seq.fill(n)(slave(XILINX_BRAM_PORT_READ(18, 10)))
  //    val writes = Seq.fill(m)(slave(XILINX_BRAM_PORT_WRITE(18, 10)))
  //  }
  //
  //  // replication
  //  require(m == 1)
  //  val rams = Seq.fill(n)(XILINX_BRAM18E2())
  //  rams.zip(io.reads).foreach { case (ram, port) => ram.ioA >> port }
  //  rams.zip(io.writes).foreach { case (ram, port) => ram.ioA << port }

  // multipumping
  val io = new Bundle {
    val reads = Seq.fill(n)(slave(XILINX_BRAM_PORT_READ(18, 10)))
    val writes = Seq.fill(m)(slave(XILINX_BRAM_PORT_WRITE(18, 10)))
  }
  io.reads.foreach(_.dataOut.clearAll())

  // multipumping
  val ram = XILINX_BRAM18E2()
  ram.ioA.preAssign()
  ram.ioB.preAssign()

  val writeData = io.writes.head.dataIn +: io.writes.tail.map(port => RegNext(port.dataIn))
  val writeAddr = io.writes.head.addr +: io.writes.tail.map(port => RegNext(port.addr))
  val readAddr = io.reads.map(port => RegNext(port.addr))
  val readData = io.reads.init.map(port => Reg(port.dataOut)) :+ io.reads.last.dataOut
  io.reads.foreach(_.dataOut.allowOverride)
  io.reads.init.zip(readData.init).foreach { case (port, reg) => port.dataOut := reg }

  val multiplexCounter = CounterFreeRun(m + n)
  switch(multiplexCounter.value) {

    (0 until n).foreach { readIndex =>
      is(U(readIndex)) {
        readData(readIndex) := ram.ioA.read(readAddr(readIndex))
      }
    }
    (0 until m).foreach { writeIndex =>
      is(U(writeIndex + n)) {
        ram.ioA.write(writeAddr(writeIndex), writeData(writeIndex))
      }
    }
  }
}

case class MemoryArea() extends Component {
  val m0 = XILINX_BRAM36E2()

  val dataWidth = 4 * (8 + 1) // core width = 36
  val addressWidth = 10

  val io0 = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))
  val io1 = slave(XILINX_BRAM_PORT(dataWidth, addressWidth))

  io0 <> m0.ioA
  io1 <> m0.ioB
}

object XILINX_BRAM18E2 {
  def main(args: Array[String]): Unit = {
    GenRTL(new mWnRRAM(1, 3))
    //    VivadoSynth(new MemoryArea)
    //    VivadoImpl(new mWnRRAM(1, 3))
    SimConfig.withWave.compile(new mWnRRAM(1, 3)).doSim { dut =>
      import dut._
      clockDomain.forkStimulus(2)

      def write(port: Int, addr: BigInt, data: BigInt) = {
        io.writes(port).addr #= addr
        io.writes(port).dataIn #= data
        io.writes(port).en #= true
        io.writes(port).we #= true
      }

      def read(port: Int, addr: BigInt) = {
        val readPort = io.reads(port)
        readPort.addr #= addr
        readPort.en #= true
        readPort.dataOut.toInt
      }

      write(0, 0, 13)
      clockDomain.waitSampling()
      write(0, 1, 17)
      read(0, 1)
      read(1, 2)
      read(2, 3)
      clockDomain.waitSampling()
      write(0, 2, 19)
      read(0, 1)
      read(1, 2)
      read(2, 3)
      clockDomain.waitSampling()
      write(0, 3, 23)
      read(0, 1)
      read(1, 2)
      read(2, 3)
      clockDomain.waitSampling()
      write(0, 4, 29)
      read(0, 1)
      read(1, 2)
      read(2, 3)
      sleep(20)
    }
  }
}
