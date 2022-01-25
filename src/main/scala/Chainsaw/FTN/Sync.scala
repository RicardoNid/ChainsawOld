package Chainsaw.FTN


import com.github.tototoshi.csv._
import java.io.File

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._
import java.lang.Thread.sleep


object Sync {

  def readCsvFromUpper(filepath: String) = CSVReader.open(new File(filepath)).all().flatten.toArray.map(_.toInt)

  def bits768ToInts(bits: String) = {
    BigInt(bits, 16)
      .toString(2).padToLeft(768, '0')
      .grouped(6).toSeq
      .map(bits => BigInt(bits, 2))
      .map(_.toInt).reverse
  }

  def readCsvFromIla(filepath: String) = CSVReader.open(new File(filepath)).all()
    .tail.map(_.apply(3)).toArray.flatMap(bits768ToInts)

  def sync(preamble: Array[Int], rx: Array[Int]) = {
    eng.putVariable("preamble", preamble)
    eng.putVariable("rx", rx)

    eng.eval(s"[cor, lags] = xcorr(rx, preamble);")
    eng.eval(s"plot(lags, cor);")

    val cor = eng.getVariable[Array[Double]]("cor")
    val lags = eng.getVariable[Array[Double]]("lags").map(_.toInt)

    val lag = cor.zip(lags).maxBy(_._1)._2

    val syncPart = rx.slice(lag, lag + preamble.length)
    eng.putVariable("rxSync", syncPart)

    eng.eval(s"plot(rxSync, 'r'); hold on; plot(preamble, 'b'); legend('rx', 'tx')")
    sleep(20000)
    lag
  }

  def main(args: Array[String]): Unit = {

    //  run the following commands in tcl console:
    //  write_hw_ila_data -csv_file -force {/data/AUV901_2022/HDL_AUV901_io/adc_data.csv} hw_ila_data_3
    //  write_hw_ila_data -csv_file -force {/data/AUV901_2022/HDL_AUV901_io/tx_data.csv} hw_ila_data_4

    val preamble = readCsvFromIla("/data/AUV901_2022/HDL_AUV901_io/tx_data.csv").take(256).map(_ - 32)
    val rx = readCsvFromIla("/data/AUV901_2022/HDL_AUV901_io/adc_data.csv").map(32 - _)
    println(sync(preamble, rx))

  }
}
