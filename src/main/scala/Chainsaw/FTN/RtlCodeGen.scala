package Chainsaw.FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.matlabIO._
import Chainsaw.dspTest._

object RtlCodeGen extends App {


  //  GenRTL(TxOnBoard()(FtnParams(2, 256, doBitAlloc = false)), name = "TxOnBoardFullNotAlloc")
  //  GenRTL(TxOnBoard()(FtnParams(3, 226, doBitAlloc = false)), name = "TxOnBoardCompressedNotAlloc")
  //  GenRTL(TxOnBoard()(FtnParams(2, 256, doBitAlloc = false, useSyncData = false, useGivenAlloc = true)), name = "TxOnBoardFullAllocFor0KM")

  prepareAlloc("2_256_N15_0KM")
  GenRTL(TxOnBoard()(FtnParams(2, 256, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)), name = "TxOnBoardFullAllocFor0KM")
  prepareAlloc("2_256_N15_20KM")
  GenRTL(TxOnBoard()(FtnParams(2, 256, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)), name = "TxOnBoardFullAllocFor20KM")
  prepareAlloc("3_226_N15_0KM")
  GenRTL(TxOnBoard()(FtnParams(3, 226, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)), name = "TxOnBoardCompressedAllocFor0KM")
  prepareAlloc("3_226_N15_20KM")
  GenRTL(TxOnBoard()(FtnParams(3, 226, doBitAlloc = true, useSyncData = false, useGivenAlloc = true)), name = "TxOnBoardCompressedAllocFor20KM")

  //  GenRTL(RxOnBoard(512)(FtnParams(2, 256, doBitAlloc = false)), name = "RxOnBoardFullNotAlloc") // without bitAlloc, not compressed
  //  GenRTL(RxOnBoard(512)(FtnParams(2, 256, doBitAlloc = true)), name = "RxOnBoardFullAlloc") // with bitAlloc, not compressed
  //  GenRTL(RxOnBoard(512)(FtnParams(3, 226, doBitAlloc = false)), name = "RxOnBoardCompressedNotAlloc ") // without bitAlloc, compressed
  //  GenRTL(RxOnBoard(512)(FtnParams(3, 226, doBitAlloc = true)), name = "RxOnBoardCompressedAlloc ") // with bitAlloc, compressed
}
