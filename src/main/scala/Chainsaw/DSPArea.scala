package Chainsaw

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real

abstract class DSPArea[outputType <: Data, testCaseType, testResultType] extends ImplicitArea[outputType] {
  def timing: TimingInfo

  def referenceModel(testCase: testCaseType): testResultType
}
