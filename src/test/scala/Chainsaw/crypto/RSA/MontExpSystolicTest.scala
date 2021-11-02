package Chainsaw.crypto.RSA

import Chainsaw._
import cc.redberry.rings.scaladsl._
import org.scalatest.funsuite.AnyFunSuite
import spinal.core.sim._

import scala.collection.mutable.ArrayBuffer

case class MontExpTestCase(modeId: Int, changeKey: Boolean = true,
                           useGivenValue: Boolean = false, X: BigInt = 0, M: BigInt = 0, E: BigInt = 0)

class MontExpSystolicTest extends AnyFunSuite {

  test("testMontExpSystolic") {

    val testConfig = MontConfig(lMs = Seq(512, 1024, 2048, 3072, 4096), w = 32)

    val doGen = false // generate HDL in project directory
    val doSim = true // do verilator simulation
    val simTimes = 1
    val doSynth = false // do vivado synthesis
    val doImpl = false // do vivado implementation
    val work = false
    val comparStageByStage = false

    // this will go through every mode
    def standardTestCases(config: MontConfig) =
      config.lMs.indices.map(i => MontExpTestCase(i))

        val badCase = MontExpTestCase(2, true, true,
          BigInt("3413545443985113752419844809784064354432606935877924366288225382664406136211942824284432162052750832330323501573250034712681840284632560827574113734113980506590600535120596800223465077193205191477768017463559600865112388609798703680332122187031514708932866204348626900865142963706285322309991749308163012476300877554748739773723459764897544204147908689094921784968596513819376200199639664857808549805619124184404656127142375404078705315282986097317121116421703670660157545447595721535141826531263098419286343867952423753789989786969319953424635121282000722851539703733244829187900903557843461636068903981098152902"),
          BigInt("31055232443801959172211143249922933196182302701931966909512500835954563189047767771409742924003049586568000900401546250738162742294413315121581757024855038338942735235146259028874073180665699288383912268787554673477772572218856554473449422556804465892237365025707758189888691564326646234292032421979611502858358733338289426146617948943522382707519323433021580590341206666045020919901299490589446633391590238086372633996340608513356176715368129090713744003533103664002228263296365062770186162346662994297220370672019912823034515031174939396274809351713171267635356560813394758980242261827068581158288595470364170533573"),
          BigInt("21")
        )

            val badTestCases = Seq.fill(1)(badCase)

    def sim(name: String, testConfig: MontConfig, testCases: Seq[MontExpTestCase]) = {
      SimConfig.withWave.workspaceName(name).compile(new MontExpSystolic(testConfig)).doSim { dut =>
        import dut._
        import dut.config._

        var lastModulus, lastRadixSquare, lastExponent = BigInt(0)
        val testWordSize = testConfig.w

        def runTestCases(montExpTestCases: Seq[MontExpTestCase]) = {
          montExpTestCases.foreach { testcase =>
            // preparing data
            val modeId = testcase.modeId
            val currentTestSize = lMs(modeId)
            val currentInstanceNumber = instanceNumber(modeId)
            val currentWordPerInstance = wordPerInstance(modeId)

            val ref = new Refs(currentTestSize)
            ref.refresh()
            val testModulus = if (testcase.useGivenValue) testcase.M
            else if (testcase.changeKey) BigInt(ref.getModulus) else lastModulus

            val testRadix = BigInt(1) << (testModulus.bitLength + 2)
            val testRadixSquare = if (testcase.changeKey) BigInt(Zp(testModulus)(testRadix * testRadix).toByteArray) else lastRadixSquare
            //              val newExponent = BigInt(ref.getPrivateValue)
            val newExponent = BigInt("1001", 2)
            val testExponent = if (testcase.useGivenValue) testcase.E else if (testcase.changeKey) newExponent else lastExponent
            val testExponentLength = testExponent.bitLength

            lastModulus = testModulus
            lastRadixSquare = testRadixSquare
            lastExponent = testExponent

            val testInputs = (0 until currentInstanceNumber).map(_ => if (testcase.useGivenValue) testcase.X else BigInt(ref.getPrivateValue) / DSPRand.nextInt(10000) - DSPRand.nextInt(10000))
            // get words
            require(testExponent % 2 == 1)
            val testExponentWords = testExponent.toString(2).padTo(currentTestSize, '0').grouped(testWordSize).toArray.map(wordString => BigInt(wordString.reverse, 2))
            val testRadixSquareWords = toWords(testRadixSquare, testWordSize, currentTestSize / testWordSize)
            val testModulusWords = toWords(testModulus, testWordSize, currentTestSize / testWordSize)
            val testInputsWords = testInputs.map(input => toWords(input, testWordSize, currentTestSize / testWordSize))
            // get golden
            //          val goldens = testInputs.map(MontAlgos.Arch1ME(_, testExponent, testModulus, testWordSize, print = false))
            // FIXME: much faster, but may not be totally the same as results of Arch1ME
            val goldens = testInputs.map(input =>
              BigInt(Zp(testModulus).pow(input, testExponent).toByteArray))

            def startAMontExp(): Unit = {
              io.start #= true
              io.mode #= BigInt(1) << testcase.modeId
              io.exponentLengthIn #= testExponentLength
              io.keyReset #= testcase.changeKey
              clockDomain.waitSampling()
            }

            def unsetStart() = {
              io.start #= false
              io.mode #= BigInt(0)
              io.exponentLengthIn #= 0
              io.keyReset #= false
            }

            def runForOnce() = {
              val dutResults = Seq.fill(currentInstanceNumber)(ArrayBuffer[BigInt]())
              // monitors
              val montMultCount = testExponent.toString(2).tail.map(_.asDigit + 1).sum + 2
              val runtime = testConfig.IIs(modeId) * montMultCount + es(modeId) + 10
              println(s"estimated MontMultCount = $montMultCount, estimated runtime = $runtime")
              val starterIds = (0 until parallelFactor).filter(_ % groupPerInstance(modeId) == 0)
                .take(parallelFactor / groupPerInstance(modeId))
              val currentDataOuts = io.dataOuts.indices.filter(starterIds.contains(_)).map(io.dataOuts(_))


              def montExpResultMonitor() = if (io.valids(0).toBoolean) dutResults.zip(currentDataOuts).foreach { case (buffer, signal) => buffer += signal.toBigInt }

              startAMontExp()

              (0 until wordPerInstance(modeId)).foreach { i => // STATE = INIT, feed
                if (i == 0) unsetStart()
                io.modulusWordIn #= testModulusWords(i)
                io.radixSquareWordIn #= testRadixSquareWords(i)
                io.exponentWordIn #= testExponentWords(i)
                starterIds.zipWithIndex.foreach { case (starter, inputId) => io.xWordIns(starter) #= testInputsWords(inputId)(i) }
                clockDomain.waitSampling()
              }

              (0 until runtime).foreach { _ => // STATE = RUN, run automatically, monitoring
                montExpResultMonitor()
                clockDomain.waitSampling()
              }

              // output
              printlnYellow(s"test of mode $modeId, which run ${parallelFactor / groupPerInstance(modeId)} instance of size ${lMs(modeId)}")
              testInputs.indices.foreach { i =>
                println(s"X$i     : " + toWordsHexString(testInputs(i), testWordSize, currentWordPerInstance))
              }
              println("M      : " + toWordsHexString(testModulus, testWordSize, currentWordPerInstance))
              println("rSquare: " + toWordsHexString(testRadixSquare, testWordSize, currentWordPerInstance))
              if (comparStageByStage) MontAlgos.Arch1ME(testInputs(0), testExponent, testModulus, testWordSize, print = true) // print the partial products
              goldens.indices.foreach { i =>
                val goldenString = toWordsHexString(goldens(i), testWordSize, lMs(modeId) / w)
                val dutString = dutResults(i).init.map(_.toString(16).padToLeft(testWordSize / 4, '0')).mkString(" ") + " "
                println(s"golden result$i        : $goldenString")
                println(s"dut result$i           : $dutString")
                assert(goldenString == dutString,
                  s"bad case" +
                    s"\n\t X = ${testInputs(i)}" +
                    s"\n\t M = $testModulus" +
                    s"\n\t E = $testExponent" +
                    s"\n\t mode = $modeId"
                )
                //                  assertResult(goldenString)(dutString)
              }
            }

            runForOnce()
          }
        }

        // main part
        io.start #= false
        io.mode #= BigInt(0)
        clockDomain.forkStimulus(2)
        clockDomain.waitSampling()

        runTestCases(testCases)
      }
    }
    if (doGen) GenRTL(new MontExpSystolic(testConfig)) // for a quick semantic test
    if (doSim) {
      (0 until simTimes).foreach(_ => sim("temp", testConfig, badTestCases))
    }
    if (doSynth) VivadoSynth(new MontMultPE(testConfig.w))
    if (doSynth) VivadoSynth(new MontMultSystolic(testConfig))
    if (doSynth) VivadoSynth(new MontExpSystolic(testConfig))
    if (doImpl) VivadoImpl(new MontExpSystolic(testConfig))

    def workForCompetition() = {
      val duts = Seq(
        MontConfig(lMs = Seq(2048), w = 16),
        MontConfig(lMs = Seq(2048), w = 32),
        MontConfig(lMs = Seq(512, 1024, 2048, 3072, 4096), w = 16),
        MontConfig(lMs = Seq(512, 1024, 2048, 3072, 4096), w = 32)
      )
      val names = Seq(
        "2048_16",
        "2048_32",
        "all_16",
        "all_32"
      )
      duts.zip(names).foreach { case (config, name) =>
        GenRTL(new MontMultSystolic(config), name = "MontMult" + name)
        GenRTL(new MontExpSystolic(config), name = "MontExp" + name)
//        VivadoSynth(new MontMultSystolic(config), name = "MontMult" + name)
//        VivadoSynth(new MontExpSystolic(config), name = "MontExp" + name)
//        println(s"simulation for MontExp$name started")
//        sim("MontExp" + name, config, standardTestCases(config))
      }
    }
    if(work) workForCompetition()
  }
}