def testbenchGen(cycle: Int) = {
  import scala.util.Random
  val randGen = new Random(42)
  Range(0, cycle).foreach(_ => println("s_axis_data_tdata  = 32'd" + randGen.nextInt(100) + "; #10;"))
}

testbenchGen(100)