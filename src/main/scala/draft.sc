val logic = Seq(1.018,1.169,1.219,1.225)
val net = Seq(1.706,1.920,2.232,2.573)

val FPGA = logic.zip(net ).map{ case (d, d1) => d + d1}
val ASIC = logic.zip(net ).map{ case (d, d1) => d + d1 / 2.0}

println(FPGA.map(1 / _))
println(ASIC.map(1 / _))
