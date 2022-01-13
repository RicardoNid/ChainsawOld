def dbc(n:Double, d:Double) = {
  // d is close to 1, n is within [0.5, 2)
  var x = n
  var t = d
  (0 until 12).foreach{_ =>
    val f = 2 - t
    x = x * f
    t = t * f
  }
  x
}

println(dbc(1, 1.6) - 1 / 1.6)
println(dbc(1, 0.0030321) - 1 / 0.0030321)
println(dbc(1, 0.05) - 1 / 0.05)
