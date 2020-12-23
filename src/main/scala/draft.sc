type transformer = (Int, Int, Int, Int, Int, Int) => Any
type transformer1D = (Int, Int, Int, Int, Int, Int) => Int // order = kx, ky, ni, ox, oy, no (same as Ma,2018)
type transformer2D = (Int, Int, Int, Int, Int, Int) => (Int, Int)
type transformer3D = (Int, Int, Int, Int, Int, Int) => (Int, Int, Int)
type transformer4D = (Int, Int, Int, Int, Int, Int) => (Int, Int, Int, Int)

transformer4D

println