def CN(n: Int) = (0 until n).map(i => (i * i / 2.0) % n).distinct.length

CN(256)