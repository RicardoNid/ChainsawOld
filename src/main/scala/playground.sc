val input = Array.range(0, 12)

val temp1 = input.sortBy(_ % 3).grouped(4).toArray

val temp2 = input.sortBy(_ % 4)
