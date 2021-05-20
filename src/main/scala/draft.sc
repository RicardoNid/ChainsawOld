val patternShiftAdd = "([P|M]?[0-9]*X).*([P|M]?[0-9]*X)<<([0-9]+).*([P|M]?[0-9]*X)".r
val line1 = "P31X <-  X<<5  + M1X"
val line2 = ""
val patternShiftAdd(sum, left, shift, right) = line1
val patternShiftAdd(sum, left, shift, right) = line2