import scala.io.Source

// rows and columns
val matrix = Source.fromFile("Day3/input.txt").getLines().toList

val numbers = ('0' to '9').toSet
val invalidSymbols = numbers ++ Set('.')

val increments = for (incr_i <- -1 to 1 ; incr_j <- -1 to 1) yield (incr_i, incr_j)

var part_number_sum = 0

// I need the indexes
var part_number = ""
var valid = false
for (i <- 0 to matrix.length - 1; j <- 0 to matrix(0).length - 1) {
    if (numbers.contains(matrix(i)(j))) {
        part_number += matrix(i)(j)
        val validSymbol = (incr_i: Int, incr_j: Int) => {
            val index1 = (i + incr_i).max(0).min(matrix.length - 1)
            val index2 = (j + incr_j).max(0).min(matrix(0).length - 1)
            !invalidSymbols.contains(matrix(index1)(index2))
        }
        valid = valid || increments.exists(increment => validSymbol(increment._1, increment._2))
    }
    // end of line or end of part_number
    if ((!numbers.contains(matrix(i)(j)) || j == matrix(0).length - 1) && part_number != "") {
        // reset
        if (valid) {
            part_number_sum += part_number.toInt
            valid = false
        }
        part_number = ""
    }
}
part_number_sum