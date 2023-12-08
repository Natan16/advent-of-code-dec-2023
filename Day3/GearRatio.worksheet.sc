import scala.io.Source
import collection.mutable


// rows and columns
val matrix = Source.fromFile("Day3/input.txt").getLines().toList

val numbers = ('0' to '9').toSet
val invalidSymbols = numbers ++ Set('.')

val increments = for (incr_i <- -1 to 1 ; incr_j <- -1 to 1) yield (incr_i, incr_j)

var part_number_sum = 0

var part_number = ""
var partNumberMatrix = List[List[Any]]()
val gearMap = new mutable.HashMap[Int, List[Int]]().withDefaultValue(List())

def calculateGearNumber(i: Int, j: Int): Int = {
    matrix(0).length * i + j
}

def calculateLookupIndexes(i: Int, j: Int, len: Int): IndexedSeq[(Int, Int)] = {
    for (idx_i <- i-1 to i+1 if (matrix.length > idx_i && idx_i >= 0); 
        idx_j <- j-len to j+1 if (matrix(0).length > idx_j && idx_j >= 0)) 
        yield (idx_i, idx_j)
}

for (i <- 0 to matrix.length - 1; j <- 0 to matrix(0).length - 1) {
    if (numbers.contains(matrix(i)(j))) {
        part_number += matrix(i)(j)
    }
    // end of line or end of part_number
    if ((!numbers.contains(matrix(i)(j)) || j == matrix(0).length - 1) && part_number != "") {
        val j_prime = (if (numbers.contains(matrix(i)(j))) j else j - 1)
        val lookupIndexes = calculateLookupIndexes(i, j_prime , part_number.length)
        val findGears = () => {
            val gearIndexes = lookupIndexes.filter(index => matrix(index._1)(index._2) == '*')
            gearIndexes.map(index => calculateGearNumber(index._1, index._2))
        }
        if (lookupIndexes.exists(index => !invalidSymbols.contains(matrix(index._1)(index._2)))) {
            part_number_sum += part_number.toInt
        }
        val gears = findGears()
        gears.foreach(gear => gearMap.update(gear, gearMap(gear) ++ List(part_number.toInt)))
        part_number = ""
    }
}
part_number_sum
val validGear = (pair: Tuple2[_, List[Int]]) => pair._2.length  == 2
val gearRatio = (pair: Tuple2[_, List[Int]]) => pair._2(0) * pair._2(1)
// sum of all the gear ratios
gearMap.filter(validGear).map(gearRatio).reduce((x, y) => x + y)

