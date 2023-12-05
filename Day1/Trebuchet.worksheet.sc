import scala.io.Source

val digits = ('0' to '9').toSet

def calibrationValue(line: String): Int = {
    val reversedLine = line.reverse
    val firstDigit = line.find(c => digits.contains(c)).get
    val lastDigit = reversedLine.find(c => digits.contains(c)).get
    s"${firstDigit}${lastDigit}".toInt
}

val lines = Source.fromFile("Day1/input.txt").getLines()
lines.map(calibrationValue).reduce((x, y) => x + y)
