import scala.io.Source

val numeric = ('0' to '9').toList.map(c => c.toString)
val literal = List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
val literalNumericMap = (literal zip numeric).toMap ++ (numeric zip numeric).toMap

def substrings(line: String): List[String] = {
    val l = line.length
    val rear = (if (l>=4) List(line.substring(l-4, l-3), line.substring(l-4, l-1), line.substring(l-4, l)) else List()) ++
    (if (l>=3) List(line.substring(l-3, l-2), line.substring(l-3, l)) else List()) ++
    (if (l>=2) List(line.substring(l-2, l-1)) else List()) ++
    List(line.substring(l-1, l))
    
    val front = if (l >= 5) (line.sliding(1).toList.lazyZip(line.sliding(3).toList).
        lazyZip(line.sliding(4).toList).lazyZip(line.sliding(5).toList).
            flatten {case (a,b,c,d) => List(a,b,c,d)}).toList else List()
    front ++ rear
}


def calibrationValue(line: String): Int = {
    var firstDigit = substrings(line).find(s => (numeric ++ literal).contains(s)).get
    var lastDigit = substrings(line.reverse).find(s => (numeric ++ literal).contains(s.reverse)).get
    (literalNumericMap(firstDigit) + literalNumericMap(lastDigit.reverse)).toInt
}

val lines = Source.fromFile("Day1/input.txt").getLines()
lines.map(calibrationValue).reduce((x, y) => x + y)
