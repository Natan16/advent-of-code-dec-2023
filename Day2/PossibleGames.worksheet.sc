import scala.io.Source
import collection.mutable

val RED_MAX = 12
val GREEN_MAX = 13
val BLUE_MAX = 14

class Grab(val red: Int, val green: Int, val blue: Int) {
    def isPossible() : Boolean = {
        red <= RED_MAX && green <= GREEN_MAX && blue <= BLUE_MAX 
    }
}

class Game(val id: Int, val grabs: List[Grab]) {
    def isPossible() : Boolean = {
        grabs.forall(grab => grab.isPossible())
    }
}

def retrieveGame(line: String): Game = {
    val parts = line.split(":")
    val id = parts(0).split(" ").last.toInt
    val grabs = parts(1).split(";")

    var grabList = List[Grab]()
    for (grab <- grabs) {
        val colorMap = mutable.Map[String, Int]().withDefaultValue(0)
        val colornumbers = grab.split(",")
        for (colornumber <- colornumbers) {
            val colorAndNumber = colornumber.split(" ")
            val number = colorAndNumber(1).toInt
            val color = colorAndNumber(2)
            colorMap(color) = number
        }
        grabList = grabList.prepended(new Grab(colorMap("red"), colorMap("green"), colorMap("blue")))
    }
    new Game(id, grabList)
}

val lines = Source.fromFile("Day2/input.txt").getLines()
// sum de ids of all possible games
lines.map(retrieveGame).filter(game => game.isPossible()).map(g => g.id).reduce((x, y) => x + y)
