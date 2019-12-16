import scala.io.Source

object Main {
  
  def getFuelForFuel(fuel: Int): Int = fuel match {
    case x if x <= 0 => 0
    case x => fuel + getFuelForFuel(fuel/3-2)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromFile("input.txt").getLines.toList.map(_.toInt)
    def divide = (x: Int) => x/3
    def subtract = (x: Int) => x-2
    val fuel = lines.map(divide andThen subtract andThen getFuelForFuel).sum
    println(fuel)
  }
}
