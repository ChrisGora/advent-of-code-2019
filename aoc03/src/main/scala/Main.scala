import scala.io.Source

class Dimensions(var width: Int = 0, var height: Int = 0) {
  def update(newWidth: Int, newHeight: Int) = {
    new Dimensions(
      this.width max newWidth,
      this.height max newHeight,
    )
  }

  override def toString = s"$width x $height"
}

class Position(var x: Int = 0, var y: Int = 0) {
  def update(deltaX: Int, deltaY: Int) = {
    new Position(
      this.x + deltaX,
      this.y + deltaY,
    )
  }
  def manhattan = x + y 
}

class Operation(token: String) {
  val opcode = token(0)
  val operand = token.slice(1, token.length).toInt
}

class Wire(wire: String) {
  val operations = wire.split(",").map(new Operation(_))

  def dimensions = {
    val state = this.operations.foldLeft((new Position, new Dimensions))(
      (state, operation) => {
        val oldPosition = state(0)
        val newPosition = operation.opcode match {
          case 'U' => oldPosition.update(0, operation.operand)
          case 'D' => oldPosition.update(0, -operation.operand)
          case 'R' => oldPosition.update(operation.operand, 0)
          case 'L' => oldPosition.update(-operation.operand, 0)
        }
        val newDimensions = state(1).update(newPosition.x, newPosition.y)
        (newPosition, newDimensions)
      }
    )
    state(1)
  }
}

object Main {

  implicit class plottableGrid(grid: Array[Array[Boolean]]) {
    def plot(wire: Wire): Unit = {
      var position = new Position
      for (operation <- wire.operations) {
        val newPosition = operation.opcode match {
          case 'U' => position.update(0, operation.operand)
          case 'D' => position.update(0, -operation.operand)
          case 'R' => position.update(operation.operand, 0)
          case 'L' => position.update(-operation.operand, 0)
        }
        operation.opcode match {
          case 'U' => for (i <- position.y to newPosition.y) grid(i)(position.x) = true
          case 'D' => for (i <- newPosition.y to position.y) grid(i)(position.x) = true
          case 'R' => for (i <- position.x to newPosition.x) grid(position.y)(i) = true
          case 'L' => for (i <- newPosition.x to position.x) grid(position.y)(i) = true
        }
        position = newPosition
      }
    }

    def findIntersections(wire: Wire): List[Position] = {
      var intersections = List[Position]()
      var position = new Position
      for (operation <- wire.operations) {
        val newPosition = operation.opcode match {
          case 'U' => position.update(0, operation.operand)
          case 'D' => position.update(0, -operation.operand)
          case 'R' => position.update(operation.operand, 0)
          case 'L' => position.update(-operation.operand, 0)
        }
        val newIntersections = operation.opcode match {
          case 'U' => for (i <- position.y to newPosition.y if grid(i)(position.x)) yield new Position(position.x, i)
          case 'D' => for (i <- newPosition.y to position.y if grid(i)(position.x)) yield new Position(position.x, i)
          case 'R' => for (i <- position.x to newPosition.x if grid(position.y)(i)) yield new Position(i, position.y)
          case 'L' => for (i <- newPosition.x to position.x if grid(position.y)(i)) yield new Position(i, position.y)
        }
        intersections = intersections ++ newIntersections.toList
        position = newPosition
      }
      return intersections
    }
  }

  def main(args: Array[String]): Unit = {
    // val input = Source.fromFile("example1.txt").getLines.toList
    val input = Source.fromFile("input.txt").getLines.toList
    val firstWire = new Wire(input(0))
    val secondWire = new Wire(input(1))
    val gridDimensions = firstWire.dimensions.update(secondWire.dimensions.width, secondWire.dimensions.height)
    println(s"Dimensions $gridDimensions")
    val grid = Array.ofDim[Boolean](gridDimensions.height+1, gridDimensions.width+1)
    grid.plot(firstWire)
    val intersections = grid.findIntersections(secondWire)
    // grid foreach { row => row foreach (e => if (e) print("x") else print("-")); println }
    val sortedDistances = intersections.map(_.manhattan).sorted
    println(sortedDistances(1))
  }
}
