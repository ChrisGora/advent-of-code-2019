import scala.io.Source

object Main {

  def execute(memory: Array[Int], i: Int): Array[Int] = {
    if (i >= memory.length) {
      return memory
    }

    memory(i) match {
      case 1 => memory(memory(i+3)) = memory(memory(i+1)) + memory(memory(i+2))
      case 2 => memory(memory(i+3)) = memory(memory(i+1)) * memory(memory(i+2))
      case 99 => return memory
      case _ => throw new Exception
    }
    execute(memory, i+4)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("input.txt").mkString.split(",").map(_.toInt)
    for (i <- 0 to 99; j <- 0 to 99) {
      val memory = input.clone
      memory(1) = i
      memory(2) = j
      val finalMemory = execute(memory, 0)
      if (finalMemory(0) == 19690720) {
        println("DONE")
        println(i)
        println(j)
      }
    }
  }
}
