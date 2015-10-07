/**
 * Created by Emily on 7/10/15.
 */
//  world class
//    - cell class
//  stringToWorld
//  worldToString
//  main function (at each step transition and show)
//  transition function

class World(cells: Array[Array[Cell]]) {
  var world: Array[Array[Cell]] = cells

  def transition(): World = {
    this
  }

  def worldToString(): String = {
    "---"
  }
}

class Cell(xc: Int, yc: Int, ty: Char) {
  var xPos: Int = xc
  var yPos: Int = yc
  var thing: Char = ty
}

object Main {
  def stringToWorld(string: String) = {
    World([[Cell(1,1,1), Cell(1,1,1)],[Cell(1,1,1), Cell(1,1,1)]])
  }

  def main(args: Array[String]): Unit = {
    displayWorld(stringToWorld("_--=_ \n ___-_ \n ___-_ \n _---_"))
  }

  def displayWorld(world: World, upperLimit: Int): World = if (finished(world) || upperLimit <= 0){
    val newWorld = world.transition()
    println(worldToString(newWorld))
    displayWorld(newWorld, upperLimit - 1)
  } else {
    world
  }
}
