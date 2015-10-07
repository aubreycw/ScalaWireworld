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

}

class Cell(xc: Int, yc: Int) {
  var xPos: Int = xc
  var yPos: Int = yc

}

object Main {
  def main(args: Array[String]): Unit = {
    println("hello world!")
  }

}
