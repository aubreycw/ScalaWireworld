/**
 * Created by Emily on 7/10/15.
 */
//  world class
//    - cell class
//  stringToWorld
//  worldToString
//  main function (at each step transition and show)
//  transition function

case class World(cells: List[Cell], yr: Int, xr: Int) {
  def transition(): World = {
    this
  }

  def worldToString(): String = {
    fillInString(blankWorld(0, 0), cells)
  }

  def fillInString(string: String, cellsList: List[Cell]): String = if (cellsList.length == 0) {
    string
  } else{
    fillInString(addCellToString(string, cellsList.head), cellsList.tail)
  }

  def addCellToString(string: String, cell: Cell): String = {
    val loc = (cell.yc * (xr+1)) + cell.xc
    val start = string.substring(0, loc-1)
    val end = string.substring(loc + 1)
    val character = cell.ty.match{
      case Head => "="
      case Tail => "-"
      case Empty => "_"
    }
    start.concat(character).concat(end)
  }


  def blankWorld(yp: Int, xp: Int ): String = if (yp > yr){
    ""
  } else if (xp > xr) {
    "/n".concat(blankWorld(yp+1, 0))
  } else {
    "_".concat(blankWorld(yp, xp+1))
  }
  
  def cellsToString(cellsList: List[Cell]) : String = cellsList.head.ty match {
    case Head => "=".concat(cellsToString(cellsList.tail))
    case Tail => "-".concat(cellsToString(cellsList.tail))
    case Empty => "_".concat(cellsToString(cellsList.tail))
  }

  def finished(): Boolean = {
    true
  }
}

sealed abstract class CellType
case object Head extends CellType
case object Tail extends CellType
case object Empty extends CellType

case class Cell(xc: Int, yc: Int, ty: CellType) {}

object Main {
  def stringToWorld(string: String): World = {
    World(stringToWorldHelper(string, List(), 0, 0), string.count(\n) + 1, string.length/(string.count(\n) + 1))
  }

  def stringToWorldHelper(string: String, cells: List[Cell], row: Int, col: Int): List[Cell] = if (string.length <= 0){
    List()
  } else if (string.charAt(0) == \n) {
      stringToWorldHelper(string.substring(1), cells, row + 1, 0)
  } else if (string.charAt(0) == '='){
      List(Cell(row, col, Head )) ++ stringToWorldHelper(string.substring(1), cells, row , col+1)
  } else if (string.charAt(0) == '-'){
      List(Cell(row, col, Tail)) ++ stringToWorldHelper(string.substring(1), cells, row , col+1)
  } else {
      List()
  }

  def main(args: Array[String]): Unit = {
    displayWorld(stringToWorld("_--=_ \n ___-_ \n ___-_ \n _---_"), 10)
  }

  def displayWorld(world: World, upperLimit: Int): World = if (world.finished() || upperLimit <= 0){
    val newWorld = world.transition()
    println(newWorld.worldToString())
    displayWorld(newWorld, upperLimit - 1)
  } else {
    world
  }
}