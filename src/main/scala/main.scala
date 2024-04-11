import database.{allPerm, loadImage, updateNbrs}
import imageBuilder.Board
import imageDisplay.compressImage

import java.io.File

/*
Project Main
> Specify the input file path of the tile set
> Specify the number of rows and columns of the board
> Specify the output file path of the final image
 */

object main extends App {
  val file = new File("C:\\Users\\MIS\\Documents\\funpar-t2-22-project-future-collapse\\tileSet")
  private val files = loadImage(file)
  val perms = allPerm(files)
  updateNbrs(perms)
  val board: Board = Board(10, 10, perms)
  board.start()
  val outputFilePath = "output/test.png"
  compressImage(board, outputFilePath)
}
