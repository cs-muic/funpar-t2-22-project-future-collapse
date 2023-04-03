import database.{allPerm, loadImage, updateNbrs}
import imageBuilder.Board
import imageDisplay.compressImage
import java.io.File

object main extends App{
  val file = new File("/Users/nathans./Documents/FunPar/Project_Final/tileSet")
  val files = loadImage(file)
  val perms = allPerm(files)
  updateNbrs(perms)
  val board = Board(25, 15, perms)
  board.start()
  val outputFilePath = "/Users/nathans./Documents/FunPar/Project_Final/output/test.png"
  compressImage(board, outputFilePath)
}
