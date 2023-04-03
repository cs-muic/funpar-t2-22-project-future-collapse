import imageBuilder.*
import database.*
import java.awt.Graphics2D
import java.awt.image.BufferedImage
import javax.swing.JPanel
import java.io.File
import javax.imageio.ImageIO
import java.awt.geom.AffineTransform
import java.awt.image.AffineTransformOp
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object imageDisplay {
  
  val dim = 64

  def compressImage(board: Board, outputFilePath: String): Unit = {
    val allImages = board.board
    val height = allImages.length * dim
    val width = allImages(0).length * dim
    val Images = allImages.flatten

    val combinedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val pixels = combinedImage.createGraphics()

    var countX = 0
    var countY = 0
    for (image <- Images) {
      var readImage = ImageIO.read(image.head.name)
      val rotation = image.head.rotate

      if (rotation > 0) {
        val radians = Math.toRadians(rotation * 90)
        val x = readImage.getWidth() / 2
        val y = readImage.getHeight() / 2
        val tx = AffineTransform.getRotateInstance(radians, x, y)
        val op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR)
        readImage = op.filter(readImage, null)
      }
      pixels.drawImage(readImage, countX, countY, null)
      countX += dim
      if (countX >= width) {
        countX = 0
        countY += dim
      }
    }
    pixels.dispose()
    ImageIO.write(combinedImage, "png", new File(outputFilePath))
    println(outputFilePath)
  }
  
//  val file = new File("/Users/nathans./Documents/FunPar/Project_Final/tileSet")
//  val x = loadImage(file)
//  val perms = allPerm(x)
//  updateNbrs(perms)
//
//  val board = Board(20, 20, perms)
//  board.start()
//  val outputFilePath = "/Users/nathans./Documents/FunPar/Project_Final/output"
//  compressImage(board, outputFilePath)
}
