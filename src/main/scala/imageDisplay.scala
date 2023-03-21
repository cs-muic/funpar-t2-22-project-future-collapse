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

object imageDisplay extends App{

  def compressImage(board: Board, outputFilePath: String): Unit = {
    val allImages = board.board
    val height = allImages.length * 64
    val width = allImages(0).length * 64
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
      countX += 64
      if (countX >= width) {
        countX = 0
        countY += 64
      }
    }
    pixels.dispose()
    ImageIO.write(combinedImage, "png", new File(outputFilePath))
    println(outputFilePath)
  }

  def compressImage2(board: Board, outputFilePath: String): Unit = {
    val allImages = board.board
    val height = allImages.length * 64
    val width = allImages(0).length * 64

    val futures: Seq[Future[BufferedImage]] = allImages.map(row => Future{ writeImage(row, width) })
    val arrayBuffer = Await.result(Future.sequence(futures), Duration.Inf)

    val combinedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val pixels = combinedImage.createGraphics()
  }

  def writeImage(images: Array[Set[Tile]], width: Int): BufferedImage = {
    val combinedImage = new BufferedImage(width, 64, BufferedImage.TYPE_INT_ARGB)
    val pixels = combinedImage.createGraphics()
    for (image <- images) {
      var readImage = ImageIO.read(image.head.name)
      val rotation = image.head.rotate
      var count = 0
      if (rotation > 0) {
        val radians = Math.toRadians(rotation * 90)
        val x = readImage.getWidth() / 2
        val y = readImage.getHeight() / 2
        val tx = AffineTransform.getRotateInstance(radians, x, y)
        val op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR)
        readImage = op.filter(readImage, null)
      }
      pixels.drawImage(readImage, count, 0, null)
      count += 64
    }
    pixels.dispose()
    combinedImage
  }
  
  val file = new File("C:\\Users\\Admin\\Documents\\funpar-t2-22-project-future-collapse-master\\funpar-t2-22-project-future-collapse-master\\tileSet")
  val x = loadImage(file)
  val perms = allPerm(x)
  updateNbrs(perms)

  val board = Board(30, 30, perms)
  board.start()
  val outputFilePath = "C:\\Users\\Admin\\Documents\\funpar-t2-22-project-future-collapse-master\\funpar-t2-22-project-future-collapse-master\\testOutput\\image-test.png"
  compressImage(board, outputFilePath)
}
