import database._
import imageBuilder._

import java.awt.Graphics2D
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import javax.imageio.ImageIO
import javax.swing.JPanel
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object imageDisplay {

  val dim = 64
  /*
  Save the output of the wave function collapse as an png file.
   */
  def compressImage(board: Board, outputFilePath: String): Unit = {
    val allImages = board.board
    val height = allImages.length * dim
    val width = allImages(0).length * dim
    val Images = allImages.flatten

    val combinedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val pixels = combinedImage.createGraphics()

    Images.zipWithIndex.map { case (image, i) =>
      val x = (i % allImages(0).length) * dim
      val y = (i / allImages(0).length) * dim
      var readImage = ImageIO.read(image.head.name)
      val rotation = image.head.rotate

      if (rotation > 0) {
        val radians = Math.toRadians(rotation * 90)
        val tx = AffineTransform.getRotateInstance(radians, readImage.getWidth / 2, readImage.getHeight / 2)
        val op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR)
        readImage = op.filter(readImage, null)
      }
      (readImage, x, y)
    }.foreach { case (readImage, x, y) =>
      pixels.drawImage(readImage, x, y, null)
    }

    pixels.dispose()
    ImageIO.write(combinedImage, "png", new File(outputFilePath))
    println(outputFilePath)
  }
}
