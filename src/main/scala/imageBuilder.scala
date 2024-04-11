import database.{Tile, allPerm, loadImage, updateNbrs}
import imageDisplay.*

import java.awt.*
import java.awt.geom.AffineTransform
import java.awt.image.{AffineTransformOp, BufferedImage}
import java.io.File
import java.util.concurrent.ConcurrentHashMap
import javax.imageio.ImageIO
import javax.swing.*
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

object imageBuilder {

  private type Point = (Int, Int)
  val dim = 64

  case class Board(x: Int, y: Int, p: Set[Tile]) {
    private val Xlen = x
    private val Ylen = y
    val board: Array[Array[Set[Tile]]] = Array.fill(y)(Array.fill(x)(p))
    private val frame = new JFrame("Project Future{Collapse}: Wave Collapse Function")
    private val panel = new JPanel()
    panel.setBackground(Color.darkGray)
    frame.setSize(x * dim, y * dim)
    panel.setSize(x * dim, y * dim)
    panel.setLayout(null)
    panel.setBorder(null)
    frame.add(panel)
    frame.setResizable(true)
    frame.setVisible(true)

    /*
    Function to choose the lowest entropy tile in the board
     */

    private def pick(): Option[(Int, Int)] = {
      var smallest: Option[(Int, Int)] = None
      var minVal = Int.MaxValue
      for (i <- board.indices; j <- board(i).indices) {
        if (board(i)(j).size > 1 && board(i)(j).size < minVal) {
          minVal = board(i)(j).size
          smallest = Some(i, j)
        }
      }
      smallest
    }

    /*
    Function to check whether the given position is within the boundary of the board.
     */

    private def valid(x: Int, y: Int): Boolean = (x, y) match {
      case (i, j) if i < 0 || j < 0 => false
      case (i, j) if i >= Ylen || j >= Xlen => false
      case _ => true
    }

    /*
    Function to check whether the given position has been placed.
     */
    private def complete(x: Int, y: Int): Boolean = {
      if (valid(x, y)) board(x)(y).size == 1 else false
    }

    /*
    Helper function of updateNbrs to update the neighbors of the current tile.
     */
    private def updateHelper(nbrSet: Set[Tile], current: Set[Tile], dir: String): Set[Tile] = {
      val nbrsCurrent: Set[Tile] = current.flatMap(_.nbrs(dir))
      nbrSet.intersect(nbrsCurrent)
    }

    /*
    Function to update the neighbors of the current tile.
     */
    def updateNbrs(x: Int, y: Int): (Point, Point, Point, Point) = {
      val current = board(x)(y)
      val nbrUp = if (valid(x - 1, y) && !complete(x - 1, y)) Some(board(x - 1)(y)) else None
      val nbrRight = if (valid(x, y + 1) && !complete(x, y + 1)) Some(board(x)(y + 1)) else None
      val nbrDown = if (valid(x + 1, y) && !complete(x + 1, y)) Some(board(x + 1)(y)) else None
      val nbrLeft = if (valid(x, y - 1) && !complete(x, y - 1)) Some(board(x)(y - 1)) else None
      if (nbrUp.isDefined) board(x - 1)(y) = updateHelper(nbrUp.get, current, "up")
      if (nbrRight.isDefined) board(x)(y + 1) = updateHelper(nbrRight.get, current, "right")
      if (nbrDown.isDefined) board(x + 1)(y) = updateHelper(nbrDown.get, current, "down")
      if (nbrLeft.isDefined) board(x)(y - 1) = updateHelper(nbrLeft.get, current, "left")
      ((x - 1, y), (x, y + 1), (x + 1, y), (x, y + 1))
    }

    /*
    Function to update the board based on the wave function collapse algorithm.
     */
    private def updateLoop(nbr: Point, v: ConcurrentHashMap[Point, Int]): Unit = !valid(nbr._1, nbr._2) match {
      case true => ()
      case false =>
        if (v.size == Xlen * Ylen) () else {
          if (v.containsKey(nbr)) () else {
            val nbrsTuple = updateNbrs(nbr._1, nbr._2)
            val nbrSq: List[Point] = List(nbrsTuple._1, nbrsTuple._2, nbrsTuple._3, nbrsTuple._4)
            v.put(nbr, 1)
            nbrSq.foreach(x => updateLoop(x, v))
          }
        }
    }

    /*
    Function to display the updated board on the JPanel.
     */
    private def JpanelUpdate(x: Int, y: Int): Unit = {
      val readImage = ImageIO.read(board(x)(y).head.name)
      val x_pos: Int = readImage.getWidth() / 2
      val y_pos: Int = readImage.getHeight() / 2
      val rotation = board(x)(y).head.rotate
      val radians = Math.toRadians(rotation * 90)
      val tx = AffineTransform.getRotateInstance(radians, x_pos, y_pos)
      val op = new AffineTransformOp(tx, AffineTransformOp.TYPE_BILINEAR)
      val rotatedImage = op.filter(readImage, null)
      val rotatedIcon = new ImageIcon(rotatedImage)
      val label = new JLabel(rotatedIcon)
      val labelWidth = rotatedIcon.getIconWidth
      val labelHeight = rotatedIcon.getIconHeight
      label.setSize(labelWidth, labelHeight)
      label.setLocation(y * dim, x * dim)
      panel.add(label)
      panel.revalidate()
      panel.repaint()
    }

    /*
    Function to start the wave function collapse algorithm.
     */
    def start(): Unit = {
      val pickS = System.nanoTime()
      val st = pick()
      st match {
        case None => ()
        case Some((x, y)) =>
          val current = board(x)(y).toArray
          val random = new Random
          board(x)(y) = Set(current(random.nextInt(current.length)))
          val findNbrs = System.nanoTime()
          val nbrsTuple = updateNbrs(x, y)
          val nbrs: List[Point] = List(nbrsTuple._1, nbrsTuple._2, nbrsTuple._3, nbrsTuple._4)
          val visitedMap = new ConcurrentHashMap[Point, Int]()
          val updatingMap = System.nanoTime()
          val futures: Seq[Future[Unit]] = nbrs.map(p => Future {
            updateLoop(p, visitedMap)
          })
          Await.result(Future.sequence(futures), Duration.Inf)
          JpanelUpdate(x, y)
          start()
      }
    }

    /*
    Function to display the board.
     */
    def print(): Unit = {
      board.foreach(row => println(row.map(x => x.map(r => (r.name.getName, r.rotate))).toList))
    }
  }
}
