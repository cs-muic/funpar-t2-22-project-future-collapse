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


object imageBuilder extends App {

  type Point = (Int, Int)

  case class Board(x: Int, y: Int, p: Set[Tile]) {
    private val Xlen = x
    private val Ylen = y
    val board: Array[Array[Set[Tile]]] = Array.fill(y)(Array.fill(x)(p))
    var frame = new JFrame("Project Future{Collapse}: Wave Collapse Function")
    var panel = new JPanel()
    panel.setBackground(Color.darkGray)
    frame.setSize(x * 64, y * 64)
    panel.setSize(x * 64, y * 64)
    panel.setLayout(null)
    panel.setBorder(null)
    frame.add(panel)
    frame.setResizable(true)
    frame.setVisible(true)

    def pick(): Option[(Int, Int)] = {
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

    def valid(x: Int, y: Int): Boolean = (x, y) match {
      case (i, j) if i < 0 || j < 0 => false
      case (i, j) if i >= Ylen || j >= Xlen => false
      case _ => true
    }

    def complete(x: Int, y: Int): Boolean = {
      if (valid(x, y)) (board(x)(y).size == 1) else false
    }

    def updateHelper(nbrSet: Set[Tile], current: Set[Tile], dir: String): Set[Tile] = {
      val newNbrSet: Set[Tile] = nbrSet
      val nbrsCurrent: Set[Tile] = current.flatMap(_.nbrs(dir))
      val results = newNbrSet.intersect(nbrsCurrent)
      results
    }

    def updateNbrs(x: Int, y: Int): (Point, Point, Point, Point) = {
      val current = board(x)(y)
      val nbrUp = if (valid(x - 1, y) && !complete(x - 1, y)) Some(board(x - 1)(y)) else None
      val nbrRight = if (valid(x, y + 1) && !complete(x, y + 1)) Some(board(x)(y + 1)) else None
      val nbrDown = if (valid(x + 1, y) && !complete(x + 1, y)) Some(board(x + 1)(y)) else None
      val nbrLeft = if (valid(x, y - 1) && !complete(x, y - 1)) Some(board(x)(y - 1)) else None

//      val thread1 = new Thread {
//        override def run(): Unit = if (nbrUp != None) board(x - 1)(y) = updateHelper(nbrUp.get, current, "up")
//      }
//      val thread2 = new Thread {
//        override def run(): Unit = if (nbrRight != None) board(x)(y + 1) = updateHelper(nbrRight.get, current, "right")
//      }
//      val thread3 = new Thread {
//        override def run(): Unit = if (nbrDown != None) board(x + 1)(y) = updateHelper(nbrDown.get, current, "down")
//      }
//      val thread4 = new Thread {
//        override def run(): Unit = if (nbrLeft != None) board(x)(y - 1) = updateHelper(nbrLeft.get, current, "left")
//      }
//
//      val threads = List(thread1, thread2, thread3, thread4)
//      threads.foreach(_.start())
//      threads.foreach(_.join())

      if (nbrUp != None) board(x - 1)(y) = updateHelper(nbrUp.get, current, "up")
      if (nbrRight != None) board(x)(y + 1) = updateHelper(nbrRight.get, current, "right")
      if (nbrDown != None) board(x + 1)(y) = updateHelper(nbrDown.get, current, "down")
      if (nbrLeft != None) board(x)(y - 1) = updateHelper(nbrLeft.get, current, "left")

      ((x - 1, y), (x, y + 1), (x + 1, y), (x, y + 1))
    }

    def updateLoop(nbr: Point, v: ConcurrentHashMap[Point, Int]): Unit = !valid(nbr._1, nbr._2) match {
      case true => ()
      case false => {
        if (v.size == Xlen * Ylen) () else {
          if (v.containsKey(nbr)) () else {
            val nbrSq: List[Point] = updateNbrs(nbr._1, nbr._2).toList
            v.put(nbr, 1)
            nbrSq.map(x => updateLoop(x, v))
          }
        }
      }
    }
    
    def JpanelUpdate(x: Int, y: Int): Unit = {
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
      label.setLocation(y * 64, x * 64)
      panel.add(label)
      panel.revalidate()
      panel.repaint()
    }

    def start(): Unit = {
      val pickS = System.nanoTime()
      val st = pick()
      //println(s"Pick : ${(System.nanoTime() - pickS)/1_000}")
      st match {
        case None => ()
        case Some((x, y)) => {
          val current = board(x)(y).toArray
          val random = new Random
          board(x)(y) = Set(current(random.nextInt(current.size)))
          val findNbrs = System.nanoTime()
          val nbrs: List[Point] = updateNbrs(x, y).toList
          //println(s"Finding nbrs : ${(System.nanoTime() - findNbrs)/1_000}")
          val visitedMap = new ConcurrentHashMap[Point, Int]()
          val updatingMap = System.nanoTime()
          val futures : Seq[Future[Unit]] = nbrs.map(p => Future { updateLoop(p, visitedMap)})
          Await.result(Future.sequence(futures),Duration.Inf)
          //println(s"Updating Map : ${(System.nanoTime() - updatingMap)/1_000_00}")
//          println(".")
          JpanelUpdate(x, y)
          start()
        }
      }
    }

    def print(): Unit = {
      board.foreach(row => println(row.map(x => x.map(r => (r.name.getName, r.rotate))).toList))
    }
   
  }

  //main
//  val board = new Board(50, 50, perms)
//  val t = System.nanoTime()
//  board.start()
//  println(s"${(System.nanoTime() - t)/1_000_000_000}" + " s")
}
