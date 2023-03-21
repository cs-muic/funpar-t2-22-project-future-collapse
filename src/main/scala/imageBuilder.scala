import database.*
import java.io.File
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Future

object imageBuilder extends App {

  type Point = (Int, Int)

  case class Board(x: Int, y: Int, p: Set[Image]) {
    val Xlen = x
    val Ylen = y
    val board = Array.fill(y)(Array.fill(x)(p));

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

    def updateHelper(nbrSet: Set[Image], current: Set[Image], dir: String): Set[Image] = {
      val newNbrArray: Array[Set[Image]] = Array.fill(current.size)(Set())
      for ((c, idx) <- current.zipWithIndex) {
        val ok = c.nbrs.get(dir)
        newNbrArray(idx) = nbrSet.filter(i => ok.get.contains(i))
      }
      newNbrArray.toSet.flatten
    }

    def updateNbrs(x: Int, y: Int): (Point, Point, Point, Point) = {
      val current = board(x)(y)
      val nbrUp = if (valid(x - 1, y) && !complete(x - 1, y)) Some(board(x - 1)(y)) else None
      val nbrRight = if (valid(x, y + 1) && !complete(x, y + 1)) Some(board(x)(y + 1)) else None
      val nbrDown = if (valid(x + 1, y) && !complete(x + 1, y)) Some(board(x + 1)(y)) else None
      val nbrLeft = if (valid(x, y - 1) && !complete(x, y - 1)) Some(board(x)(y - 1)) else None

      val thread1 = new Thread {
        override def run(): Unit = if (nbrUp != None) board(x - 1)(y) = updateHelper(nbrUp.get, current, "up")
      }
      val thread2 = new Thread {
        override def run(): Unit = if (nbrRight != None) board(x)(y + 1) = updateHelper(nbrRight.get, current, "right")
      }
      val thread3 = new Thread {
        override def run(): Unit = if (nbrDown != None) board(x + 1)(y) = updateHelper(nbrDown.get, current, "down")
      }
      val thread4 = new Thread {
        override def run(): Unit = if (nbrLeft != None) board(x)(y - 1) = updateHelper(nbrLeft.get, current, "left")
      }

      val threads = List(thread1, thread2, thread3, thread4)
      threads.foreach(_.start())
      threads.foreach(_.join())

      ((x - 1, y), (x, y + 1), (x + 1, y), (x, y + 1))
    }

    def updateLoop(nbr: Point, v: ConcurrentHashMap[Point, Int]): Unit = !valid(nbr._1, nbr._2) match {
      case true => ()
      case false => {
        if (v.size == Xlen * Ylen) () else {
          if (v.containsKey(nbr)) () else {
            val nbrSq: List[Point] = updateNbrs(nbr._1, nbr._2).toList
            v.put(nbr, 1)
            nbrSq.foreach(x => updateLoop(x, v))
          }
        }
      }
    }

    def start(): Unit = {
      val st = pick()
      st match {
        case None => ()
        case Some((x, y)) => {
          val current = board(x)(y).toArray
          val random = new Random
          board(x)(y) = Set(current(random.nextInt(current.size)))
          val nbrs: List[Point] = updateNbrs(x, y).toList
          val visitedMap = new ConcurrentHashMap[Point, Int]()
          val threads = nbrs.map(p => new Thread {
            override def run(): Unit = updateLoop(p, visitedMap)
          })
          threads.foreach(_.start())
          threads.foreach(_.join())
          start()
        }
      }
    }

    def print(): Unit = {
      board.foreach(row => println(row.map(x => x.map(r => (r.name.getName, r.rotate))).toList))
    }
  }

  //main
  val board = new Board(15, 15, perms)
  board.start()
  board.print()
}