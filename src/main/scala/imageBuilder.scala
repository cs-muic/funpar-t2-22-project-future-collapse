import database.*
import java.io.File
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random

import scala.concurrent.Future

object imageBuilder extends App {

  type Point = (Int, Int)

  case class Board(x: Int, y: Int, p: Set[Image]) {
    val Xlen = x
    val Ylen = y
    val board = Array.fill(y)(Array.fill(x)(p));


    def scuffedMinBy(i: Int): Int = i match {
      case 1 => Int.MaxValue
      case n => n
    }

    def minSlot(row: Array[Set[Image]], rowIndex: Int): Option[(Set[Image], Int, Int)] = {
      val minX = row.zipWithIndex.minBy((a, b) => scuffedMinBy(a.size))
      if (minX._1.size == 1) None
      else Some((minX._1, minX._2, rowIndex))
    }

    def pick(): Option[(Int, Int)] = {
      val futures = for (i <- 0 until Ylen) yield Future {
        minSlot(board(i), i)
      }
      val rowMins = Await.result(Future.sequence(futures), Duration.Inf).filter(_.isDefined)
      if (rowMins.isEmpty) None
      else {
        val min = rowMins.map(_.get).minBy(_._1.size)
        Some(min._2, min._3)
      }
    }

    def valid(x: Int, y: Int): Boolean = (x, y) match {
      case (i, j) if i < 0 || j < 0 => false
      case (i, j) if i >= Xlen || j >= Ylen => false
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
      val nbrUp = if (valid(x, y - 1) && !complete(x, y - 1)) Some(board(x)(y - 1)) else None
      val nbrRight = if (valid(x + 1, y) && !complete(x + 1, y)) Some(board(x + 1)(y)) else None
      val nbrDown = if (valid(x, y + 1) && !complete(x, y + 1)) Some(board(x)(y + 1)) else None
      val nbrLeft = if (valid(x - 1, y) && !complete(x - 1, y)) Some(board(x - 1)(y)) else None

      val thread1 = new Thread {
        override def run(): Unit = if (nbrUp != None) board(x)(y - 1) = updateHelper(nbrUp.get, current, "up")
      }
      val thread2 = new Thread {
        override def run(): Unit = if (nbrRight != None) board(x + 1)(y) = updateHelper(nbrRight.get, current, "right")
      }
      val thread3 = new Thread {
        override def run(): Unit = if (nbrDown != None) board(x)(y + 1) = updateHelper(nbrDown.get, current, "down")
      }
      val thread4 = new Thread {
        override def run(): Unit = if (nbrLeft != None) board(x - 1)(y) = updateHelper(nbrLeft.get, current, "left")
      }

      val threads = List(thread1, thread2, thread3, thread4)
      threads.foreach(_.start())
      threads.foreach(_.join())

      ((x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y))
    }
    
    //TODO : Repair This
    def updateLoop(nbrs: List[Point]): Unit = nbrs.forall((x, y) => !valid(x, y)) match {
      case true => ()
      case false => {
        val valids: List[Point] = nbrs.filter((i, j) => valid(i, j))
        val nbrSq = valids.map((i,j) => updateNbrs(i,j))
        nbrSq.foreach(x => {
          val pointList: List[Point] = x.toList
          updateLoop(pointList)
        })
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
          updateLoop(nbrs)
          start()
        }
      }
    }

    def print(): Unit = {
      board.foreach(row => row.foreach(e => println(e)))
    }
  }

  //main
  val board = new Board(2, 2, perms)
  board.start()
  board.print()
}
