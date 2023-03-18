import database.*
import java.io.File
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

import scala.concurrent.Future

object imageBuilder extends App {

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
      case (i, j) if i > Xlen || j > Ylen => false
      case _ => true
    }

    def complete(x: Int, y: Int): Boolean = board(x)(y).size match {
      case 1 => true
      case _ => false
    }

    def updateHelper(nbrSet: Set[Image], current: Set[Image], relativeDir: String): Set[Image] = {
      val newNbrSet = mutable.Set[Image]()
      for (c <- current) {
        newNbrSet ++ nbrSet.filter(i => i.nbrs.get(relativeDir).contains(c))
      }
      newNbrSet.toSet
    }

    def updateNbrs(x: Int, y: Int): Unit = {
      val current = board(x)(y)
      val nbrUp = if (valid(x, y - 1) && !complete(x, y - 1)) Some(board(x)(y - 1)) else None
      val nbrRight = if (valid(x + 1, y) && !complete(x + 1, y)) Some(board(x + 1)(y)) else None
      val nbrDown = if (valid(x, y + 1) && !complete(x, y + 1)) Some(board(x)(y + 1)) else None
      val nbrLeft = if (valid(x - 1, y) && !complete(x - 1, y)) Some(board(x - 1)(y)) else None

      val thread1 = new Thread {
        override def run(): Unit = if (nbrUp.isDefined) board(x)(y - 1) = updateHelper(nbrUp.get, current, "down")
      }
      val thread2 = new Thread {
        override def run(): Unit = if (nbrRight.isDefined) board(x + 1)(y) = updateHelper(nbrUp.get, current, "left")
      }
      val thread3 = new Thread {
        override def run(): Unit = if (nbrDown.isDefined) board(x)(y + 1) = updateHelper(nbrUp.get, current, "up")
      }
      val thread4 = new Thread {
        override def run(): Unit = if (nbrLeft.isDefined) board(x - 1)(y) = updateHelper(nbrUp.get, current, "right")
      }

      val threads = List(thread1, thread2, thread3, thread4)
      threads.foreach(_.start())
      threads.foreach(_.join())
    }

    def start(): Any = {}
  }
}
