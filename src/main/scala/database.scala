import java.io.File
import scala.collection.immutable.List
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object database extends App {
  case class Image(filename: File, colorDirection: Array[String], rotation: Int) {
    val name: File = filename
    val up: String = colorDirection(0)
    val right: String = colorDirection(1)
    val down: String = colorDirection(2)
    val left: String = colorDirection(3)
    var nbrs: mutable.HashMap[String, Set[Image]] = new mutable.HashMap[String, Set[Image]]
    val rotate: Int = rotation
  }

  /*
  The loadImage function takes in a directory path and returns the Array of Images that the directory contains
  This function assumes that the directory contains exactly 13 images
  */
  def loadImage(dirName: File): Array[Image] = {
    val file = dirName.listFiles
    val imgArr: Array[Image] = new Array[Image](file.length)

    imgArr(0) = Image(file(0), Array("000", "000", "000", "000"), 0)
    imgArr(1) = Image(file(1), Array("101", "101", "101", "101"), 0)
    imgArr(2) = Image(file(2), Array("111", "101", "111", "101"), 0)
    imgArr(3) = Image(file(3), Array("001", "100", "001", "100"), 0)
    imgArr(4) = Image(file(4), Array("111", "101", "101", "111"), 0)
    imgArr(5) = Image(file(5), Array("111", "101", "101", "101"), 0)
    imgArr(6) = Image(file(6), Array("111", "111", "111", "101"), 0)
    imgArr(7) = Image(file(7), Array("111", "100", "001", "111"), 0)
    imgArr(8) = Image(file(8), Array("000", "001", "100", "000"), 0)
    imgArr(9) = Image(file(9), Array("000", "001", "101", "100"), 0)
    imgArr(10) = Image(file(10), Array("100", "000", "001", "111"), 0)
    imgArr(11) = Image(file(11), Array("101", "100", "001", "101"), 0)
    imgArr(12) = Image(file(12), Array("101", "100", "001", "111"), 0)
    imgArr(13) = Image(file(13), Array("111", "100", "001", "101"), 0)
    imgArr
  }

  def allPerm(imgArr: Array[Image]): Set[Image] = {
    val futures = imgArr.map(img =>
      Future {
        val lastScore = img.name.toString.lastIndexOf('_')
        img.name.toString.substring(lastScore + 1, lastScore + 3) match {
          case x if x == "00" || x == "01" => Set(img)
          case x if x == "02" || x == "03" => Set(Image(img.name, Array(img.left, img.up, img.right, img.down), 1), img)
          case _ =>
            Set(img,
              Image(img.name, Array(img.left, img.up, img.right, img.down), 1),
              Image(img.name, Array(img.down, img.left, img.up, img.right), 2),
              Image(img.name, Array(img.right, img.down, img.left, img.up), 3)
            )
        }
      }
    )
    
    Await.result(Future.sequence(futures.toSet), Duration.Inf).flatten
  }

  def updateNbrsHelper(allImages: Set[Image], img: Image): Unit = {
    val newNbrs = mutable.HashMap[String, Set[Image]]()
    newNbrs.put("up", allImages.filter(i => i.down.equals(img.up.reverse)))
    newNbrs.put("down", allImages.filter(i => i.up.equals(img.down.reverse)))
    newNbrs.put("right", allImages.filter(i => i.left.equals(img.right.reverse)))
    newNbrs.put("left", allImages.filter(i => i.right.equals(img.left.reverse)))
    img.nbrs = newNbrs
  }
  def updateNbrs(allImages: Set[Image]): Unit = {
    val futures = for (i <- allImages) yield Future { updateNbrsHelper(allImages.toSet, i) }
    Await.result(Future.sequence(futures), Duration.Inf)
  }

  val file = new File("/Users/nathans./Documents/FunPar/funpar-t2-22-project-future-collapse-master/tileSet")
  val x = loadImage(file)
  val perms = allPerm(x)
  updateNbrs(perms)
  for (smt <- perms) {
    println(smt.nbrs)
  }
}

//imgArr(0) = Image(file(0), Array((0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(1) = Image(file(1), Array((1, 0, 1), (1, 0, 1), (1, 0, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(2) = Image(file(2), Array((1, 1, 1), (1, 0, 1), (1, 1, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(3) = Image(file(3), Array((0, 0, 1), (1, 0, 0), (0, 0, 1), (1, 0, 0)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(4) = Image(file(4), Array((1, 1, 1), (1, 0, 1), (1, 0, 1), (1, 1, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(5) = Image(file(5), Array((1, 1, 1), (1, 0, 1), (1, 0, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(6) = Image(file(6), Array((1, 1, 1), (1, 1, 1), (1, 1, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(7) = Image(file(7), Array((1, 1, 1), (1, 0, 0), (0, 0, 1), (1, 1, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(8) = Image(file(8), Array((0, 0, 0), (0, 0, 1), (1, 0, 0), (0, 0, 0)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(9) = Image(file(9), Array((0, 0, 0), (0, 0, 1), (1, 0, 1), (1, 0, 0)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(10) = Image(file(10), Array((1, 0, 0), (0, 0, 0), (0, 0, 1), (1, 1, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(11) = Image(file(11), Array((1, 0, 1), (1, 0, 0), (0, 0, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(12) = Image(file(12), Array((1, 0, 1), (1, 0, 0), (0, 0, 1), (1, 1, 1)), new mutable.HashMap[String, List[Image]], 0)
//imgArr(13) = Image(file(13), Array((1, 1, 1), (1, 0, 0), (0, 0, 1), (1, 0, 1)), new mutable.HashMap[String, List[Image]], 0)
