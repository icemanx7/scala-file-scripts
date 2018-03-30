import java.io.File

import scala.annotation.tailrec

object ReadFiles {

  def main(args: Array[String]): Unit = {
    println("Richard")
    val files = getListOfFiles("/home/icemanx7/Documents")
    println(makeAllFiles(files, List()))
    println(reduceFoldersToList("/home/icemanx7/Documents").filter(x => x.getName.takeRight(3) == "pdf").length)
    reduceFoldersToList("/home/icemanx7/Documents").filter(x => x.getName.takeRight(3) == "pdf").foreach(println)
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList
    } else {
      List[File]()
    }
  }

  def reduceFoldersToList(path: String): List[File] =  {
    val files = getListOfFiles(path)
    @tailrec
    def reduceAll(files: List[File]): List[File] =  {
      val doesContainFolder = files.map(x => x.isDirectory).fold(false)(_ || _)
      if(doesContainFolder) {
        reduceAll(makeAllFiles(files,List()))
      }
      else {
        files
      }

    }
    reduceAll(files)
  }

  @tailrec
  def makeAllFiles(files: List[File], acc: List[File]): List[File] = files match {
    case Nil => acc
    case x :: rest => {
      if(x.isDirectory) {
        println(x.getAbsolutePath)
        makeAllFiles(rest, getListOfFiles(x.getAbsolutePath) ++ acc)
      }
      else {
        makeAllFiles(rest, List[File](x) ++ acc)
      }
    }
  }





}
