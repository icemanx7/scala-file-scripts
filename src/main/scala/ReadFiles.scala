import java.io.File

import scala.annotation.tailrec
import scala.util.Try

object ReadFiles {

  def main(args: Array[String]): Unit = {
    initMove(args)
  }

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.toList
    } else {
      List[File]()
    }
  }

  def getCommandArgs(cArgs: Array[String]): Option[(String, String)] = {
    Try((cArgs(0), cArgs(1))).toOption
  }

  def initMove(cArgs: Array[String]): Unit = {
    val somePathAndType = getCommandArgs(cArgs)
    somePathAndType match {
      case None =>
        println("Please input 2 path arguments.")
        println("usage: scala Readfiles.scala <path> <file type>")
      case Some(pathType) =>
        val initList = reduceFoldersToList(pathType._1)
        val filterList = filterByFileType(pathType._2, initList)
        moveToSuppliedFolder(pathType._1, filterList)
    }
  }


  def moveToSuppliedFolder(origPath: String, list: List[File]): Unit = list.map { x =>
    x.renameTo(getSrcAndDest(origPath, x))
  }

  def getSrcAndDest(origPath: String, file: File) = {
    new File(s"${origPath}/${file.getName}")
  }

  def filterByFileType(fileType: String, list: List[File]): List[File] = {
    list.filter(x => x.getName.takeRight(3) == fileType)
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
