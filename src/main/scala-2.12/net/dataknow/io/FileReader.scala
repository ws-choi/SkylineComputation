package net.dataknow.io

import java.io.File

import scala.io.Source

class FileReader ( inputFile: File) {

  var fileReader = Source.fromFile(inputFile).bufferedReader()
  var line:String = null
  def ReadLine = line
  def hasNext = {
    line = fileReader.readLine()
    line != null
  }

}
