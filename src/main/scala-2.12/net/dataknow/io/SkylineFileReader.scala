package net.dataknow.io

import java.io.File

import net.dataknow.Spatial.Point

class SkylineFileReader(input: File) extends FileReader(input){

  var lastTempRead = -1

  def ReadTuple: (Point, Int) ={
    val split = ReadLine.split("@")
    val (tuple, timeStamp) = (Point.parseFromCsv(split(0)), if(split.size == 1) -1 else split(1).toInt)
    if( timeStamp > lastTempRead) lastTempRead = timeStamp
    (tuple, timeStamp)
  }

}
