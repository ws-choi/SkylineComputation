package net.dataknow.io

import java.io.File

import net.dataknow.Spatial.Point

class SkylineFileReader(input: File) extends FileReader(input){

  def ReadTuple: (Point, Int) ={
    val split = ReadLine.split("@")
    (Point.parseFromCsv(split(0)), if(split.size == 1) -1 else split(1).toInt)
  }

}
