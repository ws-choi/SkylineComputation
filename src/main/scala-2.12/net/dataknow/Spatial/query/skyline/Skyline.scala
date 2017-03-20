package net.dataknow.Spatial.query.skyline
import java.io.{File, FileWriter}

import util.control.Breaks._
import net.dataknow.DataGenerator.UniformDistGenerator
import net.dataknow.Spatial.Point
import net.dataknow.io.{FIleWriter_, FileReader, SkylineFileReader}

import scala.collection.mutable

object Skyline {

  def isSame(A: Set[Point], B: Set[Point]): Boolean = {

    A.forall( a => B.exists(b=> a==b)) && B.forall( b => A.exists(a=> a==b)) //Warning: Contains Not work. Do not change it
  }

  def main(args: Array[String]): Unit = {

    val result =(1 to 100).forall { x=>
      generateInputFile(1000, 2)

      val NLResult = withResponse(Skyline.BreakableNestedLoop(new File("input")))
      val BNLResult = withResponse(Skyline.BlockNestedLoop(new File("input"), 50))
      //println()

      val res = isSame(NLResult._1, BNLResult._1)

      println(x+": NL-" + NLResult._2 +", BNL-"+BNLResult._2 +", result: " + res)

      res
    }


  }

  def generateInputFile (size: Int, dim: Int)= {

    val writer = new FIleWriter_("input")
    (1 to size).foreach(x=>writer.WriteLine(UniformDistGenerator.next(dim).toString))
    writer.close
  }

  def NestedLoop(input: File): Set[Point] = {

    val outerReader = new FileReader(input)
    val result = new mutable.ArrayBuffer[Point]

    while(outerReader.hasNext){
      val outer = Point.parseFromCsv(outerReader.ReadLine)
      val innerReader = new FileReader(input)
      var outerIsDominated = false

      while(innerReader.hasNext){
        val inner = Point.parseFromCsv(innerReader.ReadLine)
        if(inner dominate outer) {
          outerIsDominated = true
        }
      }
      innerReader.close()
      if(!outerIsDominated) result += outer
    }

    outerReader.close()
    result.toSet
  }
  def BreakableNestedLoop(input: File): Set[Point] = {

    val outerReader = new FileReader(input)
    val result = new mutable.ArrayBuffer[Point]


    while (outerReader.hasNext) {
      val outer = Point.parseFromCsv(outerReader.ReadLine)
      val innerReader = new FileReader(input)
      var outerIsDominated = false

      breakable {
        while (innerReader.hasNext) {
          val inner = Point.parseFromCsv(innerReader.ReadLine)
          if (inner dominate outer) {
            outerIsDominated = true
            break
          }
        }

        if (!outerIsDominated) result += outer
      }
      innerReader.close()
    }
    outerReader.close()
    result.toSet
  }

  def BlockNestedLoop(input: File, i: Int): Set[Point] = {

    val file = new File("skyline")
    if(file.exists()){
      System.gc()
       new File(file.getAbsolutePath)
         .getParentFile
         .listFiles()
         .filter(f => f.getPath.contains("temp"))
         .foreach(f => f.delete())
      file.delete()

    }
    BlockNestedLoop(input, new File("skyline"), i)
    val result = new mutable.ArrayBuffer[Point]
    val outputReader = new FileReader(new File("skyline"))
    while(outputReader.hasNext) result += Point.parseFromCsv(outputReader.ReadLine)
    outputReader.close()

    result.toSet
  }


  def BlockNestedLoop(input:File, skylineOutput:File, sizeOfWindow: Int):Unit = {

    var inputReader = new SkylineFileReader(input)
    val window = new mutable.LinkedHashSet[(Point, Int)]
    val outputFilePath = skylineOutput.getPath
    var notFinish = true
    var curTimeStamp = 0

    while(notFinish) //start Iteration - for File (input or temp)
    {
      var lastTempRead = -1
      val tempFileName = "temp" + curTimeStamp
      val tempWriter = new FIleWriter_(tempFileName)
      var tempFileRead = false

      while(inputReader.hasNext){ //scan all tuples in input file

        tempFileRead = true
        val (p, timestamp) = inputReader.ReadTuple //Read a tuple From the given file
        if( timestamp > lastTempRead) lastTempRead = timestamp

        var pIsDominated = false
        val eliminateFromWindow = new mutable.LinkedHashSet[(Point, Int)]

        breakable {
          for ( (w, wstamp) <- window) {
            if( w dominate p){ //case 1
              pIsDominated = true
              break
            }
            else if (p dominate w)
              eliminateFromWindow add (w, wstamp) // case 2
          }
        }

        eliminateFromWindow.foreach(e => window.remove(e)) //case 2

        //Compare to Skyline obj
        if(skylineOutput.exists()){
          val skylineReader = new FileReader(skylineOutput)
          breakable {
            while (skylineReader.hasNext) {
              val s = Point.parseFromCsv(skylineReader.ReadLine)
              if (s dominate p) {
                pIsDominated = true
                break
              }
            }
          }

          skylineReader.close()
        }

        if (!pIsDominated) {

          val writeTimeStamp = getCurrentWithIncrement//if(timestamp == -1) getCurrentWithIncrement else timestamp

          if (window.size < sizeOfWindow) //case 3
            window add (p, writeTimeStamp)

          else
              tempWriter.WriteLine(p.toString+"@"+curTimeStamp)
        }

      }

      val skyline = window.filter( w_with_stamp => w_with_stamp._2 < lastTempRead)
      printSkylinesToFile(skyline)
      skyline.foreach(s => window.remove(s))


      tempWriter.close //flush
      if(!tempFileRead) {
        notFinish = false
        printSkylinesToFile(window)
        inputReader.close()
      }

      if(input.getPath.contains("temp")) {
        inputReader.close()
        input.deleteOnExit()
      }

      inputReader.close()
      inputReader = new SkylineFileReader(new File(tempFileName))
    }

    def getCurrentWithIncrement = {
      val out = curTimeStamp
      curTimeStamp += 1
      out
    }

    def printSkylinesToFile (skylines: Iterable[(Point, Int)]):Unit = {
      val outputWriter = new FileWriter(outputFilePath, true)
      skylines.foreach(s => outputWriter.append(s._1.toString() + "\n"))
      outputWriter.close() //flush
    }

  }

  def withResponse(func: =>Set[Point]): (Set[Point], Long) = {
    val start = System.currentTimeMillis()
    val res = func
    val end = System.currentTimeMillis()
    (res, end-start)
  }
}
