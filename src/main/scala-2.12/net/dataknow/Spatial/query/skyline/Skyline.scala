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

    (1 to 1000).forall { x=>

      generateInputFile(1000, 6)

      val NLResult = withResponse(Skyline.BreakableNestedLoop(new File("input")))
      val BNLResult = withResponse(Skyline.BlockNestedLoop(new File("input"), 50))
      val res = isSame(NLResult._1, BNLResult._1)
      println(x+": NL-" + NLResult._2 +", BNL-"+BNLResult._2 +", result: " + res)
      res
    }

  }

  // generate a random data set and write it to file
  def generateInputFile (size: Int, dim: Int)= {

    val writer = new FIleWriter_("input")
    (1 to size).foreach(x=>writer.WriteLine(UniformDistGenerator.next(dim).toString))
    writer.close()
  }

  // Simple, Nested, Loop to compute skyline
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

  // Simple, Nested, Loop to compute skyline with small optimization by utilizing 'break' statement
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

  def BlockNestedLoop(input: File, winodwSize: Int): Set[Point] = {

    val file = new File("skyline")

    // Delete all temp files
    if(file.exists()){
      System.gc()
       new File(file.getAbsolutePath)
         .getParentFile
         .listFiles()
         .filter(f => f.getPath.contains("temp"))
         .foreach(f => f.delete())
      file.delete()
    }

    // Compute Skyline
    BlockNestedLoop(input, new File("skyline"), winodwSize)

    // Read Skyline from file
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

      val tempFileName = "temp" + curTimeStamp
      val tempWriter = new FIleWriter_(tempFileName)
      var tempFileRead = false

      while(inputReader.hasNext){ //scan all tuples in input file

        tempFileRead = true

        val (p, _) = inputReader.ReadTuple //Read a tuple From the given file

        var pIsDominated = false
        val eliminateFromWindow = new mutable.LinkedHashSet[(Point, Int)]

        breakable {
          for ( (w, wStamp) <- window) {

            //case 1: p is dominated by a tuple within the window
            if( w dominate p){
              pIsDominated = true
              break
            }

            //case 2: p dominates one or more tuples in the window
            else if (p dominate w)
              eliminateFromWindow add (w, wStamp)
          }
        }

        //case 2: In this case, these tuples are eliminated
        eliminateFromWindow.foreach(e => window.remove(e)) //case 2

        // (Personally added code) I think this code is not necessary: pIsDominated = pIsDominated || compareToExisting(p)

        //case 3: incomparable
        if (!pIsDominated) {
          // Assign a new timestamp
          val writeTimeStamp = getCurrentWithIncrement
          // if there is enough room for p
          if (window.size < sizeOfWindow) window.add(p, writeTimeStamp)
          //else write p to temp file
          else tempWriter.WriteLine(p.toString+"@"+curTimeStamp)
        }

      }

      /*If we read a tuple from the temporary file with timestamp t,
      we can output all tuples from the window with timestamp smaller than t */

      val skyline = window.filter( w_with_stamp => w_with_stamp._2 < inputReader.lastTempRead)
      printSkylinesToFile(skyline)
      skyline.foreach(s => window.remove(s))

      //flush
      tempWriter.close()

      // empty temp File: means there is no element to check further. Terminate.
      if(!tempFileRead) {
        notFinish = false
        printSkylinesToFile(window) //flush elements
        inputReader.close()
      }
      // else, go to next iteration
      else{
        inputReader.close()
        inputReader = new SkylineFileReader(new File(tempFileName))
      }

    }

    // below is useful method to simplify code by encapsulating sophisticated but not interesting task

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

  // also used to simplify code
  def withResponse(func: =>Set[Point]): (Set[Point], Long) = {
    val start = System.currentTimeMillis()
    val res = func
    val end = System.currentTimeMillis()
    (res, end-start)
  }
}
