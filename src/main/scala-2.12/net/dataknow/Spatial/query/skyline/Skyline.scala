package net.dataknow.Spatial.query.skyline
import java.io.{File, FileWriter}

import util.control.Breaks._
import net.dataknow.DataGenerator.UniformDistGenerator
import net.dataknow.Spatial.Point
import net.dataknow.io.{FIleWriter_, FileReader, SkylineFileReader}

import scala.collection.mutable

object Skyline {

  def isSame(A: Set[Point], B: Set[Point]): Boolean = {

    A.forall( a => B.exists(b=> a==b)) && B.forall( b => A.exists(a=> a==b))
  }

  def main(args: Array[String]): Unit = {


/*

    val result =(1 to 100).forall { x=>
      generateInputFile(1000, 6)

*/

      val NLResult = withResponse(Skyline.BreakableNestedLoop(new File("input")))
      val BNLResult = withResponse(Skyline.NewBlockNestedLoop(new File("input"), 30))
      println()

/*

      val res = isSame(NLResult._1, BNLResult._1)

      println(x+": NL-" + NLResult._2 +", BNL-"+BNLResult._2 +", result: " + res)

      res
    }

*/


 //   println(result)

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
      var outerIsDominated = false;

      while(innerReader.hasNext){
        val inner = Point.parseFromCsv(innerReader.ReadLine)
        if(inner dominate outer) {
          outerIsDominated = true;
        }
      }

      if(!outerIsDominated) result += outer
    }

    result.toSet
  }
  def BreakableNestedLoop(input: File): Set[Point] = {

    val outerReader = new FileReader(input)
    val result = new mutable.ArrayBuffer[Point]


    while (outerReader.hasNext) {
      val outer = Point.parseFromCsv(outerReader.ReadLine)
      val innerReader = new FileReader(input)
      var outerIsDominated = false;

      breakable {
        while (innerReader.hasNext) {
          val inner = Point.parseFromCsv(innerReader.ReadLine)
          if (inner dominate outer) {
            outerIsDominated = true;
            break;
          }
        }

        if (!outerIsDominated) result += outer
      }
    }
    result.toSet
  }

  def BlockNestedLoop(input:File, sizeOfWindow: Int): Set[Point] = BlockNestedLoop(input, Set.empty, sizeOfWindow)

  def BlockNestedLoop(input:File, windwBefore: Set[Point], sizeOfWindow: Int): Set[Point] = {

    val outerReader = new FileReader(input)
    val tempWriter = new FIleWriter_(input.getPath+".temp")
    val window = new mutable.LinkedHashSet[Point]
    var writerUsed = false;

    while (outerReader.hasNext) {

      val p = Point.parseFromCsv(outerReader.ReadLine)
      var pIsDominated = false
      val eliminateFromWindow = new mutable.LinkedHashSet[Point]


      val iter = window.iterator

      breakable {
        while (iter.hasNext) {
          val w = iter.next()
          if (w dominate p) // case 1
            {
              pIsDominated = true
              break
            }
          else if (p dominate w) //case 2
            eliminateFromWindow += w

        }
      }

      eliminateFromWindow.foreach(e => window.remove(e)) //case 2

      if(!pIsDominated) if(windwBefore.exists(w => w dominate p)) pIsDominated = true
      if(!pIsDominated) {
        if (window.size < sizeOfWindow) //case 3
          window += p
        else {
          writerUsed = true;
          tempWriter.WriteLine(p.toString)
        }
      }
    }

    tempWriter.close

    if(!writerUsed) window.toSet
    else (window.toSet ++ BlockNestedLoop(new File(input.getPath + ".temp"), window.toSet ++ windwBefore, sizeOfWindow))
  }

  def NewBlockNestedLoop(input: File, i: Int): Set[Point] = {

    val file = new File("skyline")
    if(file.exists()){
       (new File(file.getAbsolutePath))
         .getParentFile
         .listFiles()
         .filter(f => f.getPath.contains("temp"))
         .foreach(f => f.delete())
      file.delete()
    }
    NewBlockNestedLoop(input, new File("skyline"), i)
    val result = new mutable.ArrayBuffer[Point]
    val outputReader = new FileReader(new File("skyline"))
    while(outputReader.hasNext) result += Point.parseFromCsv(outputReader.ReadLine)

    return result.toSet
  }

  def NewBlockNestedLoop(input:File, output:File, sizeOfWindow: Int):Unit = {

    var inputReader = new SkylineFileReader(input)
    val tempWriter = new FIleWriter_(input.getPath+".temp")
    val window = new mutable.LinkedHashSet[(Point, Int)]
    var notFinish = true

    while(notFinish) //start Iteration - for File (input or temp)
    {
      while(inputReader.hasNext){ //scan all tuples in input file

        val (p, timestamp) = inputReader.ReadTuple //Read a tuple From the given file

        var pIsDominated = false
        val eliminateFromWindow = new mutable.LinkedHashSet[(Point, Int)]

        breakable {
          for ( (w, wstamp) <- window) {
            if( w dominate p){ //case 1
              pIsDominated = true
              break
            }
            else if (p dominate w)
              ;
          }
          while (iter.hasNext) {
            val w = iter.next()
            if (w dominate p) // case 1
            {
              pIsDominated = true
              break
            }
            else if (p dominate w) //case 2
              eliminateFromWindow += w

          }
        }
        eliminateFromWindow.foreach(e => window.remove(e)) //case 2


      }

      // If we read a tuple from the temporary file with timestamp t,
      // we can output all tuples from the window with timestamp smaller than t.
    }



/*
    while (inputReader.hasNext) {

      val p = Point.parseFromCsv(inputReader.ReadLine)
      var pIsDominated = false
      val eliminateFromWindow = new mutable.LinkedHashSet[Point]
      val iter = window.iterator
      breakable {
        while (iter.hasNext) {
          val w = iter.next()
          if (w dominate p) // case 1
          {
            pIsDominated = true
            break
          }
          else if (p dominate w) //case 2
            eliminateFromWindow += w

        }
      }
      eliminateFromWindow.foreach(e => window.remove(e)) //case 2

      if(output.exists()) {
        val skylineReader = new FileReader(output)
        breakable{
          while(skylineReader.hasNext){
            val s = Point.parseFromCsv(skylineReader.ReadLine)
            if(s dominate p){
              pIsDominated = true
              break
            }
          }
        }
      }

      if (!pIsDominated) {
        if (window.size < sizeOfWindow) //case 3
          {
            window += p/*
            if(writerUsed)
              tempWriter.WriteLine(p.toString)*/
          }
        else {
          writerUsed = true;
          tempWriter.WriteLine(p.toString)
        }
      }
    }

    val outputWriter = new FileWriter("skyline", true)
    window.foreach(w=>outputWriter.write(w.toString +"\n"))
    outputWriter.close

    tempWriter.close
    
    if(writerUsed)
      NewBlockNestedLoop(new File(input.getPath+".temp"), new File("skyline"), sizeOfWindow)
*/
  }

  def withResponse(func: =>Set[Point]): (Set[Point], Long) = {
    val start = System.currentTimeMillis()
    val res = func
    val end = System.currentTimeMillis()
    (res, end-start)
  }
}
