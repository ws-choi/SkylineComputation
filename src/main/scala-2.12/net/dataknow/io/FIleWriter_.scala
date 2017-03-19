package net.dataknow.io

import java.io.{BufferedWriter, File, FileWriter}

import scala.io.Source

class FIleWriter_(val fileName:String) {

  var fileWriter = new BufferedWriter(new FileWriter(new File(fileName)))
  def WriteLine(string:String) = fileWriter.write(string + "\n")
  def AppendLine(string:String) = fileWriter.append(string +"\n")
  def close = fileWriter.close()


}
