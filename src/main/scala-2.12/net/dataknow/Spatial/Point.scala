package net.dataknow.Spatial

object Point{
  def apply(size: Int, values: Array[Double]): Point = new Point(size, values)

  def parseFromCsv(string:String): Point ={
    val values = string.split(",").map(string => string.toDouble).array
    Point(values.length, values)
  }
}
class Point(val dim: Int, val values:Array[Double]){

  override def equals(obj: scala.Any): Boolean = {
    if (super.equals(obj)) return true
    val other = obj.asInstanceOf[Point]
    this.dim == other.dim && this.values.zip(other.values).forall( pair => pair._1 == pair._2)
  }
  def dominate (other: Point) = {
    val compVec = this.values.zip(other.values)
    compVec.forall( pair => pair._1 <= pair._2) && compVec.exists(pair => pair._1 < pair._2)
  }
  def toCSV : String =  values.map(v=>v.toString).reduce((a,b)=>a+","+b)
  override def toString: String = toCSV
}
