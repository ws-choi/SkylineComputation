package net.dataknow.DataGenerator

import net.dataknow.Spatial.Point

trait DataGenerator {

  def csv(dim:Int, size:Int) = get(dim, size).map(toCsv(_))
  def csv(dim:Int, size:Int, scale:Int) = get(dim, size, scale).map(toCsv(_))

  def json(dim:Int, size:Int) = get(dim, size).map(toJson(_))
  def json(dim:Int, size:Int, scale:Int) = get(dim, size, scale).map(toJson(_))

  def get(dim:Int, size:Int) = (1 to size).map(_ => (next(dim))).toArray
  def get(dim:Int, size:Int, scale:Int) = (1 to size).map(_ => (next(dim, scale))).toArray

  def next (dim:Int):Point
  def next(dim: Int, scale:Int): Point

  def toJson (data: Point):String = {
    "{" + data.values.zipWithIndex.map(x =>("\"dim("+ (x._2 + 1) + ")\":\"" + x._1 + "\"") ).reduce(  (x,y) => x + "," + y ) + "}"
  }

  def toCsv(data: Point): String = data.values.map( _.toString()).reduce( (x,y) => x + "," + y )

}
