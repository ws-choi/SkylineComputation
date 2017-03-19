package net.dataknow.DataGenerator

import net.dataknow.Spatial.Point

object UniformDistGenerator extends DataGenerator{

  override def next(dim: Int): Point = {
    Point(dim, (1 to dim).toList.map(_ => Math.random()).toArray)
  }

  override def next(dim: Int, scale:Int): Point = {
    Point(dim, (1 to dim).toList.map(_ => Math.floor(Math.random() * Math.pow(10,scale))  ).toArray)
  }



}
