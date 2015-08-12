package roll
import scala.scalajs.js

import scala.scalajs.js.annotation.JSName
import acyclic.file
package object cp{
  import acyclic.pkg

  @JSName("cp")
  object Cp extends js.Object{

    def momentForBox(m: Double, width: Double, height: Double): Double = ???
    def areaForPoly(verts: Array[Double]): Double = ???
    def momentForPoly(m: Double, verts: Array[Double], offset: Vect): Double = ???
    def centroidForPoly(verts: Array[Double]): Vect = ???
    def recenterPoly(verts: Array[Double]): Unit = ???
  }
}