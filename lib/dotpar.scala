import scala.collection.mutable.ArraySeq

object Dotpar {
  // multiplexed print
  def println(x: Any) = {
    def dotpar_string(y: Any):String = {
      y match {
        case str: Array[Char] =>
          str.map(_.toString).reduceLeft(_+_)
        case arr: Array[_] =>
          "[" + arr.map(dotpar_string(_)).reduceLeft(_+", "+_) + "]"
        case _ => y.toString
      }
    }
    Console.println(dotpar_string(x))
  }

  // converts double to int for array index, throws error
  def array_index(ind: Double):Int = {
    val err = 0.00001; // chosen by fiat
    val r:Int = scala.math.round(ind).intValue
    if(scala.math.abs(r - ind) > err) {
      throw new IllegalArgumentException("Only integeral array indicies allowed")
    }
    r
  }

  def fill[T : Manifest](fn:((Double) => T), num:Double) = {
    var tmp_arr = new Array[T](num.toInt)
    for (i <- 0 until num.toInt) {
      tmp_arr.update(i, fn(i));
    }
    tmp_arr
  }

  // each
  def each[T](arr:Array[T], function:((T) => Any)) = {
    arr foreach function
  }
  // filter
  def filter[T](arr:Array[T], function:((T) => Boolean)):Array[T] = {
    arr.filter(function)
  }
  // map
  def map[T, TT](arr:Array[T], function:(T => TT))
      (implicit m:ClassManifest[TT]):Array[TT] = {
    (arr map function).toArray
  }
  // map
  def par_map[T, TT](arr:Array[T], function:(T => TT))
      (implicit m:ClassManifest[TT]):Array[TT] = {
    (arr.par map function).toArray
  }
  // reduce
  def reduce[T](arr:Array[T], function:((T, T) => T), start:T):T = {
    if (arr.length == 0) {
      start
    } else {
      arr reduceLeft function
    }
  }
  // TODO: make parallel
  def par_reduce[T](arr:Array[T], function:((T, T) => T), start:T):T = {
    if (arr.length == 0) {
      start
    } else {
      arr reduceLeft function
    }
  }
}
