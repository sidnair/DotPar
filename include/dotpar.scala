import scala.collection.mutable.ArraySeq

object Dotpar {
  // ??? print?

  // multiplexed print
  def dp_println(x: Any) = {
    def dotpar_string(y: Any):String = {
      y match {
        case str: ArraySeq[Char] =>
          str.map(_.toString).reduceLeft(_+_)
        case arr: ArraySeq[_] =>
          "[" + arr.map(dotpar_string(_)).reduceLeft(_+", "+_) + "]"
        case _ => y.toString
      }
    }
    println(dotpar_string(x))
  }

  // converts double to int for array index, throws error
  def dp_array_index(ind: Double):Int = {
    val err = 0.00001; // chosen by fiat
    val r:Int = scala.math.round(ind).intValue
    if(scala.math.abs(r - ind) > err) {
      throw new IllegalArgumentException("Only integeral array indicies allowed")
    }
    r
  }

  // each
  def dp_each[T](arr:ArraySeq[T], function:((T) => Any)) = {
    arr foreach function
  }
  // filter
  def dp_filter[T](arr:ArraySeq[T], function:((T) => Boolean)):ArraySeq[T] = {
    arr.filter(function)
  }
  // map
  def dp_map[T, TT](arr:ArraySeq[T], function:(T => TT)):ArraySeq[TT] = {
    arr map function
  }
  // reduce
  def dp_reduce[T](arr:ArraySeq[T], function:((T, T) => T)):T = {
    arr reduceLeft function
  }
}
