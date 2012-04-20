object Dotpar {
  // ??? print?

  // multiplexed print
  def println(x: Any) = {
    def dotpar_string(y: Any):String = {
      y match {
        case str: Array[Char] =>
          str.map(_.toString).reduceLeft(_+_)
        case arr: Array[_] =>
          \"[\" + arr.map(dotpar_string(_)).reduceLeft(_+\", \"+_) + \"]\"
        case _ => y.toString
      }
    }
    println(dotpar_string(x))
  }

  // converts double to int for array index, throws error
  def array_index(ind: Double):Int = {
    val err = 0.00001; // chosen by fiat
    val r:Int = scala.math.round(ind).intValue
    if(scala.math.abs(r - ind) > err) {
      throw new IllegalArgumentException(\"Only integeral array indicies allowed\")
    }
    r
  }

  // each
  // map
  // reduce
}
