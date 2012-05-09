object Main {
    def square(element:Double):Double = {
        return (element * element)
  }
  def sum(a:Double, b:Double):Double = {
        return (a + b)
  }
  def main(args: Array[String]) {
    var numbers:Array[Double] = Array(0.);
    def fill_arr(index:Double):Double = {
            return (index + 1.)
    }
    numbers = Dotpar.fill(fill_arr, 500000.)
;
    Dotpar.map(numbers, square);
    Dotpar.reduce(numbers, sum, 0.)
;
    var list_comp:Array[Double] = (numbers.filter({(x:Double) => ((x % 2.) == 0.)}).map({(x:Double) => (x * x)})).toArray;
;
  }

}

