import scala.annotation.tailrec
// Kacper Wójcicki

// Zad 2

def fib(n: Int) : Int = {
  if n < 0 then throw new Exception("Invalid argument!")
  else if n == 0 then 0
  else if n == 1 then 1
  else fib(n-1) + fib(n-2)
}

fib(4) == 3

def fib2(n: Int) : Int = {
  @tailrec
  def fibTailRec(n:Int, x1:Int, x2:Int) : Int = {
    if n == 0 then x2
    else fibTailRec(n - 1, x2, x1+x2)
  }
  if n < 0 then throw new Exception("Invalid argument!")
  else fibTailRec(n, 1, 0)
}

fib2(4) == 3

//fib(44)
//fib2(44)

// Zad 3

def root3(a : Double, epsilon : Double) : Double = {
  @tailrec
  def root3Rec(a : Double, i: Int, res: Double, epsilon : Double) : Double = {
    if math.abs(res * res * res - a) <= epsilon * math.abs(a) then res
    else {
      if i == 0 then {
        if a > 1 then root3Rec(a, i+1, a / 3, epsilon)
        else root3Rec(a, i+1, a, epsilon)
      }
      else root3Rec(a, i+1, res + (a / (res * res) - res) / 3, epsilon)
    }
  }

  root3Rec(a, 0, 0, epsilon)
}

root3(8, 0.000000000001) == 2.0

val root3FunExtended: List[Double] => Double = (xs:List[Double]) => {
  if math.abs(xs(1) * xs(1) * xs(1) - xs(0)) <= xs(2) * math.abs(xs(0)) then xs(1)
  else root3FunExtended(List(xs(0), xs(1) + (xs(0) / (xs(1) * xs(1)) - xs(1)) / 3, xs(2)))
}

val root3Fun: Double => Double = (a:Double) => {
  if a > 1 then root3FunExtended(List(a, a/3, 0.00001))
  else root3FunExtended(List(a,a, 0.00001))
}

root3Fun(8)

