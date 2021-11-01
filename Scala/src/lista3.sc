// Kacper Wójcicki

// Zad 2
def curry3 [A, B, C, D](f: (A,B,C) => D) = (x:A) => (y:B) => (z:C) => f(x,y,z)

def uncurry3[A,B,C,D](f: A => B => C => D) = (x: A, y: B, z: C) => f(x)(y)(z)

// Zad 3
def sumProd(xs:List[Int]): (Int,Int) = 
    xs.foldLeft(0, 1)((acc:(Int, Int), x: Int) => (x + acc._1, x * acc._2))

sumProd(List(1,2,3,4)) == (10, 24)
sumProd(List()) == (0, 1)
  