
// Kacper Wójcicki

// zadanie 1
def flatten1[A] (xss: List[List[A]]): List[A] = {
  def flaten2[A] (xss: List[List[A]], rs: List[A]): List[A] = {
    if xss == List() then rs
    else flaten2(xss.tail, rs ::: xss.head)
  }
  flaten2(xss, List())
}

flatten1(List(List(1,2), List(3,4), List(5,6))) == List(1,2,3,4,5,6)
flatten1(List(List())) == List()
flatten1(List(List(List('a','b'), 'c'), List('d'))) == List(List('a','b'),'c','d')

// zadanie 2
def count[A] (x: A, xs: List[A]) : Int = {
  def countRecursion[A] (x: A, xs: List[A], r: Int) : Int = {
    if(xs == List()) then r
    else {
      if(xs.head == x) then countRecursion(x, xs.tail, r+1)
      else countRecursion(x, xs.tail, r)
    }
  }
  if!xs.contains(x) then 0
  else countRecursion(x, xs, 0)
}

count ('a', List('a', 'l', 'a')) == 2
count(1, List(2,3,4)) == 0
count(1.2, Nil) == 0

// zadanie 3

def replicate[A] (x: A, n: Int): List[A] = {
  def replicateIn[A](x: A, n:Int, xs:List[A]) : List[A] = {
    if n == 0 then xs
    else replicateIn(x, n-1, x::xs)
  }
  replicateIn(x, n, List())
  //List.fill(n)(x)
}

replicate('a', 3) == List('a', 'a', 'a')
replicate('a', 0) == List()

// zadanie 4

def sqrList(xs: List[Int]): List[Int] = {
  if xs == List() then List()
  else (xs.head * xs.head) :: sqrList(xs.tail)
  //xs.map(x => x * x)
}

sqrList(List(1,2,3,-4)) == List(1,4,9,16)

//val sqrList2: List[Int] => List[Int] = (xs: List[Int]) => xs.map(x => x*x)

val sqrList3: List[Int] => List[Int] = (xs: List[Int]) => {
  if xs == List() then List()
  else (xs.head * xs.head) :: sqrList3(xs.tail)
}

sqrList3(List(1,2,3,-4)) == List(1,4,9,16)
// zadanie 5
def palindrome[A] (xs: List[A]): Boolean = {
  def palindromeChecker[A] (xs:List[A], i: Int) : Boolean = {
    if i >= (xs.length / 2) then true
    else if xs(i) != xs(xs.length - 1 - i) then false
    else palindromeChecker(xs, i+1)
  }
  palindromeChecker(xs, 0)
}

palindrome(List('a', 'l', 'a')) == true
palindrome(List('k', 'a', 'j', 'a', 'k')) == true
palindrome(List('k', 'a', 'j', 'b',  'a', 'k')) == false

// zadanie 6
def listLength[A](xs: List[A]): Int = {
  def listLengthRecursion[A](xs: List[A], i: Int): Int = {
    if xs(i) == xs.last then i+1
    else listLengthRecursion(xs, i+1)
  }
  if xs == List() then 0
  else listLengthRecursion(xs,0)
}

listLength(List(1,2,3)) == 3
listLength(List()) == 0

