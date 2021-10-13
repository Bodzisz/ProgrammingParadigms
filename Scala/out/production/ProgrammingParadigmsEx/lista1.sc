
// Kacper Wójcicki

// zadanie 1

def flaten2[A] (xss: List[List[A]], rs: List[A]): List[A] = {
  if xss == List() then rs
  else flaten2(xss.tail, rs ::: xss.head)
}


def flatten1[A] (xss: List[List[A]]): List[A] = {
  flaten2(xss, List())
}

flatten1(List(List(1,2), List(3,4), List(5,6))) == List(1,2,3,4,5,6)
flatten1(List(List())) == List()
flatten1(List(List(List('a','b'), 'c'), List('d'))) == List(List('a','b'),'c','d')

// zadanie 2

def countRecursion[A] (x: A, xs: List[A], r: Int) : Int = {
  if(xs == List()) then r
  else {
    if(xs.head == x) then countRecursion(x, xs.tail, r+1)
    else countRecursion(x, xs.tail, r)
  }
}

def count[A] (x: A, xs: List[A]) : Int = {
  if!xs.contains(x) then 0
  else countRecursion(x, xs, 0)
}

count ('a', List('a', 'l', 'a')) == 2
count(1, List(2,3,4)) == 0
count(1.2, Nil) == 0

// zadanie 3

def replicate[A] (x: A, n: Int): List[A] = {
  List.fill(n)(x)
}

replicate('a', 3) == List('a', 'a', 'a')

// zadanie 4

def sqrListRecursion(xs: List[Int], rs: List[Int], i: Int): List[Int] = {
  if i < 0 then rs
  else sqrListRecursion(xs, (xs(i) * xs(i))::rs, i-1)
}

def sqrList(xs: List[Int]): List[Int] = {
  sqrListRecursion(xs, Nil, xs.length-1)
}

sqrList(List(1,2,3,-4)) == List(1,4,9,16)

//val sqrList2: List[Int] => List[Int] = (xs: List[Int]) => xs.map(x => x*x)

val sqrList3: List[Int] => List[Int] = (xs: List[Int]) => {
  if xs == List() then List()
  else (xs.head * xs.head) :: sqrList3(xs.tail)
}

sqrList3(List(1,2,3,-4)) == List(1,4,9,16)
// zadanie 5

def palindromeChecker[A] (xs:List[A], i: Int) : Boolean = {
  if i >= (xs.length / 2) then true
  else if xs(i) != xs(xs.length - 1 - i) then false
  else palindromeChecker(xs, i+1)
}

def palindrome[A] (xs: List[A]): Boolean = {
  palindromeChecker(xs, 0)
}

palindrome(List('a', 'l', 'a')) == true
palindrome(List('k', 'a', 'j', 'a', 'k')) == true
palindrome(List('k', 'a', 'j', 'b',  'a', 'k')) == false

// zadanie 6

def listLengthRecursion[A](xs: List[A], i: Int): Int = {
  if xs(i) == xs.last then i+1
  else listLengthRecursion(xs, i+1)
}

def listLength[A](xs: List[A]): Int = {
  if xs == List() then 0
  else listLengthRecursion(xs,0)
}

listLength(List(1,2,3)) == 3
listLength(List()) == 0

