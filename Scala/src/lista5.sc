// Kacper Wójcicki

// Zad 1
def lrepeat[A](k : Int, llist : LazyList[A]) : LazyList[A] =
  def lrepeatIn(inCounter : Int, element : A, tail : LazyList[A]) : LazyList[A] =
    inCounter match
      case 0 => tail
      case _ => lrepeatIn(inCounter - 1, element, element #:: tail)
  if llist == LazyList() then LazyList()
  else  lrepeatIn(k, llist.head, lrepeat(k, llist.tail))

lrepeat(3, LazyList(1,2,3)).toList == List(1,1,1,2,2,2,3,3,3)
lrepeat(3, LazyList('a','b','c','d')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(0, LazyList(1,2,3)).toList == List()
lrepeat(5, LazyList()).toList == List()
