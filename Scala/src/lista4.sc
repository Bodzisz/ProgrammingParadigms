// Kacper Wójcicki

// Zad 3

sealed trait BT[A]
case class Empty[A]() extends BT[A]
case class Node[A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val tt = Node(1,
Node(2,
Node(4,
Empty[Int](),
Empty[Int]()
),
Empty[Int]()
),
Node(3,
Node(5,
Empty[Int](),
Node(6,
Empty[Int](),
Empty[Int]()
)
),
Empty[Int]()
)
)


def breadthBT[A](bt: BT[A]) : List[A] =
    def breadthBTIn(nodes: List[BT[A]], resultList: List[A]) : List[A] =
        if nodes == List() then resultList.reverse
        else
            nodes.head match
                case Empty() => breadthBTIn(nodes.tail, resultList)
                case Node(value, leftChild, rightChild) =>  breadthBTIn(nodes.tail ::: List(leftChild, rightChild), value :: resultList)
    breadthBTIn(List(bt), List())

breadthBT(tt) == List(1, 2, 3, 4, 5, 6)
