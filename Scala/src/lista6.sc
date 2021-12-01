// Kacper Wójcicki

// Zad 1
def whileLoop(condition : => Boolean)(expression : => Unit) : Unit =
  if condition then
    expression
    whileLoop(condition)(expression)

var count = 0
whileLoop(count < 3) {
    println(count)
    count = count + 1
}
