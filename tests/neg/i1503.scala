object Test {

  val cond = true
  def foo1() = println("hi")
  def bar1() = println("there")

  def foo2(x: Int) = println("hi")
  def bar2(x: Int) = println("there")

  def main(args: Array[String]) = {
    (if (cond) foo1 else bar1)() // error: Unit does not take parameters
    (if (cond) foo2 else bar2)(22) // error: missing arguments // error: missing arguments
  }
}
