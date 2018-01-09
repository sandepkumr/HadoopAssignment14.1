
class Calc(n: Int, d: Int) {
  require(d != 0)
  private val g = gcd(n.abs, d.abs)
  val numerator = n / g
  val denominator = d / g
  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(-x, y)
    else if (y < 0) gcd(x, -y)
    else gcd(y % x, x)
  }
  def this(n: Int) = this(n, 1) // auxiliary constructor
  def add(r: Calc): Calc =
    new Calc(numerator * r.denominator + r.numerator * denominator,
      denominator * r.denominator)
  def add(i: Int): Calc = // overloaded for add
    new Calc(numerator + i * denominator, denominator)
  def subtract(r: Calc) =
    new Calc(numerator * r.denominator -
      r.numerator * denominator, denominator * r.denominator)
  def subtract(i: Int): Calc = // overloaded for subtract
    new Calc(numerator - i * denominator, denominator)
  def multiply(r: Calc) =
    new Calc(numerator * r.numerator, denominator * r.denominator)
  def multiply(i: Int): Calc = // overloaded for multiply
    new Calc(numerator * i, denominator)
  def divide(r: Calc) = new Calc(numerator * r.denominator, denominator * r.numerator)
  def divide(i: Int): Calc = // overloaded for division
    new Calc(numerator, denominator * i)
  override def toString = numerator + "/" + denominator
}

// Single Ton Object

object CalcObj {
  def main(args: Array[String]): Unit = {
    val a = new Calc(9, 5)
    val b = new Calc(5)
    val p = a add b
    println("Result of 9/5 + 5 is "+p)
    val q = a multiply b
    println("Result of 9/5 * 5 is "+q)
    val r = a subtract b
    println("Result of 9/5 - 5 is "+r)
    val s = a divide b
    println("Result of 9/5 divided by 5 is "+s)
  }
}