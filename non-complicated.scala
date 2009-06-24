// Thomas Rampelberg

def power(x: Double, y: Int): Double = y match {
  case 0 => x
  case _ => x * power(x, y-1)
}

println(power(2, 4))

def sum(is: List[Int]): Int = is match {
  case Nil => 0
  case x :: xs => x + sum(xs)
}

println(sum(List(1,2,3,4,5,6,7,8,9,10)))