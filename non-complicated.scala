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

def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case _ :: xs => 1 + length(xs)
}

println(length(List(1,2,3,4,5,6,7,8,9,10)))

def map[A, B](as: List[A], f: A => B): List[B] = as match {
    case x :: Nil => List(f(x))
    case x :: xs => f(x) :: map(xs, f)
    case _ => error("You shouldn't be here!")
}

val q = List(1,2,3)
println(map(List(q,q,q,q), length))

