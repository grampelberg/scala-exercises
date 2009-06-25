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

def sum_fold(is: List[Int]): Int = is.foldLeft(0)(_+_)

println(sum_fold(List(1,2,3,4,5,6,7,8,9,10)))

def length[A](as: List[A]): Int = as match {
    case Nil => 0
    case _ :: xs => 1 + length(xs)
}

println(length(List(1,2,3,4,5,6,7,8,9,10)))

def map[A, B](as: List[A], f: A => B): List[B] = as match {
    case Nil => List()
    case x :: xs => f(x) :: xs.map(f)
}

val q = List(1,2,3)
println(map(List(q,q,q,q), length))

def filter[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => List()
    case x :: xs if (f(x)) => x :: xs.filter(f)
    case _ :: xs => xs.filter(f)
}

println(filter(List(1,4,5,3,2,4,1), (x: Int) => x == 4))

def flatten[A](as: List[List[A]]): List[A] = as match {
    case Nil => List()
    case x :: xs => x ::: flatten(xs)
}

println(flatten(List(List(1,2,3,4), List(5,6,7,8))))

def flatMap[A, B](as: List[A], f: A => List[B]): List[B] = flatten(as.map(f))

println(flatMap(List(1,2,3,4,5), (x: Int) => List(x + 1)))

def maximum(is: List[Int]): Int = {
    def submax(as: List[Int], max: Int): Int = as match {
        case Nil => max
        case x :: xs if (x > max) => submax(xs, x)
        case _ :: xs => submax(xs, max)
    }
    submax(is, 0)
}

println(maximum(List(10,2,3,4,5)))

def reverse[A](as: List[A]): List[A] = as match {
    case Nil => List()
    case x :: xs => reverse(xs) ::: List(x)
}

println(reverse(List(1,2,3,4,5)))
