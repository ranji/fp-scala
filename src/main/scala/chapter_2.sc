import scala.annotation.tailrec

object Fibonacci {

  def getFibonacciNumber(n:Int): Int = {
    def go(position:Int):Int = {
      if (position == 0 || position == 1) position
      else go(position - 2) + go( position - 1)
    }
    go(n)
  }

  //2.1
  def getFibonacciNum(n:Int): Int = {
    @tailrec
    def go(last:Int,curr:Int,position:Int):Int = {
      if (position == 0) last
      else if (position == 1 ) curr
      else go(curr,last+curr,position-1)

    }
    go(0,1,n)
  }
}

Fibonacci.getFibonacciNumber(6)
Fibonacci.getFibonacciNum(6)
Fibonacci.getFibonacciNum(0)
Fibonacci.getFibonacciNum(1)

object Sorter {
  //2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(index:Int = 0, sorted:Boolean = true): Boolean = {
      if (index + 1 >= as.length) sorted
      else if (!sorted) sorted
      else loop(index + 1, sorted = ordered(as(index), as(index + 1)))
    }
    loop()
  }
}

Sorter.isSorted(Array(1,2,3,4),(x:Int,y:Int) => x < y)

Sorter.isSorted(Array(1,0,3),(x:Int,y:Int) => x < y)

Sorter.isSorted(Array("A","AB","AC"),(x:String,y:String) => x < y)

Sorter.isSorted(Array("A","Ac","Ab"),(x:String,y:String) => x < y)

Sorter.isSorted(Array(),(x:String,y:String) => x < y)

object Currying{
  //2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a:A) =>  (b:B) => f(a,b)
  //2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a:A, b:B ) => f(a)(b)
}

object Compose{
  //2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))
}