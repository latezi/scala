package Lab_ex4

object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.

  trait Reversable[T]{
    def reverse(value:T):T
  }

  // b) Реализуйте функцию Reverse для String.
  object Reversable {

    def reverse[T: Reversable](a: T): T = implicitly[Reversable[T]].reverse(a)

    implicit object ReversableString extends Reversable[String]{
      def reverse(str:String):String=str.reverse
    }
  }
  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(mystr: String): String = Reversable.reverse(mystr)

  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.

  trait Smash[T]{
    def smash(a: T, b: T): T
  }

  object Smash {
    def smash[T : Smash](a: T, b: T): T = implicitly[Smash[T]].smash(a,b)
    implicit object SmashInt extends Smash[Int]{
      def smash(value1:Int,value2:Int):Int=value1+value2
    }
    implicit object SmashDouble extends Smash[Double]{
      def smash(value1:Double,value2:Double):Double=value1*value2
    }
    implicit object SmashString extends Smash[String]{
      def smash(value1:String,value2:String):String=value1+value2
    }
  }

  // d) Реализуйте  функции Smash для типа Int и Double.
  //    Используйте сложение для типа Int и умножение для типа Double.

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = Smash.smash(a,b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = Smash.smash(a,b)


  // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра. 

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = Smash.smash(a,b)

  def main(args:Array[String])={

    println("Перевернуть строку? Легко! - " +testReversableString("BVT1901 Kaplenko"))
    println()
    println("SmashInt = "+testSmashInt(7,9))
    println()
    println("SmashDouble = "+testSmashDouble(7,9))
    println()
    println("SmashString = " + testSmashString("sun","flower"))
  }
}

