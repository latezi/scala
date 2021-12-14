package exercise1

/** Напишите отдельные функции, решающие поставленную задачу. 
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {
  def main(args: Array[String]): Unit = {
    print("Площадь окружности = ")
    println(testCircle(10))

    print("Каррированная функция площади прямоугольника = ")
    println(testRectangleCurried(3,7))


    print("Не каррированная функция площади прямоугольника = ")
    println(testRectangleUc(2,3))
  }

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  def fSo(r:Double):Double = {Math.pow(r,2)  * Math.PI }


  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = fSo(r)



  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def fSp (a:Double)(b:Double): Double = a * b


  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = fSp(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  def fSnotc (a:Double, b:Double): Double = a * b


  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = fSnotc(a,b)
}
