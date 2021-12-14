import scala.annotation.tailrec

/** Реализуйте функции для решения следующих задач.
 * Примечание: Попытайтесь сделать все функции с хвостовой рекурсией, используйте аннотацию для подстверждения.
 * рекурсия будет хвостовой если:
 *   1. рекурсия реализуется в одном направлении
 *   2. вызов рекурсивной функции будет последней операцией перед возвратом
 */
object RecursiveFunc {

  def length[A](as: List[A]): Int = {
    @tailrec
    def loop(m: List[A], agg: Int): Int = m match {
      case Cons(_, tail) => loop(tail, agg + 1)
      case Nil()         => agg
    }

    loop(as, 0)
  }

  /* a) Напишите функцию которая записывает в обратном порядке список:
   *        def reverse[A](list: List[A]): List[A]
   */
  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def loop(rem: List[A],agg:List[A]): List[A]= rem match {
      case Cons(n,tail)=> loop(tail,Cons(n,agg))
      case Nil()       =>agg
    }
    loop (list, new Nil[A])
  }


  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testReverse[A](list: List[A]): List[A] = reverse (list)

  /* b) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def map[A, B](list: List[A])(f: A => B): List[B]
   */
  def map[A,B](list:List[A])(f: A=>B):List[B]={
    @tailrec
    def loop(rem: List[A],agg:List[B]): List[B]= rem match {
      case Cons(n,tail)=> loop(tail,Cons(f(n),agg))
      case Nil()       =>agg
    }
    reverse(loop(list,new Nil[B]))
  }



  // используйте функцию из пункта  (b) здесь, не изменяйте сигнатуру
  def testMap[A, B](list: List[A], f: A => B): List[B] = map(list)(f)

  /* c) Напишите функцию, которая присоединяет один список к другому:
   *        def append[A](l: List[A], r: List[A]): List[A]
   */
  def append[A](l: List[A], r: List[A]): List[A]={
    @tailrec
    def loop(rem: List[A],agg:List[A]): List[A]= rem match {
      case Cons(n,tail)=> loop(tail,Cons(n,agg))
      case Nil()       =>agg
    }
    reverse(loop(r,reverse(l)))
  }

  // используйте функцию из пункта  (c) здесь, не изменяйте сигнатуру
  def testAppend[A](l: List[A], r: List[A]): List[A] = append(l,r)

  /* d) Напишите функцию, которая применяет функцию к каждому значению списка:
   *        def flatMap[A, B](list: List[A])(f: A => List[B]): List[B]
   * 
   *    она получает функцию, которая создает новый List[B] для каждого элемента типа A в 
   *    списке. Поэтому вы создаете List[List[B]]. 
   */
  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] ={
    @tailrec
    def loop(rem: List[A],agg:List[B]): List[B]= rem match {
      case Cons(n,tail)=> loop(tail,append(agg,f(n)))
      case Nil()       =>agg
    }
    loop(list,new Nil[B])
  }
  // используйте функцию из пункта  (d) здесь, не изменяйте сигнатуру
  def testFlatMap[A, B](list: List[A], f: A => List[B]): List[B] = flatMap(list)(f)

  /* e) Вопрос: Возможно ли написать функцию с хвостовой рекурсией для `Tree`s? Если нет, почему? */

  def main (args: Array[String]):Unit={
    println("reverse(Cons(1,Cons(2,Cons(3,Nil()))))) = "+ reverse(Cons(1,Cons(2,Cons(3,Nil())))))
    println("map(Cons(1,Cons(2,Cons(3,Nil()))))((x:Int)=>x*x) ="+ map(Cons(1,Cons(2,Cons(3,Nil()))))((x:Int)=>x*x))
    println("append(Cons(1,Cons(2,Cons(3,Nil()))),Cons(4,Cons(5,Nil()))) = "+append(Cons(1,Cons(2,Cons(3,Nil()))),Cons(4,Cons(5,Nil()))))
    println("flatMap(Cons(1,Cons(2,Cons(3,Nil()))))((x:Int)=>Cons(x,Cons(9,Nil()))) = " + flatMap(Cons(1,Cons(2,Cons(3,Nil()))))((x:Int)=>Cons(x,Cons(9,Nil()))))
  }
}