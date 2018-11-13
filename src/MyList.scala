abstract class MyList[+A] {   // use covariance
  /*
      head = first element of the list
      tail = remainder of the list
      isEmpty = is this list empty
      add(int) => new list with this element added
      toString => a string representation of the list
   */

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

  // higher-order functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  // concatenation
  def ++[B >: A](list: MyList[B]): MyList[B]

  // hofs (high-order functions)
  def foreach(f: A => Unit): Unit
  def sort(compare: (A, A) => Int): MyList[A]
  def zipWith[B, C](list: MyList[B], zip:(A, B) => C): MyList[C]
  def fold[B](start: B) (operator: (B, A) => B): B
}


case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true
  def add[B >: Nothing](element: B): MyList[B] = new Cons(element, Empty)
  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  // hofs
  def foreach(f: Nothing => Unit): Unit = ()
  def sort(compare: (Nothing, Nothing) => Int) = Empty
  def zipWith[B, C](list: MyList[B], zip:(Nothing, B) => C) = {
    if (!list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else Empty
  }
  def fold[B](start: B) (operator: (B, Nothing) => B): B = start
}


case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false
  def add[B >: A](element: B): MyList[B] = new Cons(element, this)
  def printElements: String =
    if(t.isEmpty) "" + h
    else h + " " + t.printElements

  /*
      [1,2,3].filter(n % 2 == 0)
        = [2,3].filter(n % 2 == 0)
        = new Cons(2, [3].filter(n % 2 == 0))
        = new Cons(2, Empty.filter(n % 2 == 0)
        = new Cons(2, Empty)
     */
  def filter(predicate: A => Boolean): MyList[A] =
    if (predicate(h)) new Cons(h, t.filter(predicate))
    else t.filter(predicate)

  /*
    [1,2,3].map(n * 2)
      = new Cons(2, [2,3].map(n * 2))
      = new Cons(2, new Cons(4, [3].map(n * 2)))
      = new Cons(2, new Cons(4, new Cons(6, Empty.map(n * 2))))
      = new Cons(2, new Cons(4, new Cons(6, Empty))))
   */
  def map[B](transformer: A => B): MyList[B] =
    new Cons(transformer(h), t.map(transformer))

  /*
    [1,2] ++ [3,4,5]
    = new Cons(1, [2] ++ [3,4,5])
    = new Cons(1, new Cons(2, Empty ++ [3,4,5]))
    = new Cons(1, new Cons(2, [3,4,5]))  <=>  new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5)))))
   */
  def ++[B >: A](list: MyList[B]): MyList[B] = new Cons(h, t ++ list)

  /*
    [1,2].flatMap(n => [n, n+1])
    = [1,2] ++ [2].flatMap(n => [n, n+1])
    = [1,2] ++ [2,3] ++ Empty.flatMap(n => [n, n+1])
    = [1,2] ++ [2,3] ++ Empty
    = [1,2,2,3]git
   */
  def flatMap[B](transformer: A => MyList[B]): MyList[B] =
    transformer(h) ++ t.flatMap(transformer)

  // hofs
  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def sort(compare: (A, A) => Int): MyList[A] = {              // use algorithm of insertion sort
    def insert(x: A, sortedList: MyList[A]): MyList[A] =
      if (sortedList.isEmpty) new Cons(x, Empty)
      else if (compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
      else new Cons(sortedList.head, insert(x, sortedList.tail))

    val sortedTail = t.sort(compare)
    insert(h, sortedTail)
  }

  def zipWith[B, C](list: MyList[B], zip:(A, B) => C): MyList[C] =
    if (list.isEmpty) throw new RuntimeException("Lists do not have the same length")
    else new Cons(zip(h, list.head), t.zipWith(list.tail, zip))

  /*
    [1,2,3].fold(0)(+)
    = [2,3].fold(1)(+)
    = [3].fold(3)(+)
    = [].fold(6)(+)
    = 6
   */
  def fold[B](start: B) (operator: (B, A) => B): B =
    t.fold(operator(start, h))(operator)
}


object ListTest extends App {
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = new Cons(4, new Cons(5, Empty))
  val cloneListOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfStrings: MyList[String] = new Cons("Hello", new Cons("Scala", Empty))

  println(listOfIntegers.toString)                              //> [1 2 3]
  println(listOfStrings.toString)                               //> [Hello Scala]

  println(listOfIntegers.map(elem => elem * 2).toString)        //> [2 4 6]
  println(listOfIntegers.map(_ * 2).toString)                   // syntax sugar

  println(listOfIntegers.filter(elem => elem % 2 ==0).toString) //> [2]
  println(listOfIntegers.filter(_ % 2 ==0).toString)            // syntax sugar

  println((listOfIntegers ++ anotherListOfIntegers).toString)   //> [1 2 3 4 5]

  println(listOfIntegers.flatMap(elem => new Cons(elem, new Cons(elem + 1, Empty))).toString)  //> [1 2 2 3 3 4]

  println( cloneListOfIntegers == listOfIntegers )              //> true


  // hofs tests
  listOfIntegers.foreach(println)               // = listOfIntegers.foreach(x => println(x))
  println(listOfIntegers.sort((x,y) => y - x))  // = println(listOfIntegers.sort(-_ + _))      //> [3 2 1]
  println(anotherListOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _))           //> [4-Hello 5-Scala]

  println(listOfIntegers.fold(0)(_ + _))                                                       //> 6


  // for comprehensions test
  val combinations = for {
    n <- listOfIntegers
    string <- listOfStrings
  } yield n + "-" + string
  println(combinations)
  println


  // additional tests for pattern matching
  val pmTest0: MyList[Int] = Empty
  val pmTest1: MyList[Int] = new Cons(11, Empty)
  val pmTest2: MyList[Int] = new Cons(22, new Cons(21, Empty))
  val pmTest4: MyList[Int] = new Cons(44, new Cons( 43, new Cons(42, new Cons(41, Empty))))
  val pmTest1a: MyList[Int] = new Cons(-1, Empty)
  val pmTest2a: MyList[Int] = new Cons(22, new Cons(0, Empty))

  def TestList(testCons: MyList[Int]): String =
    testCons match {
      case Empty => "List is empty"
      case Cons(head, Cons(subhead, subtail)) if subhead ==0 => "Guard works"
      case Cons(head, Cons(subhead, subtail)) => s"Head is $head, subhead is $subhead and tail is $subtail"
      case Cons(-1, Empty) | Cons(-2, Empty) => "Multi-pattern works"    // don't use variable 'head' and 'tail' here -> error
      case Cons(head, tail) => s"Head is $head and tail is $tail"
      case _ => "something else"
  }

  println(TestList(pmTest0))   //> List is empty
  println(TestList(pmTest1))   //> Head is 11 and tail is []
  println(TestList(pmTest2))   //> Head is 22, subhead is 21 and tail is []
  println(TestList(pmTest4))   //> Head is 44, subhead is 43 and tail is [42 41]
  println(TestList(pmTest1a))  //> Multi-pattern works
  println(TestList(pmTest2a))  //> Guard works
}



/* additional TASK #9
  Test pattern matching
*/



/* TASK #8
  MyList supports for comprehensions?
    map(f: A => B) => MyList[B]
    flatMap(f: A => MyList[B]) => MyList[B]
    filter(p: A => Boolean) => MyList[A]
*/



/* TASK #7
    Expand MyList
      - foreach method A => Unit
        [1,2,3].foreach(x => println(x))

      - sort function ((A, A) => Int) => MyList
        [1,2,3].sort((x, y) => y - x) => [3,2,1]

      - zipWith (list, (A, A) => B) => MyList[B]
        [1,2,3].zipWith([4,5,6], x * y) => [1 * 4, 2 * 5, 3 * 6] = [4,10,18]

      - fold(start)(function) => a value
        [1,2,3].fold(0)(x + y) = 0 + 1 + 2 + 3 = 6
*/



/* TASK #6
  Replace all FunctionX calls with lambdas
*/



/* TASK #5
  Transform the MyPredicate and MyTransformer into function types
*/



/* TASK #4
  Expand MyList - use case classes and case objects
*/



/* TASK #3
1. Generic trait MyPredicate[-T] with a little method test(T) => Boolean
     class EvenPredicate extends MyPredicate[Int]
2. Generic trait MyTransformer[-A, B] with a method transform(A) => B
     class StringToIntTransformer extends MyTransformer[String, Int]
3. MyList:
   - map(transformer) => MyList
       e.g.: [1,2,3].map(n * 2) = [2,4,6]
   - filter(predicate) => MyList
       e.g.: [1,2,3,4].filter(n % 2) = [2,4]
   - flatMap(transformer from A to MyList[B]) => MyList[B]
       e.g.: [1,2,3].flatMap(n => [n, n+1]) => [1,2,2,3,3,4]
*/



/* TASK #2
  Expand MyList to be generic
*/






/* alternative sort with tail call optimization, from the end of list
  def sort(compare: (A, A) => Int): MyList[A] = {   // insertion sort, from end
    @tailrec
    def insert(sortedList1: MyList[A], z: A, sortedList2: MyList[A]): MyList[A] = {
      if (sortedList2.isEmpty)
        sortedList1 ++ new Cons(z, Empty)
      else if (compare(z, sortedList2.head) <= 0)
        sortedList1 ++ new Cons(z, sortedList2)
      else
        insert(sortedList1 ++ new Cons(sortedList2.head, Empty), z, sortedList2.tail)
    }

    val sortedTail = t.sort(compare)
    insert(Empty, h, sortedTail)
  }
 */

/* alternative sort with tail call optimization, from the end of list
    def sort(compare: (A, A) => Int): MyList[A] = {   // insertion sort, from beginning
      @tailrec
      def insert(sortedList1: MyList[A], z: A, sortedList2: MyList[A]): MyList[A] =
        if (sortedList2.isEmpty)
          sortedList1 ++ new Cons(z, Empty)
        else if (compare(z, sortedList2.head) <= 0)
          sortedList1 ++ new Cons(z, sortedList2)
        else
          insert(sortedList1 ++ new Cons(sortedList2.head, Empty), z, sortedList2.tail)

      @tailrec
      def scroll(sortedList: MyList[A], unsortedList: MyList[A]): MyList[A] =
        if (unsortedList.isEmpty)
          sortedList
        else
          scroll(insert(Empty, unsortedList.head, sortedList), unsortedList.tail)

      scroll(Empty, this)               // alternative:  if (t.isEmpty) this
                                        //               else scroll(new Cons(h, Empty), t)
    }
 */

/* commands for tests of sort-method
  val listOfIntegers: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  println(listOfIntegers.sort((x,y) => x - y))  // = println(listOfIntegers.sort(-_ + _))      //> [1 2 3]
  println(listOfIntegers.sort((x,y) => y - x))  // = println(listOfIntegers.sort(-_ + _))      //> [3 2 1]

  val t1: MyList[Int] = new Cons (7, new Cons(1, new Cons(5, new Cons(6, new Cons(2, new Cons(4, Empty))))))
  // val t1: MyList[Int] = new Cons(5, new Cons(4, new Cons(3, new Cons(2, new Cons(1, Empty)))))
  // val t1: MyList[Int] = new Cons(1, new Cons(2, new Cons(3, new Cons(4, new Cons(5, Empty)))))
  println(t1.sort((x,y) => x - y))                 //> [1 2 3 5 6 7]
  println(t1.sort((x,y) => y - x))                 //> [7 6 5 3 2 1]

  val t2: MyList[Int] = new Cons(3, Empty)
  println(t2.sort((x,y) => x - y))                 //> [3]
  println(t2.sort((x,y) => y - x))                 //> [3]

  val t3: MyList[Int] = Empty
  println(t3.sort((x,y) => x - y))                 //> []
  println(t3.sort((x,y) => y - x))                 //> []
 */