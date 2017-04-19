package pp201701.hw3
import pp201701.hw3.Data.DataBundle._

/*
 * ** The submitted code should be runnable. Before upload, you MUST check it
      whether Test.scala runs successfully. Otherwise you may get 0 points for
      all problems.

 * ** Compress the 'src' folder only. Don't put .sh files in the zip file.
      You can put the .iml file together, if you feel difficulty for some reasons.

 * ** Use only the features taught in class. Especially, don't use imperative
      features such as while, for, return, and so on. You may get deduction
      in your points.

 * ** Do not use equivalent built-in features in Scala. The core logic should be
      implemented by you.

 * ** When you give up a problem, use the commented code:
      ex.) If you want to give up the first problem,
        def push[A](stk: Stack[A])(a: A): Stack[A] = stk
 */

/*
 Implement below functions, which is currently blank. (???)
 */

object Main {
  /*
   Exercise 1: Stack and Queue
   Implement Stack / Queue functions.
   Queue can be implemented with two stacks, in purely functional way.

   'pushList' is for your convenience. You can ignore it if you don't want to use it.
   */

  def emptyStk[A] = MyNil[A]()

  def push[A](stk: Stack[A])(a: A): Stack[A] = stk match{
    case MyNil() => MyCons[A](a,MyNil())
    case someList => MyCons[A](a,someList)
  }

  def pop[A](stk: Stack[A]): Option[(A, Stack[A])] = stk match{
    case MyNil() => None
    case MyCons(hd,tl) => Some[(A,Stack[A])]((hd,tl))
  }

  def pushList[A](seed: Stack[A])(as: List[A]): Stack[A] =
     as.foldLeft(seed)((stk, a) => push(stk)(a))

  /*
   Queue can be implemented using two stacks, efficiently.
   In other words, amortized cost is constant.
   Say there is stack A and stack B.
   When you enQ, push to stack A.
   When you deQ, merge two stacks by popping all elements from A, and pushing
   then into B. And then pop an element from B.
   Or you may further optimize this by merging only when needed.
   */
  def emptyQ[A] = (emptyStk[A], emptyStk[A])

  def enQ[A](q: Queue[A])(a: A): Queue[A] = (push[A](q._1)(a),q._2)

  def enQList[A](seed: Queue[A])(as: List[A]): Queue[A] =
     as.foldLeft(seed)((q, a) => enQ(q)(a))

  def deQ[A](q: Queue[A]): Option[(A, Queue[A])] ={
    def moveEleFromOneToAnother[A](stk1:Stack[A])(stk2:Stack[A]):Queue[A] = stk1 match{
      case MyNil() => (stk1,stk2)
      case MyCons(hd,tl) => moveEleFromOneToAnother[A](tl)(MyCons(hd,stk2))
    }
    def popEleFromQueue[A](modifiedQueue:Queue[A]):Option[(A,Queue[A])] = modifiedQueue match{
      case (MyNil(),MyNil()) => None
      case (MyNil(),MyCons(hd,tl)) => Some[(A,Queue[A])](hd,(MyNil(),tl))
      case (someList,otherList) => popEleFromQueue[A](moveEleFromOneToAnother(someList)(otherList))
    }
    q match{
      case (MyNil(),MyNil()) => None
      case (MyNil(),MyCons(hd,tl)) => Some[(A,Queue[A])]((hd,(MyNil(),tl)))
      case (someList,otherList) => popEleFromQueue[A](moveEleFromOneToAnother(someList)(otherList))
    }
  }
  /*
   Exercise 2: Binary Search Tree
   Implement insert/lookup for Binary Search Tree.
   (https://en.wikipedia.org/wiki/Binary_search_tree)

   For each Node, every key in the left subtree should be less than the root's key.
   Likewise, every key in the right subtree should be greater than the root's key.

   Each time, comparator(cmp) will be given as an argument.
   You may assume that the same cmp has been used to build given tree.
   The result of cmp(k1, k2) should be interpreted as the following:
     negative: k1 < k2
     zero:     k1 == k2
     positive: k1 > k2

   For insert function, if the given key already exists in the given tree,
   you should overwrite it.
   For lookup function, return the result in Option type, meaning
     if the key exists -> Some(value)
     else -> None
   */
  def emptyBST[K, V]: BSTree[K, V] = Leaf()

  def insert[K, V]
    (t: BSTree[K, V])(keyValue: (K, V))(cmp: K => K => Int): BSTree[K, V] = t match{
    case Leaf() => Node[(K,V)](keyValue,Leaf(),Leaf())
    case Node(cmpTarget,left,right) if cmp(cmpTarget._1)(keyValue._1) == 0 => Node(keyValue,left,right)
    case Node(cmpTarget,left,right) if cmp(cmpTarget._1)(keyValue._1) < 0 => insert[K,V](left)(keyValue)(cmp)
    case Node(cmpTarget,left,right) => insert[K,V](right)(keyValue)(cmp)
  }

  def insertList[K, V]
    (seed: BSTree[K, V])(keyValues: List[(K, V)])(cmp: K => K => Int) =
    keyValues.foldLeft(seed)((tree, keyValue) => insert(tree)(keyValue)(cmp))

  def lookup[K, V](t: BSTree[K, V])(key: K)(cmp: K => K => Int): Option[V] = t match{
    case Leaf() => None
    case Node(cmpTarget,left,right) if cmp(cmpTarget._1)(key) == 0 => Some[V](cmpTarget._2)
    case Node(cmpTarget,left,right) if cmp(cmpTarget._1)(key) < 0 => lookup[K,V](left)(key)(cmp)
    case Node(cmpTarget,left,right) => lookup[K,V](right)(key)(cmp)
  }

  /*
   Exercise 3: Structural Sub Type
   Complete the definition of MyClass
   DO NOT USE "Any" in anywhere in your code.
   */

  /*
   You can solve this problem without deep knowledge about 'class'.
   Though we are going to learn it soon.
   */
  class MyClass[A,B,C,D,E,F]() {
    type Func1 = { val a: A } => { val b: B }
    type Func2 = { val b: B } => { val a: A }

    type Ty1 = {
      def apply: { val func: Func1 ; val c: C } => { val b: B ; val d: D }
      val a: A
      val b: B
    }

    type Ty2 = {
      def apply: { val func: Func2 ; val e: E } => { val b: B ; val f: F }
      val a: A
      val c: C
    }

    /*
     Find suitable common supertype of Ty1 and Ty2,
     and replace "Any" with that type.
     */

    type FuncCommonTy = {} => {val a:A; val b:B}


    type CommonTy = {
      def apply: {val func:FuncCommonTy; val c:C; val e:E} => {val b:B}
      val a : A
    }

    /*
     Fill in the apply function here.
     The answer should be in this form: x.apply(...)
     */
    def apply(x: CommonTy, _a: A, _b: B, _c: C, _d: D, _e: E, _f: F) =
    x.apply(new {
      val func:FuncCommonTy= (y:{}) => new {val a:A=_a;val b:B=_b}
      val c:C = _c;
      val e:E = _e
    })
  }
}
