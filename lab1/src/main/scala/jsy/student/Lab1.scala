package jsy.student

import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {
  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * Ian Smith
   *
   * Partner: Noah
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */

  def abs(n: Double): Double =
    if(n>=0) n // no return keywords needed
    else -n     // no return keyword needed (thanks, IntelliJ)

  def xor(a: Boolean, b: Boolean): Boolean = {
    //(a && !b) || (b && !a) // exactly one must be true in order to return true
    // without using Boolean operators
    if (a) { // if a then b must be false
      if (b) false else true
    }
    else { // if !a then b must be true
      if (b) true else false
    }
  }

  def repeat(s: String, n: Int): String = {
    require(n >= 0) // do not allow negative
    /*if(n == 0){ // repeat no times (0 or less)
      ""
    } else if (n == 1) { // repeat once
      s
    } else {            // use recursion
      s + repeat(s,n-1)
    }*/
    // more scala-like
    if (n == 0) "" else s + repeat(s,n-1)
  }

  def sqrtStep(c: Double, xn: Double): Double = xn - (scala.math.pow(xn,2) - c)/(2*xn) // scala.math.pow to square xn

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    def sqrtHelper(c : Double, xi : Double, n : Double) : Double = { // helper function
      if (n == 0) xi else sqrtHelper(c, sqrtStep(c,xi), n-1) // call helper function recursively
      // n = 0 is base case, then just find next step with sqrtStep and call recursively (n times)
    }
    require(n>=0)           // n can't be negative
    sqrtHelper(c, x0, n)    // call helper function! (a recursive function)
  }

  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    def sqrtErrHelper(c : Double, xi : Double, epsilon : Double) : Double = { // recursive helper function
      if(abs(math.pow(xi,2) - c) < epsilon) xi else sqrtErrHelper(c, sqrtStep(c, xi), epsilon) // if err not small enough, step again
    }
    require(epsilon > 0) // epsilon must be greater than zero
    sqrtErrHelper(c, x0, epsilon)
  }

  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => (d >= min) && (d < max) && check(l, min, d) && check(r, d, max)
    }
    check(t, Int.MinValue, Int.MaxValue)
  }

  def insert(t: SearchTree, n: Int): SearchTree = t match {
    case Empty => Node(Empty, n, Empty) // base case, once reach an empty node, return this node
    case Node(l,d,r) => if (n < d) Node(insert(l, n), d, r) else Node(l, d, insert(r,n)) // if n is less than d, construct left subtree
    // otherwise construct right subtree
  }

  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l) // returns right subtree of node to be deleted (and its value)
        (Node(l1, d ,r), m) // this right subtree becomes parent's left subtree and parent's right subtree remains the same
    }
  }

  def delete(t: SearchTree, n: Int): SearchTree = ???

  /* JavaScripty */

  def eval(e: Expr): Double = e match {
    case N(n) => ???
    case _ => ???
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}
