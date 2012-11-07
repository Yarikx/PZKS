package org.kpi.pzks

import java.io.File
import java.io.FileOutputStream
import java.lang.Double

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.collection.mutable.StringBuilder
import scala.util.Random

import org.kpi.pzks.BraceOpener._;
import org.kpi.pzks.Parser.CloseBrace
import org.kpi.pzks.Parser.Digits
import org.kpi.pzks.Parser.Dot
import org.kpi.pzks.Parser.Node
import org.kpi.pzks.Parser.OpenBrace
import org.kpi.pzks.Parser.Operation
import org.kpi.pzks.Parser.Symbols
import org.kpi.pzks.Parser.TailDigits
import org.kpi.pzks.Parser.parseString

class Element;

case class Op(c: Char) extends Element{
  def group = TreeBuilder.getGroup(this)
  override def toString = c.toString
}
case class Const(v: Double) extends Element{
  override def toString = v.toString
}
case class Var(v: String) extends Element{
  override def toString = v.toString
}
case class Ob extends Element
case class Cb extends Element
case class Expr(elements: List[Element]) extends Element {
  def isPure = elements match {
    case List(_, Op(o), _) => true
    case _ => false
  }

  override def toString = "(%s)".format(elements.mkString(", "))
}

object TreeBuilder extends App {

  def groupElements(nodes: Seq[Node], str: String) = {

    val elements = nodes.toList.map(x => x match {
      case d: Dot => new Digits
      case d: TailDigits => new Digits
      case other => other
    })

    val zipped = elements.tail.zip(s.toList)

    def buildRecur(nodes: List[(Node, Char)], res: List[Element]): (List[(Node, Char)], List[Element]) = {

      if (nodes.isEmpty) {
        return (nodes, res)
      }

      val firstChar = nodes.head._1;

      firstChar match {
        case d: OpenBrace =>
          return buildRecur(nodes.tail, new Ob :: res)
        case d: CloseBrace =>
          return buildRecur(nodes.tail, new Cb :: res)
        case _ =>
      }

      val (first, last) = nodes.span(x => x._1.getClass() == nodes.head._1.getClass())

      val element = nodes.head._1 match {
        case x: Symbols =>
          val chars = first.map(_._2).mkString
          Var(chars)
        case x: Operation =>
          Op(first(0)._2)
        case x: Digits =>
          val value = Double.parseDouble((first.map(_._2).mkString))
          Const(value)
      }

      if (last.isEmpty) {
        (last, element :: res)
      } else {
        buildRecur(last, element :: res)
      }
    }

    buildRecur(zipped, List[Element]())._2.reverse

  }

  def buildSimpleTree(src: List[Element]): List[Element] = {

    def getElementsAfterBraces(tail: List[Element]) = {

      def recur(l: List[Element], acum: Int): List[Element] = {
        val head :: tail = l
        head match {
          case Ob() => recur(tail, acum + 1)
          case Cb() => if (acum == 1) tail else recur(tail, acum - 1)
          case _ => recur(tail, acum)
        }
      }

      recur(tail, 1)

    }

    val braces = src.zip(0 until src.size).filter(x => x.isInstanceOf[OpenBrace] || x.isInstanceOf[CloseBrace])

    val (beforeBraces, startOfBraces) = src.span(_ != Ob())
    if (beforeBraces == src) {
      return beforeBraces
    }

    val afterBraces = getElementsAfterBraces(startOfBraces.tail)

    val treeInBraces = Expr(buildSimpleTree(startOfBraces.tail.take(startOfBraces.size - 2 - afterBraces.size)))

    beforeBraces ::: treeInBraces :: buildSimpleTree(afterBraces)

  }

  def applyToAll(f: (List[Element]) => List[Element])(list: List[Element]): List[Element] = {
    val q = list.map {
      case e: Expr =>
        Expr(applyToAll(f)(e.elements))
      case el: Element => el
    }
    f(q)
  }

  def groupBy[A](f: (A, A) => Boolean)(elements: List[A]) = {
    
    @tailrec
    def recur(result: List[List[A]], list: List[A]): List[List[A]] = {
      if(list.isEmpty){
        return result.reverse
      }
      val head = list.head
      val (first, last) = list.span(f(head, _))

      val res = first :: result

      if (last.isEmpty) {
        return res.reverse
      } else {
        return recur(res, last)
      }
    }

    recur(List[List[A]](), elements)
  }

  def getGroup(e: Element): Int = {
    e match {
      case Op('/') | Op('*') => 1
      case Op('+') | Op('-') => 2
      case _ => 3
    }
  }

  def getOperationWithIndices(l: List[Element]) = {
    val operations = l.zip(l.indices) collect {
      case (o: Op, i: Int) => (o, i)
    }

    operations
  }

  def collectSimilar(l: List[Element]): List[Element] = {
    val operations = getOperationWithIndices(l);
    if(operations.isEmpty){
      return l
    }
    val opGroups = groupBy((x: (Op, Int), y: (Op, Int)) => getGroup(x._1) == getGroup(y._1))(operations)
    val slice = opGroups.find(x => getGroup(x(0)._1) == 1) match {
      case Some(mulList) =>
        mulList
      case None =>
        opGroups(0)
    }

    val first = math.max(slice.head._2 - 1, 0)
    val last = slice.last._2 + 1

    if (first == 0 && last == l.size - 1) {
      return l;
    } else {
      val before = l.take(first)
      val middle = Expr(l.slice(first, last + 1))
      val after = l.drop(last + 1)
      val result = before ::: middle :: after
      return collectSimilar(result)
    }
  }

  def colapseSecondOp(op: Op, oposite: Op)(list: List[Element]): List[Element] = {
    val ops = getOperationWithIndices(list);
    val opGroups = groupBy((x: (Op, Int), y: (Op, Int)) => x._1 == y._1)(ops)
    val slice = opGroups.find(x => x(0)._1 == op) match {
      case Some(divList) =>
        if (divList.size > 1) {
          divList
        } else {
          return list
        }

      case None =>
        return list
    }

    val first = math.max(slice.head._2 + 1, 0)
    val last = slice.last._2 + 1

    val before = list.take(first)
    val fullSlice = list.slice(first, last + 1).map {
      case oper@Op(q) if(oper == op) => oposite
      case x => x
    }
    val middle = Expr(fullSlice)
    val after = list.drop(last + 1)
    val result = before ::: middle :: after
    result
  }

  def colapseDivides(list: List[Element]): List[Element] =
    colapseSecondOp(Op('/'), Op('*'))(list)

  def colapseMinuses(list: List[Element]): List[Element] =
    colapseSecondOp(Op('-'), Op('+'))(list)

  def colapseUnariExp(list: List[Element]): List[Element] = {
    val t = list.map {
      case Expr(List(e: Expr)) => List(e)
      case Expr(List(e: Element)) => List(e)
      case Expr(List(o: Op, e: Expr)) => List(o, e)
      case x => List(x)
    }
    t.flatten
  }
  
  def operate(c:Char, c1:Double, c2:Double)={
    c match{
      case '+' => c1+c2
      case '-' => c1-c2
      case '/' => c1/c2
      case '*' => c1*c2
    }
  }
  
  //High level
  def operateConstants(list: List[Element]): List[Element] ={
    val found = list.sliding(3).find({
      case List(Const(c1), Op(o), Const(c2)) => true
      case _ => false
    })
    
    found match{
      case None => return list;
      case Some(slice@List(Const(c1), Op(o), Const(c2))) =>
        val res = operate(o, c1,c2)
        
        val start = list.indexOfSlice(slice)
        
        list.take(start) ::: Const(res) :: list.drop(start+3)
    }
    
    
  }
  
  //other

  def pair(list: List[Element]): List[Element] = {
    if (Expr(list).isPure) {
      list
    } else {
      val (start, rest) = list.splitAt(3);
      val exp = Expr(start)
      //TODO add check
      val end = if (rest.size > 3) {
        val exTail = Expr(rest tail)
        if (exTail isPure) {
          List(rest.head, exTail)
        } else {
          rest.head :: pair(rest.tail)
        }
      } else {
        rest
      }
      exp :: end
    }
  }

  def applyLoop(f: (List[Element]) => List[Element])(list: List[Element]): List[Element] = {
    def recur(l: List[Element]): List[Element] = {
      val q = f(l)
      if (q == l) {
        q
      } else {
        recur(q)
      }
    }
    recur(list)
  }

  def buildGraphWizFile(e: Expr): File = {
    import sys.process._
    val rnd = new Random;
    val map = Map[Element, Int]()
    val buf = new StringBuilder("graph foo{");

    class El(val id: Int)
    case class Oper(c: Char, l: El, r: El) extends El(rnd.nextInt(1000000))
    case class Val(s: String) extends El(rnd.nextInt(1000000))

    def convert(e: Element): El = e match {
      case Var(s) => Val(s)
      case Const(x) => Val(x.toString)
      case ex: Expr =>
        val ql = convert(ex.elements(0))
        val qr = convert(ex.elements(2))
        val o = ex.elements(1).asInstanceOf[Op].c
        Oper(o, ql, qr)
    }

    val checked = e match{
      case Expr(List(Expr(List(el:Element)))) => el
      case Expr(List(el:Element)) => el
      case x => x
    }
    
    val converted = convert(checked)
    def buildLinks(ex: El) {

      ex match {
        case v @ Val(s) =>
          buf ++= "el%d [label=\"%s\"]\n".format(v.id, s)
        case o @ Oper(c, l, r) =>
          buf ++= "el%d [label=\"%c\"]\n".format(o.id, c)
          buf ++= "el%d -- el%d\n".format(o.id, l.id)
          buf ++= "el%d -- el%d\n".format(o.id, r.id)
          buildLinks(l)
          buildLinks(r)

      }
    }
    buildLinks(converted)
    buf ++= "}"
    println(buf.toString)

    val q = new FileOutputStream(new File("/tmp/example.dot"))
    q.write(buf.toString.map(_.toByte).toArray)
    q.close()

    Runtime.getRuntime().exec("rm /tmp/qwe.png").waitFor()
    Runtime.getRuntime().exec("rm /tmp/example.png").waitFor()
    Runtime.getRuntime().exec("dot -Tpng -o /tmp/qwe.png /tmp/example.dot ").waitFor()
    Runtime.getRuntime().exec("eog /tmp/qwe.png")

    null
  }

  def applyAll(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => { val q = applyToAll(f)(el); println(q); q })
  }
  
  val safeOptimization = ((f: List[Element] =>(List[Element])) =>  applyAll(
      f,
      colapseUnariExp,
      collectSimilar
    )_
  )
  
  def applyOptimisators(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => { val q = safeOptimization(f)(el); println(q); q })
  }
  
  val highOptimisation = ((f: List[Element] =>(List[Element])) =>  applyOptimisators(
      f,
      collectSimilar,
      colapseMinuses,
      colapseDivides,
      collectSimilar
    )_
  )
  
  def applyHighOptimisators(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => { val q = highOptimisation(f)(el); println(q); q })
  }

  val optomizations = applyHighOptimisators(
    collectSimilar,
    operateConstants) _

  val s = "a*(b+c)-d/(e+f)"
//    val s = "a+b*c*(b+c*d)+x"
  //  val s = "a+b-c-t-j+e"
  //  val s = "abc+5/3-r"
  val parsed = parseString(s)
  val elements = groupElements(parsed, s)
  val simpleTree = buildSimpleTree(elements)
  println("simple tree")
  println(simpleTree)

  val optimized = applyLoop(optomizations)(simpleTree)
  
  println("braces ************");
  collectLoop(optimized)(createAllVariantsOfBraces).foreach(println)
  println("braces ************");

  val paired = applyToAll(pair)(optimized)

  println(s)
  println("pair")
  println(paired)
  buildGraphWizFile(Expr(paired))

}