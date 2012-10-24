package org.kpi.pzks

import java.io.File
import java.io.FileOutputStream
import java.lang.Double

import scala.collection.mutable.Map
import scala.collection.mutable.StringBuilder
import scala.util.Random

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

case class Op(c: Char) extends Element;
case class Const(v: Double) extends Element;
case class Var(v: String) extends Element;
case class Ob extends Element
case class Cb extends Element
case class Expr(elements: List[Element]) extends Element {
  def isPure = elements match {
    case List(_, Op(o), _) => true
    case _ => false
  }

  override def toString = "Expr(%s)".format(elements.mkString(", "))
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
    def recur(result: List[List[A]], list: List[A]): List[List[A]] = {
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

  def colapseDivides(list: List[Element]): List[Element] = {
    val ops = getOperationWithIndices(list);
    val opGroups = groupBy((x: (Op, Int), y: (Op, Int)) => getGroup(x._1) == getGroup(y._1))(ops)
    val slice = opGroups.find(x => x(0)._1 == Op('/')) match {
      case Some(divList) if(divList.size>1) =>
        divList
      case None =>
        return list
    }
    
    val first = math.max(slice.head._2-1, 0)
    val last = slice.last._2 + 1
    
    val before = list.take(first)
    val fullSlice = list.slice(first, last+1).map{
      case Op('/') => Op('*')
      case x => x
    }
    val middle = List(Op('/'),Expr(fullSlice))
    val after = list.drop(last + 1)
    val result = before ::: middle ::: after
    result
  }
  
  def colapseUnariExp(list: List[Element]): List[Element] = {
    list.map{
      case Expr(List(e:Expr)) => e
      case x => x
    }
  }

  def pair(list: List[Element]): List[Element] = {
    if (Expr(list).isPure) {
      list
    } else {
      val (start, rest) = list.splitAt(3);
      val exp = Expr(start)
      //TODO add check
      val end = if (rest.size > 3) {
        val exTail = Expr(rest tail)
        if(exTail isPure){
          List(rest.head, exTail)
        }else{
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

    val converted = convert(e)
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
    buf++="}"
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

  //  val s = "abc+(123-4/abc+(2-1))"
  //  val s = "a+b*c*(b+c*d)+x"
  val s = "a+b+c*b*d-c+d+e"
  //  val s = "abc+5/3-r"
  val parsed = parseString(s)
  val elements = groupElements(parsed, s)
  val simpleTree = buildSimpleTree(elements)
  println("simple tree")
  println(simpleTree)
  
  val qq = applyToAll(collectSimilar)(simpleTree) //collectSimilar(simpleTree)
  
  val qqq = applyToAll(colapseDivides)(qq)
  
  val qqqq = applyLoop(applyToAll(pair))(qq)

  println(s)
  println("collectSimilar")
  println(qq)
//  println("colapse Divides")
//  println(qqq)
  println("pair")
  println(qqqq)
  buildGraphWizFile(Expr(qqqq))

}