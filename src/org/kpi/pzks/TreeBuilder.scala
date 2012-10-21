package org.kpi.pzks

import java.lang.Double

import org.kpi.pzks.Parser._

class Element;

case class Op(c: Char) extends Element;
case class Const(v: Double) extends Element;
case class Var(v: String) extends Element;
case class Ob() extends Element
case class Cb() extends Element

object TreeBuilder extends App {

  val s = "abc+bcd-5*((abdc*(546.34-3)+3)/2)"

  val elements = parseString(s).toList.map(x => x match {
    case d: Dot => new Digits
    case d: TailDigits => new Digits
    case other => other
  })
  println(elements)

  val zipped = elements.tail.zip(s.toList)
  println(zipped)

  def groupElements(seq: Seq[(Node, Char)]) = {
    val list = seq.toList

    def buildRecur(nodes: List[(Node, Char)], res: List[Element]): (List[(Node, Char)], List[Element]) = {
      
      if(nodes.isEmpty){
        return (nodes, res)
      }
      
      val firstChar = nodes.head._1;
      
      firstChar match {
        case d:OpenBrace =>
          return buildRecur(nodes.tail, new Ob :: res)
        case d:CloseBrace => 
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

    buildRecur(list, List[Element]())._2.reverse

  }

  println(groupElements(zipped))

}