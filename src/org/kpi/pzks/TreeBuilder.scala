package org.kpi.pzks

import java.lang.Double

import org.kpi.pzks.Parser._

class Element;

case class Op(c: Char) extends Element;
case class Const(v: Double) extends Element;
case class Var(v: String) extends Element;
case class Ob extends Element
case class Cb extends Element
case class Expr(elements: List[Element]) extends Element{
  def isPure = elements match {
    case List(_, Op(o), _) => true
    case _ => false
  }
}


object TreeBuilder extends App {

  def groupElements(nodes: Seq[Node], str: String) = {

    val elements = nodes.toList.map(x => x match {
      case d: Dot => new Digits
      case d: TailDigits => new Digits
      case other => other
    })

    val zipped = elements.tail.zip(s.toList)

//    val list = nodes.toList

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
  
  def buildSimpleTree(src: List[Element]): List[Element]={
    
    def getElementsAfterBraces(tail: List[Element])={

      def recur(l: List[Element], acum: Int):List[Element]={
        val head :: tail = l
        head match{
          case Ob() => recur(tail, acum+1)
          case Cb() => if(acum == 1) tail else recur(tail,acum-1)
          case _ => recur(tail,acum)
        }
      }
      
      recur(tail,1)
      
    }
    
    val braces = src.zip(0 until src.size).filter(x => x.isInstanceOf[OpenBrace] || x.isInstanceOf[CloseBrace])
    
    val (beforeBraces,startOfBraces) = src.span(_ != Ob())
    if(beforeBraces == src){
      return beforeBraces
    }
    
    val afterBraces = getElementsAfterBraces(startOfBraces.tail)
    
    val treeInBraces = Expr(buildSimpleTree(startOfBraces.tail.take(startOfBraces.size -2 - afterBraces.size)))
    
    beforeBraces ::: treeInBraces :: buildSimpleTree(afterBraces)
    
  }
  
  def reformatExpression(ex : Expr):Expr={
    if(ex.isPure){
      return ex
    }
    
    def findMulDiv(ex: Expr)={
      val list = ex.elements
      list.find(x => x== Op('/') || x == Op('*')) match{
        case None => ex
        case Some(el) =>
          val index = list.indexOf(el)
          val before = list.take(index - 1)
          val newExpr = Expr(list.drop(index -1).take(3))
          val after = list.drop(index+2)
          
          val newList = before ::: newExpr :: after
          Expr(newList)
      }
    }
    
//    val newExpr = 
    
    
    
    
    findMulDiv(ex);
  }
  
  
  val s = "abc+(123-4/abc+(2-1))"
//  val s = "abc+5/3-r"
  val parsed = parseString(s)
  val elements = groupElements(parsed, s)
  val simpleTree = buildSimpleTree(elements)
  val reformated = reformatExpression(Expr(simpleTree));
  
  println(s)
  println(reformated)

//  val nodes = parseString(s)
  
  
//  println(groupElements(nodes,s))

}