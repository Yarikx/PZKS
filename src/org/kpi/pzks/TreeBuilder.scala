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
  
  def applyToAll(f:(List[Element])=>Expr)(ex: Expr):Expr={
    val list = ex.elements;
    val r = list.map(_ match{
      case e:Expr =>
        applyToAll(f)(e)
      case any => any
    })
    Expr(r)
  }
  
  def groupBy[A](f: (A,A)=>Boolean)(elements: List[A])={
    def recur(result:List[List[A]], list:List[A]):List[List[A]]={
      val head = list.head
      val (first, last) = list.span(f(head,_))
      
      val res = first :: result
      
      if(last.isEmpty){
        return res.reverse
      }else{
        return recur(res, last)
      }
    }
    
    recur(List[List[A]](), elements)
  }
  
//  val groupByType = groupBy((x,y) =>x.getClass == y.getClass()) _
  
  def collectSimilar(l:List[Element]):List[Element]={
    def getGroup(e:Element):Int={
      e match{
        case Op('/') | Op('*')=> 1
        case Op('+') | Op('-')=> 2
        case _ => 3
      }
    }

    val operations = l.zip(l.indices)collect{
      case (o:Op, i:Int) => (o, i)
    }
    
    val opGroups = groupBy((x:(Op,Int),y:(Op,Int)) => getGroup(x._1)==getGroup(y._1))(operations)
    val slice = opGroups.find(x => getGroup(x(1)._1)==1) match{
      case Some(mulList) =>
        mulList
      case None =>
        opGroups(0)
    }
    
    val first = math.max(slice.head._2-1, 0)
    val last = slice.last._2+1
    
    if(first==0 &&last == l.size-1){
      return l;
    }else{
      val before = l.take(first)
      val middle = Expr(l.slice(first, last+1))
      val after = l.drop(last+1)
      val result = before ::: middle :: after
      return collectSimilar(result)
    }
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
  
  
//  val s = "abc+(123-4/abc+(2-1))"
  val s = "a+b+c*d*e+f+g"
//  val s = "abc+5/3-r"
  val parsed = parseString(s)
  val elements = groupElements(parsed, s)
  val simpleTree = buildSimpleTree(elements)
//  val reformated = reformatExpression(Expr(simpleTree));
  val qq = collectSimilar(simpleTree)
//  val similar = applyToAll(qq)(Expr(simpleTree))
  
  println(s)
  println(qq)

//  val nodes = parseString(s)
  
  
//  println(groupElements(nodes,s))

}