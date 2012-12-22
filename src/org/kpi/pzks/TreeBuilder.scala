package org.kpi.pzks

import java.io.File
import scala.io.Source
import java.io.FileOutputStream
import java.lang.Double

import scala.annotation.tailrec
import scala.collection.mutable.Map
import scala.collection.mutable.StringBuilder
import scala.util.Random

import org.kpi.pzks.BraceOpener._
import org.kpi.pzks.Parser.CloseBrace
import org.kpi.pzks.Parser.Digits
import org.kpi.pzks.Parser.Dot
import org.kpi.pzks.Parser.Node
import org.kpi.pzks.Parser.OpenBrace
import org.kpi.pzks.Parser.Operation
import org.kpi.pzks.Parser.Symbols
import org.kpi.pzks.Parser.TailDigits
import org.kpi.pzks.Parser.parseString
import BraceEncloser._;
import GraphTool._;

class Element;

trait Negativable extends Element{
  def neg:Element
  def isNegative:Boolean
}

case class Op(c: Char) extends Element {
  def group = TreeBuilder.getGroup(this)
  override def toString = c.toString
  def oposite= Op(c match {
    case '+' => '-'
    case '-' => '+'
    case '*' => '/'
    case '/' => '*'
  })
}
case class Const(v: scala.Double) extends Element with Negativable{
  override def toString = v.toString
  def neg=Const(-v)
  def isNegative = v<0
}
case class Var(v: String) extends Element with Negativable{
  val negative = false
  override def toString = (if(negative)"-"else"")+v.toString
  def neg={
    val oposite = !negative 
    new Var(v){override val negative = oposite}
  }
  def isNegative = negative
}
case class Ob() extends Element
case class Cb() extends Element
case class Expr(elements: List[Element]) extends Element {
  def isPure = elements match {
    case List(_, Op(o), _) => true
    case _ => false
  }

  override def toString = "(%s)".format(elements.mkString(""))
}

object TreeBuilder extends App {

  def groupElements(nodes: Seq[Node], str: String) = {

    val elements = nodes.toList.map(x => x match {
      case d: Dot => new Digits
      case d: TailDigits => new Digits
      case other => other
    })

    val zipped = elements.tail.zip(str.toList)

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
    //Fix negative on numbers
    def fixNegative(l:List[Element])=l match{
      case (n:Negativable)::Const(c)::tail => 
        n.neg::tail
      case x => x
    }
    
    val braces = src.zip(0 until src.size).filter(x => x.isInstanceOf[OpenBrace] || x.isInstanceOf[CloseBrace])

    val (beforeBraces, startOfBraces) = src.span(_ != Ob())
    if (beforeBraces == src) {
      return beforeBraces
    }

    val afterBraces = getElementsAfterBraces(startOfBraces.tail)

    val treeInBraces = buildSimpleTree(startOfBraces.tail.take(startOfBraces.size - 2 - afterBraces.size))
    val processedExprTree = Expr(fixNegative(treeInBraces))

    fixNegative(beforeBraces) ::: processedExprTree :: buildSimpleTree(afterBraces)

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
      if (list.isEmpty) {
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
    if (operations.isEmpty) {
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
      case oper @ Op(q) if (oper == op) => oposite
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
    
  def openUnariOpBeforeBraces(l: List[Element]): List[Element] = {
    l match {
      case Op('-')::Expr(list)::tail => 
        assert(list.collect{
        		case o:Op => o
        	  }.forall(x => x.group ==2)
         , "can not open negative expr [%s]" format list)
         
        val head::rest = list
        val newHead = head match{
          case n: Negativable => n.neg
          case x => throw new IllegalStateException("first element [%s] of expr [%s] is not negativable" format (head, list));
        }
        val processedList = rest.map{
          case Op('+') => Op('-')
          case Op('-') => Op('+')
          case x => x
        }
        newHead::processedList:::tail
      case Op('-')::(n:Negativable)::rest => openUnariOpBeforeBraces(n.neg::rest)
      case x => x
    }
  }
  
  //Fix negative vars and consts after ops
    def fixNegVals(l:List[Element])={
      val slices  = l sliding 2
      val toReplace = slices collect {
        case orig@List(o:Op, n:Negativable) if (o.group == 2 && n.isNegative)=> (orig, o.oposite::n.neg::Nil)
      } toList;
      if(toReplace.isEmpty){
        l
      }else{
        toReplace.foldLeft(l)((x,y) => {
          val(orig, res) = y
          val index = x.indexOfSlice(orig)
          x.patch(index, res, 2)
        })
      }
    }
    
  def replaceConstants(list: List[Element]): List[Element] ={
    val operations = list.collect({case o:Op => o}).distinct
    val sameOp = operations.size == 1
    if(!sameOp || operations(0).c=='-'){
      return list
    }
    val operands = list.filter({
      case o:Op => false;
      case _ => true
    })
    
    val sorted = operands.sortWith((x,y) => (x,y)match{
      case (c:Const, _) => true
      case _ => false
    })
    
    val head = sorted.head
    val tail = sorted.tail
    head :: tail.flatMap(x=> List(operations(0), x))
  }
  
  def sort(list: List[Element]): List[Element] ={
    val operations = list.collect({case o:Op => o}).distinct
    val sameOp = operations.size == 1
    if(!sameOp || list.size < 4 || operations.contains(Op('/'))){
      return list
    }
    val operands = list.filter({
      case o:Op => false;
      case _ => true
    })
    
    val sorted = operands.sortBy(_.hashCode())
    
    val head = sorted.head
    val tail = sorted.tail
    head :: tail.flatMap(x=> List(operations(0), x))
  }

  def colapseUnariExp(list: List[Element]): List[Element] = {
    val t = list.map {
      //TODO delete or make recursive call to first line
      case Expr(List(e: Expr)) => List(e)
      case Expr(List(e: Element)) => List(e)
      case Expr(List(o: Op, e: Expr)) => List(o, e)
      case x => List(x)
    }
    t.flatten  match{
      case List(Expr(l)) => l
      case x => x
    }
  }

  def operate(c: Char, c1: Double, c2: Double) = {
    c match {
      case '+' => c1 + c2
      case '-' => c1 - c2
      case '/' => c1 / c2
      case '*' => c1 * c2
    }
  }

  //High level
  def operateConstants(list: List[Element]): List[Element] = {
    val found = list.sliding(3).find({
      case List(Const(c1), Op(o), Const(c2)) => true
      case _ => false
    })

    found match {
      case None => return list;
      case Some(slice @ List(Const(c1), Op(o), Const(c2))) =>
        val res = operate(o, c1, c2)

        val start = list.indexOfSlice(slice)

        list.take(start) ::: Const(res) :: list.drop(start + 3)
    }
  }

  def fixNested(list: List[Element]): List[Element] = {
    def replaceNested(el: Element, o: Op, expr: Expr, l: List[Element], start:List[Element]) = {
      val index = start.indexOfSlice(l);
      val exprElements = o match{
        case Op('-') => expr.elements.map({
          case Op('-') => Op('+')
          case Op('+') => Op('-')
          case Op(op) => throw new IllegalStateException("operation '%c' is from wrong group" format op)
          case x => x
        })
        case x => expr.elements
      }
      val newSeq = colapseDivides(el :: o :: exprElements)
      start.patch(index, newSeq, l.size)
    }
    def testFirst={
      val trinity = list.take(3);
      trinity match{
        case l@List(e@Expr(exList), o: Op, el1: Element) if exList.collect { case o: Op => o }.forall(x => (x.c == '+' || x.c == '*' || x.c == '-') && x.group == o.group) =>
          list.patch(0, exList, 1)
        case x => list
      }
    }
    
    if(list.toString.contains("+q), +, b")){
      println("thats it")
    }
    
    val tested = testFirst
    val start = if(tested==list)
      list 
    else 
      optimizations(tested)
//    val tested = applyAll(collectSimilar, colapseDivides, colapseMinuses, collectSimilar)(testFirst)
//    if(tested)
    val sliding = start.sliding(3)
    val found = sliding.collect {
      case l@List(el1: Element, o: Op, e@Expr(exList)) if exList.collect { case o: Op => o }.forall(x => (x.c == '+' || x.c == '*') && x.group == o.group) =>
        (el1, o, e, l)
      case l@List(el1: Element, o: Op, e@Expr(exList)) if exList.collect { case o: Op => o }.forall(x => (x.c == '+' || x.c == '*'|| x.c == '-') && x.group == o.group) =>
        (el1, o, e, l)
      case l@List(e@Expr(exList), o: Op, el1: Element) if exList.collect { case o: Op => o }.forall(x => (x.c == '+' || x.c == '*' ) && x.group == o.group) =>
        if(o.group == 1){
          (el1, Op('*'), e, l)
        }else{
          (el1,Op('+'),e , l)
        }
    }
    
    if (!found.isEmpty) {
      found.next match{
        case (el1: Element, o: Op, expr: Expr, l) => replaceNested(el1, o, expr, l, start)
      }
    } else {
      return start
    }

  }

  //other

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
  
  

  def applyAll(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => applyToAll(f)(el))
  }

  val safeOptimization = ((f: List[Element] => (List[Element])) => applyAll(
    f,
    applyLoop(colapseUnariExp),
    collectSimilar)_)

  def applyOptimisators(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => safeOptimization(f)(el))
  }

  val highOptimisation = ((f: List[Element] => (List[Element])) => applyOptimisators(
    f,
    collectSimilar,
    colapseMinuses,
    colapseDivides,
    collectSimilar,
    fixNegVals
    )_)

  def applyHighOptimisators(fs: (List[Element]) => List[Element]*)(list: List[Element]) = {
    fs.foldLeft(list)((el, f) => highOptimisation(f)(el))
  }

  val optimizations = applyHighOptimisators(
    openUnariOpBeforeBraces,
    collectSimilar,
    fixNested,
    collectSimilar,
    operateConstants,
    sort,
    replaceConstants
    ) _
    
  def getString() = {
    Source.fromFile("/tmp/input").getLines().next.trim
  }
  
  def getOpt(s:String)={
    val parsed = parseString(s)
    		val elements = groupElements(parsed, s)
  val simpleTree = buildSimpleTree(elements)
  println("simple tree")
  println(simpleTree)

  val optimized = applyLoop(optimizations)(simpleTree)
  optimized
  }
  
  def memoize[A, B](f: A => B) = new (A => B) {
    val cache = scala.collection.mutable.Map[A, B]()
    def apply(x: A): B = cache.getOrElseUpdate(x, f(x))
  }

  val s = getString()
  
  val optimized = getOpt(s)
//
//  
//  val testS = "b*(1+a)-3-y"
//    val test = List(Var("b"),Op('*'), Expr(List(Const(1), Op('+'), Var("a"))), Op('-'), Const(3), Op('-'), Var("y"))
    
//    println(createDifVariants(test).map(x => applyLoop(applyAll(fixNested))(x)))
//  val test = b
  
  println("braces ************");

  val op1 = collectLoop(optimized)(createAllVariantsOfBracesMem) //.foreach(l => println(l.mkString))
  val op2 = collectLoop(optimized)(searchForAll) //.foreach(l => println(l.mkString))
  
  val all = (op1 ++ op2).toList
//  all.foreach(println)
  val exp = Expr(all(1))
  println(" expr = " + exp)
  println("****leafs***")
  val tree = getBinaryTree(exp)
  val leafs = getLeafs(tree)
  val tree2 =  removeLeaf(tree, leafs(0), Progress(3,5))
  val tree3 = replaceProgress(tree2, 20)
  
  drawTree(tree3)
  println("braces ************");
  
  
  
  

//  val paired = applyToAll(pair)(optimized)

//  println(s)
//  println("pair")
//  println(paired)
//  buildGraphWizFile(Expr(paired))
}