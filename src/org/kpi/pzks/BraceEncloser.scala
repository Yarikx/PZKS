package org.kpi.pzks

import org.kpi.pzks.TreeBuilder._
import scala.collection.generic.GenericTraversableTemplate
import scala.collection.TraversableLike
import scala.collection.mutable.ListBuffer
import scala.collection.generic.CanBuildFrom
import scala.collection.generic.TraversableFactory

object BraceEncloser {

  //  abstract class Item0

  case class Item(
    el: Element,
    negative: Boolean = false)

  case class Line(line: List[Item], pos: Op, neg: Op, negative: Boolean = false) extends Traversable[Item] {

    def foreach[U](f: Item => U) = line.foreach(f)
    override def equals(that: Any) = {
      that match {
        case thatLine: Line => thatLine.line.toSet == this.line.toSet
        case _ => false
      }
    }

    override def toString = "Line%c[%s]".format(pos.c, line.collect {
      case Item(el, n) => "%c[%s]".format((if (n) neg else pos).c, el)
    }.mkString(",   "))

    def compare(that: Line) = {
      that.line.map(_.el).toSet == this.line.map(_.el).toSet
    }

    def elements = line.map(_.el)
    val group = pos.group
    def set = line.toSet

    def getShared(that: Line): Option[Line] = {
      if (that.group == this.group) {
        val shared = this.set intersect that.set
        if (shared.size > 0) {
          Some(Line(shared.toList, this.pos, this.neg, false))
        } else {
          None
        }
      } else {
        None
      }
    }

    def oposite = Line(line, pos, neg, !negative)

  }

  object Line {

    def convert(l: List[Element]) = {
      val group = l.collect {
        case o: Op => o
      }.distinct.map(_.group).distinct
      require(group.size == 1, "can not create line from [%s]".format(l))

      val pos = if (group(0) == 1) Op('*') else Op('+')
      val neg = pos.oposite

      val withFirst = l.head match {
        case Op(o) => l
        case e: Element => pos :: l
      }
      require(withFirst.size % 2 == 0, "created line has wrong values [%s]".format(l))
      val tmpLine = withFirst.grouped(2).collect {
        case (o: Op) :: el :: Nil => Item(el, el == neg)
      }.toList

      (tmpLine, pos, neg)
    }

    def apply(l: List[Element]) = {
      val (line, pos, neg) = convert(l)
      new Line(line, pos, neg)
    }

    def apply(l: List[Element], b: Boolean) = {
      val (line, pos, neg) = convert(l)
      new Line(line, pos, neg, b)
    }
  }

  //  def searchForAll(elements: List[Element]): Set[List[Element]] = {
  def searchForAll(elements: List[Element]) = {

    val mainLine = Line(elements)
    val g = mainLine.group
    val subLines = mainLine.map {
      case Item(Expr(els), neg) => Line(els, neg)
      case Item(e: Element, neg) =>
        val list = e :: Op('*') :: Const(1) :: Nil
        Line(list, neg)
      case x => throw new IllegalStateException("can not create sublines for [%s]".format(x))
    }.toList
    
    println("ok")

    for (
      i <- 0 until subLines.size;
      j <- i+1 until subLines.size;
      l1 = subLines(i);
      l2 = subLines(j)
    ) yield {
      val option = l1 getShared l2
      option match {
        case Some(l: Line) => 
          
          //TODO
      }
      option

    }
    

  }

  def createAllVariantsOfBraces(elements: List[Element]): Set[List[Element]] = {
    val mainSet = elements.zip(elements.indices).flatMap(tuple => {
      val (element, i) = tuple
      val oneSet = element match {
        case Expr(list) => {
          val variants = createAllVariantsOfBraces(list)
          //TODO fix expressions like a*(b*c) => a*b*c
          val qq = variants.map(x => elements.updated(i, Expr(x)))
          qq
        }
        case x: Element => Set[List[Element]]()
      }
      oneSet
    }).toSet

    val result = ((mainSet + elements).flatMap(createDifVariants).toSet + elements).map(x => applyLoop(applyAll(fixNested))(x))
    result
  }

  def createDifVariants(list: List[Element]) = {
    val sliding = list.sliding(3).toList
    val zipped = sliding zip sliding.indices
    val found = zipped.filter(tuple => {
      val (window, index) = tuple
      window match {
        case Seq(e: Element, o: Op, expr: Expr) if (o.group == 1) => true
        case Seq(expr: Expr, o: Op, e: Element) if (o.group == 1) => true
        case _ => false
      }
    })

    val f = found.collect {
      case (Seq(e: Element, o: Op, expr: Expr), i) if (o.c == '*') => list.patch(i, unbrace(e, o, expr), 3)
      case (Seq(expr: Expr, o: Op, e: Element), i) if (o.group == 1) => list.patch(i, unbrace(e, o, expr), 3)
    }.toSet
    (f + list).map(colapseUnariExp)
  }

  def unbrace(e: Element, o: Op, expr: Expr) = {
    val opened: List[Element] = expr.elements.map({
      case op: Op => op
      case el: Element => Expr(List(el, o, e))
    })
    List(Expr(opened))
  }

}