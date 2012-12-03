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

  case class Line(line: List[Item], pos: Op, negative: Boolean = false) extends Traversable[Item] {

    val neg = pos.oposite

    def foreach[U](f: Item => U) = line.foreach(f)
    override def equals(that: Any) = {
      that match {
        case thatLine: Line => thatLine.line.toSet == this.line.toSet
        case _ => false
      }
    }

    override def toString = (if (negative) "!" else "") + "Line%c[%s]".format(pos.c, line.collect {
      case Item(el, n) => "%c[%s]".format((if (n) neg else pos).c, el)
    }.mkString(",   "))

    def compare(that: Line) = {
      that.line.map(_.el).toSet == this.line.map(_.el).toSet
    }

    def elements = line.map(_.el)
    val group = pos.group
    def set = line.toSet

    def getShared(that: Line): Option[Seq[Expr]] = {
      if (that.group == this.group) {
        val shared = for (
          i <- 0 until this.line.size;
          j <- 0 until that.line.size;
          my = this.line(i);
          his = that.line(j);
          if my == his
        ) yield {
          val myWithOne = Line(this.line.updated(i, Item(Const(1), false)), this.pos, this.negative).optimize
          val hisWithOne = Line(that.line.updated(j, Item(Const(1), false)), that.pos, that.negative).optimize
          
          val elements = (myWithOne.toElements ::: hisWithOne.toElements).tail
          
          val opt = applyLoop(optimizations)(elements)
          
          val withBraces = Expr(opt) :: (if(my.negative) Op('/') else Op('*')) :: my.el :: Nil
          
          println("D*********************")
          println("my [%s ]" format myWithOne.toElements)
          println("his [%s ]" format hisWithOne.toElements)
          println("all [%s ]" format opt)
          println("braces [%s ]" format withBraces)
          println("/D*********************")
          
          Expr(withBraces)
        }

        Some(shared)
      } else {
        None
      }
    }

    def optimize = {
      val optimized1 = line.filterNot {
        case Item(Const(1), true) => true
        case _ => false
      }

      val optimized = if (optimized1.forall(!_.negative) || optimized1.size > 2) {
        optimized1.filterNot {
          case Item(Const(1), false) => true
          case _ => false
        }
      } else optimized1

      Line(optimized, pos, negative)
    }

    def oposite = Line(line, pos, !negative)

    def -(that: Line) = {
      require(this.pos == that.pos)
      Line(line.filterNot(that.line.contains(_)), pos, negative)
    }

    def +(that: Line) = {
      require(this.pos == that.pos)
      Line(this.line ::: that.line, pos, negative)
    }

    def toElements = {
      val tmp = line.flatMap(item => (if (item.negative) neg else pos) :: item.el :: Nil)
      val res = if (negative) {
        Op('-') :: Expr(tmp.tail) :: Nil
      } else {
        tmp
      }
      res
    }

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
        case (o: Op) :: el :: Nil => Item(el, o == neg)
      }.toList

      (tmpLine, pos, neg)
    }

    def apply(l: List[Element]) = {
      val (line, pos, neg) = convert(l)
      new Line(line, pos)
    }

    def apply(l: List[Element], b: Boolean) = {
      val (line, pos, neg) = convert(l)
      new Line(line, pos, b)
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

    val all = for (
      i <- 0 until subLines.size;
      j <- i + 1 until subLines.size;
      l1 = subLines(i);
      l2 = subLines(j)
    ) yield {
      val option = l1 getShared l2
      option match {
        case Some(sharedVariants: Seq[Expr]) =>
          val lines = for(shared <- sharedVariants) yield{
            
            val q = mainLine.line
            val filtered = q.filterNot{
              case it:Item if it==q(i) => true
              case it:Item if it==q(j) => true
              case _ => false
            }
            
            Line(Item(shared, false)::filtered, mainLine.pos, mainLine.negative)
          }
          lines.filterNot(_.isEmpty)
        case _ => Vector()
      }

    }
    
    all.filterNot(_.isEmpty).flatten.map(x=> x.toElements.tail)
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