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
          if my == his;
          if my != Item(Const(1.0), false)
        ) yield {
          val myWithOne = Line(this.line.updated(i, Item(Const(1), false)), this.pos, this.negative).optimize
          val hisWithOne = Line(that.line.updated(j, Item(Const(1), false)), that.pos, that.negative).optimize
          
          println("opa opa"+myWithOne)
          println("opa opa"+hisWithOne)
          
          val hh = hisWithOne.toElements match{
            case Op('*')::rest => Op('+')::rest
            case Op('+')::rest => Op('+')::rest
            case x => x
          }
          val elements = (myWithOne.toElements ::: hh).tail
          
          val opt = applyLoop(optimizations)(elements)
          
          val withBraces = Expr(opt) :: (if(my.negative) Op('/') else Op('*')) :: my.el :: Nil
          
//          println("D*********************")
//          println("my [%s ]" format myWithOne.toElements)
//          println("his [%s ]" format hisWithOne.toElements)
//          println("all [%s ]" format opt)
//          println("braces [%s ]" format withBraces)
//          println("/D*********************")
          
          Expr(withBraces)
        }

        Some(shared)
      } else {
        None
      }
    }

    def optimize = {
      val optimized1 = line.filter {
        case Item(Const(1), true) => false
        case _ => true
      }

      val optimized = if (optimized1.forall(!_.negative) || optimized1.size > 2) {
        optimized1.filter {
          case Item(Const(1), false) => false
          case _ => true
        } match{
          case Nil => List(Item(Const(1),false))
          case x => x
        }
      } else optimized1
      
//      val optimized = optimized1

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
    
    
    def isGoodForLine(l: List[Element])={
      val group = l.collect {
        case o: Op => o
      }.distinct.map(_.group).distinct
      group.size == 1
    }

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
  def searchForAll(elements: List[Element]): Set[List[Element]] = {

    if(!Line.isGoodForLine(elements)){
      return Set(elements)
    }
    
    val mainLine = Line(elements)
    val g = mainLine.group
    val subLines = mainLine.map {
      case Item(Expr(els), neg) => Line(els, neg)
      case Item(e: Element, neg) =>
        val list = e :: Op('*') :: Const(1) :: Nil
        Line(list, neg)
      case x => return Set(elements)
//      case x => throw new IllegalStateException("can not create sublines for [%s]".format(x))
    }.toList


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
    
    all.filterNot(_.isEmpty).flatten.map(x=> x.toElements.tail).toSet+elements
  }

}