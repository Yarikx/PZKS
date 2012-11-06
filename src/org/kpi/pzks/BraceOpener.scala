package org.kpi.pzks

import org.kpi.pzks.TreeBuilder._;

object BraceOpener {

  def createAllVariantsOfBraces(elements: List[Element]): Set[List[Element]] = {
    val mainSet = elements.zip(elements.indices).flatMap(tuple => {
      val (element, i) = tuple
      val oneSet = element match {
        case Expr(list) => {
          val variants = createAllVariantsOfBraces(list)
          variants.map(variant => {
            list.updated(i, Expr(variant))
          })
        }
        case x:Element => Set[List[Element]]()
      }
      oneSet
    }).toSet
    
    (mainSet + elements).flatMap(createDifVariants).toSet + elements
  }
  
  def createDifVariants(list:List[Element])={
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
    
    found.collect{
      case (Seq(e: Element, o: Op, expr: Expr), i) if (o.group == 1) => list.patch(i, unbrace(e,o,expr), 3)
      case (Seq(expr: Expr, o: Op, e: Element), i) if (o.group == 1) => list.patch(i, unbrace(e,o,expr), 3)
    }.toSet+list
  }

  def unbrace(e: Element, o: Op, expr: Expr)= {
    val opened:List[Element] = expr.elements.map({
      case op: Op => op
      case el: Element => Expr(List(e, o, el))
    })
    opened
  }
}