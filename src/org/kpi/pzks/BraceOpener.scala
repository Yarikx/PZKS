package org.kpi.pzks

import org.kpi.pzks.TreeBuilder._;

object BraceOpener{
  def findClosedBraces(list:Seq[Element])={
    val sliding = list.sliding(3).toList
    val zipped = sliding zip sliding.indices
    val found = zipped.filter(tuple =>{
      val (window, index) = tuple
      window match{
        case Seq(e: Element, o:Op, expr:Expr) if(o.group==1) => true
        case Seq(expr:Expr, o:Op, e: Element) if(o.group==1) => true
        case _ => false
      }
    })
    found
  }
  
  def createAllVariantsOfBraces(elements:List[Element], set:Set[List[Element]]):Set[List[Element]]={
    elements.map({
      case Expr(list) => {
        val variants = createAllVariantsOfBraces(list,set)
      }
    })
    
    null
  }
  
  def unbrace(e: Element, o:Op, expr:Expr): Set[List[Element]] = {
    val opened = expr.elements.map({
      case op:Op => op
      case el:Element => Expr(List(e, o, el))
    })
    Set(opened)
  }
}