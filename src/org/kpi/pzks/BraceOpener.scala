package org.kpi.pzks

import org.kpi.pzks.TreeBuilder._;

object BraceOpener {

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
        case x:Element => Set[List[Element]]()
      }
      oneSet
    }).toSet
    
    val result = ((mainSet + elements).flatMap(createDifVariants).toSet + elements).map(x => applyLoop(applyAll(fixNested))(x))
    result
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
    
    val f = found.collect{
      case (Seq(e: Element, o: Op, expr: Expr), i) if (o.c == '*') => list.patch(i, unbrace(e,o,expr), 3)
      case (Seq(expr: Expr, o: Op, e: Element), i) if (o.group == 1) => list.patch(i, unbrace(e,o,expr), 3)
    }.toSet
    (f+list).map(colapseUnariExp)
  }

  def unbrace(e: Element, o: Op, expr: Expr)= {
    val opened:List[Element] = expr.elements.map({
      case op: Op => op
      case el: Element => Expr(List(el, o, e))
    })
    List(Expr(opened))
  }
  
  def collectLoop(elements: List[Element])(f: List[Element] => Set[List[Element]])={
    def recur(lists: Set[List[Element]], calced: Set[List[Element]]):Set[List[Element]]={
      
      val toCalculate = lists--calced
      
      
      val res = toCalculate.flatMap(x => f(x).map(applyLoop(optimizations)(_))).toSet -- calced
      val calced2 = calced ++ toCalculate
//      val res = lists.flatMap(x => f(x)).toSet
      res.foreach(println)
      println("---**************************")
      if(res.isEmpty){
        calced2.map(applyLoop(optimizations)(_))
      }else{
        recur(res, calced2)
      }
    }
    recur(f(elements), Set())
  }
  
  
}