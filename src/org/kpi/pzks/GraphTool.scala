package org.kpi.pzks

import java.io.FileOutputStream
import java.io.File
import scala.util.Random
import sys.process._
import TreeBuilder._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.LinkedList
import scala.collection.mutable.MutableList

object GraphTool {

  val rnd = new Random;

  class El(val id: Int){
    def leafs = getLeafs(this)
    def replace(leaf:Oper) = replaceProgress(this, leaf)
    def rm(leaf:Oper, p:Progress) = removeLeaf(this, leaf, p)
  }
  case class Oper(c: Char, l: El, r: El) extends El(rnd.nextInt(1000000)){
    def value={
      c match{
        case '+' => 1
        case '-' => 1
        case '*' =>	3
        case '/' => 5
      }
    }
  }
  case class Val(s: String) extends El(rnd.nextInt(1000000))
  
  def pair(list: List[Element]): List[Element] = {
    if (Expr(list).isPure) {
      list
    } else {
      val (start, rest) = list.splitAt(3);
      val exp = Expr(start)
      //TODO add check
      val end = if (rest.size > 3) {
        val exTail = Expr(rest tail)
        if (exTail isPure) {
          List(rest.head, exTail)
        } else {
          rest.head :: pair(rest.tail)
        }
      } else {
        rest
      }
      exp :: end
    }
  }

  def convert(e: Element): El = e match {
    case v: Var => Val(v.toString)
    case Const(x) => Val(x.toString)
    case Expr(List(ql: Element, Op(o), qr: Element)) =>
      Oper(o, convert(ql), convert(qr))
    case Expr(List(e: Element)) => convert(e)
    case x => throw new IllegalStateException("Wrong expression format for building binary tree [%s]" format x)
  }

  def up(exp: Expr): Element = {
    exp match {
      case Expr(List(ex: Expr)) => up(ex)
      case elem: Element => elem
    }
  }

  def getBinaryTree(e: Expr) = {
    val paired = applyToAll(pair)(e.elements)
    val checked = up(Expr(paired))
    val converted = convert(checked)
    converted
  }
  
  def drawTree(el:El){
    
    val map = Map[Element, Int]()
    val buf = new StringBuilder("graph foo{");
    
    def buildLinks(ex: El) {
      ex match {
        case v @ Val(s) =>
          buf ++= "el%d [label=\"%s\"]\n".format(v.id, s)
        case p @ Progress(o) =>
          buf ++= "el%d [label=\"progress %s\"]\n".format(p.id, o)
        case o @ Oper(c, l, r) =>
          buf ++= "el%d [label=\"%c\"]\n".format(o.id, c)
          buf ++= "el%d -- el%d\n".format(o.id, l.id)
          buf ++= "el%d -- el%d\n".format(o.id, r.id)
          buildLinks(l)
          buildLinks(r)

      }
    }
    
    buildLinks(el)
    buf ++= "}"

    val q = new FileOutputStream(new File("/tmp/example.dot"))
    q.write(buf.toString.map(_.toByte).toArray)
    q.close()

    Runtime.getRuntime().exec("rm /tmp/qwe.png").waitFor()
    Runtime.getRuntime().exec("rm /tmp/example.png").waitFor()
    Runtime.getRuntime().exec("dot -Tpng -o /tmp/qwe.png /tmp/example.dot ").waitFor()
    Runtime.getRuntime().exec("eog /tmp/qwe.png")

  }

  def buildGraphWizFile(e: Expr){
    val converted = getBinaryTree(e)
    drawTree(converted)
  }
  
  def getLeafs(el:El):List[Oper]={
    el match{
      case v:Val => Nil
      case p:Progress => Nil
      case o@Oper(c ,l:Val, r:Val) => o::Nil 
      case Oper(_, l,r) => getLeafs(l):::getLeafs(r)
    }
  }
  
  case class Progress(op:Oper) extends El(rnd.nextInt(1000000))
  
  
  def removeLeaf(el:El, leaf: Oper, p: Progress)={
    def rm(elem: El):El={
      elem match{
        case o:Oper if o.id == leaf.id => p
        case Oper(c, l, r) =>
          val ll = l match{
            case op:Oper => rm(op)
            case any => any
          }
          val rr = r match{
            case op:Oper => rm(op)
            case any => any
          }
          Oper(c, ll, rr)
        case x => x
      }
    }
    
    rm(el)
  }
  
  def replaceProgress(el:El, leaf:Oper)={
    def replace(elem: El):El={
      elem match{
        case Progress(o) if o==leaf => Val("calculated")
        case Oper(c, l, r) =>
          Oper(c, replace(l), replace(r))
        case x => x
      }
    }
    
    replace(el)
  }
  
  def buildProc(el:El, N:Int){
    val layers :Array[MutableList[Option[Oper]]] = Array.fill(N)(new MutableList)
    val current :Array[Option[Oper]] = Array.fill(N)(None)
    
    var run = true
    var tree = el
    
    
    implicit var time = 0
    while(run){
      val leafs = tree.leafs
      
      current(N-1) foreach(oper => tree = tree.replace(oper))
      
      for(i <- (1 to N-1).reverse){
        current(i) = current(i-1)
      }
      current(0) = None
      
      val leaf = leafs.headOption
      leaf foreach(l => tree = tree.rm(l, Progress(l)))
      
      println("current leaf = "+leaf)
      current(0) = leaf
      val step = current.map{
        case Some(o) => o.value
        case None => 0
      }.reduce(scala.math.max)
      println("step = "+step)
      
      for(i <- 0 until N;
    	  c = current(i);
          l = layers(i);
    	  j <- 0 until step){
        val q = c match{
          case Some(op) if j<op.value => Some(op)
          case _ => None
        }
        
        l += q
      }
      
      time+=step
      if(time >= 100 || tree.isInstanceOf[Val]) run = false
    }
    
    layers.foreach{list =>
      println(list.map{
        case None => "[ ]"
        case Some(op) => "[%c]" format op.c
      })
    }
    
  }
   
}