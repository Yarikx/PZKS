package org.kpi.pzks

import java.io.FileOutputStream
import java.io.File
import scala.util.Random
import sys.process._
import TreeBuilder._;

object GraphTool {

  val rnd = new Random;

  class El(val id: Int)
  case class Oper(c: Char, l: El, r: El) extends El(rnd.nextInt(1000000))
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

  def buildGraphWizFile(e: Expr): File = {

    val map = Map[Element, Int]()
    val buf = new StringBuilder("graph foo{");

    def buildLinks(ex: El) {
      

      ex match {
        case v @ Val(s) =>
          buf ++= "el%d [label=\"%s\"]\n".format(v.id, s)
        case o @ Oper(c, l, r) =>
          buf ++= "el%d [label=\"%c\"]\n".format(o.id, c)
          buf ++= "el%d -- el%d\n".format(o.id, l.id)
          buf ++= "el%d -- el%d\n".format(o.id, r.id)
          buildLinks(l)
          buildLinks(r)

      }
    }
    
    val converted = getBinaryTree(e)
    buildLinks(converted)
    buf ++= "}"

    val q = new FileOutputStream(new File("/tmp/example.dot"))
    q.write(buf.toString.map(_.toByte).toArray)
    q.close()

    Runtime.getRuntime().exec("rm /tmp/qwe.png").waitFor()
    Runtime.getRuntime().exec("rm /tmp/example.png").waitFor()
    Runtime.getRuntime().exec("dot -Tpng -o /tmp/qwe.png /tmp/example.dot ").waitFor()
    Runtime.getRuntime().exec("eog /tmp/qwe.png")

    null
  }
  
  def getLeafs(el:El):List[Oper]={
    el match{
      case v:Val => Nil
      case o@Oper(c ,l:Val, r:Val) => o::Nil 
      case Oper(_, l,r) => getLeafs(l):::getLeafs(r)
    }
  }
}