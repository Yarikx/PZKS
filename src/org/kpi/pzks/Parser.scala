package org.kpi.pzks

import scala.collection.mutable.ListBuffer

object Parser {

  val digits = '0' to '9'
  val symbols = ('a' to 'z') ++ ('A' to 'Z')
  val operations = Seq('*', '/', '+', '-')
  val eof = '\n'

  abstract class Node {
    def getF: PartialFunction[Char, Node]

    def parse(chars: List[Char]) = {
      val first = chars.head
      val tail = chars.tail

      val nextNode = getF(first)

      (nextNode, tail)
    }
    
    override def toString={
      this.getClass().getSimpleName()
    }
  }

  class Start extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if digits contains x => new Digits
      case x if symbols contains x => new Symbols
      case '-' => new Operation
      case '(' => new OpenBrace
      case _ => new Fail("wrong start of expression")
    }
  }
  
  class Digits extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if digits contains x => new Digits
      case x if symbols contains x => new Fail("symbol after digits no allowed")
      case x if operations contains x => new Operation
      case '.' => new Dot
      case ')' => new CloseBrace
      case '\n' => new End
      case _ => new Fail("wrong symbol after digit")
    }
  }
  
  class TailDigits extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if digits contains x => new TailDigits
      case x if symbols contains x => new Fail("symbol after digits no allowed")
      case x if operations contains x => new Operation
      case '.' => new Fail("too much dots in digits")
      case ')' => new CloseBrace
      case '\n' => new End
      case _ => new Fail("wrong symbol after digit")
    }
  }
  
  class Dot extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if digits contains x => new TailDigits
      case '\n' => new End
      case _ => new Fail("not a digit after dot")
    }
  }
  
  class Symbols extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if symbols contains x => new Symbols
      case x if operations contains x => new Operation
      case x if digits contains x => new Fail("digit after symbol no allowed")
      case '(' => new Fail("missed operation before brace")
      case ')' => new CloseBrace
      case '\n' => new End
      case _ => new Fail("wrong symbol after symbol")
    }
  }
  
  class Operation extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if symbols contains x => new Symbols
      case x if digits contains x => new Digits
      case '(' => new OpenBrace
      case x if operations contains x => new Fail("operation after operation is not allowed")
      case '\n' => new Fail("Unexpected end of expression")
      case _ => new Fail("wrong symbol after operation")
    }
  }
  
  class OpenBrace extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if symbols contains x => new Symbols
      case x if digits contains x => new Digits
      case '-' => new Operation
      case '(' => new OpenBrace
      case ')' => new Fail("empty braces")
      case '\n' => new Fail("Unexpected end of expression")
      case _ => new Fail("wrong symbol after opened brace")
    }
  }
  
  class CloseBrace extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x if operations contains x => new Operation
      case ')' => new CloseBrace
      case '(' => new Fail("error: open brace after closeing")
      case '\n' => new End
      case _ => new Fail("wrong symbol after closed brace")
    }
  }

  class End extends Node {
    def getF: PartialFunction[Char, Node] = {
      case x: Any => new End
    }
  }
  
  class Fail(s: String) extends Node {
    println(s)
    
    def getF: PartialFunction[Char, Node] = null
  }

  def parseString(s: String) = {
    
    val buf = new ListBuffer[Node];
    var openBraces = 0;
    
    def checkBraces(end: Boolean):Boolean={
      if(openBraces<0){
        println("closed brace before opening")
        return false;
      }
      
      if(end && openBraces!=0){
        println("open braces must be closed")
        return false;
      }
      return true
      
    }

    def parseRecur(chars: List[Char], node: Node): (List[Char], Node) = {
      buf += node
      
      node match{
        case x: OpenBrace => openBraces+=1
        case x: CloseBrace => openBraces-=1
        case _ => 
      }
      
      if(!checkBraces(false)){
        return (chars, new Fail("error"))
      }
      
      val (nextNode, tail) = node.parse(chars)
      nextNode match {
        case x: End =>
          checkBraces(true)
          (tail, x)
        case x: Fail => (tail, x)
        case x: Node => parseRecur(tail, x)
      }
    }

    val chars = s.replaceAll(" *", "").toList :+ '\n'
    parseRecur(chars, new Start)
    buf
  }
  
////  val s = "(2+3)-(4-5)"s
//  val s = "-2+(3/a*(2-3))-(-23.3+(-1))"
//  
//  
//  val q = parseString(s);
//  println(q)
//  
//  TreeBuilder.main(null)
  
  

}