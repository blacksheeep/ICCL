package NFNcore.Lambda

import scala.util.parsing.combinator.syntactical.StdTokenParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.{Parsers, PackratParsers}

class LambdaLexer extends StdLexical {
  override def letter = elem("letter", c => (c.isLetter && c != 'λ') || c == '/')
}

class LambdaParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical
  val lexical = new LambdaLexer
  lexical.delimiters ++= Seq("λ", ".", "(", ")", "-", "/")
  lexical.reserved ++= Seq("call", "lookup", "ifelse", "function", "endfunction", "list")
  
  var globaldict: Map[String, String] = Map()
  
  
  def apply(source: String){
    parse(source)
  }
  
  def parse(source: String): ParseResult[Expr] = {
    val tokens = new lexical.Scanner(source)
    phrase(expr)(tokens)
  }
  
  lazy val expr: PackratParser[Expr] = application | notapp | function
  lazy val notapp: PackratParser[Expr] = lambda | variable | parens | extension
  lazy val lambda: PackratParser[Lambda] = "λ" ~> variable ~ "." ~ expr ^^ { case v ~ "." ~ e  => Lambda(v, e) }
  lazy val application: PackratParser[Apply] = expr ~ notapp ^^ { case left ~ right => Apply(left, right) }
  lazy val variable: PackratParser[Var] = ident ^^ { name => Var(name, -1) }
  lazy val parens: PackratParser[Expr] = "(" ~> expr <~ ")"
  //Extensions
  lazy val extension: PackratParser[Expr] = buildin | number | string | list
  lazy val number: PackratParser[Const] =  posNumber | negNumber
  lazy val posNumber: PackratParser[Const] = numericLit ^^ {case n => Const(n.toInt)}
  lazy val negNumber: PackratParser[Const] = (("-" ~> numericLit) ^^ {case n => Const(-n.toInt)})
  lazy val string: PackratParser[Str] = stringLit ^^ Str
  lazy val list: PackratParser[Lst] =  "list" ~> "(" ~> rep(notapp) <~ ")" ^^ Lst
  //build in functions
  lazy val buildin: PackratParser[Expr] = call | lookup | ifelse
  lazy val call: PackratParser[Call] = "call" ~> numericLit ~ ident ~ rep(notapp) ^^ {case num ~ name ~ params => Call(name, num.toInt, params)}
  lazy val lookup: PackratParser[Lookup] = "lookup" ~> ident ^^ {case n => Lookup(n)}
  lazy val ifelse: PackratParser[Ifelse] = "ifelse" ~> notapp ~ notapp ~ notapp ^^ {case condition ~ fulfilled ~ notfulfilled => Ifelse(condition, fulfilled, notfulfilled)}
  //defining lambda functions, refer function parameters with _1, _2, _3, ..._n
  lazy val function: PackratParser[Function] = "function" ~> ident ~ numericLit ~ expr ~ "endfunction" ~ expr ^^ {case name ~ numOfParams ~ expr ~ endfunction ~ prog => Function(name, numOfParams.toInt, -1, expr, prog)}

  def applyDict(source: String) :String = {
    // replace key with value in source
    globaldict.foldLeft(source){(src, dictionary) => src.replace(dictionary._1, dictionary._2)} 
  }
}