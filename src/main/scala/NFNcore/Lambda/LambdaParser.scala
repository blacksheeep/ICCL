package NFNcore.Lambda

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StdTokenParsers

class LambdaLexer extends StdLexical {
  override def letter = elem("letter", c => (c.isLetter && c != 'λ') /*|| c == '/'*/)
}

class LambdaParser extends StdTokenParsers with PackratParsers {
  type Tokens = StdLexical
  val lexical = new LambdaLexer
  lexical.delimiters ++= Seq("λ", ".", "(", ")", "-", "/", ";")
  lexical.reserved ++= Seq("call", "lookup", "ifelse", "function", "endfunction", "list")
  
  var globaldict: Map[String, String] = Map()

  //Buildin Functions
  globaldict += "add" -> "call 3 /local/add;"
  globaldict += "eq" -> "call 3 /local/eq;"
  globaldict += "gt" -> "call 3 /local/gt;"
  globaldict += "head" -> "call 2 /local/head;"
  globaldict += "tail" -> "call 2 /local/tail;"
  globaldict += "len" -> "call 2 /local/length;"
  globaldict += "prepend" -> "call 3 /local/prepend;"
  
  
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
  lazy val extension: PackratParser[Expr] = buildin | number | string | list | name
  lazy val name: PackratParser[Name] = rep(nameComponent) <~ ";" ^^ {case s => println("Name", s); Name(s.map(_.const))}
  lazy val nameComponent: PackratParser[Str] = "/" ~> ident ^^ {case s => println("NameComp", s); Str(s)}
  lazy val number: PackratParser[Const] =  posNumber | negNumber
  lazy val posNumber: PackratParser[Const] = numericLit ^^ {case n => println(n); Const(n.toInt)}
  lazy val negNumber: PackratParser[Const] = (("-" ~> numericLit) ^^ {case n => Const(-n.toInt)})
  lazy val string: PackratParser[Str] = stringLit ^^ Str
  lazy val list: PackratParser[Lst] =  "list" ~> "(" ~> rep(notapp) <~ ")" ^^ Lst
  //build in functions
  lazy val buildin: PackratParser[Expr] = call | ifelse
  lazy val call: PackratParser[Call] = "call" ~> numericLit ~ name ~ rep(notapp) ^^ {case num ~ n ~ params => println("Call "); Call(n, num.toInt, params)}
  lazy val ifelse: PackratParser[Ifelse] = "ifelse" ~> notapp ~ notapp ~ notapp ^^ {case condition ~ fulfilled ~ notfulfilled => Ifelse(condition, fulfilled, notfulfilled)}
  //defining lambda functions, refer function parameters with _1, _2, _3, ..._n
  lazy val function: PackratParser[Function] = "function" ~> name ~ numericLit ~ expr ~ "endfunction" ~ expr ^^ {case n ~ numOfParams ~ expr ~ endfunction ~ prog => Function(n, numOfParams.toInt, -1, expr, prog)}

  def applyDict(source: String) :String = {
    // replace key with value in source
    globaldict.foldLeft(source){(src, dictionary) => src.replace(dictionary._1, dictionary._2)} 
  }
}