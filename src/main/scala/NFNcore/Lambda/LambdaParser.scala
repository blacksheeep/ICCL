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
  globaldict += "checkCS" -> "call 2 /local/checkCS;"
  globaldict += "grabCS" -> "call 2 /local/grabCS;"
  globaldict += "pushCS" -> "call 2 /local/pushCS;"
  globaldict += "checkPIT" -> "call 2 /local/checkPIT;"
  globaldict += "grabPIT" -> "call 2 /local/grabPIT;"
  globaldict += "pushPIT" -> "call 2 /local/pushPIT;"
  globaldict += "checkFIB" -> "call 2 /local/checkFIB;"
  globaldict += "grabFIB" -> "call 2 /local/grabFIB;"
  globaldict += "pushFIB" -> "call 2 /local/pushFIB;"
  globaldict += "sendInterest" -> "call 3 /local/sendInterest;"
  globaldict += "sendContent" -> "call 3 /local/sendContent;"
  
  
  def apply(source: String){
    parse(source)
  }
  
  def parse(source: String): ParseResult[Expr] = {
    val t1 = new lexical.Scanner(source)


    var token_list: Vector[String] = Vector()

    var t = t1

    while(!t.atEnd){
      val str: String = t.first.chars.toString

      token_list = token_list ++ Vector(str)
      t = t.rest
    }
    println(token_list)
    val srcmapped: Vector[String] = token_list.map(e => {
      if(globaldict.contains(e)) globaldict(e) else e
    })
    val src_str = srcmapped.fold("") {(z,i) => z+i + " "}
    println(src_str)
    val tokens = new lexical.Scanner(src_str)
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
  lazy val name: PackratParser[Name] = rep(nameComponent) <~ ";" ^^ {case s => Name(s.map(_.const).toVector)}
  lazy val nameComponent: PackratParser[Str] = "/" ~> ident ^^ {case s => Str(s)}
  lazy val number: PackratParser[Const] =  posNumber | negNumber
  lazy val posNumber: PackratParser[Const] = numericLit ^^ {case n => Const(n.toInt)}
  lazy val negNumber: PackratParser[Const] = (("-" ~> numericLit) ^^ {case n => Const(-n.toInt)})
  lazy val string: PackratParser[Str] = stringLit ^^ Str
  lazy val list: PackratParser[Lst] =  "list" ~> "(" ~> rep(notapp) <~ ")" ^^ {case elm => Lst(elm.toVector)}
  //build in functions
  lazy val buildin: PackratParser[Expr] = call | ifelse
  lazy val call: PackratParser[Call] = "call" ~> numericLit ~ name ~ rep(notapp) ^^ {case num ~ n ~ params => Call(n, num.toInt, params.toVector)}
  lazy val ifelse: PackratParser[Ifelse] = "ifelse" ~> notapp ~ notapp ~ notapp ^^ {case condition ~ fulfilled ~ notfulfilled => Ifelse(condition, fulfilled, notfulfilled)}
  //defining lambda functions, refer function parameters with _1, _2, _3, ..._n
  lazy val function: PackratParser[Function] = "function" ~> name ~ numericLit ~ expr ~ "endfunction" ~ expr ^^ {case n ~ numOfParams ~ expr ~ endfunction ~ prog => Function(n, numOfParams.toInt, -1, expr, prog)}

  def applyDict(source: String) :String = {
    // replace key with value in source
    globaldict.foldLeft(source){(src, dictionary) => src.replace(dictionary._1, dictionary._2)} 
  }
}