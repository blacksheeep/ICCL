package NFNcore.Lambda.Debug

import NFNcore.Lambda._


class LambdaASTPrinter{
  def apply(expr: Expr) :String = expr match {
    case Lambda(arg, body) => p"λ$arg.$body"
    case Apply(fun, body) => p"$fun $body"
    case Var(name, _) => s"$name"
    case _ => ???
  }
  
  implicit class PrettyPrinting(val sc: StringContext) {
    def p(args: Expr*) = sc.s((args map parensIfNeeded):_*)
  }
  
  def parensIfNeeded(expr: Expr) = expr match {
    case Var(name, _) => name
    case _         => "(" + apply(expr) + ")"
  }
}