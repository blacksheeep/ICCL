package NFNcore.Lambda

// AST
sealed trait Expr
case class Lambda(arg: Var, body: Expr) extends Expr
case class Apply(fun: Expr, arg: Expr) extends Expr
case class Var(name: String, num: Int = 0) extends Expr

// Extensions
case class Const(const: Int) extends Expr
case class Str(const: String) extends Expr
case class Lst(list: List[Expr]) extends Expr
case class Name(name: List[String]) extends Expr

// BuildIn
case class Call(fname: Name, numParams: Int, params: List[Expr]) extends Expr
case class Lookup(name: String) extends Expr
case class Ifelse(condition: Expr, fullfilled: Expr, notfullfilled: Expr) extends Expr

//Lambda Functions definitions
case class Function(name: Name, numOfParam: Int, startVarNum: Int, expr: Expr, prog: Expr) extends Expr
