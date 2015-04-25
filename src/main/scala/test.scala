import Logging.{Debuglevel, DEBUGMSG}
import NFNcore.Lambda._
import NFNcore.NFNNode


object test extends App{
  
  val parser = new LambdaParser


  /*val res = parser.parse("/test/name/foo; /bla/blub")

  println(res)
  val compiler = new LambdaCompiler

  val opcode = compiler(res.get)

  println(opcode)

  val krivine = new Krivine(null)

  val reduced = krivine(opcode)

  println(reduced)*/
  
  //val ast = parser.parse("(λs.((λz.(s z)) x)) y")     
  //val ast = parser.parse("(λx.(λy.y(λz.z)x)a)b")   
  //val ast = parser.parse("(λx.(λy.y x y) a) b")
  //val ast = parser.parse("""call 2 /hallo/bla (call 3 /test/foo /spo/test)""")
  //val ast = parser.parse("""x""")
  
  //val ast = parser.parse("(λx.x) (λx.x) b")
  //val ast = parser.parse("(λx.x) a (λx.x) b")
  //val ast = parser.parse("(λx.x x) (λx.x)")
  
  //val ast = parser.parse("(λx.((λy.x y) z)) w")
  

  //val src = parser.applyDict("add 1 2")
  //val src = parser.applyDict("/test/name 2")

  //val src = "(λx.(λy.y(λz.z)x)a)b"
  //val src = "(λx.x) (λx.x) b"
  //val src = parser.applyDict("add 3 (add 1 2)")
  //val src = parser.applyDict("(λx.add x (add 1 2)) 3")
  //val src = parser.applyDict("ifelse (add 1 1) (add 3 (add 1 2)) 0")
  
  //val src = parser.applyDict("function /ad3; 3 (add _3 (add _2 _1)) endfunction (λx.call 4 /ad3; x 3 4) 2" ) // what if parameter is a var??

  //val src = parser.applyDict("list((add 1 2) (add 2 3))")
  //val src = parser.applyDict("function /ad3; 3 (add _3 (add _2 _1)) endfunction (λx.call 4 /ad3; (call 4 ad3 x 2 3) 4 5) 1" )
  
  
  //val src = parser.applyDict("function rec 2 (ifelse (eq _1 _2) (add _1 _2) (call 3 rec (add _1 1) _2)) endfunction call 3 rec 1 5")
  //val src = parser.applyDict("function rec 2 (ifelse (eq _1 _2) (add _1 _2) (call 3 rec (add _1 ((λx.x) 2)) _2)) 1 endfunction (λx.call 3 rec 1 x) 8")
  
  val src = parser.applyDict("""
    function /sumlist; 1
      ifelse (eq (len _1) 1) 
        (head _1)
        (add 
          (call 3 /sumlist; (tail _1))
          (head _1)
        ) 
    endfunction
    call 3 /sumlist; list(1 2 3 4 5 6 7 8 9 10)
    """)
    
  //val src = parser.applyDict("head (tail (prepend 3 list(2 1)))")

 // DEBUGMSG(Debuglevel.INFO, src)
  println(src)
  val ast = parser.parse(src)
  DEBUGMSG(Debuglevel.INFO, ast.toString())
  
  //val pretty = new LambdaASTPrinter
  //println(pretty(ast.get))

  val compiler = new LambdaCompiler
  
  val opcode = compiler(ast.get)
  DEBUGMSG(Debuglevel.INFO, "Opcodes: " + opcode)
  val krivine = new Krivine(null, null)

  val reduced = krivine(opcode)
 
  DEBUGMSG(Debuglevel.INFO,"Result: "+ reduced)




  
}