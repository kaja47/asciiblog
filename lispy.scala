package asciiblog

import scala.annotation.unchecked.uncheckedVariance

object RunLispy extends App {
  val script = args match {
    case Array() =>
      println("lispy repl")
      new Repl().loop()
    case Array("t", rest @ _*) =>
      println(Lispy.tokenize(rest.mkString(" ")).mkString(" "))
    case Array("p", rest @ _*) =>
      println(Lispy.parse(rest.mkString(" ")))
    case Array("s", rest @ _*) =>
      new Repl().eval(rest.mkString(" "))
    case files =>
      val repl = new Repl()
      for (f <- files) {
        repl.eval(io.Source.fromFile(f).mkString)
      }
  }
}


class Repl {
  private var env: Map[String, Any] = Lispy.env

  def eval(line: String) = try {
    val (res, newEnv) = Lispy.evalMain(Lispy.parse(line), env)
    env = newEnv
    println(res)
  } catch {
    case e: Exception => println(e)
  }

  def loop() = {
    import java.io._
    val reader = new BufferedReader(new InputStreamReader(System.in))
    while (true) {
      eval(reader.readLine())
    }
  }

}


object Lispy {

  val tokensRegex = """(?x) \G  ( ' |  \( | \) | \s+ | \d+ | "((?:\\"|[^"])*+)" | [\S&&[^()]]+  )  """.r

  sealed trait Token
  sealed trait AST

  case object Beg extends Token
  case object End extends Token
  case object Quo extends Token
  case class Num(value: Int) extends Token with AST { override def toString = value.toString }
  case class Str(value: String) extends Token with AST { override def toString = s""""$value"""" }
  case class Sym(value: String) extends Token with AST { override def toString = s"Sym($value)" }
  case class Lst(values: List[AST]) extends AST with Seq[AST] {
    override def toString = values.mkString("Lst(", ", ", ")")
    def iterator = values.iterator
    def apply(x: Int) = values(x)
    def length = values.length
  }
  case class ASTSeq(asts: List[AST]) extends AST {
    override def toString = asts.mkString("ASTSeq(", ", ", ")")
  }

  case class Func(f: PartialFunction[List[Any], Any]) extends (List[Any] => Any) with AST {
    def apply(args: List[Any]): Any = f(args)
  }

  case class Func1(f: PartialFunction[Any, Any]) extends (Any => Any) with AST {
    def apply(arg: Any): Any = f(arg)
  }

  def tokenize(txt: String): Vector[Token] =
    tokensRegex.findAllIn(txt)
      .filter(t => !Character.isWhitespace(t(0)))
      .map { t =>
        t.head match {
          case '('  => Beg
          case ')'  => End
          case '\'' => Quo
          case x if Character.isDigit(x) => Num(t.toInt)
          case '"'  => Str(t.tail.init.replaceAll("\\\"", "\""))
          case _    => Sym(t)
        }
      }.toVector

  def _parse(tokens: Vector[Token]): List[AST] = {

    def parseList(tokens: Vector[Token], _pos: Int): (Int, Lst) = {
      var pos = _pos
      var xs = Vector[AST]()

      while (true) {
        tokens(pos) match {
          case End =>
            return (pos+1, Lst(xs.toList))

          case t =>
            val (newpos, res) = parseAny(tokens, pos)
            pos = newpos
            xs :+= res
        }
      }

      sys.error("this is imposible")
    }

    def parseAny(tokens: Vector[Token], pos: Int): (Int, AST) =
      tokens(pos) match {
        case Beg => parseList(tokens, pos+1)
        case End => sys.error("unexpected END")
        case Quo =>
          val (newpos, ys) = parseAny(tokens, pos+1)
          return (newpos, Lst(Sym("quote") :: ys :: Nil))
        case t => (pos+1, t.asInstanceOf[AST])
      }

    if (tokens.isEmpty) return Nil

    val (newpos, res) = parseAny(tokens, 0)
    res :: (if (newpos < tokens.length) _parse(tokens.drop(newpos)) else Nil)
  }


  def rewrite(ast: AST): AST = ast match {
    //(.method obj arg) -> (javacall obj method arg)
    case Lst(Sym(s) :: obj :: args) if s(0) == '.' =>
      Lst(Sym("javacall") :: rewrite(obj) :: Str(s.tail) :: args.map(rewrite))

    // .symbol by itself -> (fn (args...) (.symbol args...))
    case Sym(s) if s(0) == '.' =>
      Func { case obj :: args => javacall(obj, s.tail, args) }

    // (Classname/methodName args)
    case Lst(Sym(s) :: args) if s.contains("/") =>
      val Array(cls, meth) = s.split("/", 2)
      Lst(Sym("javastatic") :: Str(cls) :: Str(meth) :: args.map(rewrite))

    //(new class args) -> (javanew class args)
    case Lst(Sym("new") :: Sym(cls) :: args)  =>
      Lst(Sym("javanew") :: Str(cls) :: args.map(rewrite))

    case Lst(Sym("defn") :: name :: args :: body) =>
      Lst(Sym("def") :: name :: Lst(Sym("fn") :: args :: body.map(rewrite)) :: Nil)

    case Lst(s :: args) => Lst(s :: args.map(rewrite))
    case ASTSeq(asts) => ASTSeq(asts.map(rewrite))
    case ast => ast
  }


  def parse(code: String): ASTSeq = ASTSeq(_parse(tokenize(code)) map rewrite)


  val env = Map[String, Any](
    "+"  -> Func { case args => args.map(number).sum },
    "*"  -> Func { case args => args.map(number).product },
    "-"  -> numfun2(_ - _),
    "/"  -> numfun2(_ / _),
    ">"  -> numfun2(_ > _),
    ">=" -> numfun2(_ >= _),
    "<"  -> numfun2(_ < _),
    "<=" -> numfun2(_ <= _),
    "==" -> Func { case a :: b :: Nil => a == b },
    "!=" -> Func { case a :: b :: Nil => a != b },
    "not"-> Func { case a :: Nil => !truthy(a) },

    "fix-null" -> Func { case a :: b :: Nil => if (a != null) a else b },
    "fix"      -> Func { case a :: b :: Nil => if (truthy(a)) a else b },
    "fix-map"  -> Func { case a :: funcs => funcs.foldLeft(a) { case (a, f) =>
      (a, f) match {
        case (null, Func(f)) => null
        case (null, v) => v
        case (a, Func(f)) => f(a :: Nil)
        case (a, v) => a
      }
    }},

    "list"   -> Func { case rest => rest },
    "vec"    -> Func { case rest => rest.toVector },
    "map"    -> Func { case seq :: (f: Func) :: args => seq.asInstanceOf[collection.Seq[Any]].map { x => f(x :: args) } },
    "filter" -> Func { case seq :: (f: Func) :: args => seq.asInstanceOf[collection.Seq[Any]].filter { x => truthy(f(x :: args)) } },
    "size"   -> Func { case seq :: Nil =>
      seq match {
        case s: String => s.length
        case s: Seq[_] => s.size
        case s: java.util.Collection[_] => s.size
        case s => s.asInstanceOf[{ def size: Int }].size
      }
    },


    "javacall"   -> Func { case (meth: String) :: obj :: args           => javacall(obj, meth, args) },
    "javanew"    -> Func { case (cls: String) :: args                   => javacall(Class.forName(cls), "new", args) },
    "javastatic" -> Func { case (cls: String) :: (meth: String) :: args => javacall(Class.forName(cls), meth, args) },

    "true"  -> true,
    "false" -> false,
    "null"  -> null,

    "null?"     -> Func { case a :: Nil => a == null },
    "not-null?" -> Func { case a :: Nil => a != null },
    "sym?"      -> Func { case a :: Nil => a.isInstanceOf[Sym] },
    "str?"      -> Func { case a :: Nil => a.isInstanceOf[Str] },

    "join*"     -> Func { case args => args.mkString },
    "join"      -> Func { case (arg: IterableOnce[_]) :: Nil => arg.mkString(" ") },
    "println"   -> Func { case args => args.foreach(a => print(a)); println(); args.last },
  )


  def javacall(obj: Any, method: String, args: Seq[Any]): Any = {
    new java.beans.Expression(obj, method, args.toArray.asInstanceOf[Array[Object]]).getValue
  }

  type Env = Map[String, Any]


  def eval(ast: AST, env: Env): Any = ast match {
    case asts: ASTSeq => evalMain(asts, env)
    case ast => evalExpr(ast, env)
  }

  def evalMain(asts: ASTSeq, env: Env): (Any, Env) = {

    def e(asts: List[AST], env: Env): (Any, Env) =
      asts match {
        case Lst(Sym("def") :: Sym(name) :: ast :: Nil) :: rest =>
          e(rest, env + (name -> evalExpr(ast, env)))

        case ast :: Nil =>
          (evalExpr(ast, env), env)

        case ast :: rest =>
          evalExpr(ast, env)
          e(rest, env)

        case Nil => (null, env)
      }

    e(asts.asts, env)
  }

  def evalExpr(ast: AST, env: Env): Any = {
    ast match {
      case Num(value) => value
      case Str(value) => value
      case Lst(Sym("quote") :: arg :: Nil) => arg
      case Lst(Sym("if") :: cond :: thn :: els :: Nil) =>
        if (truthy(evalExpr(cond, env))) evalExpr(thn, env) else evalExpr(els, env)
      case Lst(Sym("if") :: cond :: thn :: Nil) =>
        if (truthy(evalExpr(cond, env))) evalExpr(thn, env) else null
      case Lst(Sym("let") :: (bindings: Lst) :: expr :: Nil) =>
        val newEnv = bindings.values.grouped(2).foldLeft(env) { case (env, Seq(Sym(s), v)) =>
          env + (s -> evalExpr(v, env))
        }
        evalExpr(expr, newEnv)

      case Lst(Sym("fn") :: Lst(Sym(formalArg) :: Nil) :: body :: Nil) =>
        Func1 { case arg => evalExpr(body, env + (formalArg -> arg)) }
      case Lst(Sym("fn") :: Lst(formalArgs) :: body :: Nil) =>
        Func { case args => evalExpr(body, env ++ (formalArgs.map(_.asInstanceOf[Sym].value) zip args)) }

      case Lst(Sym("fn") :: Sym(fa) :: body :: Nil) =>
        Func { case args => evalExpr(body, env + (fa -> args)) }

      case Lst(Sym("and") :: args) => args.forall(a => truthy(evalExpr(a, env)))
      case Lst(Sym("or") :: args) => args.exists(a => truthy(evalExpr(a, env)))

      case Lst(expr :: values) =>
        evalExpr(expr, env) match {
          case Func(f) => f(values.map(evalExpr(_, env)))
          case Func1(f) if values.length == 1 => f(values.head)
          case f: Function1[Any, Any] @uncheckedVariance if values.length == 1 => f.apply(evalExpr(values.head, env))
        }

      case Sym("env") => env
      case Sym(s) =>
        if (!env.contains(s)) throw new LispyException("symbol `"+s+"` not found")
        env(s)

      case ASTSeq(ast :: Nil) =>
        evalExpr(ast, env)
      case ASTSeq(ast :: rest) =>
        evalExpr(ast, env)
        evalExpr(ASTSeq(rest), env)

      case f: Func  => f
      case f: Func1 => f

      case _ => sys.error(ast.toString)
    }
  }


  def truthy(x: Any) = x match {
    case "" | 0 | false | null => false
    case _ => true
  }

  def number(x: Any) = x match {
    case x: Integer => x.intValue()
    case Num(x) => x
    case _ => sys.error(s"$x is not a number")
  }


  def numfun2(f: (Int, Int) => Any) = Func{ case a :: b :: Nil => f(number(a), number(b)) }
}


class LispyException(msg: String) extends Exception(msg)
