package asciiblog

object Lispy {

  val tokensRegex = """(?x) \G  (  \( | \) | \s+ | \d+ | "((?:\\"|[^"])*+)" | [\S&&[^()]]+  )  """.r

  sealed trait Token
  sealed trait AST

  case object Beg extends Token
  case object End extends Token
  case class Num(value: Int) extends Token with AST { override def toString = value.toString }
  case class Str(value: String) extends Token with AST { override def toString = s""""$value"""" }
  case class Sym(value: String) extends Token with AST { override def toString = value }
  case class Lst(values: List[AST]) extends AST { override def toString = values.mkString("Lst(", ", ", ")") }
  case class ASTSeq(asts: List[AST]) extends AST

  def tokenize(txt: String): Vector[Token] =
    tokensRegex.findAllIn(txt)
      .filter(t => !Character.isWhitespace(t(0)))
      .map { t =>
        t.head match {
          case '(' => Beg
          case ')' => End
          case x if Character.isDigit(x) => Num(t.toInt)
          case '"' => Str(t.tail.init.replaceAll("""\\"""", """""""))
          case _   => Sym(t)
        }
      }.toVector

  def _parse(tokens: Vector[Token]): List[AST] = {

    def parseList(tokens: Vector[Token], _pos: Int = 0): (Int, Lst) = {
      var pos = _pos
      var xs = Vector[AST]()

      while (true) {
        tokens(pos) match {
          case Beg =>
            val (newpos, ys) = parseList(tokens, pos+1)
            pos = newpos
            xs :+= ys
          case End =>
            return (pos+1, Lst(xs.toList))
          case t =>
            pos += 1
            xs :+= t.asInstanceOf[AST]
        }
      }

      sys.error("this is imposible")
    }

    tokens match {
      case Vector(Beg, rest @ _*) =>
        val (endPos, list) = parseList(rest.toVector, 0)
        list :: _parse(tokens.drop(endPos+1))

      case Vector(End, rest @ _*) => sys.error(") given, expected anything else")

      case Vector(t, rest @ _*) =>
        t.asInstanceOf[AST] :: _parse(tokens.drop(1))

      case Vector() =>
        Nil
    }

  }


  def rewrite(asts: List[AST]): List[AST] = asts map rewrite

  def rewrite(ast: AST): AST = ast match {
    //(.method obj arg) -> (javacall method obj arg)
    case Lst(Sym(s) :: args) if s(0) == '.' =>
      Lst(Sym("javacall") :: Str(s.tail) :: args.map(rewrite))

    case Lst(Sym("defn") :: name :: args :: body) =>
      Lst(Sym("def") :: name :: Lst(Sym("fn") :: args :: body) :: Nil)

    case Lst(s :: args) => Lst(s :: args.map(rewrite))
    case ASTSeq(asts) => ASTSeq(asts.map(rewrite))
    case ast => ast
  }


  def parse(code: String): AST = ASTSeq(rewrite(_parse(tokenize(code))))


  import java.text.SimpleDateFormat
  import java.util.Date

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

    "javacall" -> Func{
      case (method: String) :: obj :: args =>
        try {
          obj.getClass.getDeclaredMethod(method, args.map(_.getClass): _*)
            .invoke(obj, args.asInstanceOf[Seq[Object]]: _*)
        } catch { case e: java.lang.NoSuchMethodException =>
          obj.getClass.getMethod(method, args.map(_.getClass): _*)
            .invoke(obj, args.asInstanceOf[Seq[Object]]: _*)
        }
    },

    "true"  -> true,
    "false" -> false,
    "null"  -> null,

    "null?"     -> Func { case a :: Nil => a == null },
    "not-null?" -> Func { case a :: Nil => a != null },

    "join"  -> Func { case args => args.mkString },
    "format-date" -> Func { case (d: Date) :: (f: String) :: Nil => new SimpleDateFormat(f).format(d) },
    "println"  -> Func { case args => args.foreach(a => print(a)); println() }
  )


  type Env = Map[String, Any]


  def eval(ast: AST, env: Env): Any = ast match {
    case asts: ASTSeq => evalMain(asts, env)
    case ast => evalExpr(ast, env)
  }

  def evalMain(asts: ASTSeq, env: Env) = {

    def e(asts: List[AST], env: Env): Any =
      asts match {
        case Lst(Sym("def") :: Sym(name) :: ast :: Nil) :: rest =>
          e(rest, env + (name -> evalExpr(ast, env)))

        case ast :: Nil =>
          evalExpr(ast, env)

        case ast :: rest =>
          evalExpr(ast, env)
          e(rest, env)

        case Nil => null
      }

    e(asts.asts, env)
  }

  def evalExpr(ast: AST, env: Env): Any = {
    ast match {
      case Num(value) => value
      case Str(value) => value
      case Lst(Sym("if") :: cond :: thn :: els :: Nil) =>
        if (truthy(evalExpr(cond, env))) evalExpr(thn, env) else evalExpr(els, env)
      case Lst(Sym("if") :: cond :: thn :: Nil) =>
        if (truthy(evalExpr(cond, env))) evalExpr(thn, env) else null
      case Lst(Sym("let") :: (bindings: Lst) :: expr :: Nil) =>
        val newEnv = bindings.values.grouped(2).foldLeft(env) { case (env, Seq(Sym(s), v)) =>
          env + (s -> evalExpr(v, env))
        }
        evalExpr(expr, newEnv)
      case Lst(Sym("fn") :: Lst(formalArgs) :: body :: Nil) =>
        Func { case args => evalExpr(body, env ++ (formalArgs.map(_.asInstanceOf[Sym].value) zip args)) }

      case Lst(Sym("and") :: args) => args.forall(a => truthy(evalExpr(a, env)))
      case Lst(Sym("or") :: args) => args.exists(a => truthy(evalExpr(a, env)))

      case Lst(Sym(s) :: values) =>
        env(s).asInstanceOf[Func](values.map(v => evalExpr(v, env)))
      case Sym(s) => env(s)

      case ASTSeq(ast :: Nil) =>
        evalExpr(ast, env)
      case ASTSeq(ast :: rest) =>
        evalExpr(ast, env)
        evalExpr(ASTSeq(rest), env)

      case _ => sys.error(ast.toString)
    }
  }


  def truthy(x: Any) = x match {
    case "" | 0 | false | null => false
    case _ => true
  }

  def number(x: Any) = x match {
    case x: Integer => x.intValue()
    case _ => sys.error(s"$x is not a number")
  }

  case class Func(f: PartialFunction[List[Any], Any]) extends (List[Any] => Any) with AST {
    def apply(args: List[Any]): Any = f(args)
  }

  def numfun2(f: (Int, Int) => Any) = Func{ case a :: b :: Nil => f(number(a), number(b)) }

}
