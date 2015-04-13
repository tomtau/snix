package snix.nix

import snix.nix.Types._

object Pretty extends org.kiama.output.PrettyPrinter {

  type NixDoc = Doc

  def prettyAtom(atom: NAtom): NixDoc = atom match {
    case NInt(i)    => value(i)
    case NBool(b)    => if (b) "true" else "false"
    case NNull         => "null"
    case NPath(s, p) => if (s) "<" ++ p.toString ++ ">" else p.toString
  }

  def prettyString(str: NString[NixDoc]): NixDoc = {
    val escapeCodes = List(('\n', "\\n" ), ('\r', "\\r" ), ('\t', "\\t" ), ('\\', "\\\\"), ('$' , "\\$" ), ('\"', "\\\\\"")).toMap

    def escape(x: Char): String = escapeCodes.getOrElse(x, x.toString)


    str match {
      case NUri(t) => t
      case _ => sys.error("TODO: prettyString")
    }

  }

  def prettyBind(b: Binding[NixDoc]): NixDoc = sys.error("TODO: prettyBind")

  def prettyFormals(f: Formals[NixDoc]): NixDoc = f match {
    case FormalName(n) => n
    case FormalSet(s) => prettyParamSet(s)
    case FormalLeftAt(n, s) => n <> "@" <> prettyParamSet(s)
    case FormalRightAt(s, n) => prettyParamSet(s) <> "@" <> n
  }

  def prettyParamSet(ps: FormalParamSet[NixDoc]): NixDoc = lbrace <+> hcat(ps.toList.map(prettySetArg)) <+> rbrace


  def prettySetArg(x: (String, Option[NixDoc])): NixDoc = {
    val (n, s) = x

    val f: NixDoc = s match {
      case None => n
      case Some(v) => n <+> "?" <+> withoutParens(v)
    }
    // TODO: not the last one (in map in prettyParamSet)
    (comma <> space) <> f
  }

  def prettyOper(op: NOperF[NixDoc]): NixDoc = sys.error("TODO: prettyOper")

  def prettySelector(args: NSelector[NixDoc]): NixDoc = sys.error("TODO: prettySelector")

  def withoutParens(d: NixDoc): NixDoc = parens(d) // TODO: precedence

  def prettyNix (t : NExpr) : NixDoc = {
    def show(x: NExprF[NixDoc]): NixDoc = x match {
      case NConstant(atom) => prettyAtom(atom)
      case NStr(s) => prettyString(s)
      case NList(xs) => {
        group(nest(vsep(lbrace :: xs.map(withoutParens).toList), 2) <@> rbrace)
      }
      case NSet(rec, xs) => {
        def prefix(p: NSetBind): NixDoc = p match {
          case Rec => "rec" <> space
          case NonRec => empty
        }
        group(nest(vsep((prefix(rec) <> lbrace) :: xs.map(prettyBind).toList),2) <@> rbrace)
      }
      case NAbs(args, body) => (prettyFormals(args) <> colon) </> withoutParens(body)
      case NOper(oper) => prettyOper(oper)
      case NSelect(r, attr, o) => { // TODO: precedence
        val ordoc = o match {
          case None => empty
          case Some(s) => (space <> "or") <+> withoutParens(s)
        }
        r <> dot <> prettySelector(attr) <> ordoc
      }
      case NHasAttr(r, attr) => withoutParens(r) <+> "?" <+> prettySelector(attr)
      case NApp(fun, arg) => withoutParens(fun) <+> withoutParens(arg)
      case NSym(name) => name
      case NLet(binds, body) => group(nest(vsep("let" :: binds.map(prettyBind).toList) <@> "in" <+> withoutParens(body),2))
      case NIf(conds, trueBody, falseBody) => {
        val l1 = ("if" <+> withoutParens(conds)) <@> align ("then" <+> withoutParens(trueBody))
        group(nest(l1 <@> align("else" <+> withoutParens(falseBody)),2))
      }
      case NWith(scope, body) => "with" <+> withoutParens(scope) <> semi <+> withoutParens(body)
      case NAssert(cond, body) => "assert" <+> withoutParens(cond) <> semi <+> withoutParens(body)
    }

    cata(show)(t)
  }

}