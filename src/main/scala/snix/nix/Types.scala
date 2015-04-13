package snix.nix

/*
Nix AST converted from: https://github.com/jwiegley/hnix/blob/master/Nix/Types.hs
 */
object Types {
  sealed trait FilePath {
    def toString: String
  }

  private final case class FilePath_(path: String) extends FilePath {
    override def toString = path
  }

  // Atoms are values that evaluate to themselves. This means that they appear in both
  // the parsed AST (in the form of literals) and the evaluated form.
  sealed trait NAtom
  final case class NInt(i: Long) extends NAtom

  // The first argument of 'NPath' is 'True' if the path must be looked up in the Nix
  // search path.
  // For example, @<nixpkgs/pkgs>@ is represented by @NPath True "nixpkgs/pkgs"@,
  // while @foo/bar@ is represented by @NPath False "foo/bar@.
  final case class NPath(b: Boolean, fp: FilePath) extends NAtom
  final case class NBool(b: Boolean) extends NAtom
  final case object NNull extends NAtom

  // 'Antiquoted' represents an expression that is either
  // antiquoted (surrounded by ${...}) or plain (not antiquoted).
  sealed trait Antiquoted[+V,+R]
  final case class Plain[V,R](v: V) extends Antiquoted[V,R]
  final case class AAntiquoted[V,R](r: R) extends Antiquoted[V,R]

  sealed trait StringKind
  final case object DoubleQuoted extends StringKind
  final case object Indented extends StringKind

  // A 'NixString' is a list of things that are either a plain string
  // or an antiquoted expression. After the antiquotes have been evaluated,
  // the final string is constructed by concating all the part
  sealed trait NString[+T]
  final case class NNString[T](k: StringKind, tl: Seq[Antiquoted[String,T]]) extends NString[T]
  final case class NUri[T](t: String) extends NString[T]

  sealed trait NKeyName[+T]
  final case class DynamicKey[T](k: Antiquoted[NString[T],T]) extends NKeyName[T]
  final case class StaticKey[T](t: String) extends NKeyName[T]

  type NSelector[T] = Seq[NKeyName[T]]

  sealed trait NSetBind
  final case object Rec extends NSetBind
  final case object NonRec extends NSetBind

  // A single line of the bindings section of a let expression or of
  // a set.
  sealed trait Binding[+T]
  final case class NamedVar[T](s: NSelector[T], v: T) extends Binding[T]
  final case class Inherit[T](o: Option[T], ss: Seq[NSelector[T]]) extends Binding[T]

  type FormalParamSet[T] = Map[String, Option[T]]

  // @Formals@ represents all the ways the formal parameters to a
  // function can be represented.
  sealed trait Formals[+T]
  final case class FormalName[T](n: String) extends Formals[T]
  final case class FormalSet[T](s: FormalParamSet[T]) extends Formals[T]
  final case class FormalLeftAt[T](l: String, s: FormalParamSet[T]) extends Formals[T]
  final case class FormalRightAt[T](s: FormalParamSet[T], r: String) extends Formals[T]

  sealed trait NOperF[+T]
  final case class NUnary[T](op: NUnaryOp, v: T) extends NOperF[T]
  final case class NBinary[T](op: NBinaryOp, l: T, r: T) extends NOperF[T]

  sealed trait NUnaryOp
  final case object NNeg extends NUnaryOp
  final case object NNot extends NUnaryOp

  sealed trait NBinaryOp
  final case object NEq extends NBinaryOp
  final case object NNEq extends NBinaryOp
  final case object NLt extends NBinaryOp
  final case object NLte extends NBinaryOp
  final case object NGt extends NBinaryOp
  final case object NGte extends NBinaryOp
  final case object NAnd extends NBinaryOp
  final case object NOr extends NBinaryOp
  final case object NImpl extends NBinaryOp
  final case object NUpdate extends NBinaryOp
  final case object NPlus extends NBinaryOp
  final case object NMinus extends NBinaryOp
  final case object NMult extends NBinaryOp
  final case object NDiv extends NBinaryOp
  final case object NConcat extends NBinaryOp

  sealed trait NExprF[+T]
  // value types
  final case class NConstant[T](atom: NAtom) extends NExprF[T]
  final case class NStr[T](str: NString[T]) extends NExprF[T]
  final case class NList[T](l: Seq[T]) extends NExprF[T]
  final case class NSet[T](sb: NSetBind, binds: Seq[Binding[T]]) extends NExprF[T]
  final case class NAbs[T](f: Formals[T], v: T) extends NExprF[T]

  // operators
  final case class NOper[T](o: NOperF[T]) extends NExprF[T]
  final case class NSelect[T](v: T, s: NSelector[T], o: Option[T]) extends NExprF[T]
  final case class NHasAttr[T](v: T, s: NSelector[T]) extends NExprF[T]
  final case class NApp[T](l: T, r: T) extends NExprF[T]

  // language constructs
  // A 'NSym' is a reference to a variable. For example, @f@ is represented as
  // @NSym "f"@ and @a@ as @NSym "a" in @f a@.
  final case class NSym[T](t: String) extends NExprF[T]
  final case class NLet[T](b: Seq[Binding[T]], e: T) extends NExprF[T]
  final case class NIf[T](e1: T, e2: T, e3: T) extends NExprF[T]
  final case class NWith[T](e1: T, e2: T) extends NExprF[T]
  final case class NAssert[T](e1: T, e2: T) extends NExprF[T]

  // TODO: bring in Scalaz?
  final case class Mu[F[_]](out: F[Mu[F]])

  trait Functor[T[_]]{
    def fmap[A,B](ta:T[A], f:A=>B):T[B]
  }

  def cata[A, F[_]](f: F[A] => A)(t: Mu[F])(implicit fc: Functor[F]): A = {
    f(fc.fmap(t.out, cata[A, F](f)))
  }

  type NExpr = Mu[NExprF]

  def mkInt(x: Long): NExpr = Mu[NExprF](NConstant(NInt(x)))

  def mkStr(k: StringKind, x: String) = Mu[NExprF](NStr(NNString(k,if (x.isEmpty) List.empty else List(Plain(x)))))

  def mkUri(x: String): NExpr = Mu[NExprF](NStr(NUri(x)))

  def mkPath(b: Boolean, fp: String): NExpr = Mu[NExprF](NConstant(NPath(b, FilePath_(fp))))

  def mkSym(x: String): NExpr = Mu[NExprF](NSym(x))

  def mkSelector(x: String): NSelector[NExpr] = List(StaticKey(x))

  def mkBool(b: Boolean): NExpr = Mu[NExprF](NConstant(NBool(b)))

  def mkNull = Mu[NExprF](NConstant(NNull))

  def mkOper(op: NUnaryOp, e: NExpr): NExpr = Mu[NExprF](NOper(NUnary(op, e)))

  def mkOper2(op: NBinaryOp, e1: NExpr, e2: NExpr): NExpr = Mu[NExprF](NOper(NBinary(op, e1, e2)))

}
