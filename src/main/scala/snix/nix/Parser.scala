package snix.nix

import snix.nix.Types._
import scala.util.parsing.combinator.RegexParsers

object Parser extends RegexParsers {

  def parseNixString(nixExpr: String): ParseResult[NExpr] = sys.error("TODO: Parser")
}
