package razie.actionables

import java.util.List
import razie.base.AttrAccess

/** just to have HasActionable as a Trait - cannot use scala code from java code */
trait THasActionables extends HasActionables {
  type fun = (AttrAccess) => AttrAccess
}
