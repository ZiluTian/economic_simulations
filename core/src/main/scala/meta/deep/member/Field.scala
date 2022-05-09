package meta.deep.member

import meta.deep.IR.Predef._
import meta.deep.algo.Algo
import meta.runtime.Actor
import scala.collection.mutable.ListBuffer

/**
  * Class representation of a field for code generation
  * @param name: the symbol of the field in string 
  * @param init: the initial value of the field 
  * @param mutable: a boolean indicating whether the field is mutable 
  * @param parameter: a boolean indicating whether the field is in the parameter 
  * @param isCopiedToSubclass: a boolean
  */
case class Field (
    modifiers: ListBuffer[String],
    name: String,
    tpeRep: String, 
    init: String, 
    mutable: Boolean, 
    parameter: Boolean) {
      // add "override" to the modifier in replicated fields
      def replica(): Field = {
        Field("override" +: modifiers, name, tpeRep, init, mutable, parameter)
      }
    }