module Language.Modules

import Data.List1
import Language.Reflection
import Language.Reflection.Syntax

import public Language.Modules.SigEntry
import public Language.Modules.Signature

  
%language ElabReflection
Set : Type
unit : Set
append : Set -> Set -> Set
neg : Set -> Set

mon : Signature
mon = %runElab genSig "Mon" ["Set", "unit", "append"]

negs : Signature
negs = %runElab genSig "Negs" ["Set", "neg"]

additiveMonoid = %runElab renameInSig "unit" "0" $ %runElab renameInSig "append" "+" mon
semigroup = %runElab delFromSig "unit" mon
group = %runElab combineSigs "Group" mon negs ["Set"]
  
%runElab loadSig group

mon' = `[
namespace Mon
  private
  Set : Type
  unit : Set
  append : Set -> Set -> Set]

