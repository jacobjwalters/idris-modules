module Language.Modules.Declaration

import Data.List
import Data.List1
import Language.Reflection

import Language.Modules.Utils.Namespaces

%language ElabReflection

||| Represents a single declaration in a module.
||| Note: Use `mkDecl` to construct safely!
public export
record Declaration where
  constructor MkDecl
  vis : Visibility
  name : Name
  type : Type
  val : type
 
export
Eq Declaration where
  d1 == d2 = d1.name == d2.name
export
Ord Declaration where
  compare d1 d2 = compare d1.name d2.name
  
||| Pretty print a `Declaration`.
export
ppDecl : Declaration -> String
ppDecl (MkDecl vis name type val) = "\{show vis} \{show name}"

||| Gets the `Visibility` (export modifier) of a `Name`.
getVisibility : Name -> Elab Visibility
getVisibility name = case !(getVis name) of
  []      => fail "I don't know what \"\{show name}\" is!"
  [(n,v)] => pure v
  ns      => fail """
               \"\{show name}\" isn't a unique name! Try qualifying it.
               Conflicting names: \{show ns}
               """

||| Safe constructor for a `Declaration`, given the name of an already existing Idris declaration.
export
genDecl : Name -> Elab Declaration
genDecl name = do
  tys <- getType name
  case tys of
    []            => fail "I don't know what \"\{show name}\" is!"
    [(nm, ttimp)] => pure $ MkDecl !(getVisibility name)
                                   (dropNS name)
                                   !(check ttimp)
                                   !(check $ IVar EmptyFC nm)
    _ => let
      qns = map (show . fst) tys
      in fail """
           \"\{show name}\" isn't a unique name! Try qualifying it.
           Conflicting names: \{show qns}
           """

