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
  type : Type  -- revisit for erasure?
  val : type
  
||| Pretty print a `Declaration`.
export
ppDecl : Declaration -> String
ppDecl (MkDecl vis name type val) = "\{show vis} \{show name}"  -- TODO: show type?

||| Updates the namespace of the `Declaration` to the supplied one.
export
updateDeclNS : Namespace -> Declaration -> Declaration
updateDeclNS ns (MkDecl vis nm ty val) = MkDecl vis (inNS nm ns) ty val
  
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
mkDecl : Namespace -> Name -> Elab Declaration
mkDecl ns name = do
  tys <- getType name  -- TODO don't just get the first match!
  case tys of
    []            => fail "I don't know what \"\{show name}\" is!"
    [(nm, ttimp)] => pure $ MkDecl !(getVisibility name)
                                   (inNS name ns)
                                   !(check ttimp)
                                   !(check $ IVar EmptyFC nm)
    _ => let
      qns = map (show . fst) tys
      in fail """
           \"\{show name}\" isn't a unique name! Try qualifying it.
           Conflicting names: \{show qns}
           """

