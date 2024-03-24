module Language.Modules.Module

import Data.List1
import Data.String
import Language.Reflection
--import Language.Reflection.Syntax

import public Language.Modules.Utils.Namespaces
import Language.Modules.Declaration

%language ElabReflection  
   

||| Represents a module, as a record of its qualified name, declarations, and the names of the exported declarations.
||| Note: Use `mkMod` to construct safely!
public export
record Module where
  constructor MkMod
  name : Name
  vals : List Declaration
  
||| Pretty print a `Module`.
export
ppMod : Module -> String
ppMod (MkMod name vals) =
  "module " ++ (show name)
    ++ concatMap (("\n  " ++) . ppDecl) vals
  
||| Safely construct a `Module`, given a list of `Declarations` to include.
export
mkMod : Name -> List Declaration -> Elab Module
mkMod n vals = pure $ MkMod !(inCurrentNS n) vals
  
||| Get the list of exported names from a `Module`.
export
exports : Module -> List Name
exports m = name <$> filter (\v => v.vis /= Private) m.vals

||| Generate a `Module` given a name for it, and a list of `Name`s of Idris declarations to include.
public export
genMod : Name -> List Name -> Elab Module
genMod name defs = do
  decls <- traverse (mkDecl !(newNS name)) defs

  let names = map (.name) decls
  when (names /= nub names)
    (fail "Reused name(s) in module! \{show $ names \\ nub names}")

  mkMod name decls

||| Reifies a `Module` into the current namespace.
export
reifyMod : Module -> Elab ()
reifyMod (MkMod (NS ns name) vals) =
  declare [INamespace EmptyFC ns $ concat !(traverse reifyDecl vals)]

  where mkTy = MkTy EmptyFC EmptyFC

        reifyDecl : Declaration -> Elab $ List TTImp.Decl
        reifyDecl d = pure
          [ IClaim EmptyFC MW d.vis [] (mkTy d.name !(quote d.type))
          --, !(quote d.type)
          ]

reifyMod _ = fail "Improperly constructed Namespace! Please ensure you're using the smart constructors."

||| Reifies a `Module` into the supplied namespace (written as a qualified name "A.B.Name").
export
reifyMod' : Namespace -> Module -> Elab ()
reifyMod' ns (MkMod _ vals) =
  declare [INamespace EmptyFC ns $ concat !(traverse (reifyDecl ns) vals)]
  where mkTy = MkTy EmptyFC EmptyFC
        reifyDecl : Namespace -> Declaration -> Elab $ List TTImp.Decl
        reifyDecl ns d = pure
          [ IClaim EmptyFC MW d.vis [] (mkTy (inNS d.name ns) !(quote d.type))
          --, reify value
          ]

||| Adds an Idris declaration to a `Module`, given its name.
export
addToMod : Name -> Module -> Elab Module
addToMod name (MkMod nm vals) =
  mkMod nm (!(mkDecl !(newNS name) name) :: vals)

||| Removes a declaration from a `Module`, given its name.
export
delFromMod : Name -> Module -> Elab Module
delFromMod name (MkMod nm vals) =
  let vals' = filter (\d => dropNS d.name /= name) vals
  in mkMod nm vals'
  
||| Hides a name in a `Module` (by setting its `vis` to `Private`).
export
hideNameInMod : Name -> Module -> Elab Module
hideNameInMod n (MkMod name vals) =
  let e' = filter (/= show n) []
      v' = map (\v => if v.name == n
                        then {vis := Private} v
                        else v)
               vals
  in mkMod name v'
  
||| Hide multiple names in a `Module`.
export
hideNamesInMod : List Name -> Module -> Elab Module
hideNamesInMod ns (MkMod name vals) =
  let e' = filter (not . (`elem` (map show ns))) []
      v' = map (\v => if v.name `elem` ns
                        then {vis := Private} v
                        else v)
               vals
  in mkMod name v'

||| Renames a single declaration in a `Module`.
export
renameInMod : Name -> Name -> Module -> Elab Module
renameInMod name name' (MkMod nm vals) =
  let vals' = map (\d => {name := if dropNS d.name == name
                                    then name'
                                    else d.name} d)
                  vals
  in mkMod nm vals'

||| Renames the `Module` itself, updating the namespace of all declarations.
export
renameMod : Name -> Module -> Elab Module
renameMod name (MkMod nm vals) = mkMod name (map (updateDeclNS !(newNS name)) vals)

||| Combines two `Modules`, potentially over some shared data (given by the names).
export
combineMods : Name -> Module -> Module -> List Name -> Elab Module
combineMods name (MkMod n1 v1) (MkMod n2 v2) names =
  let ns   = !(newNS name)
      v1'  = map (updateDeclNS ns) v1
      v2'  = map (updateDeclNS ns) v2
      vals = unionBy (\s1,s2 => s1.name == s2.name) v1' v2'
  in mkMod name vals
