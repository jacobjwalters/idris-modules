module Language.Modules.Module

import Data.List1
import Data.String
import Language.Reflection
--import Language.Reflection.Syntax

import public Language.Modules.Utils.Namespaces
import Language.Modules.Declaration

%language ElabReflection
   

||| Represents a module, as a record of its qualified name, declarations, and the names of the exported declarations.
||| Note: Use `MkMod` to construct safely!
public export
record Module where
  constructor MkMod
  vals : List Declaration
  
||| Pretty print a `Module`.
export
ppMod : Module -> String
ppMod (MkMod vals) = "module" ++ concatMap (("\n  " ++) . ppDecl) vals
  
||| Get the list of all names from a `Module`.
export
names : Module -> List Name
names m = name <$> m.vals
(.names) = names

||| Get the list of exported names from a `Module`.
export
exports : Module -> List Name
exports m = name <$> filter (\v => v.vis /= Private) m.vals
(.exports) = exports
  
||| Get the list of `public export` names from a `Module`.
export
publicExports : Module -> List Name
publicExports m = name <$> filter (\v => v.vis == Public) m.vals
(.publicExports) = publicExports


||| Generate a `Module` given a name for it, and a list of `Name`s of Idris declarations to include.
public export
genMod : List Name -> Elab Module
genMod defNames = do
  when (defNames /= nub defNames)
    (fail "Reused name(s) in module! \{show $ defNames \\ nub defNames}")

  MkMod <$> traverse genDecl defNames

||| Reifies a `Module` into the supplied namespace (written as a qualified name "A.B.Name").
export
reifyMod : Namespace -> Module -> Elab ()
reifyMod ns (MkMod vals) =
  declare [INamespace EmptyFC ns $ concat !(traverse (reifyDecl ns) vals)]
  where mkTy = MkTy EmptyFC EmptyFC
        reifyDecl : Namespace -> Declaration -> Elab $ List TTImp.Decl
        reifyDecl ns d = pure
          [ IClaim EmptyFC MW d.vis [] (mkTy (inNS d.name ns) !(quote d.type))
          --, reify value would go here
          ]
(.reify) = flip reifyMod

||| Adds an Idris declaration to a `Module`, given its name.
export
addToMod : Declaration -> Module -> Module
addToMod decl (MkMod vals) = MkMod (decl :: vals)
(.add) = flip addToMod

||| Removes a declaration from a `Module`, given its name.
export
delFromMod : Name -> Module -> Module
delFromMod name (MkMod vals) =
  MkMod $ filter (\d => dropNS d.name /= name) vals
(.del) = flip delFromMod
  
||| Hides a name in a `Module` (by setting its `vis` to `Private`).
export
hideInMod : Name -> Module -> Module
hideInMod n (MkMod vals) =
  MkMod $ map (\v => if v.name == n
                        then {vis := Private} v
                        else v)
               vals
(.hide) = flip hideInMod


||| Renames a single declaration in a `Module`.
export
renameInMod : (Name, Name) -> Module -> Module
renameInMod (name, name') (MkMod vals) =
  MkMod $ map (\d => {name := if dropNS d.name == name
                                    then name'
                                    else d.name} d)
                  vals
(.rename) = flip renameInMod

||| Combines two `Modules`, potentially over some shared data (given by the names).
export
combineMods : Module -> Module -> List Name -> Module
combineMods m1 m2 shared = case (all (\n => (n `elem` m1.names) && (n `elem` m2.names)) shared) of
  True => MkMod $ unionBy (\s1,s2 => (s1.name == s2.name))
                          m1.vals m2.vals
  False => let nonShared = filter (\n => not ((n `elem` m1.names)
                                           && (n `elem` m2.names)))
                                  shared
           in assert_total $ idris_crash "combineMods: These names must be in both modules: `{show nonShared}"

||| Combines two `Modules`, over a shared module.
export
combineOver : Module -> Module -> Module -> Module
combineOver shared m1 m2 = combineMods m1 m2 shared.names
(.pushout) = combineOver
