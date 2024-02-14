module Language.Modules.Signature

import Data.List1
import Language.Reflection
import Language.Reflection.Syntax

import Language.Modules.SigEntry

%language ElabReflection  
   

||| Represents a signature of a module, as its qualified name, declarations, and the names of the exported declarations.
||| Note: Use `mkSig` to construct safely!
public export
record Signature where
  constructor MkSig
  name : Name
  exports : List String
  vals : List SigEntry

||| Safely construct a `Signature`.
export
mkSig : Name -> List String -> List SigEntry -> Elab Signature
mkSig name exports vals = pure $ MkSig !(inCurrentNS name) exports vals
  -- TODO get exports

||| Generate a `Signature` given a name for it, and a list of names for its entries.
public export
genSig : Name -> List Name -> Elab Signature
genSig name defs = do
  entries <- traverse (mkEntry !(newNS name)) defs

  let names = map (.name) entries
  when (names /= nub names)
    (fail "Reused name(s) in signature! \{show $ names \\ nub names}")

  mkSig name [] entries

||| Loads a signature into the current namespace.
export
loadSig : Signature -> Elab ()
loadSig (MkSig (NS ns name) exports vals) =
  declare $ [INamespace EmptyFC ns $ concat !(traverse sigToDecl vals)]

  where sigToDecl : SigEntry -> Elab $ List Decl
        sigToDecl (MkEntry visibility name type val) =
          pure $ [ simpleClaim Public {-TODO-}  name !(quote type)
                 --, def name [PatClause EmptyFC ?tt1 ?tt2] ]  -- TODO sort out defs
                 ]

loadSig _ = fail "Improperly constructed Namespace! This is probably a bug in the module system."

||| EXPERIMENTAL: Reads a `Signature` from a `namespace` declaration.
export
readSig : List Decl -> Elab (Signature)
readSig [INamespace fc ns decls] = mkSig "test" [] []
readSig _ = fail "readSig wasn't given a single Namespace!"

||| Adds a new entry to a `Signature`, given its name.
export
addToSig : Name -> Signature -> Elab Signature
addToSig name (MkSig nm exports vals) =
  -- TODO add to exports
  pure $ MkSig nm exports (!(mkEntry !(newNS name) name) :: vals)

-- TODO: hide as well/instead? (just change vis to private)
||| Removes an entry from a `Signature`, given its name.
export
delFromSig : Name -> Signature -> Elab Signature
delFromSig name (MkSig nm exports vals) =
  let vals'    = filter (\(MkEntry _ n _ _) => name /= dropNS n) vals
      exports' = exports  -- TODO
  in mkSig nm exports' vals'

||| Renames a single entry in a `Signature`.
export
renameInSig : Name -> Name -> Signature -> Elab Signature
renameInSig name name' (MkSig nm exports vals) =
  let exports' = exports  -- TODO
      vals' = map (\(MkEntry e n t v) =>
                     MkEntry e (if dropNS n == name then name' else name) t v)
                  vals
  in mkSig nm exports' vals'

-- TODO update namespaces
||| Renames the `Signature` itself, updating the namespace of all entries.
export
renameSig : Name -> Signature -> Elab Signature
renameSig name (MkSig nm exports vals) = mkSig name exports (map (updateEntryNS !(newNS name)) vals)

||| Combines two `Signatures`, potentially over some shared data (given by the names).
export
combineSigs : Name -> Signature -> Signature -> List Name -> Elab Signature
combineSigs name (MkSig n1 e1 v1) (MkSig n2 e2 v2) names =
  let ns = !(newNS name)
      v1' = map (updateEntryNS ns) v1
      v2' = map (updateEntryNS ns) v2
      exports = union e1 e2  -- TODO: check shared data is exported?
      vals    = unionBy (\s1,s2 => s1.name == s2.name) v1' v2'
  in mkSig name exports vals
