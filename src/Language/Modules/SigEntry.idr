module Language.Modules.SigEntry

import Data.List
import Data.List1
import Language.Reflection
import Language.Reflection.Syntax

%language ElabReflection

||| Represents a single declaration in a signature.
||| Note: Use `mkEntry` to construct safely!
public export
record SigEntry where
  constructor MkEntry
  visibility : Visibility
  name : Name
  type : Type  -- revisit for erasure?
  val : type

||| Adds a chosen name to the supplied namespace.
||| Useful when creating a new sub-namespace of the current one.
nsCons : Name -> Namespace -> Namespace
nsCons name (MkNS ns) = MkNS $ show name :: ns

||| Gets the current namespace.
-- Ideally this would be built in to the elaborator, but this hack will do for now.
currentNS : Elab Namespace
currentNS = assert_total $ do
  (NS ns _) <- inCurrentNS ""
  pure ns

||| Generate a namespace inside the given name
export
newNS : Name -> Elab Namespace
newNS name = pure $ nsCons name !currentNS

||| Sets the namespace of a name to the given namespace.
inNS : Name -> Namespace -> Name
inNS (NS ns' nm)       ns = NS ns $ inNS nm ns
inNS (UN un)           ns = UN un
inNS (DN str nm)       ns = DN str $ inNS nm ns
inNS (Nested x nm)     ns = Nested x $ inNS nm ns
inNS x                 _  = x

||| Updates the namespace of the entry to the supplied one.
export
updateEntryNS : Namespace -> SigEntry -> SigEntry
updateEntryNS ns (MkEntry vis nm ty val) = MkEntry vis (inNS nm ns) ty val

||| Safe constructor for a `SigEntry`, given the name of an already existing definition.
export
mkEntry : Namespace -> Name -> Elab SigEntry
mkEntry ns name = do
  tys <- getType name  -- TODO don't just get the first match!
  case tys of
    []            => fail "I don't know what \"\{name}\" is!"
    [(nm, ttimp)] => pure $ MkEntry Public
                                    (NS ns name)
                                    !(check ttimp)
                                    !(check $ IVar EmptyFC nm)
    _ => let
      qns = map (show . fst) tys
      in fail """
           \"\{name}\" isn't a unique name! Try qualifying it.
           Conflicting names: \{show qns}
           """
