module Language.Modules.Utils.Namespaces
  
import Data.String
import Data.List1
import Language.Reflection

||| Appends a chosen `Name` to the supplied `Namespace`.
||| Useful when creating a new sub-namespace of the current one.
||| EXAMPLE:
||| nsCons "Modules" (MkNS (NS ["Languages"])) = MkNS (NS ["Modules", "Languages"])
export
nsCons : Name -> Namespace -> Namespace
nsCons (NS _ nm) ns = nsCons nm ns
nsCons name (MkNS ns) = MkNS $ show name :: ns

||| Gets the current `Namespace`.
-- Ideally this would be built in to the elaborator, but this hack will do for now.
export
currentNS : Elab Namespace
currentNS = assert_total $ do
  (NS ns _) <- inCurrentNS (UN $ Basic "")
  pure ns

||| Append the given `Name` to the current `Namespace`.
||| Contrast with `inCurrentNS`, which annotates a name with the namespace; this
||| constructs a new namespace entirely.
export
newNS : Name -> Elab Namespace
newNS name = pure $ nsCons name !currentNS

||| Sets the namespace of a `Name` to the supplied `Namespace`.
export
inNS : Name -> Namespace -> Name
inNS (NS ns' nm) ns = NS ns nm
inNS nm          ns = NS ns nm

||| Converts a qualified `String` (e.g. `"A.B.C"`) into a `Namespace`.
public export
stringToNS : String -> Namespace
stringToNS str = MkNS $ toList $ reverse $ split (== '.') str
