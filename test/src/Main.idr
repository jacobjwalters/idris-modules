module Main
  
import Language.Modules
  
%language ElabReflection

namespace Defs
  export Set : Type
  export unit : Set
  export append : Set -> Set -> Set
  export neg : Set -> Set

mon : Module
mon = %runElab genMod `{Mon} [`{Set}, `{unit}, `{append}]

negs : Module
negs = %runElab genMod `{Negs} [`{Set}, `{neg}]

additiveMonoid = %runElab renameMod `{AdditiveMonoid}
               $ %runElab renameInMod `{unit} `{zero}
               $ %runElab renameInMod `{append} `{(+)} mon

semigroup = %runElab delFromMod `{unit} mon

group = %runElab combineMods `{Group} mon negs [`{Set}]
  
%runElab reifyMod group
  
-- TODO check out FromElab

mon' = `[
namespace Mon
  private
  Set : Type
  unit : Set
  append : Set -> Set -> Set]
  
private       fooP  : Int
export        fooE  : Int
public export fooPE : Int
  
visTest = %runElab genMod `{VisTest} [`{fooP}, `{fooE}, `{fooPE}]
  
bar : Int -> Int
bar a = 1 + a


main : IO ()
main = putStrLn "Elab scripts worked!"
