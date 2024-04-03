module Main
  
import Language.Modules
--import Language.Modules.Utils.Namespaces

import AlgebraicHierarchy
  
%language ElabReflection

private       fooP  : Int
export        fooE  : Int
public export fooPE : Int
visTest = %runElab genMod [`{fooP}, `{fooE}, `{fooPE}]

{-
%runElab set.reify (MkNS ["Set", "Algebra"])
%runElab magma.reify (MkNS ["Magma", "Algebra"])
%runElab leftUnitalMagma.reify (MkNS ["Left", "Unital", "Magma", "Algebra"])
%runElab rightUnitalMagma.reify (MkNS ["Right", "Unital", "Magma", "Algebra"])
%runElab unitalMagma.reify (MkNS ["Unital", "Magma", "Algebra"])
%runElab leftQuasigroup.reify (MkNS ["Left", "Quasigroup", "Algebra"])
%runElab rightQuasigroup.reify (MkNS ["Right", "Quasigroup", "Algebra"])
%runElab quasigroup.reify (MkNS ["Quasigroup", "Algebra"])
%runElab associativeQuasigroup.reify (MkNS ["Associative", "Quasigroup", "Algebra"])
%runElab semigroup.reify (MkNS ["Semigroup", "Algebra"])
%runElab leftUnitalSemigroup.reify (MkNS ["Left", "Unital", "Semigroup", "Algebra"])
%runElab rightUnitalSemigroup.reify (MkNS ["Right", "Unital", "Semigroup", "Algebra"])
%runElab monoid.reify (MkNS ["Monoid", "Algebra"])
%runElab leftUnitalLoop.reify (MkNS ["Left", "Unital", "Loop", "Algebra"])
%runElab rightUnitalLoop.reify (MkNS ["Right", "Unital", "Loop", "Algebra"])
%runElab loop.reify (MkNS ["Loop", "Algebra"])
%runElab group.reify (MkNS ["Group", "Algebra"])
-}
  
linEven : (1 x : Nat) -> Bool
linEven 0 = True
linEven (S 0) = False
linEven (S (S k)) = linEven k
  
%runElab reifyMod (MkNS ["Even", "Linear"]) (%runElab genMod [`{linEven}])
  
main : IO ()
main = putStrLn "Elab scripts worked!"
