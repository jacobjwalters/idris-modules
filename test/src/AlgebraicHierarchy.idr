module AlgebraicHierarchy

import Language.Modules

%language ElabReflection
%unbound_implicits off

export Set : Type
export Op : Set -> Set -> Set
export Lunit : Set
export Runit : Set
export Linv : Set -> Set
export Rinv : Set -> Set

export lunitality : {a : Set} -> Op Lunit a = a
export runitality : {a : Set} -> Op a Runit = a
export associativity : {a, b, c : Set} -> Op a (Op b c) = Op (Op a b) c
export linverse : {a : Set} -> Op (Linv a) a = Lunit
export rinverse : {a : Set} -> Op  a (Rinv a) = Runit

||| Helper functions for adding unitalities
addLunit, addRunit : Module -> Module
addLunit m = foldr addToMod m
  [ %runElab genDecl `{lunitality}
  , %runElab genDecl `{Lunit} ]
addRunit m = foldr addToMod m
  [ %runElab genDecl `{runitality}
  , %runElab genDecl `{Runit} ]

||| Helper functions for adding inverses
addLinv, addRinv : Module -> Module
addLinv m = foldr addToMod m
  [ %runElab genDecl `{linverse}
  , %runElab genDecl `{Linv} ]
addRinv m = foldr addToMod m
  [ %runElab genDecl `{rinverse}
  , %runElab genDecl `{Rinv} ]

export
set : Module
set = %runElab genMod [`{Set}]
  
||| A pointed set
export
pointedSet : Module
pointedSet = set.add $ %runElab genDecl `{Lunit}

||| A magma. Closure is enforced by the totality checker.
export
magma : Module
magma = set.add $ %runElab genDecl `{Op}
  
||| Unital magmas
export
leftUnitalMagma, rightUnitalMagma, unitalMagma : Module
leftUnitalMagma  = addLunit magma
rightUnitalMagma = addRunit magma
unitalMagma = magma.pushout leftUnitalMagma rightUnitalMagma
            
||| Quasigroups
export
leftQuasigroup, rightQuasigroup, quasigroup : Module
leftQuasigroup  = addLinv magma
rightQuasigroup = addRinv magma
quasigroup = magma.pushout leftQuasigroup rightQuasigroup

||| Semigroups
export
semigroup : Module
semigroup = magma.add $ %runElab genDecl `{associativity}

||| (Unital) semigroups and monoids
export
leftUnitalSemigroup, rightUnitalSemigroup, monoid : Module
leftUnitalSemigroup  = addLunit semigroup
rightUnitalSemigroup = addRunit semigroup
monoid = semigroup.pushout leftUnitalSemigroup rightUnitalSemigroup

||| Associative quasigroups
export
associativeQuasigroup : Module
associativeQuasigroup = magma.pushout quasigroup semigroup

||| Loops
export
leftUnitalLoop, rightUnitalLoop, loop : Module
leftUnitalLoop  = magma.pushout quasigroup leftUnitalMagma
rightUnitalLoop = magma.pushout quasigroup rightUnitalMagma
loop            = magma.pushout quasigroup unitalMagma

||| Groups!
export
group : Module
group = semigroup.pushout associativeQuasigroup monoid
  
||| Additive groups
export
additiveGroup : Module
additiveGroup = hideInMod `{Runit} $ hideInMod `{Rinv} $
  foldr renameInMod group
    [ (`{Op},    `{(+)})
    , (`{Lunit}, `{Zero})
    , (`{Linv},  `{(-)})
    ]
  
||| Additive monoids
additiveMonoid : Module
additiveMonoid = foldr delFromMod additiveGroup
  [`{Rinv}, `{(-)}, `{linverse}, `{rinverse}]

-- Reification
%runElab set.reify (MkNS ["Set", "Algebra"])
%runElab pointedSet.reify (MkNS ["Pointed", "Set", "Algebra"])
%runElab magma.reify (MkNS ["Magma", "Algebra"])
%runElab leftUnitalMagma.reify (MkNS ["Left", "Unital", "Magma", "Algebra"])
%runElab rightUnitalMagma.reify (MkNS ["Right", "Unital", "Magma", "Algebra"])
%runElab unitalMagma.reify (MkNS ["Unital", "Magma", "Algebra"])
%runElab leftQuasigroup.reify (MkNS ["Left", "Quasigroup", "Algebra"])
%runElab rightQuasigroup.reify (MkNS ["Right", "Quasigroup", "Algebra"])
%runElab quasigroup.reify (MkNS ["Quasigroup", "Algebra"])
%runElab associativeQuasigroup.reify
  (MkNS ["Associative", "Quasigroup", "Algebra"])
%runElab semigroup.reify (MkNS ["Semigroup", "Algebra"])
%runElab leftUnitalSemigroup.reify
  (MkNS ["Left", "Unital", "Semigroup", "Algebra"])
%runElab rightUnitalSemigroup.reify
  (MkNS ["Right", "Unital", "Semigroup", "Algebra"])
%runElab monoid.reify (MkNS ["Monoid", "Algebra"])
%runElab leftUnitalLoop.reify (MkNS ["Left", "Unital", "Loop", "Algebra"])
%runElab rightUnitalLoop.reify (MkNS ["Right", "Unital", "Loop", "Algebra"])
%runElab loop.reify (MkNS ["Loop", "Algebra"])
%runElab group.reify (MkNS ["Group", "Algebra"])
%runElab additiveGroup.reify (MkNS ["Additive", "Group", "Algebra"])
%runElab additiveMonoid.reify (MkNS ["Additive", "Monoid", "Algebra"])
