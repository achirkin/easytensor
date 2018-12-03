
At the current state the program compiles, runs, and selects proper
instances for DataFrames at compile and run time.
The plugin succesfully fullfilled its two tasks:

  * Unified deriving of an instance for a newtype over the closed type family
    using the KnownBackend instance (type checker plugin)

  * Deriving all visible specific instances for a newtype over the closed type family

Although it compiles and runs, coercions are not totally correct,
compiling with `-dcore-lint` fails.

TODO:

  [ ] Fix coercion in InferBackendInstance
  [ ] Add cast in the instance binds
  [ ] Think whether I should auto-gemerate KnownBackend instance
      or do it manually.
      I guess, currently I have both which will turn out as duplicate instances.
  [ ] Cleanup the code
  [ ] Analyze plugin running performance
  [ ] Merge into easytensor

Some Core dumps for the reference:

```Haskell

Numeric.DataFrame.DFBackend.$fEqDFBackend [InlPrag=CONLIKE]
  :: forall t. Eq t => Eq (DFBackend t 1 (ScalarBase t))
[LclIdX[DFunId],
 Unf=DFun: \ (@ t) (v_B1 :: Eq t) ->
       ghc-prim-0.5.1.1:GHC.Classes.C:Eq TYPE: DFBackend
                                                 t 1 (ScalarBase t)
                                         $c== @ t v_B1
                                         $c/= @ t v_B1]
Numeric.DataFrame.DFBackend.$fEqDFBackend
  = \ (@ t) ($dEq_aRMu :: Eq t) ->
      ghc-prim-0.5.1.1:GHC.Classes.C:Eq
        @ (DFBackend t 1 (ScalarBase t))
        ($c== @ t $dEq_aRMu)
        ($c/= @ t $dEq_aRMu)

--------------------------------------------------------------------------------
Numeric.DataFrame.DFBackend.$fEqDFBackend0 [InlPrag=CONLIKE]
  :: forall t. Eq t => Eq (DFBackend t 2 (Vec2Base t))
[LclIdX[DFunId],
 Unf=DFun: \ (@ t) (v_B1 :: Eq t) ->
       ghc-prim-0.5.1.1:GHC.Classes.C:Eq TYPE: DFBackend t 2 (Vec2Base t)
                                         $c== @ t v_B1
                                         $c/= @ t v_B1]
Numeric.DataFrame.DFBackend.$fEqDFBackend0
  = \ (@ t) ($dEq_aS5E :: Eq t) ->
      ghc-prim-0.5.1.1:GHC.Classes.C:Eq
        @ (DFBackend t 2 (Vec2Base t))
        ($c== @ t $dEq_aS5E)
        ($c/= @ t $dEq_aS5E)


Numeric.DataFrame.DFBackend.$fEqDFBackend [InlPrag=CONLIKE]
  :: forall t (n :: Nat) b.
     (KnownBackend (DataFrame * Nat t n), Eq t) =>
     Eq (DFBackend t n b)
[LclIdX[DFunId],
 Unf=DFun: \ (@ t)
             (@ (n :: Nat))
             (@ b)
             (v_B1 :: KnownBackend (DataFrame * Nat t n))
             (v_B2 :: Eq t) ->
       ghc-prim-0.5.1.1:GHC.Classes.C:Eq TYPE: DFBackend t n b
                                         $c== @ t @ n @ b v_B1 v_B2
                                         $c/= @ t @ n @ b v_B1 v_B2]
Numeric.DataFrame.DFBackend.$fEqDFBackend
  = \ (@ t)
      (@ (n :: Nat))
      (@ b)
      ($dKnownBackend_aS4X :: KnownBackend (DataFrame * Nat t n))
      ($dEq_aS4Y :: Eq t) ->
      ghc-prim-0.5.1.1:GHC.Classes.C:Eq
        @ (DFBackend t n b)
        ($c== @ t @ n @ b $dKnownBackend_aS4X $dEq_aS4Y)
        ($c/= @ t @ n @ b $dKnownBackend_aS4X $dEq_aS4Y)



NonRec: (Numeric.DataFrame.DFBackend.$fEqDFBackend0,
         \ (@ t) ($dEq_aTTZ :: Eq t) ->
           ghc-prim-0.5.1.1:GHC.Classes.C:Eq
             @ (DFBackend t 2 (Vec2Base t))
             ($c== @ t $dEq_aTTZ)
             ($c/= @ t $dEq_aTTZ))

--------------------------------------------------------------------------------
sillyFun2
  :: forall t (n :: Nat) a.
     Eq a =>
     DFBackend t n a -> DFBackend t n a -> DFBackend t n a
[LclIdX]
sillyFun2
  = \ (@ t) (@ (n :: Nat)) (@ a) ($dEq_aS1E :: Eq a) ->
      (sillyFun @ a $dEq_aS1E)
      `cast` (Sym
                (Numeric.DataFrame.DFBackend.N:DFBackend[0] <t>_P <n>_P <a>_R)
              -> Sym
                   (Numeric.DataFrame.DFBackend.N:DFBackend[0] <t>_P <n>_P <a>_R)
              -> Sym
                   (Numeric.DataFrame.DFBackend.N:DFBackend[0] <t>_P <n>_P <a>_R)
              :: ((a -> a -> a) :: *)
                 ~~
                 ((DFBackend t n a -> DFBackend t n a -> DFBackend t n a) :: *))


sillyFun3 :: forall a. Eq a => a -> a -> a
[LclIdX]
sillyFun3 = sillyFun

```
