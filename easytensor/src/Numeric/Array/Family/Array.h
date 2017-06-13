
--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | Do something in a loop for int i from 0 to n
loop1# :: Int# -> (Int# -> State# s -> State# s) -> State# s -> State# s
loop1# n f = loop0 0#
  where
    loop0 i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop0 (i +# 1#) s1
{-# INLINE loop1# #-}


-- | Do something in a loop for int i from 0 to n-1 and j from 0 to m-1
loop2# :: Int# -> Int# -> (Int# -> Int#-> State# s -> State# s)
       -> State# s -> State# s
loop2# n m f = loop0 0# 0#
  where
    loop0 i j s | isTrue# (j ==# m) = s
                | isTrue# (i ==# n) = loop0 0# (j +# 1#) s
                | otherwise         = case f i j s of s1 -> loop0 (i +# 1#) j s1
{-# INLINE loop2# #-}

-- | Do something in a loop for int i from 0 to n
loop1a# :: Int# -> (Int# -> a -> a) -> a -> a
loop1a# n f = loop0 0#
  where
    loop0 i s | isTrue# (i ==# n) = s
              | otherwise = case f i s of s1 -> loop0 (i +# 1#) s1
{-# INLINE loop1a# #-}


-- | Treat a single number as an array
broadcastArray :: EL_TYPE_BOXED -> ARR_TYPE ds
broadcastArray (EL_CONSTR x) = ARR_FROMSCALAR x
{-# INLINE broadcastArray #-}

-- | Accumulates only idempotent operations!
--   Being applied to FromScalars, executes only once!
accumV2 :: (EL_TYPE_PRIM-> EL_TYPE_PRIM -> a -> a)
        -> ARR_TYPE ds -> ARR_TYPE ds -> a -> a
accumV2 f (ARR_FROMSCALAR a)
          (ARR_FROMSCALAR b) = f a b
accumV2 f (ARR_CONSTR offset n a)
          (ARR_FROMSCALAR b) = loop1a# n
    (\i -> f (INDEX_ARRAY a (offset +# i)) b)
accumV2 f (ARR_FROMSCALAR a)
          (ARR_CONSTR offset n b) = loop1a# n
    (\i -> f a (INDEX_ARRAY b (offset +# i)))
accumV2 f (ARR_CONSTR offsetA n a)
          (ARR_CONSTR offsetB _ b) = loop1a# n
    (\i -> f (INDEX_ARRAY a (offsetA +# i))
             (INDEX_ARRAY b (offsetB +# i))
    )

mapV :: (EL_TYPE_PRIM -> EL_TYPE_PRIM) -> ARR_TYPE ds -> ARR_TYPE ds
mapV f (ARR_FROMSCALAR x) = ARR_FROMSCALAR (f x)
mapV f (ARR_CONSTR offset n a) = case runRW#
     ( \s0 -> case newByteArray# (n *# EL_SIZE) s0 of
         (# s1, marr #) -> case loop1# n
               (\i ss -> case f (INDEX_ARRAY a (offset +# i)) of
                 r -> WRITE_ARRAY marr i r ss
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ARR_CONSTR 0# n r
{-# INLINE mapV #-}

zipV :: (EL_TYPE_PRIM -> EL_TYPE_PRIM -> EL_TYPE_PRIM)
     -> ARR_TYPE ds -> ARR_TYPE ds -> ARR_TYPE ds
zipV f (ARR_FROMSCALAR a)
       (ARR_FROMSCALAR b) = ARR_FROMSCALAR (f a b)
zipV f x (ARR_FROMSCALAR b) = mapV (`f` b) x
zipV f (ARR_FROMSCALAR a) y = mapV (f a) y
zipV f (ARR_CONSTR offsetA n a)
       (ARR_CONSTR offsetB _ b) = case runRW#
     ( \s0 -> case newByteArray# (n *# EL_SIZE ) s0 of
         (# s1, marr #) -> case loop1# n
               (\i ss -> case f (INDEX_ARRAY a (offsetA +# i))
                                (INDEX_ARRAY b (offsetB +# i)) of
                 r -> WRITE_ARRAY marr i r ss
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ARR_CONSTR 0# n r
{-# INLINE zipV #-}



--------------------------------------------------------------------------------
-- * Instances
--------------------------------------------------------------------------------




wr :: ARR_TYPE (ds :: [Nat]) -> Int# -> Int#
   -> (MutableByteArray# RealWorld -> State# RealWorld -> State# RealWorld)
   -> ARR_TYPE ds
wr _ bs n ff = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
               (# s1, marr #) ->  case ff marr s1 of
                 s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> ARR_CONSTR 0# n r
{-# INLINE wr #-}

data ArrayUpdate# (f :: * -> *) s
  = AU# Int# !(f (MutableByteArray# s -> State# s -> State# s))

instance Dimensions ds => ElementWise (Idx ds) EL_TYPE_BOXED (ARR_TYPE (ds :: [Nat])) where
  (!) (ARR_CONSTR off _ a) i
       = case fromEnum i of I# j -> EL_CONSTR (INDEX_ARRAY a (off +# j))
  (!) (ARR_FROMSCALAR x) _ = EL_CONSTR x
  {-# INLINE (!) #-}

  broadcast (EL_CONSTR x) = ARR_FROMSCALAR x
  {-# INLINE broadcast #-}

  ewmap f x@(ARR_CONSTR offset n arr) = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dim `inSpaceOf` x)
             (\ii ss -> case readMutVar# mi ss of
               (# sss, I# i #) ->
                 case f ii (EL_CONSTR (INDEX_ARRAY arr (offset +# i))) of
                  (EL_CONSTR r) -> writeMutVar# mi (I# (i +# 1#))
                                            (WRITE_ARRAY marr i r sss)
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ARR_CONSTR 0# n r
    where
      bs = n *# EL_SIZE
  ewmap f x@(ARR_FROMSCALAR scalVal) = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dim `inSpaceOf` x)
             (\ii ss -> case readMutVar# mi ss of
               (# sss, I# i #) ->
                 case f ii (EL_CONSTR scalVal) of
                  (EL_CONSTR r) -> writeMutVar# mi (I# (i +# 1#))
                                               (WRITE_ARRAY marr i r sss)
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ARR_CONSTR 0# n r
    where
      n = case totalDim x of I# d -> d
      bs = n *# EL_SIZE
  {-# INLINE ewmap #-}

  ewgen f = case runRW#
     (\s0 -> case newByteArray# bs s0 of
       (# s1, marr #) -> case newMutVar# 0 s1 of
         (# s2, mi #) -> case loopS (dim `inSpaceOf` x)
             (\ii ss -> case readMutVar# mi ss of
               (# sss, I# i #) -> case f ii of
                  (EL_CONSTR r) -> writeMutVar# mi (I# (i +# 1#))
                                               (WRITE_ARRAY marr i r sss)
             ) s2 of
           s3 -> unsafeFreezeByteArray# marr s3
     ) of (# _, r #) -> ARR_CONSTR 0# n r
    where
      x = undefined :: ARR_TYPE ds
      n = case totalDim x of I# d -> d
      bs = n *# EL_SIZE
  {-# INLINE ewgen #-}

  ewgenA f
      = case loopA (dim `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ ff -> wr x bs n <$> ff
    where
      g ds (AU# i ff) = AU# ( i +# 1# )
                          $ (\(EL_CONSTR z) u a s -> WRITE_ARRAY a i z (u a s))
                           <$> f ds <*> ff
      x = undefined :: ARR_TYPE ds
      n = case totalDim x of I# d -> d
      bs = n *# EL_SIZE

  ewfold f v0 x@(ARR_CONSTR offset _ arr) = case runRW#
    (\s0 -> case newMutVar# (0,v0) s0 of
       (# s1, miv #) -> case loopS (dim `inSpaceOf` x)
             (\ii ss -> case readMutVar# miv ss of
               (# sss, (I# i, v) #) -> writeMutVar# miv
                      ( I# (i +# 1#)
                      , f ii (EL_CONSTR (INDEX_ARRAY arr (offset +# i))) v
                      ) sss
             ) s1 of
          s2 -> readMutVar# miv s2
    ) of (# _, (_, r) #) -> r
  ewfold f v0 x@(ARR_FROMSCALAR scalVal) = case runRW#
    (\s0 -> case newMutVar# v0 s0 of
        (# s1, miv #) -> case loopS (dim `inSpaceOf` x)
              (\ii ss -> case readMutVar# miv ss of
                (# sss, v #) -> writeMutVar# miv
                       ( f ii (EL_CONSTR scalVal) v
                       ) sss
              ) s1 of
           s2 -> readMutVar# miv s2
    ) of (# _, r #) -> r
  {-# INLINE ewfold #-}

  indexWise f x@(ARR_CONSTR offset n arr)
      = case loopA (dim `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ ff -> wr x bs n <$> ff
    where
      g ds (AU# i ff) = AU# ( i +# 1# )
                          $ (\(EL_CONSTR z) u a s -> WRITE_ARRAY a i z (u a s))
                           <$> f ds (EL_CONSTR (INDEX_ARRAY arr (offset +# i))) <*> ff
      bs = n *# EL_SIZE

  indexWise f x@(ARR_FROMSCALAR scalVal)
      = case loopA (dim `inSpaceOf` x) g (AU# 0# (pure (\_ s -> s))) of
        AU# _ ff -> wr x bs n <$> ff
    where
      n = case totalDim x of I# d -> d
      g ds (AU# i ff) = AU# ( i +# 1# )
                          $ (\(EL_CONSTR z) u a s -> WRITE_ARRAY a i z (u a s))
                           <$> f ds (EL_CONSTR scalVal) <*> ff
      bs = n *# EL_SIZE


  elementWise f x@(ARR_CONSTR offset n arr) =
      wr x bs n <$> loop1a# n g (pure (\_ s -> s))
    where
      g i ff = (\(EL_CONSTR z) u a s -> WRITE_ARRAY a i z (u a s))
                      <$> f (EL_CONSTR (INDEX_ARRAY arr (offset +# i))) <*> ff
      bs = n *# EL_SIZE
  elementWise f x@(ARR_FROMSCALAR scalVal) =
      wr x bs n <$> loop1a# n g (pure (\_ s -> s))
    where
      fa = f (EL_CONSTR scalVal)
      n = case totalDim x of I# d -> d
      g i ff = (\(EL_CONSTR z) u a s -> WRITE_ARRAY a i z (u a s))
                      <$> fa <*> ff
      bs = n *# EL_SIZE


instance Dimensions ds
      => Show (ARR_TYPE ds) where
  show x
      | D <- dim @ds = "{ " ++ show (x ! Z) ++ " }"
      | _ :* D <- dim @ds = ('{' :) . drop 1 $
                      foldr (\i s -> ", " ++ show (x ! i) ++ s) " }"
                              [minBound .. maxBound]
      | (_ :: Proxy n) :* (_ :: Proxy m) :* (_ :: Dim dss) <- dim @ds
      , DimensionsEvidence <- inferDropNDimensions (Proxy @2) x
        = let loopInner :: Idx dss -> Idx '[n,m] -> String
              loopInner ods (n:!m:!_) = ('{' :) . drop 2 $
                              foldr (\i ss -> '\n':
                                      foldr (\j s ->
                                               ", " ++ show (x ! (i :! j :! ods)) ++ s
                                            ) ss [1..m]
                                    ) " }" [1..n]
              loopOuter :: Idx dss -> String -> String
              loopOuter Z s  = "\n" ++ loopInner Z maxBound ++ s
              loopOuter ds s = "\n(i j" ++ drop 3 (show ds) ++ "):\n"
                                    ++ loopInner ds maxBound ++ s
          in drop 1 $ foldr loopOuter "" [minBound..maxBound]
      | otherwise
        = error "Show ARR_TYPE -- impossible pattern match on Dim!"

instance Eq (ARR_TYPE ds) where
  a == b = accumV2 (\x y r -> r && isTrue# (OP_EQ x y)) a b True
  {-# INLINE (==) #-}
  a /= b = accumV2 (\x y r -> r || isTrue# (OP_NE x y)) a b False
  {-# INLINE (/=) #-}


-- | Implement partial ordering for `>`, `<`, `>=`, `<=`
--     and lexicographical ordering for `compare`
instance Ord (ARR_TYPE ds) where
  a > b = accumV2 (\x y r -> r && isTrue# (OP_GT x y)) a b True
  {-# INLINE (>) #-}
  a < b = accumV2 (\x y r -> r && isTrue# (OP_LT x y)) a b True
  {-# INLINE (<) #-}
  a >= b = accumV2 (\x y r -> r && isTrue# (OP_GE x y)) a b True
  {-# INLINE (>=) #-}
  a <= b = accumV2 (\x y r -> r && isTrue# (OP_LE x y)) a b True
  {-# INLINE (<=) #-}
  -- | Compare lexicographically
  compare a b = accumV2 (\x y r -> r `mappend`
                          if isTrue# (OP_GT x y)
                          then GT
                          else if isTrue# (OP_LT x y)
                               then LT
                               else EQ
                        ) a b EQ
  {-# INLINE compare #-}
  -- | Element-wise minimum
  min = zipV  (\x y -> if isTrue# (OP_GT x y) then y else x)
  {-# INLINE min #-}
  -- | Element-wise maximum
  max = zipV  (\x y -> if isTrue# (OP_GT x y) then x else y)
  {-# INLINE max #-}


instance Num (ARR_TYPE ds) where
  (+) = zipV OP_PLUS
  {-# INLINE (+) #-}
  (-) = zipV OP_MINUS
  {-# INLINE (-) #-}
  (*) = zipV OP_TIMES
  {-# INLINE (*) #-}
  negate = mapV OP_NEGATE
  {-# INLINE negate #-}
  abs = mapV (\x -> if isTrue# (OP_GE x EL_ZERO) then x else OP_NEGATE x)
  {-# INLINE abs #-}
  signum = mapV (\x -> if isTrue# (OP_GT x EL_ZERO)
                      then EL_ONE
                      else if isTrue# (OP_LT x EL_ZERO)
                           then EL_MINUS_ONE
                           else EL_ZERO
                )
  {-# INLINE signum #-}
  fromInteger = broadcastArray . fromInteger
  {-# INLINE fromInteger #-}


type instance ElemRep (ARR_TYPE ds) = EL_RUNTIME_REP
instance Dimensions'' ds => PrimBytes (ARR_TYPE ds) where
  type ElemPrim (ARR_TYPE ds) = EL_TYPE_PRIM
  toBytes (ARR_CONSTR off size a) = (# off, size, a #)
  toBytes (ARR_FROMSCALAR x) = case runRW#
     ( \s0 -> case newByteArray# bs s0 of
         (# s1, marr #) -> case loop1# n
               (\i ss -> WRITE_ARRAY marr i x ss
               ) s1 of
             s2 -> unsafeFreezeByteArray# marr s2
     ) of (# _, r #) -> (# 0#, n, r #)
    where
      n = case totalDim (undefined :: ArrayF ds) of I# d -> d
      bs = n *# EL_SIZE
  {-# INLINE toBytes #-}
  fromBytes (# off, size, a #) = ARR_CONSTR off size a
  {-# INLINE fromBytes #-}
  byteSize x = case totalDim x of
     I# d -> EL_SIZE *# d
  {-# INLINE byteSize #-}
  byteAlign _ = EL_ALIGNMENT
  {-# INLINE byteAlign #-}
  elementByteSize _ = EL_SIZE
  {-# INLINE elementByteSize #-}
  ix i (ARR_CONSTR off _ a) = INDEX_ARRAY a (off +# i)
  ix _ (ARR_FROMSCALAR x)  = x
  {-# INLINE ix #-}
