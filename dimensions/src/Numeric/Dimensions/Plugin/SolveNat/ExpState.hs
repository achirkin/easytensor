module Numeric.Dimensions.Plugin.SolveNat.ExpState
  ( ExpState(..)
  , esMax
  , esMin
  , esPow
  , esDiv
  , esMod
  , esLog2
  )
where

-- | Carry some properties of an expression in a state struct.
data ExpState = ExpState
  { _isZero     :: Bool
  , _isNonZero  :: Bool
  , _isSignOne  :: Bool
  , _isNonNeg   :: Bool
  , _isNonPos   :: Bool
  , _isEven     :: Bool
  , _isOdd      :: Bool
  , _isComplete :: Bool
  } deriving (Eq, Ord, Show)

-- | Refine evidence from multiple sources
instance Semigroup ExpState where
  a <> b = ExpState
    { _isZero     = _isZero a     || _isZero b
    , _isNonZero  = _isNonZero a  || _isNonZero b
    , _isSignOne  = _isSignOne a  || _isSignOne b
    , _isNonNeg   = _isNonNeg a   || _isNonNeg b
    , _isNonPos   = _isNonPos a   || _isNonPos b
    , _isEven     = _isEven a     || _isEven b
    , _isOdd      = _isOdd a      || _isOdd b
    , _isComplete = _isComplete a || _isComplete b
    }

instance Num ExpState where
  a + b = ExpState
    { _isZero     = _isZero a && _isZero b
    , _isNonZero  = or
        [ _isNonZero a && _isZero b
        , _isNonZero b && _isZero a
        , (_isNonZero a || _isNonZero b) && _isNonNeg a && _isNonNeg b
        , (_isNonZero a || _isNonZero b) && _isNonPos a && _isNonPos b
        ]
    , _isSignOne  = _isSignOne a && _isZero b || _isZero a && _isSignOne b
    , _isNonNeg   = _isNonNeg a && _isNonNeg b
    , _isNonPos   = _isNonPos a && _isNonPos b
    , _isEven     = _isEven a && _isEven b || _isOdd a && _isOdd b
    , _isOdd      = _isEven a && _isOdd b || _isOdd a && _isEven b
    , _isComplete = _isComplete a && _isComplete b
    }
  a - b = ExpState
    { _isZero     = _isZero a && _isZero b
    , _isNonZero  = or
        [ _isNonZero a && _isZero b
        , _isNonZero b && _isZero a
        , (_isNonZero a || _isNonZero b) && _isNonNeg a && _isNonPos b
        , (_isNonZero a || _isNonZero b) && _isNonPos a && _isNonNeg b
        ]
    , _isSignOne  = _isSignOne a && _isZero b || _isZero a && _isSignOne b
    , _isNonNeg   = _isNonNeg a && _isNonPos b
    , _isNonPos   = _isNonPos a && _isNonNeg b
    , _isEven     = _isEven a && _isEven b || _isOdd a && _isOdd b
    , _isOdd      = _isEven a && _isOdd b || _isOdd a && _isEven b
    , _isComplete = _isComplete a && _isComplete b
    }
  a * b = ExpState
    { _isZero     = _isZero a || _isZero b
    , _isNonZero  = _isNonZero a && _isNonZero b
    , _isSignOne  = _isSignOne a && _isSignOne b
    , _isNonNeg   = _isZero a || _isZero b || _isNonNeg a && _isNonNeg b || _isNonPos a && _isNonPos b
    , _isNonPos   = _isZero a || _isZero b || _isNonNeg a && _isNonPos b || _isNonPos a && _isNonNeg b
    , _isEven     = _isEven a || _isEven b
    , _isOdd      = _isOdd a && _isOdd b
    , _isComplete = _isComplete a && _isComplete b
    }
  negate a = a
    { _isNonNeg = _isNonPos a
    , _isNonPos = _isNonNeg a
    }
  abs a = a
    { _isNonNeg = True
    , _isNonPos = _isZero a
    }
  signum a = a
    { _isSignOne  = _isNonZero a
    , _isEven     = _isZero a
    , _isOdd      = _isNonZero a
    }
  fromInteger i = ExpState
    { _isZero     = i == 0
    , _isNonZero  = i /= 0
    , _isSignOne  = abs i == 1
    , _isNonNeg   = i >= 0
    , _isNonPos   = i <= 0
    , _isEven     = even i
    , _isOdd      = odd i
    , _isComplete = True
    }

-- | State of the maximum of two expressions
esMax :: ExpState -> ExpState -> ExpState
esMax a b = ExpState
  { _isZero     = or
    [_isZero a && _isZero b, _isZero a && _isNonPos b, _isZero b && _isNonPos a]
  , _isNonZero  = or
    [ _isNonZero a && _isNonZero b
    , _isNonZero a && _isNonNeg a
    , _isNonZero b && _isNonNeg b
    ]
  , _isSignOne  = or
    [ _isSignOne a && _isSignOne b
    , _isSignOne a && _isNonNeg a && _isZero b
    , _isSignOne a && _isNonNeg a && _isNonPos b
    , _isSignOne a && _isNonPos a && _isNonPos b && _isNonZero b
    , _isSignOne b && _isNonNeg b && _isZero a
    , _isSignOne b && _isNonNeg b && _isNonPos a
    , _isSignOne b && _isNonPos b && _isNonPos a && _isNonZero a
    ]
  , _isNonNeg   = _isNonNeg a || _isNonNeg b
  , _isNonPos   = _isNonPos a && _isNonPos b
  , _isEven     = or
    [ _isEven a && _isEven b
    , _isEven a && _isNonNeg a && _isNonPos b
    , _isEven b && _isNonNeg b && _isNonPos a
    ]
  , _isOdd      = or
    [ _isOdd a && _isOdd b
    , _isOdd a && _isNonNeg a && _isNonPos b
    , _isOdd b && _isNonNeg b && _isNonPos a
    ]
  , _isComplete = _isComplete a && _isComplete b
  }


-- | State of the minimum of two expressions
esMin :: ExpState -> ExpState -> ExpState
esMin a b = ExpState
  { _isZero     = or
    [_isZero a && _isZero b, _isZero a && _isNonNeg b, _isZero b && _isNonNeg a]
  , _isNonZero  = or
    [ _isNonZero a && _isNonZero b
    , _isNonZero a && _isNonPos a
    , _isNonZero b && _isNonPos b
    ]
  , _isSignOne  = or
    [ _isSignOne a && _isSignOne b
    , _isSignOne a && _isNonPos a && _isZero b
    , _isSignOne a && _isNonPos a && _isNonNeg b
    , _isSignOne a && _isNonNeg a && _isNonNeg b && _isNonZero b
    , _isSignOne b && _isNonPos b && _isZero a
    , _isSignOne b && _isNonPos b && _isNonNeg a
    , _isSignOne b && _isNonNeg b && _isNonNeg a && _isNonZero a
    ]
  , _isNonNeg   = _isNonNeg a && _isNonNeg b
  , _isNonPos   = _isNonPos a || _isNonPos b
  , _isEven     = or
    [ _isEven a && _isEven b
    , _isEven a && _isNonPos a && _isNonNeg b
    , _isEven b && _isNonPos b && _isNonNeg a
    ]
  , _isOdd      = or
    [ _isOdd a && _isOdd b
    , _isOdd a && _isNonNeg b && _isNonPos a
    , _isOdd b && _isNonNeg a && _isNonPos b
    ]
  , _isComplete = _isComplete a && _isComplete b
  }

-- | State of the expression @ a ^ b @
esPow :: ExpState -> ExpState -> ExpState
esPow a b = ExpState
  { _isZero     = _isZero a && _isNonNeg b && _isNonZero b
  , _isNonZero  = _isNonZero a || _isZero b
  , _isSignOne  = _isSignOne a || _isZero b
  , _isNonNeg   = _isNonNeg a || _isEven b
  , _isNonPos   = (_isNonPos a && _isOdd b)
    || (_isZero a && _isNonNeg b && _isNonZero b)
  , _isEven     = _isEven a && _isNonNeg b && _isNonZero b
  , _isOdd      = _isOdd a && _isNonNeg b || _isZero b
  , _isComplete = _isComplete a && _isComplete b && _isNonNeg b
  }

-- | State of the expression @ Div a b @
esDiv :: ExpState -> ExpState -> ExpState
esDiv a b = ExpState
  { _isZero     = _isZero a
  , _isNonZero  = _isNonZero a && _isSignOne b
  , _isSignOne  = False
  , _isNonNeg   = _isNonNeg a && _isNonNeg b || _isNonPos a && _isNonPos b
  , _isNonPos   = _isNonNeg a && _isNonPos b || _isNonPos a && _isNonNeg b
  , _isEven     = _isZero a
  , _isOdd      = False
  , _isComplete = _isComplete a && _isComplete b && _isNonZero b
  }

-- | State of the expression @ Mod a b @
esMod :: ExpState -> ExpState -> ExpState
esMod a b = ExpState
  { _isZero     = _isZero a || _isSignOne b
  , _isNonZero  = _isOdd a && _isEven b && _isNonZero b
  , _isSignOne  = _isSignOne a && _isNonZero b && _isEven b
  , _isNonNeg   = _isNonNeg b && _isNonZero b
  , _isNonPos   = _isNonPos b && _isNonZero b
  , _isEven     = _isZero a || _isSignOne b
  , _isOdd      = _isSignOne a && _isNonZero b && _isEven b
  , _isComplete = _isComplete a && _isComplete b && _isNonZero b
  }

-- | State of the expression @ Log2 a @
esLog2 :: ExpState -> ExpState
esLog2 a = ExpState
  { _isZero     = _isSignOne a && _isNonNeg a
  , _isNonZero  = _isNonNeg a && _isNonZero a && _isEven a
  , _isSignOne  = False
  , _isNonNeg   = True
  , _isNonPos   = _isSignOne a && _isNonNeg a
  , _isEven     = _isSignOne a && _isNonNeg a
  , _isOdd      = False
  , _isComplete = _isComplete a && _isNonNeg a && _isNonZero a
  }
