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

import           Data.Ratio

-- | Carry some properties of an expression in a state struct.
data ExpState = ExpState
  { _isZero     :: Bool
  , _isNonZero  :: Bool
  , _isSignOne  :: Bool
  , _isNonNeg   :: Bool
  , _isNonPos   :: Bool
  , _isEven     :: Bool
  , _isOdd      :: Bool
  , _isWhole    :: Bool
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
    , _isWhole    = _isWhole a    || _isWhole b
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
    , _isEven     = _isEven a && _isEven b || _isWhole a && _isWhole b && _isOdd a && _isOdd b
    , _isOdd      = _isWhole a && _isEven a && _isOdd b || _isWhole b && _isEven b && _isOdd a
    , _isWhole    = _isWhole a && _isWhole b
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
    , _isEven     = _isEven a && _isEven b || _isWhole a && _isWhole b && _isOdd a && _isOdd b
    , _isOdd      = _isWhole a && _isEven a && _isOdd b || _isWhole b && _isEven b && _isOdd a
    , _isWhole    = _isWhole a && _isWhole b
    , _isComplete = _isComplete a && _isComplete b
    }
  a * b = ExpState
    { _isZero     = _isZero a || _isZero b
    , _isNonZero  = _isNonZero a && _isNonZero b
    , _isSignOne  = _isSignOne a && _isSignOne b
    , _isNonNeg   = _isZero a || _isZero b || _isNonNeg a && _isNonNeg b || _isNonPos a && _isNonPos b
    , _isNonPos   = _isZero a || _isZero b || _isNonNeg a && _isNonPos b || _isNonPos a && _isNonNeg b
    , _isEven     = (_isWhole b && _isEven a) || (_isWhole a && _isEven b)
    , _isOdd      = _isOdd a && _isOdd b
    , _isWhole    = (_isWhole a && _isWhole b) || _isZero a || _isZero b
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
  signum a = a -- TODO: this is normal signum. In contrast to smooth signum we have in Exp!
    { _isSignOne  = _isNonZero a
    , _isEven     = _isZero a
    , _isOdd      = _isNonZero a
    , _isWhole    = True
    }
  fromInteger i = ExpState
    { _isZero     = i == 0
    , _isNonZero  = i /= 0
    , _isSignOne  = abs i == 1
    , _isNonNeg   = i >= 0
    , _isNonPos   = i <= 0
    , _isEven     = even i
    , _isOdd      = odd i
    , _isWhole    = True
    , _isComplete = True
    }

instance Fractional ExpState where
  recip a = ExpState
    { _isZero     = False
    , _isNonZero  = _isNonZero a
    , _isSignOne  = _isSignOne a
    , _isNonNeg   = _isNonZero a && _isNonNeg a
    , _isNonPos   = _isNonZero a && _isNonPos a
    , _isEven     = False
    , _isOdd      = _isNonZero a && _isEven a
    , _isWhole    = _isSignOne a
    , _isComplete = _isNonZero a
    }
  fromRational r = ExpState
    { _isZero     = r == 0
    , _isNonZero  = r /= 0
    , _isSignOne  = abs r == 1
    , _isNonNeg   = r >= 0
    , _isNonPos   = r <= 0
    , _isEven     = even $ numerator r
    , _isOdd      = odd $ numerator r
    , _isWhole    = denominator r == 1
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
    , _isSignOne a && _isNonPos a && _isNonPos b && _isNonZero b && _isWhole b
    , _isSignOne b && _isNonNeg b && _isZero a
    , _isSignOne b && _isNonNeg b && _isNonPos a
    , _isSignOne b && _isNonPos b && _isNonPos a && _isNonZero a && _isWhole a
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
  , _isWhole    = _isWhole a && _isWhole b
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
    , _isSignOne a && _isNonNeg a && _isNonNeg b && _isNonZero b && _isWhole b
    , _isSignOne b && _isNonPos b && _isZero a
    , _isSignOne b && _isNonPos b && _isNonNeg a
    , _isSignOne b && _isNonNeg b && _isNonNeg a && _isNonZero a && _isWhole a
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
  , _isWhole    = _isWhole a && _isWhole b
  , _isComplete = _isComplete a && _isComplete b
  }

-- | State of the expression @ a ^ b @
esPow :: ExpState -> ExpState -> ExpState
esPow a b = inferBasics $ ExpState
  { _isZero     = isReal && _isZero a && _isNonNeg b && _isNonZero b
  , _isNonZero  = _isNonZero a || _isZero b
  , _isSignOne  = isReal && (_isSignOne a || _isZero b)
  , _isNonNeg   = isReal && (_isNonNeg a || _isEven b)
  , _isNonPos   = isReal && (_isNonPos a && _isOdd b)
  , _isEven     = isReal && _isEven a && _isNonNeg b && _isNonZero b
  , _isOdd      = isReal && _isOdd a && _isNonNeg b
  , _isWhole    = isReal && _isWhole a && _isWhole b && _isNonNeg b
  , _isComplete = isReal && _isComplete a && _isComplete b
  }
  where
    isReal =
      (_isNonZero a || _isNonNeg b) -- if a is zero, b must be non-negative
        && (_isNonNeg a || _isEven b || _isOdd (recip b) || _isWhole b) -- roots of negative values are not allowed


-- | State of the expression @ Div a b @
esDiv :: ExpState -> ExpState -> ExpState
esDiv a b = ExpState
  { _isZero     = _isNonZero b && _isZero a
  , _isNonZero  = _isNonZero b && _isWhole a && _isNonZero a && _isSignOne b
  , _isSignOne  = False
  , _isNonNeg   = _isNonZero b
    && (_isNonNeg a && _isNonNeg b || _isNonPos a && _isNonPos b)
  , _isNonPos   = _isNonZero b
    && (_isNonNeg a && _isNonPos b || _isNonPos a && _isNonNeg b)
  , _isEven     = _isNonZero b && _isZero a
  , _isOdd      = False
  , _isWhole    = _isNonZero b
  , _isComplete = _isNonZero b && _isComplete a && _isComplete b
  }

-- | State of the expression @ Mod a b @
esMod :: ExpState -> ExpState -> ExpState
esMod a b = s { _isWhole = _isWhole s || _isZero s || _isSignOne s }
  where
    s = ExpState
      { _isZero     = _isNonZero b && (_isZero a || _isWhole a && _isSignOne b)
      , _isNonZero  = _isOdd a && _isEven b && _isNonZero b
      , _isSignOne  = (  (_isNonPos a && _isNonPos b)
                      || (_isNonNeg a && _isNonNeg b)
                      )
        && _isSignOne a
        && _isNonZero b
        && _isEven b
        && _isWhole b
      , _isNonNeg   = (_isNonNeg b || _isZero a) && _isNonZero b
      , _isNonPos   = (_isNonPos b || _isZero a) && _isNonZero b
      , _isEven = (_isZero a && _isNonZero b) || (_isWhole a && _isSignOne b)
      , _isOdd      = _isSignOne a && _isNonZero b && _isEven b
      , _isWhole    = _isWhole a && _isWhole b && _isNonZero b
      , _isComplete = _isComplete a && _isComplete b && _isNonZero b
      }

-- | State of the expression @ Log2 a @
esLog2 :: ExpState -> ExpState
esLog2 a = ExpState
  { _isZero     = _isSignOne a && _isNonNeg a
  , _isNonZero  = False
  , _isSignOne  = False
  , _isNonNeg   = _isSignOne a && _isNonNeg a -- if 'a' is fractional and `0 < a < 1`.
  , _isNonPos   = _isSignOne a && _isNonNeg a
  , _isEven     = _isSignOne a && _isNonNeg a
  , _isOdd      = False
  , _isWhole    = _isNonNeg a && _isNonZero a
  , _isComplete = _isComplete a && _isNonNeg a && _isNonZero a
  }

inferBasics :: ExpState -> ExpState
inferBasics a = a <> ExpState
  { _isZero     = _isNonNeg a && _isNonPos a
  , _isNonZero  = _isSignOne a || _isOdd a
  , _isSignOne  = False
  , _isNonNeg   = _isZero a
  , _isNonPos   = _isZero a
  , _isEven     = _isZero a
  , _isOdd      = _isSignOne a
  , _isWhole    = _isZero a || _isSignOne a
  , _isComplete = False
  }
