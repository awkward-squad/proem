module Proem
  ( -- * Control.Applicative
    Alternative ((<|>), empty, many, some),
    Applicative ((*>), (<*), (<*>), liftA2, pure),
    liftA3,
    optional,

    -- * Control.Category
    (<<<),
    (>>>),

    -- * Control.Concurrent
    ThreadId,
    myThreadId,

    -- * Control.Concurrent.MVar
    MVar,
    isEmptyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,

    -- * Control.Concurrent.STM
    STM,
    TVar,
    atomically,
    catchSTM,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    retry,
    throwSTM,
    writeTVar,

    -- * Control.Exception
    Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException (SomeException),
    assert,
    asyncExceptionFromException,
    asyncExceptionToException,

    -- * Control.Monad
    Monad ((>>), (>>=)),
    (=<<),
    forever,
    guard,
    join,
    when,

    -- * Control.Monad.Fix
    MonadFix (mfix),

    -- * Control.Monad.IO.Class
    MonadIO (liftIO),

    -- * Control.Monad.ST
    ST,
    runST,

    -- * Control.Parallel
    par,
    pseq,

    -- * Data.Bool
    Bool (False, True),
    (&&),
    (||),
    not,
    otherwise,

    -- * Data.ByteString
    ByteString,

    -- * Data.Char
    Char,

    -- * Data.Coerce
    Coercible,
    coerce,

    -- * Data.Either
    Either (Left, Right),
    either,
    isLeft,
    isRight,

    -- * Data.Eq
    Eq ((/=), (==)),

    -- * Data.Foldable
    Foldable (fold, foldMap', foldMap, foldl', foldr, length, null, toList),
    all,
    and,
    any,
    asum,
    find,
    foldlM,
    forM_,
    for_,
    mapM_,
    or,
    sequenceA_,
    sequence_,
    traverse_,

    -- * Data.Function
    ($),
    (&),
    (.),
    const,
    fix,
    id,

    -- * Data.Functor
    Functor ((<$), fmap),
    ($>),
    (<$!>),
    (<$>),
    (<&>),
    void,

    -- * Data.Functor.Identity
    Identity (Identity, runIdentity),

    -- * Data.Functor.Compose
    Compose (Compose, getCompose),

    -- * Data.Functor.Const
    Const (Const, getConst),

    -- * Data.Functor.Contravariant
    Contravariant ((>$), contramap),
    ($<),
    (>$<),

    -- * Data.Int
    Int,
    Int8,
    Int16,
    Int32,
    Int64,

    -- * Data.IORef
    IORef,

    -- * Data.Kind
    Constraint,
    Type,

    -- * Data.List
    (++),
    cycle,
    iterate,
    iterate',
    map,
    repeat,

    -- * Data.Map
    Map,

    -- * Data.Maybe
    Maybe (Just, Nothing),
    isJust,
    isNothing,
    maybe,

    -- * Data.Monoid
    Monoid (mconcat, mempty),

    -- * Data.Ord
    Ord ((<), (<=), (>), (>=), compare, max, min),
    Ordering (EQ, LT, GT),
    -- #if __GLASGOW_HASKELL__ > 902
    -- clamp,
    -- #endif

    -- * Data.Proxy
    Proxy (Proxy),
    Proxy#,
    proxy#,

    -- * Data.Ratio
    Rational,

    -- * Data.Semigroup
    Semigroup ((<>)),

    -- * Data.Sequence
    Seq,

    -- * Data.Set
    Set,

    -- * Data.STRef
    STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,

    -- * Data.String
    IsString (fromString),
    String,

    -- * Data.Text
    Text,

    -- * Data.Traversable
    Traversable (mapM, sequence, sequenceA, traverse),
    for,
    forM,

    -- * Data.Tuple
    fst,
    snd,

    -- * Data.Typeable
    Typeable,

    -- * Data.Void
    Void,
    absurd,

    -- * Data.Word
    Word,
    Word8,
    Word16,
    Word32,
    Word64,

    -- * Debug.Trace
    trace,
    traceIO,
    traceM,
    traceShowId,
    traceShowIdStack,
    traceStack,

    -- * GHC.Base
    ($!),

    -- * GHC.Enum
    Bounded (maxBound, minBound),

    -- * GHC.Float
    Double,
    Float,

    -- * GHC.Generics
    Generic,

    -- * GHC.Integer
    Integer,

    -- * GHC.Num
    Num ((*), (+), (-), abs, fromInteger, negate, signum),

    -- * GHC.OverloadedLabels
    IsLabel (fromLabel),

    -- * GHC.Real
    Fractional ((/), fromRational, recip),
    Integral (div, divMod, mod, quot, quotRem, rem, toInteger),
    Real (toRational),
    RealFrac (ceiling, floor, properFraction, round, truncate),
    (^),
    (^^),
    even,
    fromIntegral,
    odd,
    realToFrac,

    -- * GHC.Stack
    HasCallStack,

    -- * Numeric.Natural
    Natural,

    -- * Prelude
    error,
    seq,
    subtract,
    undefined,

    -- * System.IO
    FilePath,
    IO,
    print,

    -- * System.Mem.StableName
    StableName,
    eqStableName,
    hashStableName,
    makeStableName,

    -- * Text.Show
    Show (show),

    -- * New types and functions
    readFile,
  )
where

import Control.Applicative
  ( Alternative (empty, many, some, (<|>)),
    Applicative (liftA2, pure, (*>), (<*), (<*>)),
    liftA3,
    optional,
  )
import Control.Category ((<<<), (>>>))
import Control.Concurrent (ThreadId, myThreadId)
import Control.Concurrent.MVar
  ( MVar,
    isEmptyMVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    readMVar,
    takeMVar,
    tryPutMVar,
    tryReadMVar,
    tryTakeMVar,
  )
import Control.Exception
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException (SomeException),
    assert,
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import qualified Control.Exception
import Control.Monad (Monad ((>>), (>>=)), forever, guard, join, when, (<$!>), (=<<))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class
import Control.Monad.ST (ST, runST)
import Control.Parallel (par, pseq)
import Data.Bool (Bool (False, True), not, otherwise, (&&), (||))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString.Internal
import Data.Char (Char)
import Data.Coerce (Coercible, coerce)
import Data.Either (Either (Left, Right), either, isLeft, isRight)
import Data.Eq (Eq ((/=), (==)))
import Data.Foldable
  ( Foldable (fold, foldMap, foldMap', foldl', foldr, length, null, toList),
    all,
    and,
    any,
    asum,
    find,
    foldlM,
    forM_,
    for_,
    mapM_,
    or,
    sequenceA_,
    sequence_,
    traverse_,
  )
import Data.Function (const, fix, id, ($), (&), (.))
import Data.Functor (Functor (fmap, (<$)), void, ($>), (<$>), (<&>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Contravariant (Contravariant (contramap, (>$)), ($<), (>$<))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.IORef (IORef)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.List (cycle, iterate, iterate', map, repeat, (++))
import Data.Map (Map)
import Data.Maybe (Maybe (Just, Nothing), isJust, isNothing, maybe)
import Data.Monoid (Monoid (mconcat, mempty))
import Data.Ord (Ord (compare, max, min, (<), (<=), (>), (>=)), Ordering (EQ, GT, LT))
import Data.Proxy (Proxy (Proxy))
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Semigroup ((<>)))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.String (IsString (fromString), String)
import Data.Text (Text)
import Data.Traversable (Traversable (mapM, sequence, sequenceA, traverse), for, forM)
import Data.Tuple (fst, snd)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import qualified Debug.Trace
import qualified Foreign.C.Types
import qualified Foreign.Ptr
import GHC.Base (($!))
import GHC.Conc (STM, TVar, atomically, catchSTM, newTVar, newTVarIO, readTVar, readTVarIO, retry, throwSTM, writeTVar)
import GHC.Enum (Bounded (maxBound, minBound))
import GHC.Err (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (Proxy#, RuntimeRep, TYPE, proxy#, raise#, seq)
import GHC.Float (Double, Float)
import GHC.Generics (Generic)
import GHC.IO (FilePath, IO)
import GHC.Natural (Natural)
import GHC.Num (Num (abs, fromInteger, negate, signum, (*), (+), (-)), subtract)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Real
  ( Fractional (fromRational, recip, (/)),
    Integral (div, divMod, mod, quot, quotRem, rem, toInteger),
    Rational,
    Real (toRational),
    RealFrac (ceiling, floor, properFraction, round, truncate),
    even,
    fromIntegral,
    odd,
    realToFrac,
    (^),
    (^^),
  )
import GHC.Show (Show (show))
import GHC.Stack (HasCallStack, callStack, currentCallStack, freezeCallStack, renderStack)
import System.IO (print)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem.StableName (StableName, eqStableName, hashStableName, makeStableName)
import qualified System.Posix.Files.ByteString as Posix (fileSize, getFdStatus)
import qualified System.Posix.IO.ByteString as Posix (OpenMode (ReadOnly), closeFd, defaultFileFlags, fdReadBuf, openFd)
import qualified System.Posix.Types as Posix (COff (..))
import Prelude (Integer)

-- | Read a file.
readFile :: ByteString -> IO ByteString
readFile path =
  Control.Exception.bracket
    (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags)
    Posix.closeFd
    \fd -> do
      let loop :: Foreign.C.Types.CSize -> Foreign.Ptr.Ptr Word8 -> IO ()
          loop n ptr = do
            m <- Posix.fdReadBuf fd ptr n
            when (m < n) (loop (n - m) (Foreign.Ptr.plusPtr ptr (unsafeCSizeToInt m)))
      status <- Posix.getFdStatus fd
      let Posix.COff size64 = Posix.fileSize status
      ByteString.Internal.create
        (unsafeInt64ToInt size64)
        (loop (int64ToCSize size64))

{-# WARNING trace "trace" #-}
trace :: String -> a -> a
trace = Debug.Trace.trace

{-# WARNING traceIO "traceIO" #-}
traceIO :: String -> IO ()
traceIO = Debug.Trace.traceIO

{-# WARNING traceM "traceM" #-}
traceM :: Applicative m => String -> m ()
traceM = Debug.Trace.traceM

{-# WARNING traceShowId "traceShowId" #-}
traceShowId :: Show a => a -> a
traceShowId = Debug.Trace.traceShowId

-- Meh, this variant seems to be missing from Debug.Trace
{-# WARNING traceShowIdStack "traceShowIdStack" #-}
traceShowIdStack :: Show a => a -> a
traceShowIdStack x =
  unsafePerformIO do
    Debug.Trace.traceIO (show x)
    stack <- currentCallStack
    when (not (null stack)) (Debug.Trace.traceIO (renderStack stack))
    pure x

{-# WARNING traceStack "traceStack" #-}
traceStack :: String -> a -> a
traceStack = Debug.Trace.traceStack

{-# WARNING undefined "undefined" #-}
undefined :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a
undefined =
  raise# (errorCallWithCallStackException "undefined" (freezeCallStack callStack))

--

int64ToCSize :: Int64 -> Foreign.C.Types.CSize
int64ToCSize =
  fromIntegral
{-# INLINE int64ToCSize #-}

unsafeInt64ToInt :: Int64 -> Int
unsafeInt64ToInt =
  fromIntegral
{-# INLINE unsafeInt64ToInt #-}

unsafeCSizeToInt :: Foreign.C.Types.CSize -> Int
unsafeCSizeToInt =
  fromIntegral
{-# INLINE unsafeCSizeToInt #-}
