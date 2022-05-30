{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
    (<$>),
    (<&>),
    (<$!>),
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

    -- * Data.Semiigroup
    Semigroup ((<>)),

    -- * Data.STRef
    STRef,
    modifySTRef',
    newSTRef,
    readSTRef,
    writeSTRef,

    -- * Data.String
    IsString (fromString),
    String,

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

    -- * GHC.Err
    error,
    undefined,

    -- * GHC.Stack
    HasCallStack,

    -- * System.IO
    IO,

    -- * Text.Show
    Show (show),
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
import Control.Concurrent.MVar (MVar)
import Control.Exception
  ( Exception (displayException, fromException, toException),
    SomeAsyncException (SomeAsyncException),
    SomeException (SomeException),
    assert,
    asyncExceptionFromException,
    asyncExceptionToException,
  )
import Control.Monad (Monad ((>>), (>>=)), forever, guard, join, when, (<$!>), (=<<))
import Control.Monad.Fix (MonadFix (mfix))
import Control.Monad.IO.Class
import Control.Monad.ST (ST, runST)
import Control.Parallel (par, pseq)
import Data.Bool (Bool (False, True), not, otherwise, (&&), (||))
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
import Data.Functor (Functor (fmap, (<$)), void, (<$>), (<&>))
import Data.Functor.Compose (Compose (Compose, getCompose))
import Data.Functor.Const (Const (Const, getConst))
import Data.Functor.Contravariant (Contravariant (contramap, (>$)), ($<), (>$<))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.IORef (IORef)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.List (cycle, iterate, iterate', map, repeat, (++))
import Data.Maybe (Maybe (Just, Nothing), isJust, isNothing, maybe)
import Data.Monoid (Monoid (mconcat, mempty))
import Data.Ord (Ord (compare, max, min, (<), (<=), (>), (>=)), Ordering (EQ, GT, LT))
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef, writeSTRef)
import Data.Semigroup (Semigroup ((<>)))
import Data.String (IsString (fromString), String)
import Data.Traversable (Traversable (mapM, sequence, sequenceA, traverse), for, forM)
import Data.Tuple (fst, snd)
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import qualified Debug.Trace
import GHC.Conc (STM, TVar, atomically, catchSTM, newTVar, newTVarIO, readTVar, readTVarIO, retry, throwSTM, writeTVar)
import GHC.Err (error)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Stack (HasCallStack, callStack, currentCallStack, freezeCallStack, renderStack)
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Show (Show (show))

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
