{-# LANGUAGE CPP #-}

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#error "Proem.File: Windows not supported"
#endif

module Proem.File
  ( Path,
    path,
    read,
    write,
  )
where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString.Internal
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import Data.Coerce (coerce)
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as Ptr
import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified System.OsPath as OsPath
import qualified System.OsString.Internal.Types as OsString.Internal
import qualified System.Posix.Files.ByteString as Posix (fileSize, getFdStatus)
import qualified System.Posix.IO.PosixString as Posix
  ( OpenFileFlags (..),
    OpenMode (ReadOnly, WriteOnly),
    closeFd,
    defaultFileFlags,
    fdReadBuf,
    fdWriteBuf,
    openFd,
  )
import qualified System.Posix.Types as Posix (COff (..))
import Prelude hiding (read)

-- | A file path.
type Path =
  OsPath.OsPath

-- | 'Path' quasi-quoter.
path :: QuasiQuoter
path =
  OsPath.osp

-- | Read a file.
-- FIXME make this non-blocking
-- FIXME lock file? (or otherwise handle race conditions)
read :: Path -> IO (Either IOException ByteString)
read p =
  Exception.try do
    Exception.bracket
      ( Posix.openFd
          (coerce @OsString.Internal.OsString @OsString.Internal.PosixString p)
          Posix.ReadOnly
          Posix.defaultFileFlags
      )
      Posix.closeFd
      \fd -> do
        let loop :: C.CSize -> Ptr.Ptr Word8 -> IO ()
            loop n ptr = do
              m <- Posix.fdReadBuf fd ptr n
              when (m < n) (loop (n - m) (Ptr.plusPtr ptr (unsafeCSizeToInt m)))
        status <- Posix.getFdStatus fd
        let Posix.COff size64 = Posix.fileSize status
        ByteString.Internal.create
          (unsafeInt64ToInt size64)
          (loop (int64ToCSize size64))

-- | Write a file.
-- FIXME make this non-blocking
write :: Path -> ByteString -> IO (Either IOException ())
write p contents =
  Exception.try do
    Exception.bracket
      ( Posix.openFd
          (coerce @OsString.Internal.OsString @OsString.Internal.PosixString p)
          Posix.WriteOnly
          Posix.defaultFileFlags
            { Posix.creat = Just 0o666,
              Posix.trunc = True
            }
      )
      Posix.closeFd
      \fd -> do
        let loop :: C.CSize -> Ptr Word8 -> IO ()
            loop n ptr = do
              m <- Posix.fdWriteBuf fd ptr n
              when (m < n) (loop (n - m) (Ptr.plusPtr ptr (unsafeCSizeToInt m)))
        ByteString.Unsafe.unsafeUseAsCStringLen contents \(ptr, len) ->
          loop (intToCSize len) (Ptr.castPtr @C.CChar @Word8 ptr)

--

intToCSize :: Int -> C.CSize
intToCSize =
  fromIntegral
{-# INLINE intToCSize #-}

int64ToCSize :: Int64 -> C.CSize
int64ToCSize =
  fromIntegral
{-# INLINE int64ToCSize #-}

unsafeInt64ToInt :: Int64 -> Int
unsafeInt64ToInt =
  fromIntegral
{-# INLINE unsafeInt64ToInt #-}

unsafeCSizeToInt :: C.CSize -> Int
unsafeCSizeToInt =
  fromIntegral
{-# INLINE unsafeCSizeToInt #-}
