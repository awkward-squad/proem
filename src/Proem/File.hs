module Proem.File
  ( read,
    write,
  )
where

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as ByteString.Internal
import qualified Data.ByteString.Unsafe as ByteString.Unsafe
import Data.Int (Int64)
import Data.Word (Word8)
import qualified Foreign.C.Types as C
import Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as Ptr
import qualified System.Posix.Files.ByteString as Posix (fileSize, getFdStatus)
import qualified System.Posix.IO.ByteString as Posix (OpenFileFlags (..), OpenMode (ReadOnly, WriteOnly), closeFd, defaultFileFlags, fdReadBuf, fdWriteBuf, openFd)
import qualified System.Posix.Types as Posix (COff (..))
import Prelude hiding (read)

-- | Read a file.
-- FIXME OsPath
-- FIXME make this non-blocking
-- FIXME lock file? (or otherwise handle race conditions)
read :: ByteString -> IO (Either IOException ByteString)
read path =
  Exception.try do
    Exception.bracket
      (Posix.openFd path Posix.ReadOnly Posix.defaultFileFlags)
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
-- FIXME OsPath
-- FIXME make this non-blocking
write :: ByteString -> ByteString -> IO (Either IOException ())
write path contents =
  Exception.try do
    Exception.bracket
      ( Posix.openFd
          path
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
