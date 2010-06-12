{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | @System.FAM@ is a low-level binding to the libfam library
-- The @libfam@ documentation is available here:
-- <http://oss.sgi.com/projects/fam/>.

module System.FAM 
    ( -- FAM API
      open
    , open2
    , close
    , monitorDirectory
    , monitorFile
    , monitorCollection
    , suspendMonitor
    , resumeMonitor
    , cancelMonitor
    , nextEvent
    , pending

    -- FAM Types
    , Connection(..)
    , Request(..)
    , Event(..)

    -- FAM Codes
    , changed
    , deleted
    , startexecuting
    , stopexecuting
    , created
    , moved
    , acknowledge
    , exists
    , endexist
    ) 
where

import Foreign
import Foreign.C

#include <fam.h>
data Void
type VoidPtr       = Ptr Void

data Connection    = Connection { fd :: CInt }
type ConnectionPtr = Ptr Connection

data Request       = Request { reqnum :: CInt }
type RequestPtr    = Ptr Request

type FamCodes      = CInt

#{enum FamCodes, ,
   changed         = FAMChanged
 , deleted         = FAMDeleted
 , startexecuting  = FAMStartExecuting
 , stopexecuting   = FAMStopExecuting
 , created         = FAMCreated
 , moved           = FAMMoved
 , acknowledge     = FAMAcknowledge
 , exists          = FAMExists
 , endexist        = FAMEndExist
}

data Event         = Event { connection :: ConnectionPtr
                           , request    :: Request
                           , hostname   :: Ptr CString
                           , filename   :: CString
                           , userdata   :: VoidPtr
                           , code       :: FamCodes
                           }
type EventPtr      = Ptr Event

instance Storable Connection where
    sizeOf _     = (#size struct FAMConnection)
    alignment _  = alignment (undefined :: CInt)
    peek ptr     = do
      fd'        <- (#peek FAMConnection, fd) ptr
      return Connection { fd = fd' }
    poke ptr (Connection fd') = do
      (#poke FAMConnection, fd) ptr fd'

instance Storable Request where
    sizeOf _     = (#size struct FAMRequest)
    alignment _  = alignment (undefined :: CInt)
    peek ptr     = do
      rn'        <- (#peek FAMRequest, reqnum) ptr
      return Request { reqnum = rn' }
    poke ptr (Request rn') = do
      (#poke FAMRequest, reqnum) ptr rn'

instance Storable Event where
    sizeOf _      = (#size struct FAMEvent)
    alignment _   = alignment (undefined :: CInt)
    peek ptr      = do
      connection' <- (#peek FAMEvent, fc) ptr
      request'    <- (#peek FAMEvent, fr) ptr
      hostname'   <- (#peek FAMEvent, hostname) ptr
      filename'   <- (#peek FAMEvent, filename) ptr
      userdata'   <- (#peek FAMEvent, userdata) ptr
      code'       <- (#peek FAMEvent, code) ptr
      return Event { connection = connection'
                   , request    = request'
                   , hostname   = hostname'
                   , filename   = filename' 
                   , userdata   = userdata'
                   , code       = code'
                   }

foreign import ccall unsafe "fam.h FAMOpen" open :: ConnectionPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMOpen2" open2 :: ConnectionPtr -> CString -> IO CInt
foreign import ccall unsafe "fam.h FAMClose" close :: ConnectionPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMMonitorDirectory" monitorDirectory :: ConnectionPtr -> Ptr CString -> RequestPtr -> VoidPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMMonitorFile" monitorFile :: ConnectionPtr -> Ptr CString -> RequestPtr -> VoidPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMMonitorCollection" monitorCollection :: ConnectionPtr -> Ptr CString -> RequestPtr -> VoidPtr -> CInt -> Ptr String -> IO CInt
foreign import ccall unsafe "fam.h FAMSuspendMonitor" suspendMonitor :: ConnectionPtr -> RequestPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMResumeMonitor" resumeMonitor :: ConnectionPtr -> RequestPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMCancelMonitor" cancelMonitor :: ConnectionPtr -> RequestPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMNextEvent" nextEvent :: ConnectionPtr -> EventPtr -> IO CInt
foreign import ccall unsafe "fam.h FAMPending" pending :: ConnectionPtr -> IO CInt

