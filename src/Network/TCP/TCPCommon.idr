module Network.TCP.TCPCommon
import Network.NetworkCommon
%access public

foreignGetLastError : RawConnInfo -> IO Int
foreignGetLastError (RawConn ptr) = mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr


