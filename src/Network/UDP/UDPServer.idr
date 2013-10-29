module Network.UDP.UDPServer
import Network.NetworkCommon

data UDPRecvResult = RR Ptr

foreignUDPListen : RawConnInfo -> String -> String -> IO Ptr
foreignUDPListen (RawConn ptr) ip port =
  mkForeign (FFun "idrnet_udp_listen" [FPtr, FString, FString] FPtr) ptr ip port

foreignUDPRecv : RawConnInfo -> IO UDPRecvResult
foreignUDPRecv (RawConn ptr) = map RR $
  mkForeign (FFun "idrnet_udp_recv" [FPtr] FPtr) ptr

foreignUDPGetResult : UDPRecvResult -> IO Int
foreignUDPGetResult (RR ptr) = 
  mkForeign (FFun "idrnet_get_udp_result" [FPtr] FInt) ptr

foreignUDPGetErr : UDPRecvResult -> IO Int
foreignUDPGetErr (RR ptr) =
  mkForeign (FFun "idrnet_get_udp_err" [FPtr] FInt) ptr

foreignUDPFreeResult : UDPRecvResult -> IO ()
foreignUDPFreeResult (RR ptr) =
  mkForeign (FFun "idrnet_free_udp_res" [FPtr] FUnit) ptr

foreignUDPGetData : UDPRecvResult -> IO String
foreignUDPGetData (RR ptr) =
  mkForeign (FFun "idrnet_get_udp_data" [FPtr] FString) ptr

foreignUDPGetIP : UDPRecvResult -> IO String
foreignUDPGetIP (RR ptr) = 
  mkForeign (FFun "idrnet_get_udp_ip" [FPtr] FString) ptr

foreignUDPGetPort : UDPRecvResult -> IO Int
foreignUDPGetPort (RR ptr) = 
  mkForeign (FFun "idrnet_get_udp_port" [FPtr] FInt) ptr


