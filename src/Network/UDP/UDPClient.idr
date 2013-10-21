module Network.UDP.UDPClient
import Network.NetworkCommon

data UDPSendResult = SR Ptr

foreignUDPSend : String -> String -> String -> IO UDPSendResult
foreignUDPSend ip port dat = map SR $
  mkForeign (FFun "idrnet_udp_send" [FString, FString, FString] FPtr) ip port dat

foreignUDPGetUDPSendResult : UDPSendResult -> IO Int
foreignUDPGetUDPSendResult (SR ptr) = 
  mkForeign (FFun "idrnet_get_udp_send_result" [FPtr] FInt) ptr

foreignUDPGetSendErr : UDPSendResult -> IO ErrorCode
foreignUDPGetSendErr (SR ptr) = 
  mkForeign (FFun "idrnet_get_udp_send_err" [FPtr] FInt) ptr

foreignUDPFreeSendResult : UDPSendResult -> IO ()
foreignUDPFreeSendResult (SR ptr) =
  mkForeign (FFun "idrnet_free_udp_send" [FPtr] FUnit) ptr

sendData : IPAddr -> Port -> String -> IO (Either ErrorCode BytesSent)
sendData ip port dat = do
  res_struct <- foreignUDPSend (show ip) (show port) dat
  res <- foreignUDPGetUDPSendResult res_struct
  if (res == -1) then do
    err <- foreignUDPGetSendErr res_struct
    foreignUDPFreeSendResult res_struct
    return $ Left err
  else do
    res_sent <- foreignUDPGetUDPSendResult res_struct
    foreignUDPFreeSendResult res_struct
    return $ Right res_sent
  

