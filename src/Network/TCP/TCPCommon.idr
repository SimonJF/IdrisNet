module Network.TCP.TCPCommon
import Network.NetworkCommon
import Network.Packet
import Network.PacketLang
import Effects
%access public

-- TODO: replace instances of (Maybe ErrorCode) with ResultCode,
-- which also contains the success code 0.
-- Why oh why I decided to do it the other way I have no idea.
ResultCode : Type
ResultCode = Int

foreignGetLastError : RawConnInfo -> IO Int
foreignGetLastError (RawConn ptr) = mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr

--foreignSendPacket : RawConnInfo -> (pl : PacketLang) -> (mkTy pl) -> IO ResultCode
--foreignSendPacket (RawConnn ptr) pl pckt = do
  
  

