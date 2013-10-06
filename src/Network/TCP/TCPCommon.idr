module Network.TCP.TCPCommon
%access public

-- Common functionality for TCP clients and servers, such as data types


-- Raw connection info pointer within the C library. We don't use this directly
-- in the Idris code, but it is a parameter in all stateful operations.
RawConnInfo : Type
RawConnInfo = Ptr

-- C Error Code
ErrorCode : Type
ErrorCode = Int

Port : Type
Port = Int

BytesSent : Type
BytesSent = Int

-- TODO: Validation
-- Also, no IPv6 support yet...
data IPAddr = IPv4Addr Int Int Int Int
            | IPv6Addr 

instance Show IPAddr where
  show (IPv4Addr i1 i2 i3 i4) = concat $ intersperse "." $ map show [i1, i2, i3, i4]

foreignGetLastError : RawConnInfo -> IO Int
foreignGetLastError ptr = mkForeign (FFun "idrnet_get_last_error" [FPtr] FInt) ptr


