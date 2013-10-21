module Network.NetworkCommon

-- Common functionality for network clients and servers, such as data types


-- Raw connection info pointer within the C library. We don't use this directly
-- in the Idris code, but it is a parameter in all stateful operations.
data RawConnInfo = RawConn Ptr

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

foreignAllocateConnInfo : IO (Maybe RawConnInfo)
foreignAllocateConnInfo = do
  res <- mkForeign (FFun "idrnet_allocate_conn_info" [] FPtr)
  is_null <- nullPtr res
  return $ if is_null then Nothing else Just (RawConn res)


