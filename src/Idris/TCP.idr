module Network.TCP 
import Effects

-- Raw connection info pointer within the C library. We don't use this directly
-- in the Idris code, but it is a parameter in all stateful operations.
RawConnInfo : Type
RawConnInfo = Ptr

data ConnFailReason = ConnFailAddr Int
                    | ConnFailSocketDesc Int
                    | ConnFailConn Int

-- Result of a connection operation.
-- Connection will either return an idr_conn_info structure, which we later use, or an error.
data ConnectResult = ConnSuccess RawConnInfo
                   | ConnFail ConnFailReason



