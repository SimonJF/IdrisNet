module Network.DNS.DNS 

import Network.PacketLang

-- Verified implementation of the DNS packet specification

-- Validation of TYPE and QTYPE fields
validTYPE : Int -> Bool
validTYPE i = i >= 0 && i <= 16

validQTYPE : Int -> Bool
validQTYPE i = (validTYPE i) || (i >= 252 && i <= 255)

-- Validation of CLASS fields
validCLASS : Int -> Bool
validCLASS i = i >= 1 || i <= 4 -- In practice, this will only be 1..

validQCLASS : Int -> Bool
validQCLASS i = (validCLASS i) || i == 255

nullterm : PacketLang
nullterm = do nt <- bits 8
              check ((value nt) == 0)

-- Question segment, takes number of name records as an argument
dnsQuestion : Nat -> PacketLang
dnsQuestion n = do qnames <- LISTN n dlString -- List of length-indexed domain name segments
                   nullterm -- Null terminator
                   qtype <- bits 16 -- Code specifying type of query. TODO: verification
                   check (validQTYPE (value qtype))
                   qclass <- bits 16 -- Specifies class of query
                   check (validQCLASS (value qclass))

dnsHeader : PacketLang
dnsHeader = do ident <- bits 16 -- Request identifier
               qr <- bits 1 -- Query or response 
               opcode <- bits 4 -- Which type of query? Only 0, 1 and 2 valid
               aa <- bits 1 -- Only set in response, is responding server authority?
               tc <- bits 1 -- Was message truncated?
               rd <- bits 1 -- Recursion desired, set in query, copied into response
               ra <- bits 1 -- Recursion available; is support available in NS?
               z  <- bits 1 -- Must be 0.
               bits 4 -- Response code, only 0-5 valid

-- DNS Resource Record
-- The same for answers, authorities and additional info.
dnsRR : PacketLang
dnsRR = do domain <- LIST dlString
           nullterm 
           ty <- bits 16
           check (validTYPE (value ty))
           cls <- bits 16
           check (validCLASS (value cls))
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
           bits 32 -- FIXME: Temp. Need proper verification here...
           --let vl = value len
           --prf <- check (vl > 0)
           --CHUNK (Bit vl prf)
           --bits ((value len) * 8) prf -- Data payload. This is a tad more complex, TODO: more verification
                             -- although RFC is pretty unspecific on this, 
                             -- and it's generally just 4-byte IP

dns : PacketLang
dns = do header <- dnsHeader
         -- Technically these are parts of the header, but we 
         -- need them in scope for later
         qdcount <- bits 16 -- Number of entries in question section
         ancount <- bits 16 -- Number of entries in the answer section
         nscount <- bits 16 -- Number of NS resource records in auth records
         arcount <- bits 16 -- Number of resource records in additional records section
         question <- dnsQuestion (intToNat (value qdcount))
         answer <- dnsRR
         authority <- dnsRR
         dnsRR -- Additional




--dnsRR : 
