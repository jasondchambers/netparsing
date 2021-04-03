-- Network Element parsing library. Provides parsers for all sorts of
-- things such as IP addresses, MAC addresses, CIDR ranges, IP6 addresses
-- Indicators of Compromise and so on.
-- 
-- Extends the Functional parsing library [1] from Professor Graham Hutton
-- Be sure to watch the video featuring the good Professor himself
-- to learn more about Functional (or Combinator) parsing.
-- 
-- [1] - http://www.cs.nott.ac.uk/~pszgmh/Parsing.hs
-- [2] - https://www.youtube.com/watch?v=dDtZLm7HIJs&t=1194s
--
-- (c) 2021, Jason Chambers

-- ipaddress ::= octet.octet.octet.octet

import Parsing

isOctet :: Int -> Bool
isOctet n = n <= 255

octet :: Parser String
octet = do xs <- some digit
           if (read xs) <= 255 then
              return (xs)
           else empty


ipaddress :: Parser String
ipaddress = do a <- octet
               char '.'
               b <- octet
               char '.'
               c <- octet
               char '.'
               d <- octet
               return (a ++ "." ++ b ++ "." ++ c ++ "." ++ d)
