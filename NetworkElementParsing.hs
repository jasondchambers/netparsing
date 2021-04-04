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
-- MAC Address is a 12-digit hexadecimal number (6-Byte binary number), which is mostly represented by Colon-Hexadecimal notation
-- macaddress := oui<macdelimiter><hexdigit><hexdigit><macdelimiter><hexdigit><hexdigit><macdelimiter><hexdigit><hexdigit>
-- oui := <hexdigit><hexdigit><macdelimiter><hexdigit><hexdigit><macdelimiter><hexdigit><hexdigit>
-- macdelimiter := - | :
-- 8c:85:90:07:25:a3

import Parsing
import Data.Char

isOctet :: Int -> Bool
isOctet n = n <= 255

isMacDelimiter :: Char -> Bool
isMacDelimiter c = c == ':' || c == '-'

hexdigit :: Parser Char
hexdigit = sat isHexDigit

macdelimiter :: Parser Char
macdelimiter = sat isMacDelimiter

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

macaddress :: Parser String
macaddress = do nib1 <- hexdigit
                nib2 <- hexdigit
                delim <- macdelimiter
                nib3 <- hexdigit
                nib4 <- hexdigit
                delim <- macdelimiter
                nib5 <- hexdigit
                nib6 <- hexdigit
                delim <- macdelimiter
                nib7 <- hexdigit
                nib8 <- hexdigit
                delim <- macdelimiter
                nib9 <- hexdigit
                nib10 <- hexdigit
                delim <- macdelimiter
                nib11 <- hexdigit
                nib12 <- hexdigit
                return ([nib1] ++ [nib2] ++ [delim] ++ [nib3] ++ [nib4] ++ [delim] ++ [nib5] ++ [nib6] ++ [delim] ++ 
                        [nib7] ++ [nib8] ++ [delim] ++ [nib9] ++ [nib10] ++ [delim] ++ [nib11] ++ [nib12])
