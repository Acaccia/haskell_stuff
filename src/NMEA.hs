module NMEA where
import Data.Bits(xor)
import Data.List(foldl')
import Data.List.Split(splitOn)
import Numeric(readHex)

check :: String -> Bool
check ('$':nmea) = let [beg, end] = splitOn "*" nmea
                       xored = foldl' (\acc x -> acc `xor` fromEnum x) 0 beg
                   in case readHex end of
                        [(chksum, _)] -> xored == chksum
                        _             -> False
