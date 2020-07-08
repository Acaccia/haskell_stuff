import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    
    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.
    
    [input, lightx, lighty, intialx, initialy] <- map read . words <$> getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    
    -- game loop
    forever $ do
        input_line <- getLine
        let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.
        
        let ns = case compare initialx lightx of
                    LT -> "N"
                    GT -> "S"
                    EQ -> ""
        let ew = "" 
        
        putStrLn (ns ++ ew)
