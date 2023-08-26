module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Word
import System.FilePath (FilePath)
import Test.CPU
import Test.Hspec

loadJSON :: FilePath -> IO [Test]
loadJSON filePath = do
    json <- B.readFile filePath
    let decoded = eitherDecode json
    case decoded of
        Left err_msg -> error ("Could not parse JSON file to Tests data: " ++ err_msg)
        Right output -> return output

main :: IO ()
main = do
    json <- loadJSON "/home/lesserfish/Documents/Code/Shrimp/exe/Test/TomHarte/6502/v1/00.json"
    putStrLn $ show (json !! 0)
    return ()
