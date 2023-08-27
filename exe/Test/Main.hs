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

test :: Test -> Bool
test t = status
  where
    init_state = initial t
    final_state = final t
    barebones = bLoad init_state
    barebones' = bTick barebones
    status = verify barebones' final_state

hTest' :: Test -> Spec
hTest' t = describe "MOS6502 CPU Test" $ do
    let n = name t
    it (show n) $ do
        test t `shouldBe` True

hTest :: [Test] -> IO ()
hTest [] = return ()
hTest (y : ys) = do
    hspec $ hTest' y
    hTest ys

main :: IO ()
main = do
    tests <- loadJSON "/home/lesserfish/Documents/Code/Shrimp/exe/Test/TomHarte/6502/v1/00.json"
    hTest tests
