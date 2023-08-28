module Main where

import Data.Aeson
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word
import Numeric
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

testFile :: FilePath -> IO ()
testFile filepath = do
    tests <- loadJSON filepath
    hTest tests

testFiles :: [FilePath] -> IO ()
testFiles [] = return ()
testFiles (y : ys) = do
    testFile y
    testFiles ys

testTilFail :: [Test] -> IO ()
testTilFail [] = return ()
testTilFail (y : ys) = do
    if test y
        then do
            testTilFail ys
        else do
            putStrLn $ show . name $ y
            showDiff y

printFails :: [Test] -> IO ()
printFails [] = return ()
printFails (y : ys) = do
    if test y
        then do
            printFails ys
        else do
            putStrLn $ show . name $ y
            showDiff y
            printFails ys

showDiff :: Test -> IO ()
showDiff t = do
    let init_state = initial t
    let final_state = final t
    let b = bLoad init_state
    let prediction = bTick b
    let cyc = cycles t
    putStrLn $ "Success: " ++ show (verify prediction final_state)
    putStrLn $ show init_state
    putStrLn $ show final_state
    putStrLn $ showLog cyc
    putStrLn $ ""
    putStrLn $ show prediction

showPaddedHex x = (padHex $ showHex x "")
  where
    padHex :: String -> String
    padHex str
        | length str == 1 = '0' : str
        | otherwise = str

main :: IO ()
main = do
    let opcodes = [0x69, 0x65, 0x75, 0x6D, 0x7D, 0x79, 0x61, 0x71, 0x29, 0x25, 0x35, 0x2D, 0x3D, 0x39, 0x21, 0x31, 0x0A, 0x06, 0x16, 0x0E, 0x1E, 0x90, 0xB0, 0xF0, 0x24, 0x2C, 0x30, 0xD0, 0x10, 0x00, 0x50, 0x70, 0x18, 0xD8, 0x58, 0xB8, 0xC9, 0xC5, 0xD5, 0xCD, 0xDD, 0xD9, 0xC1, 0xD1, 0xE0, 0xE4, 0xEC, 0xC0, 0xC4, 0xCC, 0xC6, 0xD6, 0xCE, 0xDE, 0xCA, 0x88, 0x49, 0x45, 0x55, 0x4D, 0x5D, 0x59, 0x41, 0x51, 0xE6, 0xF6, 0xEE, 0xFE, 0xE8, 0xC8, 0x4C, 0x6C, 0x20, 0xA9, 0xA5, 0xB5, 0xAD, 0xBD, 0xB9, 0xA1, 0xB1, 0xA2, 0xA6, 0xB6, 0xAE, 0xBE, 0xA0, 0xA4, 0xB4, 0xAC, 0xBC, 0x4A, 0x46, 0x56, 0x4E, 0x5E, 0xEA, 0x09, 0x05, 0x15, 0x0D, 0x1D, 0x19, 0x01, 0x11, 0x48, 0x08, 0x68, 0x28, 0x2A, 0x26, 0x36, 0x2E, 0x3E, 0x6A, 0x66, 0x76, 0x6E, 0x7E, 0x40, 0x60, 0xE9, 0xE5, 0xF5, 0xED, 0xFD, 0xF9, 0xE1, 0xF1, 0x38, 0xF8, 0x78, 0x85, 0x95, 0x8D, 0x9D, 0x99, 0x81, 0x91, 0x86, 0x96, 0x8E, 0x84, 0x94, 0x8C, 0xAA, 0xA8, 0xBA, 0x8A, 0x9A, 0x98]
    let files = fmap (\x -> "./exe/Test/TomHarte/6502/v1/" ++ showPaddedHex x ++ ".json") opcodes
    testFiles files
