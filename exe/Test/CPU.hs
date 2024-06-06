{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.CPU where

import System.IO.Unsafe
import Control.Monad.State
import Data.Aeson (FromJSON)
import Data.Array
import Data.Word
import GHC.Generics
import Shrimp.MOS6502
import qualified Shrimp.Memory as M
import Text.Printf

data CPUState = CPUState
    { pc :: Word16
    , s :: Word8
    , a :: Word8
    , x :: Word8
    , y :: Word8
    , p :: Word8
    , ram :: [(Word16, Word8)]
    }
    deriving (Generic, FromJSON)

data Test = Test
    { name :: String
    , initial :: CPUState
    , final :: CPUState
    , cycles :: [(Word16, Word8, String)]
    }
    deriving (Generic, Show, FromJSON)

data Barebones = Barebones
    { bCpu :: MOS6502 ()
    , bRam :: M.RAM
    , bLog :: [(Word16, Word8, String)]
    }

instance Show CPUState where
    show cpustate = "YOU HAVE TO IMPLEMENT SHOW."

pushLog :: (Word16, Word8, String) -> Barebones -> Barebones
pushLog info barebones = barebones{bLog = (bLog barebones) ++ [info]}


cpuInterface :: M.RAM -> Interface ()
cpuInterface ram = Interface readByte writeByte peekByte where
    readByte _ addr = (\x -> ((), x)) <$> M.readByte ram addr
    writeByte _ addr byte = M.writeByte ram addr byte
    peekByte _ addr = snd <$> (readByte () addr)

loadMemory :: [(Word16, Word8)] -> IO M.RAM
loadMemory [] = M.new (1024 * 64) 0
loadMemory (y : ys) = do
    m <- loadMemory ys
    let (addr, byte) = y
    M.writeByte m addr byte
    return m

loadCPU :: Interface () -> CPUState -> MOS6502 ()
loadCPU interface (CPUState rpc rs ra rx ry rp _) = mos{registers = reg, context = (context mos){decMode = True}} where
    mos = new interface
    reg = Registers{
                  Shrimp.MOS6502.pc = rpc,
                  sp = rs,
                  acc = ra,
                  idx = rx,
                  idy = ry,
                  ps = rp
              }

bLoad :: CPUState -> IO Barebones
bLoad cpustate = do
    ram <- loadMemory $ ram cpustate
    let interface = cpuInterface ram
    let cpu = loadCPU interface cpustate
    return $ Barebones cpu ram []

bTick :: Barebones -> IO Barebones
bTick barebones = do
    (mos', _) <- execStateT tick (bCpu barebones, ())
    return barebones{bCpu = mos'}

verifyRam :: M.RAM -> [(Word16, Word8)] -> IO Bool
verifyRam ram [] = return $ True
verifyRam ram (y : ys) = do
    that <- verifyRam ram ys
    let (addr, byte) = y
    byte' <- M.readByte ram addr 
    let this = byte == byte'
    return $ this && that

verifyCPU :: MOS6502 () -> CPUState -> Bool
verifyCPU mos6502 cpustate = condition where
    pc_condition  = (Test.CPU.pc cpustate) == (Shrimp.MOS6502.pc  . registers $ mos6502) 
    sp_condition  = (Test.CPU.s  cpustate) == (sp  . registers $ mos6502)
    acc_condition = (Test.CPU.a  cpustate) == (acc . registers $ mos6502)
    x_condition   = (Test.CPU.x  cpustate) == (idx . registers $ mos6502)
    y_condition   = (Test.CPU.y  cpustate) == (idy . registers $ mos6502) 
    ps_condition  = (Test.CPU.p  cpustate) == (ps  . registers $ mos6502)
    condition = pc_condition
            && sp_condition
            && acc_condition
            && x_condition
            && y_condition
            && ps_condition

verify :: Barebones -> CPUState -> IO Bool
verify barebones final_state = do
    vram <- verifyRam (bRam barebones) (ram final_state)
    let vcpu = verifyCPU (bCpu barebones) final_state
    return $ vram && vcpu
