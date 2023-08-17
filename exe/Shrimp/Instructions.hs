module Shrimp.Instructions where
import Shrimp.CPU
import Shrimp.Memory

newtype Instruction a :: ADDR_MODE -> (CPUMonad a) ()


opINX :: Instruction
opINX addr_mode = do
    return ()
