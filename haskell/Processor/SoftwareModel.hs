module Processor.SoftwareModel (main) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Array.IArray (Array, (!))
import qualified Data.Array.IArray as Array

import Processor.Parameters
import Simulator.DisplayClock

data WordTag = T Tag | R ReturnTag deriving (Eq)
data WordData = N Int | P (Word, Word)
              | PClock -- quick and dirty hack to handle the only global pointer
              deriving (Eq)
type Word = (WordTag, WordData)

-- Imperative-style Haskell: yes, it's possible!

type RegArray = Reg -> IORef Word

initRegs :: IO RegArray
initRegs = do
  let init = newIORef (T TNil, N 0)
  n <- init
  v <- init
  x <- init
  e <- init
  a <- init
  s <- init
  t <- init
  let f Null = n
      f Value = v
      f Expr = x
      f Env = e
      f Args = a
      f Stack = s
      f Temp = t
  return f



clockProgram :: Word
clockProgram = (T TSequence,
                P ((T TApplyOne, P (localVar 0 1, (T TPrintMin, N 0))),
                   (T TSequence,
                    P ((T TApplyOne, P (localVar 0 0, (T TPrintSec, N 0))),
                       (T TLet,
                        P ((T TApplyOne, P (localVar 0 0, (T TIncr, N 0))),
                           (T TSequence,
                            P ((T TApplyOne, P (localVar 0 0, (T TIsgt60, N 0))),
                               (T TCond,
                                P ((T TSync,
                                    P ((T TFirst,
                                        P ((T TApplyOne, P (localVar 1 1, (T TIncr, N 0))),
                                           (T TLast,
                                            P ((T TNum, N 0),
                                               (T TGlobal, PClock))))),
                                        (T TNil, N 0))),
                                   (T TSync,
                                    P ((T TFirst,
                                        P ((T TApplyOne, P (localVar 1 1, (T TIncr, N 0))),
                                           (T TLast,
                                            P ((T TNum, N 0),
                                               (T TGlobal, PClock))))),
                                        (T TNil, N 0)))))))))))))

mainProgram = (T TFirst, P ((T TNum, N 0),
                            (T TLast, P ((T TNum, N 0),
                                         (T TGlobal, PClock)))))

microcode :: Array Int MicroInstruction
microcode = undefined

bools2int = foldl' (\acc b -> 2*acc + if b then 1 else 0) 0 . reverse

-- simulate a binary adder

aluCompute :: ALUOp -> Immediate -> Int -> (Bool, Int)

aluCompute ALUIncr _ n = (False, n+1) -- overflow -> what happens?
aluCompute ALUDecr _ 0 = (True, 0) -- pred(0) = 0
aluCompute ALUDecr _ n = (False, n-1)

aluCompute ALUCompareImmediate (ImmN imm) n = (n > imm, 42)

aluCompute ALUDecrUpper _ n | hi == 0   = (True, lo)
                            | otherwise = (False, (hi-1)*upperWeight + lo)
  where hi = n `div` upperWeight
        lo = n `mod` upperWeight

upperWeight = 2^10 -- the 10 is arbitrary, change later

localVar k l = (T TLocal, N $ k * upperWeight + l)

main :: IO ()
main = displayClock . makeCommandThread $ \commands -> do
  regs <- initRegs
  condReg <- newIORef False
  ctrRef <- newIORef (0 :: Int)
  forever $ do
    ctr <- readIORef ctrRef
    let jumpTo = writeIORef ctrRef
        incrCtr = jumpTo $ ctr + 1
    case microcode ! ctr of
      
      Jump { jumpIsConditional = cond, jumpAddress = addr } ->
        if cond
        then do b <- readIORef condReg
                if b then jumpTo addr else incrCtr
        else jumpTo addr
             
      Dispatch register suffix -> do
        (tag, _) <- readIORef (regs register)
        let bin = case tag of T t -> tagBin t
                              R r -> returnBin r
        let addr = bools2int $ bin ++ suffix
        jumpTo addr
        
      ExtInstr instr -> do
        wordRead@(tagRead, dataRead) <- readIORef . regs . regRead $ instr
        let ~(N intRead) = dataRead
            ~(P ptrRead) = dataRead
            
        if interactWithOutside instr
          then case outsideOpcode instr of
          [True,  True ] -> waitNextSec commands
          [False, True ] -> setHour   commands intRead
          [True , False] -> setMinute commands intRead
          [False, False] -> setSecond commands intRead

          else do
          when (writeReg instr || writeTemp instr) $ do
            let dest | writeTemp instr = Temp
                     | otherwise       = regWrite instr
                writeToDest = writeIORef (regs dest)
            if useGC instr
              -- operation by GC 
              then do let [alloc, carOrCdr] = gcOpcode instr
                      if alloc
                        then do car <- readIORef $ regs Temp
                                let cdr = wordRead
                                let tag = case immediate instr of
                                      ImmT t -> T t
                                      ImmR r -> R r
                                      ImmN _ -> error "wrong kind of immediate"
                                writeToDest (tag, P (car, cdr))
                        else if wordRead == (T TGlobal, PClock) -- HACK!
                             then writeToDest clockProgram
                             else do let (car, cdr) = ptrRead
                                     writeToDest $ if carOrCdr then cdr else car
              -- operation by ALU     
              else if aluCtrl instr == ALUNop
                   then writeToDest wordRead
                   else do let (flag, result) = aluCompute (aluCtrl instr)
                                                           (immediate instr)
                                                           intRead
                           writeIORef condReg flag
                           writeToDest (tagRead, N result)

          when (loadCondReg instr) $ do
            writeIORef condReg $ tagRead /= T TNil

        -- don't forget this at the end!      
        incrCtr
