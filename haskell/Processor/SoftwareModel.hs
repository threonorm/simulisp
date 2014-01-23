module Processor.SoftwareModel (main) where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IntMap

import Lisp.SCode
import Lisp.Primitives
import Processor.Parameters
import Processor.Microcode hiding (main)
import Processor.MicroAssembler
import Simulator.DisplayClock
import Util.BinUtils

data WordTag = T Tag | R ReturnTag deriving (Eq, Show)
data WordData = N Int | P (Word, Word)
              | PClock -- quick and dirty hack to handle the only global pointer
              deriving (Eq, Show)
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
                P ((T TApplyOne, P (localVar 0 1, (T (TPrim PPrintMin), N 0))),
                   (T TSequence,
                    P ((T TApplyOne, P (localVar 0 0, (T (TPrim PPrintSec), N 0))),
                       (T TLet,
                        P ((T TApplyOne, P (localVar 0 0, (T (TPrim PIncr), N 0))),
                           (T TSequence,
                            P ((T TApplyOne, P (localVar 0 0, (T (TPrim PIsgt60), N 0))),
                               (T TCond,
                                P ((T TSync,
                                    P ((T TFirst,
                                        P ((T TApplyOne, P (localVar 1 1, (T (TPrim PIncr), N 0))),
                                           (T TLast,
                                            P ((T TNum, N 0),
                                               (T TGlobal, PClock))))),
                                        (T TNil, N 0))),
                                   (T TSync,
                                    P ((T TFirst,
                                        P (localVar 1 1,
                                           (T TLast,
                                            P (localVar 0 0,
                                               (T TGlobal, PClock))))),
                                        (T TNil, N 0)))))))))))))

mainProgram = (T TFirst, P ((T TNum, N 0),
                            (T TLast, P ((T TNum, N 0),
                                         (T TGlobal, PClock)))))



microcode :: IntMap MicroInstruction
microcode = IntMap.fromList . secondPass . firstPass $ microprogram

bools2int = foldl' (\acc b -> 2*acc + if b then 1 else 0) 0 . reverse

-- simulate a binary adder

aluCompute :: ALUOp -> Immediate -> Int -> (Bool, Int)

aluCompute ALUIncr _ n = (False, n+1) -- overflow -> what happens?

aluCompute ALUDecrImmediate (ImmN imm) n = (n < imm, max (n - imm) 0)

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
  numCycles <- newIORef (0 :: Int)

  forever $ do
    modifyIORef numCycles (+1)
    
    ctr <- readIORef ctrRef
    let jumpTo = writeIORef ctrRef
        incrCtr = jumpTo $ ctr + 1
    case microcode ! ctr of
      
      Jump { jumpIsConditional = cond, jumpAddress = addr } -> do
        printLog $ "jump " ++ show addr
        if cond
          then do b <- readIORef condReg
                  printLog $ "conditional jump: " ++ show b
                  if b then jumpTo addr else incrCtr
          else jumpTo addr
             
      Dispatch register suffix -> do
        (tag, _) <- readIORef (regs register)
        let bin = case tag of T t -> tagBin t
                              R r -> returnBin r
        let addr = bools2int $ replicate (microAddrS - tagS - 2) False ++ bin ++ suffix
        printLog $ "dispatch " ++ show (bin ++ suffix)
        jumpTo addr
        
      ExtInstr instr -> do
        printLog "exti"
        
        wordRead@(tagRead, dataRead) <- readIORef . regs . regRead $ instr
        let ~(N intRead) = dataRead
            ~(P ptrRead) = dataRead
            
        if interactWithOutside instr
          then case boolsToInt (outsideOpcode instr) of
          7 -> do
            nc <- readIORef numCycles
            writeIORef numCycles 0
            printLog $ show nc ++ " cycles since the previous second."
            waitNextSec commands
          3 -> setHour   commands intRead
          2 -> setMinute commands intRead
          1 -> setSecond commands intRead
          _ -> return ()

          else do
            let dest | writeTemp instr = Temp
                     | otherwise       = regWrite instr
                writeOn = writeReg instr || writeTemp instr
                writeToDest x
                  | writeOn = do printLog $ show dest ++ " := " ++ show x
                                 writeIORef (regs dest) x
                  | otherwise = return ()
            if useGC instr
              -- operation by GC 
              then do let opcode = gcOpcode instr
                          alloc = opcode == GCAlloc
                          carOrCdr = opcode == GCFetchCdr
                      if alloc
                        then do car <- readIORef $ regs Temp
                                let cdr = wordRead
                                let tag = case immediate instr of
                                      ImmT t -> T t
                                      ImmR r -> R r
                                      ImmN _ -> error "wrong kind of immediate"
                                writeToDest (tag, P (car, cdr))
                        else if wordRead == (T TGlobal, PClock) -- HACK! 1
                             then writeToDest clockProgram
                             else if regRead instr == Null
                                  then writeToDest mainProgram -- HACK! 2
                                  else do
                                    let (car, cdr) = ptrRead
                                    writeToDest $ if carOrCdr then cdr else car
              -- operation by ALU     
              else if aluCtrl instr == ALUNop
                   then writeToDest wordRead
                   else do let (flag, result) = aluCompute (aluCtrl instr)
                                                           (immediate instr)
                                                           intRead
                           writeIORef condReg flag
                           writeToDest (T TNum, N result)


            when (loadCondReg instr) $ do
              printLog $ "conditional on tag " ++ show tagRead
              writeIORef condReg $ tagRead /= T TNil

        -- don't forget this at the end!      
        incrCtr

printLog :: String -> IO ()
printLog _ = return ()
-- printLog = putStrLn

