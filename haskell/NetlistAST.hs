import Data.Array
import qualified Data.Map as Map

type Ident = String
type Environment t = Map.Map Ident t

data Ty = TBit | TBitArray Int
data Value = VBit Bool | VBitArray (Array Bool)

data Binop = Or | Xor | And | Nand


-- argument of operators (variable or constant) 
data Arg = Avar Ident -- x 
         | Aconst Value -- constant 

-- Expressions (see MiniJazz documentation for more info on the operators) 
data Exp = Earg Arg -- a: Argument 
         | Ereg Ident -- REG x : register 
         | Enot Arg -- NOT a 
         | Ebinop Binop Arg Arg -- OP a1 a2 : boolean operator 
         | Emux Arg Arg Arg -- MUX a1 a2 : multiplexer 

         -- ROM addr_size word_size read_addr 
         | Erom Int -- addr size
                Int -- word size
                Arg -- read_addr
         -- RAM addr_size word_size read_addr write_enable write_addr data 
         | Eram Int -- addr size
                Int -- word size
                Arg -- read_addr
                Arg -- write_enable
                Arg -- write_addr
                Arg -- data
         | Econcat Arg Arg -- CONCAT a1 a2 : concatenation of arrays 
           -- SLICE i1 i2 a : extract the slice of a between indices i1 and i2 
         | Eslice Int Int Arg
           -- SELECT i a : ith element of a 
         | Eselect Int Arg

-- equations: x = exp 
type Equation = (Ident, Exp)

data Program = { p_inputs  :: [Ident]        -- inputs 
               , p_outputs :: [Ident]        -- outputs 
               , p_vars    :: Environment Ty -- maps variables to their types
               , p_eqs     :: [Equation]     -- equations 
               } 

