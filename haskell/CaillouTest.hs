import Caillou.Circuit
import Caillou.Arithmetic
import Caillou.Patterns
import Caillou.NetlistGen
import Netlist.Print
import Netlist.AST

main = printProgToFile (synthesizeNetlistAST serialAdder (Avar "a", Avar "b") ["a","b"]
                                             (\s -> [("s", s)]))
                       "serial-adder.net"

