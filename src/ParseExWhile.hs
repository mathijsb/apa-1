import CCO.Component    (printer, ioWrap)
import CCO.ExWhile      (parser)
import CCO.Tree         (fromTree)
import Control.Arrow    (arr, (>>>))

main = ioWrap (parser >>> arr fromTree >>> printer)
