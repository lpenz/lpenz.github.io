
import RenderLib
import System

main :: IO ()
main = do
    [tpfile, src, dst] <- getArgs
    renderT2T id tpfile src dst


