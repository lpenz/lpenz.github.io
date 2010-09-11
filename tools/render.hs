
import RendererLib
import System

main :: IO ()
main = do
    [src, dst] <- getArgs
    renderT2T id src dst


