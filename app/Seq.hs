module Main
   ( main )
where
import Lib
import qualified Data.ByteString.Char8 as B
import qualified System.Environment as Env
import qualified System.Exit as Exit

main :: IO ()
main = do
   args <- Env.getArgs
   if length args /= 2 then do
      n <- Env.getProgName
      Exit.die $ "Usage: " ++ n ++ "<answer> <filename>"
   else do
      let a = head args
      if length a /= 5 then do
         Exit.die "Starting guess must be length 5"
      else do
         let f = args !! 1
         f' <- B.readFile f
         let gSet = (getValidWords . B.words) f'
             k = initKnowledge
             a' = B.pack a
             g = B.pack "raise"
         print g
         playSeq g a' k gSet 1