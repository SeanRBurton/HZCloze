{-# LANGUAGE OverloadedStrings, TupleSections, BangPatterns #-}

import Data.Char (isSpace)
import Data.Text.ICU.Char (blockCode, BlockCode(CJKUnifiedIdeographs))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL.IO
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Posix (takeBaseName)
import System.Process (system)

segmenterPath :: String 
segmenterPath = "stanford-segmenter-2014-08-27"

temp :: String -> String -> String
temp category filePath = "Temp" </> takeBaseName filePath ++ "-" ++ category ++ ".tmp"

segment_cmd :: String -> String -> String
segment_cmd from to = "." </> segmenterPath </> "segment.sh ctb " ++ from ++ " utf8 0 | tee " ++ to

segment  :: FilePath -> FilePath -> IO TL.Text
segment from to = do system $ segment_cmd from to
                     segmented <- TL.IO.readFile to
                     return segmented
                                      
isHanzi :: Char -> Bool
isHanzi = (==CJKUnifiedIdeographs) . blockCode
                      
sentences :: TL.Text -> [TL.Text]
sentences = filter (TL.any isHanzi) . map (TL.strip) . TL.split (\c -> c == '.' || c == '。') . TL.filter (not . isSpace)

preprocess :: FilePath -> FilePath -> IO [TL.Text]
preprocess from to = do body <- TL.IO.readFile from
                        let sentences' = sentences body
                        TL.IO.writeFile to $ TL.unlines sentences'
                        return sentences'
                                            
main :: IO ()                      
main = do [filePath] <- getArgs
          let sentenceFile = temp "sentences" filePath
          let segmentFile  = temp "segmented-sentences" filePath
          sentences' <- preprocess filePath sentenceFile
          segmented  <- segment sentenceFile segmentFile
          return ()
          

