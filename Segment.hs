{-# LANGUAGE OverloadedStrings, TupleSections #-}

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL.IO
import Data.List
import Data.Function (on)
import Data.Text.ICU.Char
import CJK.Data.Pinyin
import CJK.Data.Unihan.Readings
import System.FilePath ((</>))
import System.Process 
import System.Directory

data FlashCard = FlashCard {word :: T.Text,
                            segmented :: [T.Text],
                            unsegmented :: T.Text,
                            pronunciation :: T.Text,
                            definition :: T.Text,
                            cloze :: T.Text} deriving (Show)
                            
segmenterPath :: String 
segmenterPath = "stanford-segmenter-2014-08-27"

segment  :: FilePath -> IO TL.Text
segment filePath = do system ("." </> segmenterPath </> "segment.sh ctb " ++ filePath ++ " utf8 0 > /dev/null | tee temp.seg") 
                      segmented <- TL.IO.readFile ("Temp" </> "seg.temp")
                      return segmented
                      
writeSentences :: String -> [TL.Text] -> IO()
writeSentences path = TL.IO.writeFile (path ++ "-temp" </> "sentences.temp") $ unlines sentences

sentencesFromFile :: String -> IO()
sentencesFromFile = 
                    
sentences :: TL.Text -> [TL.Text]
sentences = TL.splitWhen (\c -> c == '.' || c == "。")
                      
isHanzi :: Char -> Bool
isHanzi = (==CJKUnifiedIdeographs) . blockCode

chineseWords :: TL.Text -> [TL.Text]
chineseWords = filter (TL.all $ \c -> c == ' ' || isHanzi c) . TL.words
             
occurences :: [(TL.Text, [TL.Text])] -> M.Map TL.Text [TL.Text]
occurences = foldl (\m (unsegmented, segmented) -> foldl (occursIn unsegmented) m segmented) M.empty 
    where occursIn sentence m word = M.insertWith (const (sentence:)) word [] m
  
reading :: TL.Text -> TL.Text
reading = TL.fromStrict . T.concatMap (toAccented . head . mandarinBestEffort) . TL.toStrict
          
cloze :: TL.Text -> TL.Text -> TL.Text -> TL.Text
cloze hint w s = TL.replace w clozeText s
    where hint' = if TL.null hint 
                      then hint 
                      else "::" `TL.append` hint 
          clozeText = TL.concat ["{{c1::", w, hint', "}}"]

flashCards :: M.Map TL.Text [TL.Text] -> [TL.Text]
flashCards =  map snd . concatMap (\(a, b) -> map (a,) $ take 10 b) . sortBy (compare `on` (negate . length . snd)) . M.toList 

muddle :: Int
muddle = undefined
         
main :: IO ()
main = segment "blah" >>= (TL.IO.writeFile "out.txt" . TL.unlines . flashCards . occurences)
          
          
                            
                      
  
                      
