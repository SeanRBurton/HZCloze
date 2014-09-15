{-# LANGUAGE OverloadedStrings, TupleSections #-}

import Data.Char (isSpace)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import qualified Data.Map.Lazy as M
import Data.Text.ICU.Char (blockCode, BlockCode(CJKUnifiedIdeographs))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL.IO
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.FilePath.Posix (takeBaseName)
import System.Process (system)
import System.Random (StdGen, next, split, randoms, getStdGen)

temp :: String -> String -> String
temp category filePath = "Temp" </> takeBaseName filePath ++ "-" ++ category ++ ".tmp"

isHanzi :: Char -> Bool
isHanzi = (==CJKUnifiedIdeographs) . blockCode

hasHanzi :: [TL.Text] -> [TL.Text]
hasHanzi = filter (TL.any isHanzi)
                      
sentences :: TL.Text -> [TL.Text]
sentences = hasHanzi . map TL.strip . TL.split (\c -> c == '.' || c == '。') . TL.filter (not . isSpace)

uniqueElems :: (Ord a, Eq a) => [a] -> [a]
uniqueElems = map head . group . sort

parseSentences :: FilePath -> FilePath -> IO [TL.Text]
parseSentences from to = do body <- TL.IO.readFile from
                            let sentences' =  uniqueElems $ sentences body
                            TL.IO.writeFile to $ TL.unlines sentences'
                            return sentences'
                            
segmenterPath :: String 
segmenterPath = "stanford-segmenter-2014-08-27"

segmentCmd :: String -> String -> String
segmentCmd from to = "." </> segmenterPath </> "segment.sh ctb " ++ from ++ " utf8 0 | tee " ++ to

segment  :: FilePath -> FilePath -> IO [[TL.Text]]
segment from to = system (segmentCmd from to) >> fmap (map TL.words . TL.lines) (TL.IO.readFile to)

data FlashCard = FlashCard {word :: Maybe TL.Text,
                            unsegmented :: Maybe TL.Text,
                            segmented :: Maybe [TL.Text],
                            clozed :: Maybe TL.Text,
                            pronunciation :: Maybe TL.Text} deriving (Show)

blankFlashcard :: FlashCard
blankFlashcard = FlashCard Nothing Nothing Nothing Nothing Nothing

occurences :: [(TL.Text, [TL.Text])] -> M.Map TL.Text [FlashCard]
occurences = foldl insertSentence M.empty 
    where insertSentence :: M.Map TL.Text [FlashCard] -> (TL.Text, [TL.Text]) -> M.Map TL.Text [FlashCard]
          insertSentence m (unseg, seg) = foldl occursIn m seg
              where occursIn m w = M.insertWith (++) w [blankFlashcard{word=Just w, unsegmented=Just unseg, segmented=Just seg}] m

byFrequency :: (Ord k, Eq k) => M.Map k [a] -> [(k, [a])]
byFrequency = sortBy (compare `on` (negate . length . snd)) . M.toList

shuffle :: StdGen -> [a] -> [a]
shuffle g xs = map fst . sortBy (compare `on` snd) . zip xs $ (randoms g :: [Int])

partialShuffle :: StdGen -> Int -> [a] -> [a]
partialShuffle g maxJump xs = map (\(x, _ , _) -> x) . 
                              sortBy (\(_, r, i) (_, r', i') -> if abs (i' - i) > maxJump then compare i i' else compare r r') $ 
                              zip3 xs (randoms g :: [Int]) [0..]
                                
makeCards :: [TL.Text] -> [[TL.Text]] -> [(TL.Text, [FlashCard])]
makeCards sentences' segmentedSentences = byFrequency . occurences . zip sentences' $ map hasHanzi segmentedSentences

cloze :: Maybe (TL.Text -> TL.Text -> TL.Text) -> FlashCard -> FlashCard
cloze hint card = card {clozed = Just $ TL.replace word' clozeText text'}
    where hint' = case hint of 
                      (Just f) -> "::" `TL.append` (f word' text')
                      Nothing  -> "" 
          clozeText = TL.concat ["{{c1::", word', hint', "}}"]
          Just word' = word card
          Just text' = unsegmented card
          
clozeAll :: Maybe (TL.Text -> TL.Text -> TL.Text) -> [(TL.Text, [FlashCard])] -> [(TL.Text, [FlashCard])]
clozeAll hint = map (fmap $ map (cloze hint))

gens :: StdGen -> [StdGen]
gens g = let (a, b) = split g in a : gens b

shuffleAll :: StdGen -> [(TL.Text, [FlashCard])] -> [(TL.Text, [FlashCard])]
shuffleAll g cardss = map (\((word, cards), g') -> (word, shuffle g' cards)) . zip cardss $ gens g

reduceAll :: Int -> [(TL.Text, [FlashCard])] -> [(TL.Text, [FlashCard])]
reduceAll k = map (fmap (take k)) 

flatten :: [(TL.Text, [FlashCard])] -> [FlashCard]

main :: IO ()                      
main = do [filePath] <- getArgs
          let sentenceFile = temp "sentences" filePath
          let segmentFile  = temp "segmentedSentences" filePath
          sentences' <- parseSentences filePath sentenceFile
          segmentedSentences  <- segment sentenceFile segmentFile
          g <- getStdGen
          let cards = reduceAll 10 . shuffleAll g . clozeAll Nothing $ makeCards sentences' segmentedSentences
          TL.IO.writeFile (takeBaseName filePath ++ "-output.txt") . TL.unlines $ concatMap snd cards
          return ()
