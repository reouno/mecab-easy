{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module MeCab
    (
      wakati
    , wakatiN -- use NEologd
    , parseToNodes
    , parseToNodesN -- use NEologd
    --, Splittable
    , MeCabWord (..)
    ) where

import  qualified Text.MeCab as M
import qualified Data.Text as DT
import qualified Data.ByteString as DB
import Data.List.Split (splitOn)
import Data.String (IsString)

class Splittable s where
    splitOn' :: s -> s -> [s]

instance Splittable [Char] where
    splitOn' sep str = splitOn sep str

instance Splittable DT.Text where
    splitOn' sep str = DT.splitOn sep str

instance Splittable DB.ByteString where
    splitOn' sep str = DB.split (DB.head sep) str


data M.MeCabString s => MeCabWord s = MeCabWord
    { surface :: s
    , class0 :: s
    , class1 :: s
    , class2 :: s
    , class3 :: s
    , conjType :: s
    , conjForm :: s
    , baseForm :: s
    , reading :: s
    , pronouncing :: s
    } deriving (Eq, Read, Show)


-- 分かち書きして、半角スペース区切りの文字列を返す
-- なぜか最後に改行が入る
wakati :: M.MeCabString s => s -> IO s
wakati str = M.new2 "-O wakati -l2" >>= parse' str
    where
        parse' :: M.MeCabString s => s -> M.MeCab -> IO s
        parse' = flip M.parse

wakatiN :: M.MeCabString s => String -> s -> IO s
wakatiN path str = M.new2 ("-O wakati -l2 -d " ++ path) >>= parse' str
    where
        parse' :: M.MeCabString s => s -> M.MeCab -> IO s
        parse' = flip M.parse

parseToNodes :: (IsString s, Splittable s, M.MeCabString s)
             => s -> IO [MeCabWord s]
parseToNodes text = do
    tagger <- M.new2 ""
    result <- M.parseToNodes tagger text
    return . map nodeToWord $ result

parseToNodesN :: (IsString s, Splittable s, M.MeCabString s)
             => String -> s -> IO [MeCabWord s]
parseToNodesN path text = do
    tagger <- M.new2 ("-d " ++ path)
    result <- M.parseToNodes tagger text
    return . map nodeToWord $ result

--nodeToWord :: M.MeCabString s -> MeCabWord s
nodeToWord :: (IsString s, Splittable s, M.MeCabString s)
           => M.Node s -> MeCabWord s
nodeToWord node = MeCabWord surface' fs0 fs1 fs2 fs3 fs4 fs5 fs6 fs7 fs8
    where
        surface' = case M.nodeStat node of
            M.BOS -> "MECAB_BOS"
            M.EOS -> "MECAB_EOS"
            M.EON -> "MECAB_EON"
            _     -> M.nodeSurface node
        fs = splitOn' "," . M.nodeFeature $ node
        [fs0,fs1,fs2,fs3,fs4,fs5,fs6,fs7,fs8] = case length fs of
            7 -> fs ++ ["",""]
            9 -> fs
            _ -> replicate 9 ""
