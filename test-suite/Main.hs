{-# LANGUAGE OverloadedStrings #-}
-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import qualified Data.Text as DT
import qualified MeCab

main :: IO ()
main = do
    test <- testSpec "mecab-easy" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    -- 分かち書きすると最後に改行が入るようだ
    it "output of wakati" $ do
        text <- MeCab.wakati ("コンニチハ、世界。" :: DT.Text)
        text `shouldBe` ("コンニチハ 、 世界 。 \n" :: DT.Text)

    it "wakatiN with NEologd (ignore if this case fail because of NEologd path)" $ do
        text <- MeCab.wakatiN "/usr/local/lib/mecab/dic/mecab-ipadic-neologd" ("コンニチハ、世界。" :: DT.Text)
        text `shouldBe` ("コンニチハ 、 世界 。 \n" :: DT.Text)

    -- 最初はBOS、最後はEOSだから単語数＋２になる
    it "is length parseToNodes ok" $ do
        results <- MeCab.parseToNodes ("コンニチハ、" :: DT.Text)
        length results `shouldBe` 4

    it "parseToNodesN with NEologd (ignore if this case fail because of NEologd path)" $ do
        results <- MeCab.parseToNodesN "/usr/local/lib/mecab/dic/mecab-ipadic-neologd" ("コンニチハ、" :: DT.Text)
        length results `shouldBe` 4

    -- 最初はBOS
    it "is surface field of MeCabWord record ok" $ do
        results <- MeCab.parseToNodes ("コンニチハ" :: DT.Text)
        MeCab.surface (head results) `shouldBe` ("MECAB_BOS" :: DT.Text)

    it "are all fields of MeCabWord record ok" $ do
        results <- MeCab.parseToNodes ("コンニチハ" :: DT.Text)
        head results `shouldBe` MeCab.MeCabWord "MECAB_BOS" "BOS/EOS" "*" "*" "*" "*" "*" "*" "*" "*"
