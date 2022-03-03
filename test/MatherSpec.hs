module MatherSpec where

import SpecHelper
import Mather

main :: IO ()
main = hspec $ do
    spec

spec :: Spec
spec = describe "conjunctive normal form" $ do
    it "already NNF" $ do
        isNNF (And [Or [Atom "A", Atom "B"], Atom "C"])                                `shouldBe` True
        isNNF (Or [And [Atom "A", Or [Not $ Atom "B", Atom "C"], Atom "C"], Atom "D"]) `shouldBe` True
        isNNF (Or [Atom "A", Not $ Atom "B"])                                          `shouldBe` True
        isNNF (And [Atom "A", Not $ Atom "B"])                                         `shouldBe` True

    it "becomes NNF" $ do
        toNNF (Atom "A" :=> Atom "B")
            `shouldBe` (Or [Not $ Atom "A", Atom "B"])
        toNNF (Not $ Or [Atom "A", Atom "B", Not $ Atom "C"])
            `shouldBe` (And [Not $ Atom "A", Not $ Atom "B", Atom "C"])
        toNNF (Not $ Not $ Not $ Not $ Not $ Atom "A")
            `shouldBe` (Not $ Atom "A")

    it "already CNF" $ do
        isCNF (And [Or [Atom "A", Not $ Atom "B", Not $ Atom "C"], Or [Not $ Atom "D", Atom "E", Atom "F"]]) `shouldBe` True
        isCNF (And [Or [Atom "A", Atom "B"], Atom "C"]) `shouldBe` True
        isCNF (Or [Atom "A", Atom "B"]) `shouldBe` True
        isCNF (Atom "A") `shouldBe` True

    it "becomes CNF" $ do
        toCNF (Not $ Or [Atom "B", Atom "C"])
            `shouldBe` (And [Not $ Atom "B", Not $ Atom "C"])
        toCNF (Or [And [Atom "A", Atom "B"], Atom "C"])
            `shouldBe` (And [Or [Atom "A", Atom "C"], Or [Atom "B", Atom "C"]])
        toCNF (And [Atom "A", Or [Atom "B", And [Atom "D", Atom "E"]]])
            `shouldBe` (And [Atom "A", Or [Atom "B", Atom "D"], Or [Atom "B", Atom "E"]])
