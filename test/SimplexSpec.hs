module SimplexSpec where

import SpecHelper
import Simplex

import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = hspec $ do
    spec

spec :: Spec
spec = describe "linear real arithmetic" $ do
    it "satisfiable" $ do
        let form = Normal
                (M.fromList [
                    (Slack 1, M.fromList [(Var "x", 1), (Var "y", 1)]),
                    (Slack 2, M.fromList [(Var "x", 2), (Var "y",-1)]),
                    (Slack 3, M.fromList [(Var "x",-1), (Var "y", 2)])
                ])
                (M.fromList [(Var "x", (Min, Max))
                            ,(Var "y", (Min, Max))
                            ,(Slack 1, (Val 2, Max))
                            ,(Slack 2, (Val 0, Max))
                            ,(Slack 3, (Val 1, Max))])
                (M.fromList [(Var "x", 0)
                            ,(Var "y", 0)
                            ,(Slack 1, 0)
                            ,(Slack 2, 0)
                            ,(Slack 3, 0)])
            in check form `shouldBe`
            Just (M.fromList [
                (Var "x", 1.0),
                (Var "y", 1.0)
            ])
    it "satisfiable" $ do
        let form = Normal
                (M.fromList [
                    (Slack 1, M.fromList [(Var "x", -1), (Var "y", 1)]),
                    (Slack 2, M.fromList [(Var "x",  1), (Var "y", 1)])
                ])
                (M.fromList [
                    (Var "x", (Min, Max)),
                    (Var "y", (Min, Max)),
                    (Slack 1, (Min, Max)),
                    (Slack 2, (Min, Max))
                ])
                (M.fromList [
                    (Var "x", 0),
                    (Var "y", 0),
                    (Slack 1, 0),
                    (Slack 2, 0)
                ])
            in check form `shouldBe`
            Just (M.fromList [
                (Var "x", 0),
                (Var "y", 0)
            ])