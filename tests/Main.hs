{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Data.Check
import Data.Monoid
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "when two normalizers have the same priority" $
    it "the left one overrides the right one" $
      property $ \x -> do
        runChecker (addOneNorm <> addTwoNorm) x `shouldBe` Right (x + 1)
        runChecker (addTwoNorm <> addOneNorm) x `shouldBe` Right (x + 2)
  context "when two validators have the same priority" $
    it "the left one overrides the right one" $
      property $ \x -> do
        runChecker (validatorTrue <> validatorFalse) x `shouldBe` Left True
        runChecker (validatorFalse <> validatorTrue) x `shouldBe` Left False
  context "adding mempty to the max" $
    it "has no effect" $
      property $ \x -> do
        runChecker (addOneNorm <> mempty) x `shouldBe` Right (x + 1)
        runChecker (mempty <> addOneNorm) x `shouldBe` Right (x + 1)
        runChecker (validatorTrue <> mempty) x `shouldBe` Left True
        runChecker (mempty <> validatorTrue) x `shouldBe` Left True
  context "when using several normalizers" $
    it "they are applied, and applied in order" $
      property $ \x -> do
        runChecker (addOneNorm <> mulThreeNorm) x
          `shouldBe` Right (x * 3 + 1)
        runChecker (addThreeNorm <> mulThreeNorm) x
          `shouldBe` Right ((x + 3) * 3)
  context "when using several validators" $
    it "they are run, and run in order" $
      property $ \x ->
        runChecker (validatorGT50 <> validatorLT100) x
          `shouldBe` if | x <= 50   -> Left False
                        | x >= 100  -> Left True
                        | otherwise -> Right x
  context "when we have both normalizers and validators" $
    it "normalizers are run before validators and their output is used" $
      property $ \x ->
        runChecker (mulThreeNorm <> validatorGT50) x
          `shouldBe` if x * 3 > 50
                       then Right (x * 3)
                       else Left False
  it "normalizers and validators can run inside a monad" $
    property $ \x ->
      runCheckerM (breakingNorm <> addOneNorm <> validatorLT100) x
        `shouldBe` Nothing

----------------------------------------------------------------------------
-- Collection of test normazilers and validators

addOneNorm :: Monad m => Checker m Bool Int
addOneNorm = normalizer 3 (+ 1)

addTwoNorm :: Monad m => Checker m Bool Int
addTwoNorm = normalizer 3 (+ 2)

mulThreeNorm :: Monad m => Checker m Bool Int
mulThreeNorm = normalizer 2 (* 3)

addThreeNorm :: Monad m => Checker m Bool Int
addThreeNorm = normalizer 1 (+ 3)

breakingNorm :: Checker Maybe Bool Int
breakingNorm = normalizerM 0 (const Nothing)

validatorTrue :: Monad m => Checker m Bool Int
validatorTrue = validator 3 (const $ return True)

validatorFalse :: Monad m => Checker m Bool Int
validatorFalse = validator 3 (const $ return False)

validatorGT50 :: Monad m => Checker m Bool Int
validatorGT50 = validator 3 $ \x ->
  if x > 50 then Nothing else Just False

validatorLT100 :: Monad m => Checker m Bool Int
validatorLT100 = validator 4 $ \x ->
  if x < 100 then Nothing else Just True
