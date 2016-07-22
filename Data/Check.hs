-- |
-- Module      :  Data.Check
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Checking and normalization of data.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Data.Check
  ( Checker
  , normalizer
  , normalizerM
  , checker
  , checkerM
  , runChecker )
where

import Control.Monad
import Data.Semigroup
import Data.Set (Set)
import Numeric.Natural
import qualified Data.Set as S

data Checker m e a where
  Checker :: Monad m => Set (Normalizer m a) -> Set (Validator m e a) -> Checker m e a

data Normalizer m a  where
  Normalizer :: Monad m => Natural -> (a -> m a) -> Normalizer m a

instance Eq (Normalizer m a) where
  (Normalizer x _) == (Normalizer y _) = x == y

instance Ord (Normalizer m a) where
  (Normalizer x _) `compare` (Normalizer y _) = x `compare` y

data Validator m e a where
  Validator :: Natural -> (a -> m (Either e a)) -> Validator m e a

instance Eq (Validator m e a) where
  (Validator x _) == (Validator y _) = x == y

instance Ord (Validator m e a) where
  (Validator x _) `compare` (Validator y _) = x `compare` y

normalizer :: Monad m => Natural -> (a -> a) -> Checker m e a
normalizer n f = normalizerM n (return . f)

normalizerM :: Monad m => Natural -> (a -> m a) -> Checker m e a
normalizerM n f = Checker (S.singleton $ Normalizer n f) S.empty

checker :: Monad m => Natural -> (a -> Either e a) -> Checker m e a
checker n f = checkerM n (return . f)

checkerM :: Monad m => Natural -> (a -> m (Either e a)) -> Checker m e a
checkerM n f = Checker S.empty (S.singleton $ Validator n f)

instance Semigroup (Checker m e a) where
  (Checker ns vs) <> (Checker ns' vs') = Checker (S.union ns ns') (S.union vs vs')

instance Monad m => Monoid (Checker m e a) where
  mempty = Checker S.empty S.empty
  mappend = (<>)

runChecker :: Monad m => Checker m e a -> a -> m (Either e a)
runChecker (Checker ns vs) = n >=> v
  where
    n = appEndo (foldMap (Endo . nf) . S.toDescList $ ns) . return
    nf (Normalizer _ f) = (>>= f)
    v = appEndo (foldMap (Endo . vf) . S.toDescList $ vs) . return . return
    vf (Validator _ f) m = do
      x <- m
      case x of
        Left  e -> return (Left e)
        Right a -> f a
