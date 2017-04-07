-- |
-- Module      :  Data.Check
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a generalized approach to checking and verification
-- of data. It's useful, for example, for validation of fields on web forms.
--
-- Typically, there are a number of transformations and checks you may want
-- to perform on a particular type of data, such as text. Thus, it makes
-- sense to create all those transformations and checks once and then
-- combine them to get more complex validators that may vary on per-field
-- basis.
--
-- Certainly, if we can normalize and validate, we should normalize first.
-- However, if we have many normalizing operations, we need a way to specify
-- in which order they should be performed, or result can be unpredictable.
--
-- To specify the order in which transformations are performed, 'normalizer'
-- and 'normalizerM' functions take a “priority” argument, which is just a
-- 'Natural' number. The bigger the number, the later the function will be
-- applied, so the transformation with priority 0 will always run first.
--
-- This method applies to validators just as well. It's possible to create a
-- vocabulary of validators that can be mixed together and the result will
-- be always deterministic.
--
-- To support more real-world use cases, every check can be performed inside
-- of a monad, allowing to query a database for example.
--
-- One last thing to note is that every normalizer and every validator
-- should have a unique priority number. Normalizers (and validators) with
-- the same priority will overwrite each other. This is by design. Note that
-- normalizer won't override validator with the same priority though, their
-- priority-spaces are separate.

{-# LANGUAGE GADTs #-}

module Data.Check
  ( -- * Normalizers
    normalizer
  , normalizerM
    -- * Validators
  , validator
  , validatorM
    -- * Checkers
  , Checker
  , runChecker
  , runCheckerM )
where

import Control.Monad
import Data.Functor.Identity
import Data.Semigroup
import Data.Set (Set)
import Numeric.Natural
import qualified Data.Set as S

----------------------------------------------------------------------------
-- Normalizers

-- | @'Normalizer' m a@ is a normalizer that works on values of type @a@ in
-- monad @m@.

data Normalizer m a  where
  Normalizer :: Monad m => Natural -> (a -> m a) -> Normalizer m a

instance Eq (Normalizer m a) where
  (Normalizer x _) == (Normalizer y _) = x == y

instance Ord (Normalizer m a) where
  (Normalizer x _) `compare` (Normalizer y _) = x `compare` y

-- | Create a normalizing 'Checker'. Every normalizer has a priority — the
-- bigger the number, the later the normalizer runs. Every normalizer you
-- use should have a unique priority number.

normalizer :: Monad m
  => Natural           -- ^ Priority
  -> (a -> a)          -- ^ Normalizing transformation
  -> Checker m e a     -- ^ Normalizing 'Checker'
normalizer n f = normalizerM n (return . f)

-- | The same as 'normalizer', but allows to perform normalization inside of
-- a monad.

normalizerM :: Monad m
  => Natural           -- ^ Priority
  -> (a -> m a)        -- ^ Normalizing transformation
  -> Checker m e a     -- ^ Normalizing 'Checker'
normalizerM n f = Checker (S.singleton $ Normalizer n f) S.empty

----------------------------------------------------------------------------
-- Validators

-- | @'Validator' m e a@ is a validator that checks values of type @a@,
-- works in @m@ monad, and can return @e@ messages on failure.

data Validator m e a where
  Validator :: Natural -> (a -> m (Maybe e)) -> Validator m e a

instance Eq (Validator m e a) where
  (Validator x _) == (Validator y _) = x == y

instance Ord (Validator m e a) where
  (Validator x _) `compare` (Validator y _) = x `compare` y

-- | Create a validating 'Checker'. Every validator has a priority — the
-- bigger the number, the later the validation step runs. Every validator
-- you use should have a unique priority number.

validator :: Monad m
  => Natural           -- ^ Priority
  -> (a -> Maybe e)    -- ^ 'Nothing' if everything is OK
  -> Checker m e a     -- ^ Validating 'Checker'
validator n f = validatorM n (return . f)

-- | The same as 'validator', but allows to perform normalization inside of
-- a monad.

validatorM :: Monad m
  => Natural           -- ^ Priority
  -> (a -> m (Maybe e)) -- ^ 'Nothing' if everything is OK
  -> Checker m e a     -- ^ Validating 'Checker'
validatorM n f = Checker S.empty (S.singleton $ Validator n f)

----------------------------------------------------------------------------
-- Checkers

-- | @'Checker' m e a@ is a checker that checks value of type @a@, can
-- perform the check in @m@ monad, returning @e@ message when check fails.
--
-- 'Checker' is a 'Semigroup' and 'Monoid' — this is how you combine
-- different checkers and build more complex ones.

data Checker m e a where
  Checker :: Monad m
    => Set (Normalizer m a)
    -> Set (Validator m e a)
    -> Checker m e a

instance Semigroup (Checker m e a) where
  (Checker ns vs) <> (Checker ns' vs') = Checker (S.union ns ns') (S.union vs vs')

instance Monad m => Monoid (Checker m e a) where
  mempty = Checker S.empty S.empty
  mappend = (<>)

-- | Run a 'Checker' on given value. This is version for cases when all
-- transformations and validations are pure.

runChecker
  :: Checker Identity e a -- ^ The 'Checker' to run
  -> a                 -- ^ Value to check
  -> Either e a        -- ^ Result, 'Right' on success, 'Left' on failure
runChecker c x = runIdentity (runCheckerM c x)

-- | Version of 'runChecker' that can run transformations and checks in any
-- monad.

runCheckerM :: Monad m
  => Checker m e a     -- ^ The 'Checker' to run
  -> a                 -- ^ Value to check
  -> m (Either e a)    -- ^ Result, 'Right' on success, 'Left' on failure
runCheckerM (Checker ns vs) = n >=> \a -> maybe (Right a) Left <$> v a
  where
    n = appEndo (foldMap (Endo . nf) . S.toDescList $ ns) . return
    nf (Normalizer _ f) = (>>= f)
    v a = appEndo (foldMap (Endo . vf a) . S.toDescList $ vs) (return Nothing)
    vf a (Validator _ f) m = do
      x <- m
      case x of
        Nothing -> f a
        Just  e -> return (Just e)
