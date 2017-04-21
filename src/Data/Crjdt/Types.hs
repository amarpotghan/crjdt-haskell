{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Crjdt.Types where

import Test.SmallCheck.Series
import Data.String
import Data.Text
import Data.Void

newtype Var = Variable { getName :: Text } deriving (Show, Eq, Ord)

instance Monad m => Serial m Var where
  series = newtypeCons (Variable .  pack)

instance IsString Var where
  fromString = Variable . fromString

data TaggedKey tag = TK
  { tag :: !tag
  , key :: !BasicKey
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Serial m tag => Serial m (TaggedKey tag) where
  series = cons2 TK

instance Eq tag => Ord (TaggedKey tag) where
  compare (TK _ k1) (TK _ k2) = k1 `compare` k2

-- TODO: rethink about this type
data Key tag where
  Key :: BasicKey -> Key Void
  TaggedKey :: TaggedKey tag -> Key tag

instance Monad m => Serial m (Key Void) where
  series = Key <$> series

data BasicKey
  = DocKey
  | Head
  | Tail
  | I Id
  | Str Text
  deriving (Show, Eq, Ord)

instance Monad m => Serial m BasicKey where
  series = cons0 DocKey \/ cons0 Head \/ cons0 Tail \/ cons1 I \/ cons1 (Str . pack)

deriving instance Eq tag => Ord (Key tag)

instance IsString (Key Void) where
  fromString = Key . Str . fromString

instance Show tag => Show (Key tag) where
  show (Key t) = show t
  show (TaggedKey taggedKey) = show taggedKey

instance Eq tag => Eq (Key tag) where
  (Key t) == (Key t1) = t == t1
  (TaggedKey t) == (TaggedKey t1) = t == t1
  (Key _) == (TaggedKey _) = False
  (TaggedKey _) == (Key _) = False

type ReplicaId = Integer
type GlobalReplicaCounter = Integer

data Id = Id
  { sequenceNumber :: GlobalReplicaCounter
  , replicaNumber :: ReplicaId
  } deriving (Show, Eq, Ord)

instance Monad m => Serial m Id where
  series = cons2 Id
