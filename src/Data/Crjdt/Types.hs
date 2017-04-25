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
  , getKey :: !BasicKey
  } deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show tag => Show (TaggedKey tag) where
  show (TK t k) = show (t, k)

instance Serial m tag => Serial m (TaggedKey tag) where
  series = cons2 TK

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

instance (Ord tag, Eq tag) => Ord (Key tag) where
  (Key k) `compare` (Key k1) = k `compare` k1
  (TaggedKey k) `compare` (TaggedKey k1) = k `compare` k1

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

newtype Id = Id { getId :: (GlobalReplicaCounter, ReplicaId) } deriving (Eq, Ord)

instance Show Id where
  show = show . getId

sequenceNumber = fst . getId
replicaNumber = snd . getId

mkId sn rid = Id (sn, rid)

instance Monad m => Serial m Id where
  series = newtypeCons Id

tagWith :: tag -> BasicKey -> Key tag
tagWith t = TaggedKey . TK t

getTag :: Key tag -> tag
getTag (TaggedKey (TK t _)) = t

basicKey :: Key tag -> BasicKey
basicKey (Key k) = k
basicKey (TaggedKey (TK _ k)) = k

unTag :: Key tag -> Key Void
unTag (Key k) = Key k
unTag (TaggedKey (TK _ k)) = Key k

reTag :: a -> Key Void -> Key a
reTag given (Key k) = tagWith given k
reTag given (TaggedKey (TK _ k)) = tagWith given k
