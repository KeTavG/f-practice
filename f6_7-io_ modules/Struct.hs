module Struct where

data TDataStruct a = DataStruct a deriving (Show, Eq, Ord)

instance Functor TDataStruct where
    fmap f (DataStruct a) = DataStruct (f a)

instance Applicative TDataStruct where
    pure a = DataStruct a
    (DataStruct f) <*> (DataStruct a) = DataStruct (f a)
