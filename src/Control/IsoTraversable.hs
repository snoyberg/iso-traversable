{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeFamilies           #-}
module Control.IsoTraversable where

import           Control.Lens
import           Control.Monad         (liftM)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as S8
import           Data.Char             (toUpper)
import qualified Data.Foldable         as F
import qualified Data.Map              as Map
import           Data.Monoid           (Monoid)
import qualified Data.Traversable      as T
import qualified Data.Vector           as V
import           Data.Word             (Word8)
import           Prelude               hiding (concatMap, map, mapM, mapM_)

-- | Defines an isomorphism between the types @a@ @t e@.
class Traversable t => IsoTraversable a e t | a -> e t where
    isoTraversable :: Iso' a (t e)

instance IsoTraversable S.ByteString Word8 [] where
    isoTraversable = iso S.unpack S.pack
instance IsoTraversable [a] a [] where
    isoTraversable = iso id id
instance IsoTraversable (Map.Map k v) v (Map.Map k) where
    isoTraversable = iso id id
instance IsoTraversable (V.Vector e) e V.Vector where
    isoTraversable = iso id id
-- Can't define an instance for @Set@, since there is no isomorphism for @Set@ which
-- is an instance of @Traversable@.

-- | This class is only used to aid type inference.
class MapRelation a a' e e' | a e' -> a', a' e -> a, a -> e, a' -> e'
instance MapRelation [e] [e'] e e'
instance MapRelation S.ByteString S.ByteString Word8 Word8
instance MapRelation (Map.Map k e) (Map.Map k e') e e'
instance MapRelation (V.Vector e) (V.Vector e') e e'

toIso :: IsoTraversable a e t => a -> t e
toIso = view isoTraversable

fromIso :: IsoTraversable a e t => t e -> a
fromIso = view (from isoTraversable)

map :: (IsoTraversable a e t, IsoTraversable a' e' t, MapRelation a a' e e')
    => (e -> e')
    -> a
    -> a'
map f = fromIso . fmap f . toIso

mapM :: (IsoTraversable a e t, IsoTraversable a' e' t, MapRelation a a' e e', Monad m)
     => (e -> m e')
     -> a
     -> m a'
mapM f = liftM fromIso . T.mapM f . toIso

mapM_ :: (IsoTraversable a e t, Monad m)
      => (e -> m ignored)
      -> a
      -> m ()
mapM_ f = F.mapM_ f . view isoTraversable

concatMap :: (Monoid a', IsoTraversable a e t, MapRelation a a' e e')
          => (e -> a')
          -> a
          -> a'
concatMap f = F.fold . fmap f . view isoTraversable

{-# RULES

    "toIso/id" forall x. toIso x = x;
    "fromIso/id" forall x. fromIso x = x;

    "map ByteString" forall (f :: Word8 -> Word8) (bs :: S.ByteString). map f bs = S.map f bs;
    "concatMap ByteString" forall (f :: Word8 -> S.ByteString) (bs :: S.ByteString). concatMap f bs = S.concatMap f bs;

  #-}

main :: IO ()
main = do
    S8.putStrLn $ map (+ 1) ("foo" :: S.ByteString)
    putStrLn $ map toUpper ("bar" :: String)
    mapM (return . (+ 1)) ("baz" :: S.ByteString) >>= S8.putStrLn
    mapM_ (S8.putStrLn . S.singleton) ("bin" :: S.ByteString)
    S8.putStrLn $ concatMap (S.replicate 3) "foo"
    print $ map toUpper $ Map.fromList [(1 :: Int, 'b'), (2, 'a'), (3, 'r')]
    V.mapM_ print $ map (+ 1) $ V.enumFromTo (1 :: Int) 3