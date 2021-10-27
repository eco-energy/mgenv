{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric, GeneralisedNewtypeDeriving, DeriveAnyClass, DerivingStrategies, DerivingVia, DeriveFunctor, DeriveTraversable, DeriveFoldable, StandaloneDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes, GADTs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeOperators #-}
{-# LANGUAGE ConstraintKinds, KindSignatures, ScopedTypeVariables, TypeApplications #-}

module Physics.Time (hence, henceUF
                    , absToUTC, T', T, IntervalT
                    , unfoldT, delT, timelines
                    , runTimelines, Timelines
                    ) where
import ConCat.Category
import ConCat.Nat
import GHC.Generics hiding (Rep, R, L, C)
import ConCat.Misc
import ConCat.Synchronous
import ConCat.RAD

import Data.Time.Clock.POSIX.Compat (posixSecondsToUTCTime)
import qualified Data.Time as T

import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Stream.IsStream as S
import Data.Functor.Rep

import Data.Void
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Data.Unfold as UF
import qualified Streamly.Internal.Data.Pipe as P
import Streamly.Internal.Data.Pipe (Pipe(..))

import Streamly.Internal.Data.Time.Units
import qualified Streamly.Internal.Data.Time.Clock as Clk

import Foreign.Storable
import Prelude hiding (const, id, (.), curry, uncurry)

type Rate = Double

data IntervalT = IntervalT
  { rate :: !Rate
  , t0 :: !AbsTime
  , dt :: !RelTime
  } deriving (Eq, Ord, Show, Generic)

type T' = AbsTime :* RelTime

type T m = UF.Unfold m IntervalT T'


type Timelines m = Pipe m IntervalT (UF.Unfold m Void T')

hence :: forall m. (S.MonadAsync m) => IntervalT -> S.SerialT m T'
hence IntervalT{rate, t0, dt} = S.delayPre (1 / rate)
  $ S.iterate (\(t', _) -> (addToAbsTime t' dt, dt)) (t0, dt)

timelines :: forall m. S.MonadAsync m => Timelines m
timelines = P.map (\iv -> UF.supply iv henceUF)

runTimelines :: forall t m a. (S.IsStream t, S.MonadAsync m)
  => Pipe m T' a -> t m IntervalT -> t m a
runTimelines p s =  S.concat $ f s'
  where
    f :: t m (UF.Unfold m Void T') -> t m (t m a)
    f = S.map (S.transform p) . S.map S.unfold0
    s' :: t m (UF.Unfold m Void T')
    s' = S.transform timelines s

henceUF :: (S.MonadAsync m) => T m
henceUF = UF.many (UF.function (hence)) UF.fromStream

absToUTC :: AbsTime -> T.UTCTime
absToUTC (AbsTime tspec) = posixSecondsToUTCTime . fromIntegral . sec $ tspec

unfoldT :: (S.MonadAsync m) => UF.Unfold m T' a -> UF.Unfold m IntervalT a
unfoldT uf = UF.many henceUF uf

delT :: forall m p a s. (Num s, S.MonadAsync m)
  => (RelTime -> p s -> s)
  -> UF.Unfold m IntervalT (p s)
  -> UF.Unfold m IntervalT (T' :* (p s))
delT f ps = UF.zipWith (\t p -> (t, dfdt f t p)) henceUF ps
{-# INLINE delT #-}

dfdt :: forall p s. (Num s) => (RelTime -> p -> s) -> T' -> Unop p
dfdt f = delT' . f . snd
{-# INLINE dfdt #-}
    
delT' :: (Num s) => (p -> s) -> Unop p
delT' = (gradR $)
{-# INLINE delT' #-}
