{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Registry (
    Registry (..),
    new,
    globalRegistry,
    register,
    registerTo,
    unregisterFrom,
    ToCollector,

    -- * Sampling
    RegistrySample,
    sample,
) where

import Control.Monad (forM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import Prometheus.V3.Collector (Collector (..))
import Prometheus.V3.Metric.Base (
    IsMetric,
    Metric (..),
    MetricName,
    getMetricSamples,
    getMetricType,
 )
import Prometheus.V3.Sample (Sample)
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.IORef (IORef, modifyIORef, newIORef, readIORef)


newtype Registry = Registry (IORef (Map MetricName Collector))


new :: (MonadIO m) => m Registry
new = Registry <$> newIORef Map.empty


globalRegistry :: Registry
globalRegistry = unsafePerformIO new
{-# OPAQUE globalRegistry #-}


class ToCollector a where
    type RegisterResult a
    getCollectorName :: a -> MetricName
    toCollector :: a -> IO (Collector, RegisterResult a)
instance ToCollector Collector where
    type RegisterResult Collector = Collector
    getCollectorName = (.name)
    toCollector collector = pure (collector, collector)
instance (IsMetric a) => ToCollector (Metric a) where
    type RegisterResult (Metric a) = a
    getCollectorName = (.name)
    toCollector metric = do
        a <- metric.initialize
        let collector =
                Collector
                    { name = metric.name
                    , description = metric.description
                    , type_ = getMetricType (Proxy @a)
                    , getSamples = getMetricSamples a
                    }
        pure (collector, a)


-- | Register the given metric with the global registry.
--
-- Only safe to use with top-level variables, which must be annotated with OPAQUE.
register :: (ToCollector a) => a -> RegisterResult a
register = unsafePerformIO . registerTo globalRegistry
{-# INLINE register #-}


-- | Register the given metric with the given registry.
registerTo :: (ToCollector a) => Registry -> a -> IO (RegisterResult a)
registerTo (Registry registryMapRef) a = do
    (collector, res) <- toCollector a
    modifyIORef registryMapRef (Map.insert collector.name collector)
    pure res


-- | Unregister the given metric from the given registry.
unregisterFrom :: (ToCollector a) => Registry -> a -> IO ()
unregisterFrom (Registry registryMapRef) a = do
    modifyIORef registryMapRef (Map.delete (getCollectorName a))


type RegistrySample = Map MetricName (Collector, [Sample])


sample :: (MonadIO m) => Registry -> m RegistrySample
sample (Registry registryMapRef) = do
    registryMap <- readIORef registryMapRef
    liftIO . forM registryMap $ \collector -> do
        samples <- collector.getSamples
        -- TODO: revalidate labels with same logic as Labelled, for custom collectors
        pure (collector, samples)
