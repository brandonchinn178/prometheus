{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module Prometheus.V3.Metric.Labelled (
    Labelled (..),
    withLabels,
    labels,
) where

import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef)
import Data.Proxy (Proxy (..))
import Prometheus.V3.Label (
    IsLabelValueTuple,
    LabelTupleNames,
    toLabelNameList,
    toLabelValueList,
 )
import Prometheus.V3.Metric.Base
import Prometheus.V3.Sample (Sample (..))
import Prometheus.V3.Utils.InitMap (InitMap)
import qualified Prometheus.V3.Utils.InitMap as InitMap


data Labelled l a = Labelled
    { labelNames :: LabelTupleNames l
    , initChild :: IO a
    , metricMapRef :: IORef (InitMap l a)
    }


withLabels ::
    forall l a.
    (IsMetric a, IsLabelValueTuple l) =>
    LabelTupleNames l ->
    [l] ->
    Metric a ->
    Metric (Labelled l a)
withLabels labelNames initialLabels metric =
    validateLabelNames
        metric
            { initialize = do
                metricMap <-
                    fmap InitMap.fromList . sequence $
                        [ (vals,) <$> metric.initialize
                        | vals <- initialLabels
                        ]
                metricMapRef <- newIORef metricMap
                pure
                    Labelled
                        { labelNames
                        , initChild = metric.initialize
                        , metricMapRef
                        }
            }
  where
    -- TODO: error on duplicate labels
    validateLabelNames =
        case filter (not . isValid) (toLabelNameList labelNames) of
            [] -> id
            invalidNames -> error $ "Invalid label names: " <> show invalidNames

    isValid = isValidMetricLabel (Proxy @a)


labels :: (Hashable l) => l -> Labelled l a -> IO a
labels labelVals labelled =
    InitMap.getOrCreate
        labelVals
        labelled.initChild
        labelled.metricMapRef


instance (IsMetric a, IsLabelValueTuple l) => IsMetric (Labelled l a) where
    getMetricType _ = getMetricType (Proxy @a)
    getMetricSamples labelled = do
        metricMap <- readIORef labelled.metricMapRef
        labelledSamples <-
            sequence
                [ (labelVals,) <$> getMetricSamples child
                | (labelVals, child) <- InitMap.toList metricMap
                ]
        pure
            [ sample
                { labels = zip labelNameList (toLabelValueList labelVals) <> sample.labels
                }
            | (labelVals, samples) <- labelledSamples
            , sample <- samples
            ]
      where
        labelNameList = toLabelNameList labelled.labelNames
