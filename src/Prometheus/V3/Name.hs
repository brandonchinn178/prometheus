{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Prometheus.V3.Name (
    Name (..),
    IsName (..),
    makeName,
    NeedsEscape (..),
    EscapeScheme (..),
    getNameRaw,
    showName,
) where

import Data.Char (isDigit)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text


data Name = Name Text NeedsEscape
    deriving (Show)


instance Eq Name where
    Name s1 _ == Name s2 _ = s1 == s2
instance Ord Name where
    Name s1 _ `compare` Name s2 _ = s1 `compare` s2


getNameRaw :: Name -> Text
getNameRaw (Name s _) = s


class IsName a where
    getName :: a -> Name
    isValidLegacyChar :: Proxy a -> Char -> Bool


isValidLegacyHeadChar :: (IsName a) => Proxy a -> Char -> Bool
isValidLegacyHeadChar proxy c =
    isValidLegacyChar proxy c && (not . isDigit) c


makeName :: (IsName a) => Proxy a -> Text -> Name
makeName proxy s = Name s needsEscape
  where
    needsEscape =
        case Text.uncons s of
            Just (c, cs)
                | isValidLegacyHeadChar proxy c
                , Text.all (isValidLegacyChar proxy) cs ->
                    NoNeedsEscape
            _ -> NeedsEscape


-- | A flag to precompute whether a name needs escaping or not.
-- This way, in the happy path where names don't need escaping, we
-- validate the name once at registration and encoding the name is
-- immediate.
data NeedsEscape = NeedsEscape | NoNeedsEscape
    deriving (Show)


-- | https://prometheus.io/docs/instrumenting/escaping_schemes/
data EscapeScheme
    = -- | escaping=allow-utf-8
      NoEscaping
    | -- | escaping=underscores
      EscapingUnderscores
    | -- | escaping=dots
      EscapingDots
    | -- | escaping=values
      EscapingValues


showName :: forall a. (IsName a) => EscapeScheme -> a -> Text
showName scheme a =
    case needsEscape of
        NoNeedsEscape -> name
        NeedsEscape -> escapeName (Proxy @a) scheme name
  where
    Name name needsEscape = getName a


escapeName :: (IsName a) => Proxy a -> EscapeScheme -> Text -> Text
escapeName proxy = \case
    NoEscaping -> id
    EscapingUnderscores -> escapeUnderscores proxy
    EscapingDots -> error "escaping=dots is not yet supported" -- TODO
    EscapingValues -> error "escaping=values is not yet supported" -- TODO


escapeUnderscores :: (IsName a) => Proxy a -> Text -> Text
escapeUnderscores proxy s = prefix <> Text.map replace s
  where
    prefix =
        case Text.uncons s of
            Just (c, _) | isValidLegacyHeadChar proxy c -> ""
            _ -> "_"
    replace c = if isValidLegacyChar proxy c then c else '_'
