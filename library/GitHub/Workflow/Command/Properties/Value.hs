module GitHub.Workflow.Command.Properties.Value
  ( Value
  , toByteStringBuilder
  , ToValue
  , toValue
  ) where

import Control.Category
import Data.Aeson qualified as Aeson
import Data.Aeson.Text qualified as Aeson
import Data.ByteString.Builder qualified as BSB
import Data.Monoid qualified as Monoid
import Data.String (IsString)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Prelude (Eq, Ord, Show)

newtype Value = Value {json :: Aeson.Value}
  deriving newtype (Eq, Ord, Show, IsString, Aeson.ToJSON)

type ToValue = Aeson.ToJSON

toValue :: ToValue a => a -> Value
toValue = Value . Aeson.toJSON

toByteStringBuilder :: Value -> BSB.Builder
toByteStringBuilder =
  TL.encodeUtf8Builder
    . TL.concatMap
      ( \case
          '%' -> "%25"
          '\r' -> "%0D"
          '\n' -> "%0A"
          ':' -> "%3A"
          ',' -> "%2C"
          x -> TL.singleton x
      )
    . ( \case
          Aeson.Null -> Monoid.mempty
          Aeson.String x -> TL.fromStrict x
          x -> Aeson.encodeToLazyText x
      )
    . (.json)
