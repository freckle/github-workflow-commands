module GitHub.Workflow.Command.Properties.Value
  ( Value
  , toByteStringBuilder
  , ToValue
  , toValue
  ) where

import           Control.Category
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Text         as Aeson
import qualified Data.ByteString.Builder as BSB
import qualified Data.Monoid             as Monoid
import           Data.String             (IsString)
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Prelude                 (Eq, Ord, Show)

newtype Value = Value {json :: Aeson.Value}
  deriving newtype (Eq, Ord, Show, IsString, Aeson.ToJSON)

type ToValue = Aeson.ToJSON

toValue :: ToValue a => a -> Value
toValue = Aeson.toJSON >>> Value

toByteStringBuilder :: Value -> BSB.Builder
toByteStringBuilder =
  (.json)
    >>> \case
      Aeson.Null     -> Monoid.mempty
      Aeson.String x -> TL.fromStrict x
      x              -> Aeson.encodeToLazyText x
    >>> TL.concatMap
      ( \case
          '%'  -> "%25"
          '\r' -> "%0D"
          '\n' -> "%0A"
          ':'  -> "%3A"
          ','  -> "%2C"
          x    -> TL.singleton x
      )
    >>> TL.encodeUtf8Builder
