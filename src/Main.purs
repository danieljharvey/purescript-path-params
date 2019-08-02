module Main where

import Prelude (Unit, const, identity, show, (<>))
import Data.Argonaut.Core (caseJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson) 
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested

newtype Horse
  = Horse String

derive newtype instance encodeJsonHorse :: EncodeJson Horse

newtype Egg
  = Egg Int

derive newtype instance encodeJsonEgg :: EncodeJson Egg

horse :: Horse
horse = Horse "Horse"

egg :: Egg
egg = Egg 100

class ToPathParams a where
  toPathParams :: a -> String

instance toPathParamsTupleNil
  :: (ToPathParams a)
  => ToPathParams (Tuple a Unit) where
  toPathParams (Tuple a unit) = toPathParams a

else 

instance toPathParamsTuple2 
  :: (ToPathParams a, ToPathParams b, ToPathParams (Tuple b c)) 
  => ToPathParams (Tuple a (Tuple b c)) where
  toPathParams (Tuple a b)
    = toPathParams a <> "/" <> toPathParams b

else

instance toPathParamsJson :: (EncodeJson a) => ToPathParams a where
  toPathParams a = caseJson
              (const "")   -- Null
              (show)       -- Boolean
              (show)       -- Number
              (identity)   -- String (avoid the extra quotes)
              (toPathParams)      -- Array
              (toPathParams)      -- Object
              (encodeJson a)

makeUrl :: forall a. (ToPathParams a) => String -> a -> String
makeUrl path a
  = path <> "/" <> toPathParams a

url1 :: String
url1 = makeUrl "mygreatapi.com" (tuple1 egg)

url2 :: String
url2 = makeUrl "mygreatapi.com" (tuple2 egg horse)

url10 :: String
url10 = makeUrl "mygreatapi.com" (tuple10 egg horse egg horse egg horse egg
  horse egg horse)
 



