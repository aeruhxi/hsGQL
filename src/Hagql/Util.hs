module Hagql.Util
  ( listFromMaybe
  )
where

import           Data.Maybe                     ( fromMaybe )

listFromMaybe :: Maybe [a] -> [a]
listFromMaybe = fromMaybe []
