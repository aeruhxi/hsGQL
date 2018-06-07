module Hagql.Util
  ( maybeToList
  )
where

maybeToList :: Maybe [a] -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = x
