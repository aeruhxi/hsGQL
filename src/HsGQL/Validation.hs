module HsGQL.Validation
  ( VError (..)
  , isExecutableDefinition
  )
where

import           HsGQL.Ast
import qualified Data.Validation               as V

data VError = MustBeExecutableDefinition

newtype Validated a = Validated a

--  Documents
isExecutableDefinition :: Document -> V.Validation [VError] (Validated Document)
isExecutableDefinition docs =
  if any isExecutableDefinition' docs
  then V.Success $ Validated docs
  else V.Failure [MustBeExecutableDefinition]
  where
    isExecutableDefinition' (ExecutableDefinition _) = True
    isExecutableDefinition' _                        = False
