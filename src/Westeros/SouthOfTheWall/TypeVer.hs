module Westeros.SouthOfTheWall.TypeVer where

import qualified Westeros.SouthOfTheWall.AST as AST
import qualified Westeros.SouthOfTheWall.Error as Err (TypeError(..))

data Type where

-- This interface will provide type check consistency for their instances.
--
-- The propper way to use it is to implement exhaustive instances for everything in
-- the language susceptible of having a type and then performing type queries when
-- necessary.
class Typeable a where 
    typeQuery :: a -> AST.Type

instance Typeable AST.Expression where
    typeQuery xd = undefined