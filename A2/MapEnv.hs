-- Environments implemented using Haskell's map datatype. You can use this in 
-- StrictEnvABL instead of Env by replacing `import Env` with `import MapEnv`
module MapEnv where

import Data.Map.Strict as Map

type Env = Map.Map String

empty :: Env a
empty = Map.empty

add :: String -> a -> Env a -> Env a
add = Map.insert

get :: String -> Env a -> Maybe a
get = Map.lookup

