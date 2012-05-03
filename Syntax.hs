module Syntax where

import KappaParser

data Error = ...

-- check the well-formedness of a module
check :: Module -> [Error]

