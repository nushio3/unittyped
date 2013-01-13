module Main where

import Test.DocTest

main :: IO ()
main = doctest 
     [ "-isrc/:"
     , "-XFlexibleInstances"        
     , "-XUndecidableInstances"        
     , "-XFunctionalDependencies"        
     , "-XKindSignatures"        
     , "-XScopedTypeVariables"        
     , "-XFlexibleContexts"        
     , "-XOverlappingInstances"        
     , "-XGADTs"        
     , "-XEmptyDataDecls"        
     , "-XTypeOperators"        
     , "-XRankNTypes"        
     , "-XDeriveDataTypeable"             
     , "src/UnitTyped.hs"
     ]



