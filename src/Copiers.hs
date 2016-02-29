{-# LANGUAGE OverloadedStrings #-}

module Copiers (
      justCopy
    , justCreateAndCopy
) where

import Hakyll

justCopy :: Pattern -> Rules ()
justCopy something = match something $ do
    route   idRoute
    compile copyFileCompiler

justCreateAndCopy :: Identifier -> Rules ()
justCreateAndCopy something = create [something] $ do
    route   idRoute
    compile copyFileCompiler

