{-# OPTIONS_GHC -ddump-splices -ddump-simpl #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables    #-}

module Main where

import StaticMain


main = $$(staticMain (Options "init" "loop.wasm"))
