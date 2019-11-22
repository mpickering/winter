{-# OPTIONS_GHC -ddump-splices -ddump-simpl -ddump-ds -dsuppress-all #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleContexts    #-}
module Main where

import StaticMain


main = $$(staticMain (Options "init" "loop.wasm"))

