{-# LANGUAGE OverloadedStrings #-}

module Cabafu (
    readCabalConfig,
    getPkgVersion
              ) where

import qualified CabalLenses.PackageDescription as CL
import qualified CabalLenses.Package as CL

import Data.Either
import Distribution.PackageDescription.PrettyPrint
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
import Distribution.Simple.Utils (findPackageDesc)
import Distribution.Types.Version (Version)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Verbosity (silent)
import Distribution.Types.Dependency
import Distribution.Types.PackageName
import Distribution.Types.Version
import Distribution.Types.VersionRange

import Control.Lens ((^.), (&), (.~), set, traversed, _Snoc, _2)

type ConfigResult = Either String GenericPackageDescription

readCabalConfig :: FilePath -> IO ConfigResult
readCabalConfig f = findPackageDesc f >>= either getErr getCfg
    where 
        getErr :: String -> IO ConfigResult
        getErr = (return . Left . id)

        getCfg :: FilePath -> IO ConfigResult
        getCfg = ((Right <$>) . readCfg)

        readCfg = readGenericPackageDescription silent

getPkgVersion :: GenericPackageDescription -> Version
getPkgVersion p = p ^. (CL.packageDescriptionL . CL.packageL . CL.pkgVersionL)

setPkgVersion :: GenericPackageDescription -> Version -> GenericPackageDescription
setPkgVersion p v = p & (CL.packageDescriptionL . CL.packageL . CL.pkgVersionL) .~ v

dafuq = do
    cfg <- readCabalConfig "./"
    let dep = Dependency (mkPackageName "foo") anyVersion
    putStrLn $ showGenericPackageDescription  $ set (CL.condLibraryL  . traversed .  CL.condTreeConstraintsL . _Snoc . _2) dep (fromRight undefined cfg)

