{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Foghorn.Base.EconType
Description : Core econometric phantom types and index structures for Foghorn DSL
Copyright   : (c) Rob Tumarkin, 2025
License     : Non-commercial (see LICENSE file)
Maintainer  : https://github.com/tumarkin
Stability   : experimental
Portability : portable

This module defines the fundamental type-level building blocks of the
Foghorn DSL:

* 'EconType' enumerates phantom types representing econometric data
  (numeric, date, firm identifiers, industries, etc.). These are not
  runtime values, but type-level markers for reasoning and constraints.
* Type classes such as 'EconNum', 'EconDate', 'EconPermno', etc. classify
  subsets of 'EconType' for numeric, date, and identifier behavior.
* 'EconIndices' provides index structures (cross-sections, time series,
  and panels) built from 'EconType'.
* Singleton types ('SEconType', 'SEconIndices') enable type-level reflection.
* Utility functions and type families allow inspection, modification, and
  constraint checking on index types at both term and type levels.

This module is the backbone of econometric type safety in Foghorn: it lets
you express, check, and manipulate index structures at the type level in a
declarative way.
-}
module Foghorn.Base.EconType (
    -- * Econometric data types
    EconType (..),

    -- ** Econometric data classes
    EconNum,
    EconFractional,
    EconDate,
    EconCusip9,
    EconGvkey,
    EconPermno,
    EconIntegral,
    EconCoerce,

    -- * Econometric indices
    EconIndices (..),
    crossTimeIndices,
    crossSectionIndex,
    timeSeriesIndex,
    setCrossSection,
    setTimeSeries,

    -- ** Type-level functions
    HasCrossSection,
    SetCrossSection,
    GetCrossSection,
    HasTimeSeries,
    SetTimeSeries,
    GetTimeSeries,

    -- * Singletons
    SEconType (..),
    SEconIndices (..),
) where

import Data.Kind (Constraint)
import Data.Singletons.Base.TH
import GHC.TypeLits (ErrorMessage (..), TypeError)
import Prelude

-- == Econometric data types

{- | Econometric data types used as phantom types by the DSL
to permit econometric reasoning at the type level.
-}
data EconType
    = -- | Numeric / scalar-like
      Int_
    | Bigint_
    | Float_
    | Bool_
    | Text_
    | Serial_
    | -- | Indicators
      Indicator_
    | -- | Date-like
      AnnouncementDate_
    | Datadate_
    | Date_
    | Fyear_
    | QtrDatadate_
    | TradingDate_
    | TradingMonthEnd_
    | YearQuarter_
    | YearMonth_
    | CalendarYear_
    | -- | Firm identifiers
      Gvkey_
    | Permno_
    | Cusip6_
    | Cusip8_
    | Cusip9_
    | -- | Executive indexes
      CoPerRol_
    | -- | M & A types
      DealNumber_
    | AcqCusip6_
    | TgtCusip6_
    | AcqGvkey_
    | TgtGvkey_
    | AcqPermno_
    | TgtPermno_
    | AcqSic4_
    | TgtSic4_
    | -- | BoardEx
      Isin_
    | BoardExYear_
    | -- | Industries
      Sic1_
    | Sic2_
    | Sic3_
    | Sic4_
    | Ff12_
    | Ff48_
    deriving (Show, Eq, Ord)

-- | Singleton generation for 'EconType'.
genSingletons [''EconType]
singDecideInstances [''EconType]

deriving instance Show (SEconType a)
deriving instance Ord (SEconType a)

-- == Econometric data classes

-- | Numeric-like econometric types.
class EconNum a

instance EconNum Int_
instance EconNum Bigint_
instance EconNum Float_
instance EconNum AnnouncementDate_
instance EconNum Date_
instance EconNum Fyear_

-- | Fractional-like econometric types.
class EconFractional a

instance EconFractional Float_

-- | Date-like econometric types.
class EconDate a

instance EconDate AnnouncementDate_
instance EconDate Datadate_
instance EconDate Date_
instance EconDate TradingDate_
instance EconDate TradingMonthEnd_

-- | Firm identifier predicates.
class EconCusip9 a

instance EconCusip9 Cusip9_
instance EconCusip9 AcqCusip6_
instance EconCusip9 TgtCusip6_

class EconGvkey a
instance EconGvkey Gvkey_
instance EconGvkey AcqGvkey_
instance EconGvkey TgtGvkey_

class EconPermno a
instance EconPermno Permno_
instance EconPermno AcqPermno_
instance EconPermno TgtPermno_

-- | Integral-like marker.
class EconIntegral a

instance EconIntegral Fyear_
instance EconIntegral Int_
instance EconIntegral Bigint_
instance EconIntegral CalendarYear_
instance EconIntegral Ff12_
instance EconIntegral Ff48_
instance EconIntegral Sic1_
instance EconIntegral Sic2_
instance EconIntegral Sic3_
instance EconIntegral Sic4_
instance EconIntegral Indicator_

-- | Coercion relationships between types.
class EconCoerce a b

instance EconCoerce Date_ AnnouncementDate_
instance (EconIntegral a) => EconCoerce a Int_

-- == Econometric indices

{- | A (possibly 2D) index descriptor for datasets in the DSL.
  * 'CrossSection i' — one index dimension
  * 'TimeSeries  t' — one time dimension
  * 'Panel i t'     — two dimensions (cross-section × time)
-}
data EconIndices
    = CrossSection EconType
    | TimeSeries EconType
    | Panel EconType EconType
    deriving (Show, Eq, Ord)

-- | Singleton generation for 'EconIndices'.
genSingletons [''EconIndices]
singDecideInstances [''EconIndices]

deriving instance Show (SEconIndices i)

-- === Term-level utilities

{- | Extract both index dimensions at once as @(cross, time)@.

  >>> indices (CrossSection Permno_)
  (Just Permno_,Nothing)

  >>> indices (Panel Permno_ TradingDate_)
  (Just Permno_,Just TradingDate_)
-}
crossTimeIndices :: EconIndices -> (Maybe EconType, Maybe EconType)
crossTimeIndices (CrossSection i) = (Just i, Nothing)
crossTimeIndices (TimeSeries t) = (Nothing, Just t)
crossTimeIndices (Panel i t) = (Just i, Just t)

{- | Extract the cross-section dimension, if present.

  >>> crossSectionIndex (TimeSeries TradingDate_)
  Nothing
-}
crossSectionIndex :: EconIndices -> Maybe EconType
crossSectionIndex (CrossSection i) = Just i
crossSectionIndex (TimeSeries _) = Nothing
crossSectionIndex (Panel i _) = Just i

{- | Extract the time-series dimension, if present.

  >>> timeSeriesIndex (Panel Permno_ TradingDate_)
  Just TradingDate_
-}
timeSeriesIndex :: EconIndices -> Maybe EconType
timeSeriesIndex (CrossSection _) = Nothing
timeSeriesIndex (TimeSeries t) = Just t
timeSeriesIndex (Panel _ t) = Just t

-- | Replace or insert the cross-section dimension.
setCrossSection :: EconType -> EconIndices -> EconIndices
setCrossSection e (CrossSection _) = CrossSection e
setCrossSection e (TimeSeries t) = Panel e t
setCrossSection e (Panel _ t) = Panel e t

-- | Replace or insert the time-series dimension.
setTimeSeries :: EconType -> EconIndices -> EconIndices
setTimeSeries e (CrossSection i) = Panel i e
setTimeSeries e (TimeSeries _) = TimeSeries e
setTimeSeries e (Panel i _) = Panel i e

-- == Type-level functions

-- === Getters

-- | Get the cross-section dimension (if any).
type GetCrossSection :: EconIndices -> Maybe EconType
type family GetCrossSection i where
    GetCrossSection ('CrossSection cs) = 'Just cs
    GetCrossSection ('Panel cs _) = 'Just cs
    GetCrossSection ('TimeSeries _) = 'Nothing

-- | Get the time-series dimension (if any).
type GetTimeSeries :: EconIndices -> Maybe EconType
type family GetTimeSeries i where
    GetTimeSeries ('TimeSeries ts) = 'Just ts
    GetTimeSeries ('Panel _ ts) = 'Just ts
    GetTimeSeries ('CrossSection _) = 'Nothing

-- === Setters

-- | Set/replace the cross-section dimension at the type level.
type SetCrossSection :: EconType -> EconIndices -> EconIndices
type family SetCrossSection cs' i where
    SetCrossSection cs' ('CrossSection _) = 'CrossSection cs'
    SetCrossSection cs' ('TimeSeries ts) = 'Panel cs' ts
    SetCrossSection cs' ('Panel _ ts) = 'Panel cs' ts

-- | Set/replace the time-series dimension at the type level.
type SetTimeSeries :: EconType -> EconIndices -> EconIndices
type family SetTimeSeries ts' i where
    SetTimeSeries ts' ('CrossSection cs) = 'Panel cs ts'
    SetTimeSeries ts' ('TimeSeries _) = 'TimeSeries ts'
    SetTimeSeries ts' ('Panel cs _) = 'Panel cs ts'

-- === Predicates / Constraints

-- | Requires a cross-section dimension; errors with a readable message if absent.
type HasCrossSection :: EconIndices -> Constraint
type family HasCrossSection i where
    HasCrossSection ('CrossSection _) = ()
    HasCrossSection ('Panel _ _) = ()
    HasCrossSection other =
        TypeError
            ( 'Text "Expected indexing with a cross-section dimension, but got: "
                ':<>: 'ShowType other
            )

-- | Requires a time-series dimension; errors with a readable message if absent.
type HasTimeSeries :: EconIndices -> Constraint
type family HasTimeSeries i where
    HasTimeSeries ('TimeSeries _) = ()
    HasTimeSeries ('Panel _ _) = ()
    HasTimeSeries other =
        TypeError
            ( 'Text "Expected indexing with a time-series dimension, but got: "
                ':<>: 'ShowType other
            )
