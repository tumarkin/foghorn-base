{- |
Module : Foghorn.Base
Description : Module re-exporting core econometric kinds, indices, and constraints for the Foghorn DSL
Copyright : (c) Rob Tumarkin, 2025
License : Non-commercial (see LICENSE file)
Maintainer : https://github.com/tumarkin
Stability : experimental
Portability : portable

This base module is the public entry point for Foghorn's foundational
type-level machinery. It currently re-exports 'Foghorn.Base.EconType', which
defines:

- the promoted 'EconType' kind (e.g., 'Float_', 'Int_', 'Text_', 'Date_'),

- indexing kinds under 'EconIndices' (e.g., 'CrossSectionIndex', 'TimeSeriesIndex', 'PanelIndices'),

- core typeclass constraints (e.g., 'EconNum', 'EconFractional', 'EconDate').

Import 'Foghorn.Base' to depend on a stable API rather than individual
internal modules. No new definitions are introduced here—only re-exports.

@
import Foghorn.Base
@
-}
module Foghorn.Base (
    module Foghorn.Base.EconType,
) where

import Foghorn.Base.EconType
