# Foghorn.Base

Foundational kinds, indicates, and constraints for the **Foghorn** DSL — a
type-safe, declarative framework for econometrics.

[![License: PolyForm Noncommercial](https://img.shields.io/badge/license-PolyForm%20Noncommercial-blue)](./LICENSE)
[![GHC](https://img.shields.io/badge/GHC-9.6%2B-brightgreen)](https://www.haskell.org/ghc/)
[![Build](https://img.shields.io/badge/build-cabal-informational)](#)

---

## Overview

`Foghorn.Base` defines the core kind-level types and index schemes that other Foghorn packages rely on. It is primarily intended for **library authors and contributors extending Foghorn**, rather than applied researchers building empirical studies. End users will typically import topic-level modules (e.g., Coficat for corporate-finance) rather than this base library directly.

---

## What this is

`Foghorn.Base` (currently) re-exports
[`Foghorn.Base.EconType`](./src/Foghorn/Base/EconType.hs), which provides:

- **Kinds:** `EconType` (e.g., `'Float_`, `'Int_`, `'Text_`, `'Date_`, `'Permno_`, `'Gvkey_`, `'Cusip9_'`, …)
- **Indices:** `EconIndices` for dataset shapes  
  - `'CrossSectionIndex et`  
  - `'TimeSeriesIndex et`  
  - `'PanelIndices cs ts`
- **Constraints:** type class families for working with those kinds  
  - `EconNum`, `EconFractional`, `EconDate`, etc.

Import `Foghorn.Base` (the umbrella module) for forward-compatibilty.

---

## Installation

Foghorn is not yet on HAckage. For now, please clone the central Foghorn project repository.

--- 

## Compatibility

- GHC: 9.6+  
- Build tool: `cabal`  
- OS: Linux/macOS/Windows (standard GHC/cabal environments)

---

## Documentation

- In-source **Haddock** comments describe kinds, indices, and constraints.  
- To build HTML docs locally:
  ```bash
  cabal haddock --enable-documentation \
    --haddock-html --haddock-hyperlink-source --haddock-quickjump all
  ```

---

## Contributing

Contributions are welcome, especially improvements to documentation, additional variable definitions, or clarifications to type-level design. Please open an issue to discuss your idea before submitting a pull request.

---

## License

PolyForm Noncommercial 1.0.0.   For any commercial use, contact **Rob Tumarkin <https://github.com/tumarkin>**.

---

## Changelog

See [CHANGELOG.md](./CHANGELOG.md).

---

## Maintainers & Support

**Maintainer**
- **Rob Tumarkin** — [@tumarkin](https://github.com/tumarkin)

**Getting Help**
- Review the in-code Haddock documentation.  
- Report bugs or unexpected behavior via the [issue tracker](https://github.com/tumarkin/foghorn-base/issues).  
- For feature ideas, open an issue with the `enhancement` label.

**Support Policy**
- Maintained on a **non-commercial, academic** basis. Support is **best-effort**; response times may vary.

