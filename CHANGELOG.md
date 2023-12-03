# Changelog

## [Unreleased]

## [1.1.1] - 2023-12-03

### Changed

- Fixed a few minor mistakes in the documentation.
- Updated this `CHANGELOG` with a few more notes on the [1.1.0] release.
- Upgraded `nixpkgs` from 23.05 to 23.11.
- Upgraded `elm-review` from 2.9.1 to 2.10.3.

## [1.1.0] - 2023-12-01

### Added

- Two new operations: `fromDecimalString` and `toDecimalString`.
- This `CHANGELOG` and based it on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

### Changed

- Started to use Nix flakes.
- Widened the `elm/core` dependency.
- Reviewed and edited the documentation.
  - Moved the notes on the comparison between this library and [`cmditch/elm-bigint`] to the benchmarks `README`.
- Refactored the scripts in the `bin` directory.
  - Wrote usage notes for each script.
  - Started using `set -euo pipefail`.
  - Checked them with [shellcheck](https://www.shellcheck.net/).
- Refactored the tests.

## [1.0.1] - 2023-03-27

### Changed

- Improved the performance of division, multiplication, and comparison.
  - Implemented [an efficient divide-and-correct algorithm](https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports) for division with remainder.
  - Implemented the [Karatsuba algorithm](https://en.wikipedia.org/wiki/Karatsuba_algorithm) for multiplication.
  - Refactored `compare` and reduced the constant factors involved in its calculation.
- Updated the `README` with notes on the comparison between this library and [`cmditch/elm-bigint`].

## [1.0.0] - 2023-01-20

The genesis event.

[unreleased]: https://github.com/dwayne/elm-natural/compare/1.1.1...HEAD
[1.1.1]: https://github.com/dwayne/elm-natural/compare/1.1.0...1.1.1
[1.1.0]: https://github.com/dwayne/elm-natural/compare/1.0.1...1.1.0
[1.0.1]: https://github.com/dwayne/elm-natural/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/dwayne/elm-natural/releases/tag/1.0.0

[`cmditch/elm-bigint`]: https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/
