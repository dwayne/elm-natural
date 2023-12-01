# Changelog

## [Unreleased]

### Added

- `fromDecimalString`
- `toDecimalString`

## [1.0.1] - 2023-03-27

### Changed

- Improved the performance of division, multiplication, and comparison.
  - Implemented [an efficient divide-and-correct algorithm](https://surface.syr.edu/cgi/viewcontent.cgi?article=1162&context=eecs_techreports) for division with remainder.
  - Implemented the [Karatsuba algorithm](https://en.wikipedia.org/wiki/Karatsuba_algorithm) for multiplication.
  - Refactored `compare` and reduced the constant factors involved in its calculation.
- Updated the `README` with notes on the comparison between this library and [`cmditch/elm-bigint`](https://package.elm-lang.org/packages/cmditch/elm-bigint/2.0.1/).

## [1.0.0] - 2023-01-20

The genesis event.

[unreleased]: https://github.com/dwayne/elm-natural/compare/1.0.1...HEAD
[1.0.1]: https://github.com/dwayne/elm-natural/compare/1.0.0...1.0.1
[1.0.0]: https://github.com/dwayne/elm-natural/releases/tag/1.0.0
