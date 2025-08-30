# Changelog

## [2.3.0] - 2025-08-30

-   Use `splitter` library to increase performance when lexing comments

## [2.2.1] - 2025-03-18

-   Fix issue when skipping a comment without a proceeding newline

## [2.2.0] - 2025-03-16

-   Improve performance

## [1.0.1] - 2024-04-25

### Fixed

-   Support lexing lowercase hexadecimal

## [1.0.0] - 2024-04-13

### Changed

-   Updated minimum required Gleam version to v1.0.0
-   Named the `byte_offset` field in `Position`

## [0.7.0]

### Fixed

-   Updated for Gleam v0.32.0.

## [0.6.2]

### Fixed

-   Fixes broken support for hexadecimal ints were broken.

## [0.6.1]

### Fixed

-   Fixes broken support for binary, octal and hexadecimal ints.

## [0.6.0]

### Added

-   Support for binary, octal and hexadecimal ints, and scientific notation
    floats.

### Changed

-   Performance improvements

## [0.5.0]

### Added

-   Support for the `@` tokens.
-   Support for unexpected graphemes that are not valid tokens.

## [0.4.2]

### Added

-   Support for snake case names (#5)

## [0.4.0] - 2023-05-18

### Added

-   Basic float support (#4)
