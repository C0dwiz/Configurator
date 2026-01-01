# Revision history for Configurator

## 0.2.0.0 -- 2026-01-01

### Added
- Array/index access support with syntax like `database.servers[0]`
- `showConfig` function to pretty-print configuration for debugging
- `getConfig` function for direct raw value access
- `keyExists` function to check if a configuration key exists
- `validateValue` function for detailed validation results
- Full validation constraint system with operators (`>`, `<`, `>=`, `<=`)
- Range validation with `inRange` constraint
- Pattern matching validation with `matchesPattern`
- String validation with `nonEmpty` constraint
- JSON file format support (auto-detected by extension)
- Comprehensive unit test suite (29+ tests)
- GitHub Actions CI/CD pipeline
- Extended API documentation (API.md)
- Practical examples (EXAMPLES.md)
- Environment variable override support with prefixes

### Changed
- Improved error messages to show file paths
- Enhanced configuration lookup to support array indexing
- Expanded examples with array and validation usage

## 0.1.0.0 -- 2025-12-15

* Initial release of Configurator
* Template Haskell quasi-quoter for compile-time configuration parsing
* Core API functions: required, optional, withDefault
* YAML configuration file support
* Type-safe configuration retrieval
