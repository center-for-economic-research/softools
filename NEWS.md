# softools 0.1.0

## New features

* Initial release of softools package with core functionality for Norwegian data sources
* `get_municipality_structure()` - Retrieve municipality codes and validity periods from Statistics Norway (SSB) classification system
* `filter_municipality_structure()` - Validate and filter municipality-year combinations against official SSB structure
* `search_doffin()` - Search tender notices from Doffin (Norwegian public procurement portal)
* `get_ssb_kommunedata()` - Convenience wrapper for fetching municipality-level statistics from SSB API
* API key management utilities for Doffin integration

## Bug fixes

* Fixed column naming inconsistency between `get_municipality_structure()` and `filter_municipality_structure()` functions to ensure compatibility
