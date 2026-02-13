# Softools - AI Coding Instructions

## Project Overview
`softools` is an R package providing convenience functions for Norwegian data sources, primarily Statistics Norway (SSB) and the Doffin procurement portal. Target users are "Soffers" (researchers at Samfunnsøkonomisk Analyse/Center for Economic Research).

## Package Architecture

### Core Modules
1. **Municipality Structure** ([R/get_municipality_structure.R](R/get_municipality_structure.R), [R/filter_municipality_structure.R](R/filter_municipality_structure.R))
   - Retrieves Norwegian municipality codes and validity periods from SSB's classification system (klass 131)
   - Municipality codes can change due to mergers/splits; tracks validity with `valid_from`/`valid_to` dates
   - `valid_to` is exclusive (municipality NOT valid on that date)
   - Filter function validates municipality-year combinations against full calendar year validity

2. **SSB Data Retrieval** ([R/get_ssb_kommunedata.R](R/get_ssb_kommunedata.R))
   - Wraps `PxWebApiData::ApiData` to fetch municipality-level statistics from SSB API
   - Uses unnamed argument for municipality codes to handle different region variable names across KOSTRA vs regular tables
   - Optionally removes rows where `NAstatus == "."` (no observation), but this column is NOT guaranteed in all tables

3. **Doffin API Integration** ([R/search_doffin.R](R/search_doffin.R))
   - Searches tender notices from Doffin (Norwegian public procurement portal)
   - API key management via environment variable `DOFFIN_API_KEY`
   - Uses `httr2` for requests with `Ocp-Apim-Subscription-Key` header
   - Differentiates test vs production via `is_testing()` helper

## Development Conventions

### Documentation
- **Function docs**: Use roxygen2 with Norwegian text for functions targeting Norwegian users (see `get_ssb_dta`)
- **Examples**: Wrap in `\dontrun{}` since most require external API access or API keys
- **Parameter validation**: Check input types, lengths, and ranges explicitly with descriptive error messages

### Testing
- Uses testthat 3rd edition (`Config/testthat/edition: 3`)
- Run tests: `devtools::test()` or `testthat::test_check("softools")`
- Test file convention: `test-<function_file_name>.R` in [tests/testthat/](tests/testthat/)
- Parameterized validation tests: see [test-get_municipality_structure.R](tests/testthat/test-get_municipality_structure.R) for comprehensive input validation examples

### Dependency Management
- **Package imports**: Declare in DESCRIPTION `Imports:` field, import specific functions via roxygen2 `@importFrom`
- Uses `renv` for reproducible environments (snapshot type: implicit)
- Conditional dependencies: Use `requireNamespace("pkg", quietly = TRUE)` for suggested packages (e.g., `askpass`)

### Code Style
- Four-space indentation
- Pipe `|>` preferred for sequential operations (httr2 example in `search_doffin`)
- Use explicit namespace calls for clarity (`dplyr::`, `klassR::`) when function origin may be unclear
- Municipality codes: Always character strings, four digits (e.g., "0301", "1601")

## Key Integration Points

### External APIs
- **SSB Klass API**: Municipality classification (code 131) via `klassR::get_klass()`
- **SSB Statistics API**: Table data via `PxWebApiData::ApiData()` with `returnDataSet = 12` for metadata
- **Doffin API**: `https://betaapi.doffin.no/public/v2/search?` with subscription key authentication

### Environment Variables
- `DOFFIN_API_KEY`: Required for Doffin API calls
- `TESTTHAT`: Set to "true" during test runs (use `is_testing()` to detect)

## Common Workflows

### Building & Checking
```r
devtools::document()        # Update documentation
devtools::test()            # Run tests
devtools::check()           # Full R CMD check
```

### Adding New Functions
1. Create function in [R/](R/) with roxygen2 docs
2. Add imports to `@importFrom` tags (updates NAMESPACE)
3. Add dependencies to DESCRIPTION if needed
4. Create corresponding test file in [tests/testthat/](tests/testthat/)
5. Run `devtools::document()` to generate [man/](man/) files

### Working with Municipality Data
- Always validate municipality codes are character format with four digits
- Consider validity periods when joining data across years
- Use `filter_municipality_structure()` to validate municipality-year combinations before analysis
