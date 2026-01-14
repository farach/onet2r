# onet2r

<!-- badges: start -->
<!-- badges: end -->

onet2r is an R client for the [O*NET Web Services API](https://services.onetcenter.org/), providing easy access to occupational data from the O*NET database. 

The package offers functions to search occupations, retrieve occupation details (skills, knowledge, abilities), access database tables, and perform taxonomy crosswalks.

## Installation

You can install the development version of onet2r from GitHub:

``` r
# install.packages("pak")
pak::pak("farach/onet2r")
```

## Getting Started

### API Key Setup

To use onet2r, you need an O*NET Web Services API key:

1. Register for a free API key at <https://services.onetcenter.org/developer/>
2. Set your API key in R:

``` r
# For current session only
Sys.setenv(ONET_API_KEY = "your-api-key-here")

# For persistent use, add to your .Renviron file:
# ONET_API_KEY=your-api-key-here
```

You can also add the API key to your `.Renviron` file for automatic loading:

``` r
usethis::edit_r_environ()
# Add: ONET_API_KEY=your-api-key-here
# Restart R
```

### Basic Usage

#### Search for Occupations

Search for occupations by keyword or SOC code:

``` r
library(onet2r)

# Search by keyword
results <- onet_search("software developer")
head(results)
#> # A tibble: 6 × 3
#>   code       title                            relevance_score
#>   <chr>      <chr>                                      <dbl>
#> 1 15-1252.00 Software Developers                         95.2
#> 2 15-1253.00 Software Quality Assurance...               72.1
#> 3 15-1251.00 Computer Programmers                        68.5

# Search by SOC code
results <- onet_search("15-1252")
```

#### Get Occupation Details

Retrieve detailed information about specific occupations:

``` r
# Get occupation summary
summary <- onet_occupation("15-1252.00")

# Get full occupation details
details <- onet_occupation_details("15-1252.00")

# Get specific elements
skills <- onet_skills("15-1252.00")
knowledge <- onet_knowledge("15-1252.00")
abilities <- onet_abilities("15-1252.00")
technology <- onet_technology("15-1252.00")
```

#### Access Database Tables

Access O*NET database tables directly:

``` r
# List available tables
tables <- onet_tables()
head(tables)

# Get table information
info <- onet_table_info("skills")

# Retrieve table data (with automatic pagination)
skills_data <- onet_table("skills")

# Disable progress messages for large tables
data <- onet_table("occupation_data", show_progress = FALSE)
```

#### List All Occupations

Get a complete list of O*NET occupations:

``` r
# Get all occupations (up to 1000 by default)
occupations <- onet_occupations()
head(occupations)
#> # A tibble: 6 × 2
#>   code       title                              
#>   <chr>      <chr>                              
#> 1 11-1011.00 Chief Executives                   
#> 2 11-1011.03 Chief Sustainability Officers      
#> 3 11-1021.00 General and Operations Managers
```

#### Crosswalks

Convert between occupation taxonomies or search for civilian equivalents to military jobs:

``` r
# Military to civilian occupation crosswalk
civilian_jobs <- onet_crosswalk_military("infantry")

# Map between O*NET-SOC and 2010 SOC taxonomies
mapped <- onet_taxonomy_map("15-1252.00", from = "active", to = "2010")
```

## Error Handling

The package provides clear, informative error messages:

- **Missing API Key**: If no API key is set, you'll get instructions on how to obtain and set one
- **Invalid Input**: Input validation ensures you're using the correct format for SOC codes and other parameters
- **API Errors**: API-level errors are caught and reported with helpful context
- **Empty Results**: When a search returns no results, you'll receive an informative message and an empty tibble with the correct schema

Example error messages:

``` r
# Missing API key
onet_search("developer")
#> Error: O*NET API key not found.
#> ℹ Set your API key with `Sys.setenv(ONET_API_KEY = "your-key")`
#> ℹ Get a key at <https://services.onetcenter.org/developer/>

# Invalid SOC code format
onet_occupation("invalid-code")
#> Error: Invalid O*NET-SOC code format: "invalid-code"
#> ℹ Expected format: XX-XXXX or XX-XXXX.XX (e.g., 15-1252 or 15-1252.00)
```

## Features

- **Modern tidyverse-style API**: All functions return tibbles with snake_case column names
- **Automatic pagination**: Large database tables are fetched automatically with optional progress reporting
- **Consistent schemas**: All functions return tibbles with predictable, typed column structures
- **Robust error handling**: Clear error messages for common issues
- **Retry logic**: Automatic retry for transient errors (rate limiting, server errors)
- **Full API coverage**: Access to search, occupations, database tables, and crosswalks

## Related Resources

- [O*NET Web Services API Documentation](https://services.onetcenter.org/reference/)
- [O*NET OnLine](https://www.onetonline.org/) - Web interface to O*NET data
- [O*NET Resource Center](https://www.onetcenter.org/) - Background on O*NET

## Code of Conduct

Please note that the onet2r project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
