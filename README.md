# onet2r

<!-- badges: start -->
<!-- badges: end -->

**onet2r** is an R client for the [O\*NET Web Services API 2.0](https://services.onetcenter.org/). This package provides a modern, tidyverse-friendly interface to search occupations, retrieve occupation details (skills, knowledge, abilities, technology), access database tables, and perform taxonomy crosswalks.

## Features

- 🔍 **Search occupations** by keyword or O\*NET-SOC code
- 📊 **Retrieve occupation details** including skills, knowledge, abilities, and hot technologies
- 🗄️ **Access database tables** with automatic pagination
- 🔄 **Perform crosswalks** between military codes and civilian occupations
- 📈 **Map taxonomies** between O\*NET-SOC versions
- ✅ **Consistent tibble outputs** for easy integration with tidyverse workflows
- 🔁 **Automatic retry logic** for transient API errors
- 📝 **Type-safe schemas** with validated empty result handling

## Installation

You can install the development version of onet2r from [GitHub](https://github.com/farach/onet2r) with:

``` r
# install.packages("pak")
pak::pak("farach/onet2r")
```

Or using devtools:

``` r
# install.packages("devtools")
devtools::install_github("farach/onet2r")
```

## Getting Started

### API Key Setup

To use onet2r, you need an O\*NET Web Services API key:

1. **Register for a free API key** at <https://services.onetcenter.org/developer/>
2. **Set your API key** in your R environment:

```r
# For current session only
Sys.setenv(ONET_API_KEY = "your-api-key-here")

# Or add to your .Renviron file for persistence
usethis::edit_r_environ()
# Add this line:
# ONET_API_KEY=your-api-key-here
```

3. **Restart R** to load the environment variable from `.Renviron`

You can verify your API key is set with:

```r
library(onet2r)
onet_api_key()
```

## Basic Usage

### Search for Occupations

Search for occupations by keyword:

```r
library(onet2r)

# Search for software-related occupations
results <- onet_search("software developer")
results
#> # A tibble: 20 × 2
#>    code       title                                 
#>    <chr>      <chr>                                 
#>  1 15-1252.00 Software Developers                   
#>  2 15-1251.00 Computer Programmers                  
#>  3 15-1256.00 Software Quality Assurance Analysts…  
#> # ℹ 17 more rows

# Search by O*NET-SOC code
onet_search("15-1252")
```

### Get Occupation Details

Retrieve detailed information about a specific occupation:

```r
# Get occupation summary
summary <- onet_occupation("15-1252.00")

# Get full occupation report
details <- onet_occupation_details("15-1252.00")

# Get specific occupation attributes
skills <- onet_skills("15-1252.00")
knowledge <- onet_knowledge("15-1252.00")
abilities <- onet_abilities("15-1252.00")
tech <- onet_technology("15-1252.00")

head(skills)
#> # A tibble: 6 × 5
#>   id             name                    description          value scale
#>   <chr>          <chr>                   <chr>                <dbl> <chr>
#> 1 2.A.2.a        Reading Comprehension   Understanding writ…   4.50 Impo…
#> 2 2.A.2.b        Active Listening        Giving full atten…   4.12 Impo…
#> 3 2.A.2.c        Writing                 Communicating eff…   4.12 Impo…
```

### List All Occupations

Get a complete list of occupations in the O\*NET database:

```r
# Get all occupations (up to 1000 results)
occupations <- onet_occupations()
nrow(occupations)
#> [1] 1016

# Get first 50 occupations
onet_occupations(start = 1, end = 50)
```

### Access Database Tables

Retrieve data from O\*NET database tables with automatic pagination:

```r
# List available tables
tables <- onet_tables()
tables
#> # A tibble: 84 × 2
#>    id                      title                                    
#>    <chr>                   <chr>                                    
#>  1 abilities               Abilities                                
#>  2 content_model_reference Content Model Reference                  
#>  3 education_training_exp… Education, Training, and Experience Cate…
#> # ℹ 81 more rows

# Get column information for a table
info <- onet_table_info("skills")
info

# Retrieve all rows from a table
# Automatically paginates through large tables
skills_data <- onet_table("skills")

# Disable progress messages
skills_data <- onet_table("skills", show_progress = FALSE)
```

### Crosswalks and Taxonomy Mapping

Map between military and civilian occupations, or between taxonomy versions:

```r
# Find civilian occupations matching military roles
civilian_jobs <- onet_crosswalk_military("infantry")
civilian_jobs

# Map between O*NET-SOC taxonomy versions
mapped <- onet_taxonomy_map("15-1252.00", from = "active", to = "2010")
mapped
```

## Error Handling

The package provides informative error messages for common issues:

```r
# Invalid API key
#> Error: O*NET API key not found.
#> ℹ Set your API key with `Sys.setenv(ONET_API_KEY = "your-key")`
#> ℹ Get a key at <https://services.onetcenter.org/developer/>

# Invalid occupation code format
onet_skills("invalid-code")
#> Error: Invalid O*NET-SOC code format: "invalid-code"
#> ℹ Expected format: XX-XXXX or XX-XXXX.XX (e.g., 15-1252 or 15-1252.00)

# No results found (returns empty tibble with correct schema)
results <- onet_search("xyzabc123nonexistent")
#> No occupations found for keyword: "xyzabc123nonexistent"
results
#> # A tibble: 0 × 2
#> # ℹ 2 variables: code <chr>, title <chr>
```

API errors are automatically handled with:
- **Automatic retries** for transient errors (rate limits, server errors)
- **Clear error messages** from the O\*NET API
- **Consistent empty results** with proper tibble schemas

## Advanced Usage

### Custom Pagination

Control pagination behavior for large table retrievals:

```r
# Fetch in smaller chunks (default is 2000 rows per request)
data <- onet_table("occupation_data", page_size = 500)

# Show/hide progress messages
data <- onet_table("occupation_data", show_progress = TRUE)
```

### Working with Results

All functions return tibbles that work seamlessly with dplyr and other tidyverse packages:

```r
library(dplyr)

# Find occupations related to "data"
results <- onet_search("data analyst") |>
  arrange(title)

# Get skills for multiple occupations
codes <- c("15-1252.00", "15-1251.00")
skills_list <- codes |>
  purrr::map(onet_skills) |>
  purrr::list_rbind(.id = "occupation")
```

## API Rate Limits

The O\*NET Web Services API has rate limits. The package automatically:
- Retries requests on rate limit errors (HTTP 429)
- Uses exponential backoff to respect rate limits
- Retries on transient server errors (500, 502, 503, 504)

## Getting Help

- **Package documentation**: `?onet2r` or browse function help with `?onet_search`
- **Vignettes**: Run `browseVignettes("onet2r")` to see available guides
- **O\*NET API documentation**: <https://services.onetcenter.org/reference/>
- **Report bugs**: <https://github.com/farach/onet2r/issues>

## Code of Conduct

Please note that the onet2r project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## License

MIT © 2026 Alex Farach
