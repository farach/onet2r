# onet2r <a href="https://github.com/farach/onet2r"></a>

**onet2r** is a modern, tidyverse-first R client for the [O\*NET Web Services API v2](https://services.onetcenter.org/). It provides a clean, consistent, analysis-ready interface to U. S. occupational data.

🔗 **API Base URL:** <https://api-v2.onetcenter.org>

## ✨ Features

-   🔍 **Search occupations** by keyword or O\*NET-SOC code
-   📊 **Retrieve occupation details** — skills, knowledge, abilities, tasks, and hot technologies
-   🗄️ **Access database tables** with automatic pagination
-   🔄 **Perform crosswalks** between military codes and civilian occupations
-   📈 **Map taxonomies** between O\*NET-SOC versions
-   ✅ **Consistent tibble outputs** with snake_case columns and stable empty schemas
-   🔁 **Automatic retry logic** for rate limits (HTTP 429) and transient server errors
-   📝 **Type-safe schemas** with validated empty result handling

## 📦 Installation

Install the development version from GitHub:

``` r
# install.packages("pak")
pak::pak("farach/onet2r")
```

## 🔐 Authentication

O\*NET API requests require an API key. [Register for a free key here](https://services.onetcenter.%20org/developer/).

**Recommended:** Store your key in `.Renviron` for persistence across sessions:

``` r
usethis::edit_r_environ()
```

Add this line:

``` text
ONET_API_KEY=your-api-key-here
```

Restart R to load the environment variable.

**Alternative:** Set for the current session only:

``` r
Sys.setenv(ONET_API_KEY = "your-api-key-here")
```

Verify your key is configured:

``` r
library(onet2r)
onet_api_key()
```

## 🚀 Quick Start

``` r
library(onet2r)

# Search for occupations
onet_search("software developer")

# List occupations (paged)
occ <- onet_occupations(start = 1, end = 25)

# Get occupation summary
onet_occupation("15-1252.00")

# Retrieve structured details (returns tibbles)
onet_skills("15-1252.00", end = 5)
onet_knowledge("15-1252.00", end = 5)
onet_abilities("15-1252.00", end = 5)
onet_tasks("15-1252.00", end = 5)

# Technology endpoints
onet_hot_technology("15-1252.00", end = 5)
onet_technology_skills("15-1252.00", end = 2)
```

## 📋 API Coverage

### Occupations

| Function                    | Description                          |
|-----------------------------|--------------------------------------|
| `onet_occupations()`        | List all occupations with pagination |
| `onet_occupation()`         | Get occupation summary               |
| `onet_occupation_details()` | Get full occupation report           |
| `onet_search()`             | Search occupations by keyword        |

### Occupation Details (tibble outputs)

| Function                           | Description               |
|------------------------------------|---------------------------|
| `onet_skills()`                    | Skills data               |
| `onet_knowledge()`                 | Knowledge areas           |
| `onet_abilities()`                 | Abilities                 |
| `onet_work_styles()`               | Work styles               |
| `onet_interests()`                 | Occupational interests    |
| `onet_work_context()`              | Work context              |
| `onet_work_activities()`           | Work activities           |
| `onet_tasks()`                     | Tasks                     |
| `onet_detailed_work_activities()`  | Detailed work activities  |
| `onet_related_occupations()`       | Related occupations       |
| `onet_professional_associations()` | Professional associations |
| `onet_apprenticeship()`            | Apprenticeship info       |
| `onet_education()`                 | Education requirements    |
| `onet_job_zone()`                  | Job zone (returns list)   |

### Technology

| Function                   | Description           |
|----------------------------|-----------------------|
| `onet_hot_technology()`    | Hot technology skills |
| `onet_technology_skills()` | Technology skills     |
| `onet_in_demand_skills()`  | In-demand skills      |

### Database Tables

| Function            | Description                          |
|---------------------|--------------------------------------|
| `onet_tables()`     | List available tables                |
| `onet_table_info()` | Get table column info                |
| `onet_table()`      | Retrieve full table (auto-paginated) |
| `onet_table_page()` | Retrieve single page                 |

### Crosswalks & Taxonomy

| Function                    | Description                          |
|-----------------------------|--------------------------------------|
| `onet_crosswalk_military()` | Map military to civilian occupations |
| `onet_taxonomy_map()`       | Map between O\*NET-SOC versions      |

## 🔧 Example Workflow

A typical workflow: search → select → pull details.

``` r
library(dplyr)
library(onet2r)

# Find target occupation
target_code <- onet_search("data scientist") |>
  filter(title == "Data Scientists") |>
  pull(code)

# Pull structured details
skills <- onet_skills(target_code, end = 25)
tasks  <- onet_tasks(target_code, end = 25)
tech   <- onet_hot_technology(target_code, end = 25)

# Combine skills from multiple occupations
codes <- c("15-2051.00", "15-1252.00")

all_skills <- codes |>
  purrr::map(\(code) onet_skills(code) |> mutate(code = code)) |>
  purrr:: list_rbind()
```

## 📊 What You Get

### Consistent, Analysis-Ready Outputs

Most endpoints return tibbles with:

-   **snake_case column names** (e. g., `percentage_of_respondents`)
-   **Stable empty schemas** (empty tibble with correct columns and types)
-   **Predictable pagination** via `start`/`end` arguments

``` r
onet_education("15-1252.00")
#> # A tibble: 6 × 3
#>   code  title                          percentage_of_respondents
#>   <chr> <chr>                                              <dbl>
#> 1 6     Bachelor's degree                                   64.7
#> 2 7     Master's degree                                     20.5
#> ... 
```

### Informative Error Messages

``` r
# Missing API key
#> Error: O*NET API key not found.
#> ℹ Set your API key with `Sys.setenv(ONET_API_KEY = "your-key")`
#> ℹ Get a key at <https://services.onetcenter.org/developer/>

# Invalid occupation code
onet_skills("invalid-code")
#> Error: Invalid O*NET-SOC code format: "invalid-code"
#> ℹ Expected format: XX-XXXX or XX-XXXX. XX (e.g., 15-1252 or 15-1252.00)

# No results (returns empty tibble with correct schema)
onet_search("xyzabc123nonexistent")
#> # A tibble: 0 × 2
#> # ℹ 2 variables: code <chr>, title <chr>
```

## 💡 Best Practices

| Practice                               | Why                              |
|-----------------------------------------------|-------------------------|
| Store API key in `.Renviron`           | Keeps credentials out of scripts |
| Use `start`/`end` for pagination       | Smaller, faster requests         |
| Prefer detail endpoints over summaries | Structured data for analysis     |

## 📚 Getting Help

-   **Package documentation:** `?onet2r` or `?onet_search`
-   **Vignettes:** `browseVignettes("onet2r")`
-   **O\*NET API docs:** <https://services.onetcenter.org/reference/>
-   **Report issues:** <https://github.com/farach/onet2r/issues>

## 🤝 Contributing

Issues and PRs are welcome!

When adding a new endpoint:

-   Return a tibble for record-list endpoints
-   Include stable empty schemas
-   Add roxygen documentation for all arguments
-   Write minimal tests (smoke test + schema validation)

Please note that onet2r is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.%20html). By contributing, you agree to abide by its terms.

## 📄 License

MIT © 2026 Alex Farach
