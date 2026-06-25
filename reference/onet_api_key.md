# Get O\*NET API Key

Retrieves the O\*NET API key from the `ONET_API_KEY` environment
variable.

## Usage

``` r
onet_api_key()
```

## Value

A character string containing the API key.

## Details

To obtain an API key, register at
<https://services.onetcenter.org/developer/>. Set the key in your
environment with:

    Sys.setenv(ONET_API_KEY = "your-api-key")

Or add it to your `.Renviron` file for persistence.

## Examples

``` r
if (FALSE) { # nzchar(Sys.getenv("ONET_API_KEY"))
onet_api_key()
}
```
