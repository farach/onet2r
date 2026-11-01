# Configure Delay Between O\*NET API Requests

Sets a minimum delay before each O\*NET API request. This can be useful
for polite bulk pulls or when working near rate limits.

## Usage

``` r
onet_rate_limit(seconds = 0)
```

## Arguments

- seconds:

  Non-negative number of seconds to wait before each API request.

## Value

Invisibly returns `seconds`.

## Examples

``` r
onet_rate_limit(0)
```
