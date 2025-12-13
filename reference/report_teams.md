# Report Teams

Combines attribute values by team, maintaining the order of first
appearance.

## Usage

``` r
report_teams(attribute, team)
```

## Arguments

- attribute:

  A vector of attribute values.

- team:

  A vector indicating the team for each attribute.

## Value

A named character vector where each element is a comma-separated string
of attribute values for a team. The names correspond to the teams.

## Examples

``` r
report_teams(c("A", "B", "C", "A", "A", "A"), c(1,1,1,2,2,2))
#> Error in report_teams(c("A", "B", "C", "A", "A", "A"), c(1, 1, 1, 2, 2,     2)): could not find function "report_teams"
report_teams(c(1,2,3,4), c(2,2,1,1))
#> Error in report_teams(c(1, 2, 3, 4), c(2, 2, 1, 1)): could not find function "report_teams"
```
