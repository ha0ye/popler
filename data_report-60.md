popler\_data\_organizatonal\_hierarchy
================
Hao Ye, Ellen Bledsoe
5/21/2019

``` r
library(tidyverse)

all_data <- readRDS("list_df.rds")
df <- as_tibble(all_data[[params$dataset_index]])

cat("My project metadata key is ", 
    df$proj_metadata_key[1], "!!")
```

    ## My project metadata key is  273 !!

``` r
# figure out the spatial replication levels
df %>% 
  select(starts_with("spatial_replication_level")) %>%
  NCOL() %>%
  {./2} -> num_sr_levels
```

``` r
# transform the names of the variables
#   - get rid of the `spatial_replication_level_#_label` columns
sr_vars <- character(num_sr_levels)
for (i in seq(num_sr_levels))
{
  new_name <- paste0(i, "--", df[1, paste0("spatial_replication_level_", i, "_label")])
  old_name <- paste0("spatial_replication_level_", i)
  sr_vars[i] <- new_name
  df <- rename(df, !!new_name := !!old_name)
}
```

``` r
# extract just the spatial replication level data
data_organization <- df %>%
  select(sr_vars)
```

``` r
# generate contingency tables to summarize organizational structure:
#   - level_i vs. level_j (i < j)

cols <- expand.grid(i = seq(num_sr_levels), 
                    j = seq(num_sr_levels)) %>%
  filter(i < j)

sr_tables <- purrr::pmap(cols, function(i, j) {
    data_organization %>%
      select(sr_vars[c(i, j)]) %>%
      table()
  })
```

``` r
# loop over tables and output
purrr::map(sr_tables, knitr::kable)
```

    ## [[1]]
    ## 
    ## 
    ##                               1     10     11     12     13     14      2      3      4      5      6      7      8      9
    ## ------------------------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## site_cdr_ple055_field_E    1169   1195   1200   1190   1173   1177   1181   1179   1173   1179   1186   1196   1179   1182
    ## 
    ## [[2]]
    ## 
    ## 
    ##                              1    10    11    12    13    14    15    16    17    18    19     2    20    21    22    23     3     4     5     6     7     8     9
    ## ------------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_cdr_ple055_field_E    722   708   719   727   716   722   719   721   724   714   710   711   716   732   734   726   733   720   716   720   720   714   715
    ## 
    ## [[3]]
    ## 
    ## 
    ##        1   10   11   12   13   14   15   16   17   18   19    2   20   21   22   23    3    4    5    6    7    8    9
    ## ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## 1     52   49   50   53   52   49   52   51   52   49   51   49   51   51   53   51   49   51   52   50   50   51   51
    ## 10    51   51   51   52   52   51   55   51   55   54   50   50   52   54   52   53   51   54   53   53   52   49   49
    ## 11    54   52   51   54   55   53   52   50   52   51   53   51   52   52   53   48   53   53   52   53   52   53   51
    ## 12    51   50   50   53   51   51   53   54   52   52   50   50   50   51   54   55   51   53   55   52   51   51   50
    ## 13    50   51   50   52   49   50   50   50   50   51   52   52   50   51   52   52   52   53   49   50   55   52   50
    ## 14    53   51   52   50   51   52   49   51   51   51   51   50   52   54   52   51   52   48   51   52   50   52   51
    ## 2     49   50   53   52   49   52   50   54   52   52   49   52   53   51   50   53   52   52   49   51   53   51   52
    ## 3     54   50   51   51   50   53   49   50   52   49   51   51   51   52   50   54   52   51   53   51   50   52   52
    ## 4     51   48   53   54   50   50   51   53   51   52   49   49   49   54   53   49   53   53   48   49   52   50   52
    ## 5     51   50   52   50   52   51   50   51   51   52   49   51   53   52   55   53   52   49   51   51   50   51   52
    ## 6     49   52   50   52   52   52   53   52   52   51   50   50   53   54   53   51   51   52   51   52   54   49   51
    ## 7     54   54   53   52   50   54   51   51   52   50   54   53   51   53   52   50   56   50   51   52   53   50   50
    ## 8     51   50   50   50   51   51   52   52   51   51   50   51   49   51   53   52   55   51   51   53   49   51   54
    ## 9     52   50   53   52   52   53   52   51   51   49   51   52   50   52   52   54   54   50   50   51   49   52   50
