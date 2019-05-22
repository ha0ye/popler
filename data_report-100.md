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

    ## My project metadata key is  696 !!

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
    ##                               0.0   0.1   0.2   0.3   0.4   0.5   0.6   0.7   0.8   0.9   1.0   1.1   1.2   1.3   1.4   1.5   1.6   1.7   1.8   1.9   2.0   2.1   2.2   2.3   2.4   2.5   2.6   2.7   2.8   2.9   3.0   3.1   3.2   3.3   3.4   3.5   3.6   3.7   3.8   3.9   4.0   4.1   4.2   4.3   4.4   4.5   4.6   4.7   4.8   4.9   5.0   5.1   5.2   5.3   5.4   5.5   5.6   5.7   5.8   5.9   6.0   6.1   6.2   6.3   6.4   6.5   6.6   6.7   6.9   7.0   7.2   7.4   7.6   7.8   8.0   8.3   8.5   9.0   9.1   9.2   NA
    ## ---------------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---
    ## site_jrn_zone_creosotebush     69    12     7    10     4     7     6     9     8    10     8    10     5    20    11     6     6     3     2     6     5     3     4     4     3    55     2     0     4     2     7    10    10     9    10     9     4     6     8     4     4     3     5     2     5     3     2     6     5     5     8     5    13     3     1     7    13     4     9     3    12    54    17     1     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0    5
    ## site_jrn_zone_grassland       126    42    53    64    56    36    22    26    17    30    23    10    14    25    33    48    35    28    32    26    30    40    29    28    28    35    41    39    42    33    36    40    39    42    37    42    46    65    49    44    38    40    53    41    40    33    40    43    53    45    66    50    74   111    67    51    41    31    24    16    30    45    41    55    13     3     1     1     1     3     1     1     4     3     1     1     1     1     1     1    4
