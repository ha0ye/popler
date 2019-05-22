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

    ## My project metadata key is  171 !!

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
    ##                   1   10   11   12   13   14   15   16   17   18   19    2   20    3    4    5    6    7    8    9
    ## --------------  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## site_bnz_FP1A    14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14
    ## site_bnz_FP1B    13   13   13   13   13   13   13   13   13   13   13   13   13   13   13   13   13   13   13   13
    ## site_bnz_FP1C    15   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14
    ## site_bnz_FP2A    14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14
    ## site_bnz_FP2B    13   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14   14
    ## site_bnz_FP2C    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_FP3A    11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11
    ## site_bnz_FP3B    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_FP3C    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_FP4A    11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11
    ## site_bnz_FP4B    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_FP4C    11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11
    ## site_bnz_FP5A     1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
    ## site_bnz_FP5B     1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
    ## site_bnz_FP5C     1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1
    ## site_bnz_UP1A    19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19   19
    ## site_bnz_UP1B    16   16   16   16   16   16   16   16   16   16   16   16   16   16   16   16   16   16   16   16
    ## site_bnz_UP1C    17   17   17   17   17   17   17   17   17   17   17   17   17   17   17   17   17   17   17   17
    ## site_bnz_UP2A    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_UP2B    13   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_UP2C    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_UP3A    11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11   11
    ## site_bnz_UP3B    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12
    ## site_bnz_UP3C    12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12   12