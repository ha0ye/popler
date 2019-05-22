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

    ## My project metadata key is  822 !!

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
    ##                              1    17    19    21     24    27    31    43     50    56     6    67     7     9     G   GILL   GN
    ## ------------------------  ----  ----  ----  ----  -----  ----  ----  ----  -----  ----  ----  ----  ----  ----  ----  -----  ---
    ## site_ntl_crystal_lake        0     0     0     0      0   509     0   645      0     0   534     0     0   425     0    542   26
    ## site_ntl_sparkling_lake    529     0   477   585   2411     0     0     0      0     0     0     0     0     0     0    459    0
    ## site_ntl_trout_lake          0   678     0     0      0     0   772     0   4623   836     0   851   581     0   203    330    0
    ## 
    ## [[2]]
    ## 
    ## 
    ##                               1      2      3
    ## ------------------------  -----  -----  -----
    ## site_ntl_crystal_lake       942    835    904
    ## site_ntl_sparkling_lake    1425   1530   1506
    ## site_ntl_trout_lake        2972   2908   2994
    ## 
    ## [[3]]
    ## 
    ## 
    ##            1      2      3
    ## -----  -----  -----  -----
    ## 1        176    178    175
    ## 17       226    212    240
    ## 19       159    159    159
    ## 21       195    195    195
    ## 24       742    845    824
    ## 27       166    169    174
    ## 31       261    243    268
    ## 43       218    210    217
    ## 50      1531   1550   1542
    ## 56       267    292    277
    ## 6        181    173    180
    ## 67       300    262    289
    ## 7        201    186    194
    ## 9        172    125    128
    ## G         76     53     74
    ## GILL     458    415    458
    ## GN        10      6     10
