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

    ## My project metadata key is  763 !!

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
    ##                 combined   POOL   RIFFLE
    ## -------------  ---------  -----  -------
    ## site_knz_K2A           0   2254       23
    ## site_knz_MID         184    207      161
    ## site_knz_N1B         322   1817        0
    ## site_knz_N4D         322   4370       23
    ## site_knz_NT          138   3197     3289
    ## 
    ## [[2]]
    ## 
    ## 
    ##                    1   10   11   12   13   14   15   16   17   18   19      2      3     4    5    6    7    8    9
    ## -------------  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  -----  ----  ---  ---  ---  ---  ---
    ## site_knz_K2A     782    0    0    0    0    0    0    0    0    0    0    713    782     0    0    0    0    0    0
    ## site_knz_MID     322    0    0    0    0    0    0    0    0    0    0    115    115     0    0    0    0    0    0
    ## site_knz_N1B    1219    0    0    0    0    0    0    0    0    0    0    828     92     0    0    0    0    0    0
    ## site_knz_N4D    1679   23   23   23   23   23   23   23   23   23   23   1334   1334    23   23   23   23   23   23
    ## site_knz_NT     2691    0    0    0    0    0    0    0    0    0    0   2507   1219   161   46    0    0    0    0
    ## 
    ## [[3]]
    ## 
    ## 
    ##                1   10   11   12   13   14   15   16   17   18   19      2      3    4    5    6    7    8    9
    ## ---------  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  -----  ---  ---  ---  ---  ---  ---
    ## combined     966    0    0    0    0    0    0    0    0    0    0      0      0    0    0    0    0    0    0
    ## POOL        4347   23   23   23   23   23   23   23   23   23   23   4301   2760   92   23   23   23   23   23
    ## RIFFLE      1380    0    0    0    0    0    0    0    0    0    0   1196    782   92   46    0    0    0    0
