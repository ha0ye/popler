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

    ## My project metadata key is  790 !!

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
    ##                      1     1D      2      3      4
    ## ---------------  -----  -----  -----  -----  -----
    ## site_luq_CTE_A    6536      0   3268   5332   2752
    ## site_luq_CTE_B    8944   1204   7740   3440   5160
    ## site_luq_CTE_C    5160      0   5848   4128   2580
    ## 
    ## [[2]]
    ## 
    ## 
    ##                   CECR   DACR   GUET   MANI   MICO   PRES   PSYCH   SLOA
    ## ---------------  -----  -----  -----  -----  -----  -----  ------  -----
    ## site_luq_CTE_A    1204   2924      0   2752   2924   2924    2064   3096
    ## site_luq_CTE_B    2924   3956    172   3956   3784   4300    3268   4128
    ## site_luq_CTE_C    1548   3268      0   2064   1720   3612    2064   3440
    ## 
    ## [[3]]
    ## 
    ## 
    ##       CECR   DACR   GUET   MANI   MICO   PRES   PSYCH   SLOA
    ## ---  -----  -----  -----  -----  -----  -----  ------  -----
    ## 1     1032   3784      0   2580   3440   3784    2408   3612
    ## 1D     172    172      0    172    172    172     172    172
    ## 2     2408   2580    172   2064   1892   2752    2064   2924
    ## 3     1032   2236      0   2064   1720   2236    1548   2064
    ## 4     1032   1376      0   1892   1204   1892    1204   1892
