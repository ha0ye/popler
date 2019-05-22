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

    ## My project metadata key is  80 !!

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
    ##                         1      2      3
    ## ------------------  -----  -----  -----
    ## site_sgs_hummus_E    1487   1527   1515
    ## site_sgs_hummus_W    1598   1602   1594
    ## 
    ## [[2]]
    ## 
    ## 
    ##                        1     2     3     4     5     6
    ## ------------------  ----  ----  ----  ----  ----  ----
    ## site_sgs_hummus_E    736   829   687   767   763   747
    ## site_sgs_hummus_W    806   864   763   799   790   772
    ## 
    ## [[3]]
    ## 
    ## 
    ##    1     2     3     4     5     6
    ## ----  ----  ----  ----  ----  ----
    ##  516   548   475   520   505   521
    ##  522   574   492   547   512   482
    ##  504   571   483   499   536   516
