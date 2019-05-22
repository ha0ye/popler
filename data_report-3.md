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

    ## My project metadata key is  4 !!

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
    ##            1      2      3      4      5      6      7      8
    ## -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## ABUR    2145   2002      0      0      0      0      0      0
    ## AHND    2002   2002      0      0      0      0      0      0
    ## AQUE    2002   2002   2002   2002   2002   1859      0      0
    ## BULL    2288    143   2145    143    143   2145    143    143
    ## CARP    2145   2145   2145   2145   2145   2145   2145   2145
    ## GOLB    2002   2145      0      0      0      0      0      0
    ## IVEE    2145   2002    572    143    572    572    572    572
    ## MOHK    2002   2002      0      0      0      0      0      0
    ## NAPL    2145   2145   2145   2145   2145   2145   2145   2145
    ## SCDI       0   1573   1573      0      0      0      0      0
    ## SCTW       0   1573   1573      0      0      0      0      0
