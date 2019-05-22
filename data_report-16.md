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

    ## My project metadata key is  54 !!

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
    ##            A      B      C      D      E   pooled
    ## -----  -----  -----  -----  -----  -----  -------
    ## BOER    1526   1537   1657   1601   1528        6
    ## BOGR     411    421    327    373    422        0
    ## LATR    1248   1292   1237   1296   1521        6
    ## PJ       823    877   1091   1037   1150        4
    ## 
    ## [[2]]
    ## 
    ## 
    ##          1.0   2.0    3.0   4.0    5.0   6.0   NA
    ## -----  -----  ----  -----  ----  -----  ----  ---
    ## BOER    2350   244   2325   254   2399   277    6
    ## BOGR     659     0    606     0    689     0    0
    ## LATR    2101   191   1887   183   2017   215    6
    ## PJ      1481   208   1675   222   1206   186    4
    ## 
    ## [[3]]
    ## 
    ## 
    ##            1.0   2.0    3.0   4.0    5.0   6.0   NA
    ## -------  -----  ----  -----  ----  -----  ----  ---
    ## A         1340   133   1088   126   1194   127    0
    ## B         1204   125   1226   135   1312   125    0
    ## C         1392   120   1375   128   1157   140    0
    ## D         1331   132   1338   127   1249   130    0
    ## E         1324   133   1466   143   1399   156    0
    ## pooled       0     0      0     0      0     0   16
