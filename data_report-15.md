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

    ## My project metadata key is  53 !!

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
    ##           1       2      3
    ## ---  ------  ------  -----
    ## P     16703   17727   7099
    ## 
    ## [[2]]
    ## 
    ## 
    ##          A      B      C      D      N      R      S       V
    ## ---  -----  -----  -----  -----  -----  -----  -----  ------
    ## P     1886   1940   1813   1460   7736   6342   7674   12678
    ## 
    ## [[3]]
    ## 
    ## 
    ##     A      B      C      D      N      R      S      V
    ## -----  -----  -----  -----  -----  -----  -----  -----
    ##     0      0      0      0   3847   3026   3119   6711
    ##     0      0      0      0   3889   3316   4555   5967
    ##  1886   1940   1813   1460      0      0      0      0
    ## 
    ## [[4]]
    ## 
    ## 
    ##          1     10      2      3      4      5      6      7      8      9
    ## ---  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## P     4316   3818   4956   5112   5164   4525   3687   3265   3498   3188
    ## 
    ## [[5]]
    ## 
    ## 
    ##     1     10      2      3      4      5      6      7      8      9
    ## -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ##  1546   1752   1471   1715   1721   1929   1876   1518   1698   1477
    ##  1664   2066   1860   1870   1843   1355   1811   1747   1800   1711
    ##  1106      0   1625   1527   1600   1241      0      0      0      0
    ## 
    ## [[6]]
    ## 
    ## 
    ##          1     10      2      3      4      5      6      7      8      9
    ## ---  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## A      249      0    281    581    512    263      0      0      0      0
    ## B      200      0    413    504    400    423      0      0      0      0
    ## C      299      0    430    201    425    458      0      0      0      0
    ## D      358      0    501    241    263     97      0      0      0      0
    ## N      701   1071    614    910    793    625    629    928    819    646
    ## R      511    778    608    617    780    576    558    529    667    718
    ## S      810    891    897    727    639    505    974    601    846    784
    ## V     1188   1078   1212   1331   1352   1578   1526   1207   1166   1040
