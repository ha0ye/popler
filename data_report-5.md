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

    ## My project metadata key is  13 !!

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
    ##              10    20    30
    ## ---------  ----  ----  ----
    ## BC_I        873   873   873
    ## BC_II       873   873   873
    ## BC_Point    540   540   540
    ## Diablo      765   765   765
    ## Fern_0      729   729   729
    ## Fern_1      639   639   639
    ## Fern_2       72    72    72
    ## Fern_3       72    72    72
    ## Frys_4       72    72    72
    ## Frys_I      666   666   666
    ## Frys_II     666   666   666
    ## Frys_III    666   666   666
    ## THE         774   774   774
    ## THW         774   774   774
    ## 
    ## [[2]]
    ## 
    ## 
    ##               1     2     3     4     5     6    7    8
    ## ---------  ----  ----  ----  ----  ----  ----  ---  ---
    ## BC_I        729   675   405   243   108    81   81   81
    ## BC_II       729   675   405   243   108    81   81   81
    ## BC_Point    621   567   297   135     0     0    0    0
    ## Diablo      702   648   351   216   108   108   81   81
    ## Fern_0      675   621   324   189   108   108   81   81
    ## Fern_1      648   567   297   135    27    27    0    0
    ## Fern_2        0     0     0     0     0     0    0    0
    ## Fern_3        0     0     0     0     0     0    0    0
    ## Frys_4        0     0     0     0     0     0    0    0
    ## Frys_I      648   594   324   162    27    27    0    0
    ## Frys_II     648   594   324   162    27    27    0    0
    ## Frys_III    648   594   324   162    27    27    0    0
    ## THE         675   621   351   216   108    81   81   81
    ## THW         675   621   351   216   108    81   81   81
    ## 
    ## [[3]]
    ## 
    ## 
    ##          1      2      3     4     5     6     7     8
    ## ---  -----  -----  -----  ----  ----  ----  ----  ----
    ## 10    2466   2259   1251   693   252   216   162   162
    ## 20    2466   2259   1251   693   252   216   162   162
    ## 30    2466   2259   1251   693   252   216   162   162
