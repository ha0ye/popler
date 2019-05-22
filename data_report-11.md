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

    ## My project metadata key is  30 !!

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
    ##          1     10    11     12    13     14    15    16    17    18    19      2    20    21     22     23    24    25     26    27    28     29      3    30     31     32    33     34    35    36    37    38     39      4     40     5      6     7     8     9
    ## ---  -----  -----  ----  -----  ----  -----  ----  ----  ----  ----  ----  -----  ----  ----  -----  -----  ----  ----  -----  ----  ----  -----  -----  ----  -----  -----  ----  -----  ----  ----  ----  ----  -----  -----  -----  ----  -----  ----  ----  ----
    ## G      374    522   456    514   521    495   457   233   237   261   315    589   275   374    308    315   343   287    500   513   525    474    518   525    475    547   425    475   524   503   364   507    498    512    460   526    479   529   552   415
    ## MG     863    909   901    966   994   1014   912   491   636   471   443    964   538   430    460    542   418   523   1007   785   922    935   1041   853   1021   1022   903   1075   927   928   745   898   1000    894   1022   950    855   908   859   974
    ## MS    1092   1015   898   1065   846    787   679   737   697   867   856   1038   898   788   1041   1082   952   722    809   824   800   1003    920   953    907    828   936    790   883   857   915   815   1014   1057    976   824   1233   529   882   963
