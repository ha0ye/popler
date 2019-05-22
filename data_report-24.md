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

    ## My project metadata key is  74 !!

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
    ##                       1    10    11    12    13    14    15    16    17    18    19     2    20    21    22    23    24    25    26    27    28    29     3    30    31    32    33    34    35   36   37   38   39     4   40   41   42   43   44   45     5     6     7     8     9
    ## -----------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ----  ----  ----  ----  ----
    ## site_sgs_gztx_11    394   397   377   398   426   390   418   391   402   411   411   398   379     0     0     0     0     0     0     0     0     0   389     0     0     0     0     0     0    0    0    0    0   384    0    0    0    0    0    0   407   406   399   382   420
    ## site_sgs_gztx_19    369   395   370   377   363   379   382   368   367   368   377   370   387     0     0     0     0     0     0     0     0     0   368     0     0     0     0     0     0    0    0    0    0   386    0    0    0    0    0    0   372   373   372   387   382
    ## site_sgs_gztx_24    409   393   434   439   403   436   425   435   416   431   420   416   411     0     0     0     0     0     0     0     0     0   451     0     0     0     0     0     0    0    0    0    0   401    8    0    0    0    0    0   419   410   410   425   421
    ## site_sgs_gztx_5A    408   422   426   425   421   426   422   427   414   408   408   418   402   274   272   270   282   283   269   288   279   273   414   287   260   265   261   263   253   58   59   59   62   423   59   57   54   64   63   62   425   415   405   415   419
    ## site_sgs_gztx_5B    405   415   434   418   427   419   423   429   426   421   413   408   432   290   287   275   307   290   287   271   265   262   413   279   263   267   255   280   260   58   59   57   57   405   64   64   60   56   56   69   414   437   435   415   431
    ## site_sgs_gztx_7C    453   447   435   433   425   453   452   429   454   438   439   455   458   297   293   303   299   291   323   289   286   300   442   302   278   261   284   275   271   60   63   64   57   447   67   65   71   64   62   66   444   453   442   469   410
