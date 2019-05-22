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

    ## My project metadata key is  828 !!

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
    ##                                    1     2     4     5     6     7    8
    ## ------------------------------  ----  ----  ----  ----  ----  ----  ---
    ## site_ntl_allequash_lake          576     0     0     0     0     0    0
    ## site_ntl_big_muskellunge_lake    579     0     0     0     0     0    0
    ## site_ntl_crystal_bog_lake        595     0     0     0     0     0    0
    ## site_ntl_crystal_lake            578     0     0     0     0     0    0
    ## site_ntl_sparkling_lake          577     0     0     0     0     0    0
    ## site_ntl_trout_bog_lake          597     0     0     0     0     0    0
    ## site_ntl_trout_lake                0   593   375   357   357   369   18
    ## 
    ## [[2]]
    ## 
    ## 
    ##                                  1.0   10.0   11.2   1.25   13.0   14.0   1.5   15.0   16.0   16.5   17.0   1.75   18.0   18.5   19.0   19.2   19.5   19.9   2.0   20.0   2.1   21.0   21.5   22.0   24.0   24.5   2.5   25.0   25.7   29.0   3.0   30.0   30.5   31.0   31.5   31.6   32.0   33.0   5.0   5.25   6.0   6.5   7.0   7.5   8.0   9.0   9.5
    ## ------------------------------  ----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  ----  -----  -----  -----  -----  -----  ----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  ----  -----  ----  ----  ----  ----  ----  ----  ----
    ## site_ntl_allequash_lake            0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0    40      0   115    60   316     0    45     0     0
    ## site_ntl_big_muskellunge_lake      0      0      0      0      0      0     0      0     15      0     96      0    215     20    218     15      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## site_ntl_crystal_bog_lake         40      0      0     20      0      0    15      0      0      0      0     80      0      0      0      0      0      0   325      0    15      0      0      0      0      0    70      0      0      0    30      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## site_ntl_crystal_lake              0      0      0      0      0      0     0      0      0      0    130      0    131      0    267      0     20     15     0     15     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## site_ntl_sparkling_lake            0      0      0      0      0      0     0      0     60      0    185      0    245     12     45      0      0     15     0     15     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## site_ntl_trout_bog_lake            0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0    35     20   115    95   287    15    15     0    15
    ## site_ntl_trout_lake                0    306      9      0     12     18     0    291     18      9     18      0     12      9     33      0      9      0     0    270     0      9     24     12     39     18     0    294      6     45     0     83     15     60     30      9    336     15     0      0     0     0     0     0     0    51     9
    ## 
    ## [[3]]
    ## 
    ## 
    ##       1.0   10.0   11.2   1.25   13.0   14.0   1.5   15.0   16.0   16.5   17.0   1.75   18.0   18.5   19.0   19.2   19.5   19.9   2.0   20.0   2.1   21.0   21.5   22.0   24.0   24.5   2.5   25.0   25.7   29.0   3.0   30.0   30.5   31.0   31.5   31.6   32.0   33.0   5.0   5.25   6.0   6.5   7.0   7.5   8.0   9.0   9.5
    ## ---  ----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  ----  -----  -----  -----  -----  -----  ----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  ----  -----  ----  ----  ----  ----  ----  ----  ----
    ## 1      40      0      0     20      0      0    15      0     75      0    411     80    591     32    530     15     20     30   325     30    15      0      0      0      0      0    70      0      0      0    30      0      0      0      0      0      0      0    75     20   230   155   603    15    60     0    15
    ## 2       0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0     45     0     83     15     60     30      9    336     15     0      0     0     0     0     0     0     0     0
    ## 4       0    306      9      0      0      0     0      0      0      0      0      0      0      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0    51     9
    ## 5       0      0      0      0     12     18     0    291     18      9      9      0      0      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## 6       0      0      0      0      0      0     0      0      0      0      0      0      3      9     33      0      9      0     0    270     0      9     24      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## 7       0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0      0     0      0     0      0      0     12     39     18     0    294      6      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## 8       0      0      0      0      0      0     0      0      0      0      9      0      9      0      0      0      0      0     0      0     0      0      0      0      0      0     0      0      0      0     0      0      0      0      0      0      0      0     0      0     0     0     0     0     0     0     0
    ## 
    ## [[4]]
    ## 
    ## 
    ##                                    1     2     3     4     5    6    7
    ## ------------------------------  ----  ----  ----  ----  ----  ---  ---
    ## site_ntl_allequash_lake          128   112   112   112   112    0    0
    ## site_ntl_big_muskellunge_lake    132   113   113   109   112    0    0
    ## site_ntl_crystal_bog_lake        135   115   115   115   115    0    0
    ## site_ntl_crystal_lake            131   111   113   113   110    0    0
    ## site_ntl_sparkling_lake          132   112   112   112   109    0    0
    ## site_ntl_trout_bog_lake          133   116   116   116   116    0    0
    ## site_ntl_trout_lake              595   602   573   138   137   12   12
    ## 
    ## [[5]]
    ## 
    ## 
    ##         1     2     3     4     5    6    7
    ## ---  ----  ----  ----  ----  ----  ---  ---
    ## 1     791   679   681   677   674    0    0
    ## 2     121   137   117   111   107    0    0
    ## 4     117   117   117    18     6    0    0
    ## 5     117   114   111     0    15    0    0
    ## 6     117   111   108     3     6   12    0
    ## 7     117   117   114     6     3    0   12
    ## 8       6     6     6     0     0    0    0
    ## 
    ## [[6]]
    ## 
    ## 
    ##           1     2     3     4     5    6    7
    ## -----  ----  ----  ----  ----  ----  ---  ---
    ## 1.0       8     8     8     8     8    0    0
    ## 10.0     94    94    94    18     6    0    0
    ## 11.2      3     3     3     0     0    0    0
    ## 1.25      4     4     4     4     4    0    0
    ## 13.0      4     4     4     0     0    0    0
    ## 14.0      6     6     6     0     0    0    0
    ## 1.5       3     3     3     3     3    0    0
    ## 15.0     95    92    89     0    15    0    0
    ## 16.0     21    21    21    15    15    0    0
    ## 16.5      3     3     3     0     0    0    0
    ## 17.0     89    89    89    79    83    0    0
    ## 1.75     16    16    16    16    16    0    0
    ## 18.0    139   116   118   115   115    0    0
    ## 18.5     10    10    10     7     4    0    0
    ## 19.0    149   110   110    99    95    0    0
    ## 19.2      3     3     3     3     3    0    0
    ## 19.5      7     7     7     4     4    0    0
    ## 19.9      6     6     6     6     6    0    0
    ## 2.0      81    61    61    61    61    0    0
    ## 20.0     94    91    88     6     9   12    0
    ## 2.1       3     3     3     3     3    0    0
    ## 21.0      3     3     3     0     0    0    0
    ## 21.5      6     6     6     3     3    0    0
    ## 22.0      4     4     4     0     0    0    0
    ## 24.0     13    13    13     0     0    0    0
    ## 24.5      6     6     6     0     0    0    0
    ## 2.5      14    14    14    14    14    0    0
    ## 25.0     91    91    91     6     3    0   12
    ## 25.7      3     3     0     0     0    0    0
    ## 29.0      9     9     9     9     9    0    0
    ## 3.0       6     6     6     6     6    0    0
    ## 30.0     21    17    17    14    14    0    0
    ## 30.5      3     3     3     3     3    0    0
    ## 31.0     12    12    12    12    12    0    0
    ## 31.5      6     6     6     6     6    0    0
    ## 31.6      3     3     3     0     0    0    0
    ## 32.0     64    84    64    64    60    0    0
    ## 33.0      3     3     3     3     3    0    0
    ## 5.0      15    15    15    15    15    0    0
    ## 5.25      4     4     4     4     4    0    0
    ## 6.0      46    46    46    46    46    0    0
    ## 6.5      31    31    31    31    31    0    0
    ## 7.0     147   114   114   114   114    0    0
    ## 7.5       3     3     3     3     3    0    0
    ## 8.0      12    12    12    12    12    0    0
    ## 9.0      17    17    17     0     0    0    0
    ## 9.5       6     6     6     3     3    0    0