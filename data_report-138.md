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

    ## My project metadata key is  826 !!

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
    ##                                      1     10     11     12    13    14    15    16    17    18    19      2   20   21   22   23   24   25   26   27   28   29      3   30      4      5      6      7      8      9
    ## ------------------------------  ------  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  -----  -----  -----  -----  -----  -----
    ## site_ntl_allequash_lake           8207   1297   1580   1251   312   296   334   197   231   405     9   7655    5   48    3   48    6    2   11   11    4   10   5157    5   3240   2791   2514    962    906   1338
    ## site_ntl_big_muskellunge_lake     6268   1206    301    518   278   188   328   284   206   300    34   6330   22   13    4    1    2    5    2    5    6    5   4923    2   3431   2339   2741    909    813    999
    ## site_ntl_crystal_bog_lake          953    116    146    142    86   139   117   121   105    94    65    675   44   55   47   50   49   44   66   40   41   35    637   27    647    663    572    101    111    122
    ## site_ntl_crystal_lake            10590    606    706    736   286   208   177   180   199   140     0   9669    0    3    1    2    2    2    3    2    0    0   1814    0   1386   1764   1575    683    627    767
    ## site_ntl_fish_lake                4581    573    773    735   176   205   168   152   312   232     4   5976    3    0    6    1    0    1    4    4    1    3   4277    4   1451   1654   1585    653    534    598
    ## site_ntl_mendota_lake             6606    975    995   1353   266   117   121   111   248    24     0   6207    2    0    0    0    0    0    2    1    0    0   3391    0   1225   1452    988    506    637    874
    ## site_ntl_monona_lake              5748    803   1086   1104   159   138   100   169   202   227     0   4275    0    2    6    1    7    4    1    0    0    0   5429    0   2004   1541   1218    565    498    874
    ## site_ntl_sparkling_lake           5831    894   1084   1043   340   373   612   366   419   286     8   6381   11    8    7   23   10    3   11    6    5    6   3988    1   3048   2282   2560   1140   1238    800
    ## site_ntl_trout_bog_lake            325    103    100     89    83   122   138   110   119    97    39    406   44   38   44   49   44   44   35   43   31   29    425   20    354    317    370     87    111    109
    ## site_ntl_trout_lake              10950   1051   1209   1401   279   326   524   636   478   574   115   9657    3    1   34   28   26    6    8   11    4    6   4218    4   2498   2777   2440   1330   1262   1094
    ## site_ntl_wingra_lake              9244   1271   1690   1369   346   294   289   212   256   203     2   8424    7    0    0    5    3    1    2    0    0    0   4774    0   2320   1744   1767   2200   1716   1255
