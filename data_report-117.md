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

    ## My project metadata key is  775 !!

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
    ##                    1      2      3      4      5      6     7
    ## -------------  -----  -----  -----  -----  -----  -----  ----
    ## site_luq_es1    3672   3834   3292   3296   3590   3610     0
    ## site_luq_es2       0   3178   4930   5946   5606   4476     0
    ## site_luq_rb2       0      0    210    210    264    376   366
    ## 
    ## [[2]]
    ## 
    ## 
    ##                  10     11     12   13     14     15     16     17     18     19     2     20     21     22    23     24     25     26     27    28     3     30    32     34     37     38      4     40     5      6      7     8      9
    ## -------------  ----  -----  -----  ---  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  ----  -----  -----  -----  -----  ----  ----  -----  ----  -----  -----  -----  -----  -----  ----  -----  -----  ----  -----
    ## site_luq_es1    698      0   2020    0      0   1504   1846      0   1024   2720     0      0      0      0   964   1640      0      0      0     0   868      0     0      0      0      0   1742      0     0   1672   2092   744   1760
    ## site_luq_es2      0   1168     22    0   1910      0    556   1266      0   1052     0   1128   1068   1856    10    966   1852   2188   1100   664     0   1234   594   2204   1012   1168      0   1118     0      0      0     0      0
    ## site_luq_rb2      0     64    186   56     82    110      0      0     40     10   132      0     82      0     0     28      0      0      0     0    50      0     0      0      0      0      0      0   120    166     52    68    180
    ## 
    ## [[3]]
    ## 
    ## 
    ##   10     11     12   13     14    15     16     17     18     19    2     20     21     22    23    24     25     26     27    28     3     30    32     34    37     38     4     40    5     6      7     8      9
    ## ----  -----  -----  ---  -----  ----  -----  -----  -----  -----  ---  -----  -----  -----  ----  ----  -----  -----  -----  ----  ----  -----  ----  -----  ----  -----  ----  -----  ---  ----  -----  ----  -----
    ##    0      0   1028    0      0     0      0      0      0      0    0      0      0      0     0   822      0      0      0     0     0      0     0      0     0      0   934      0    0     0    888     0      0
    ##    0      0     22    0      0     0   1488      0      0    830    0      0      0    624    10     0    700      0      0   664   868     32   562      0     0      8     0      0    0     0   1204     0      0
    ##  698      0     84    0    804    40    914      0     18   1860    0      0      0      0     0   948      0      0   1100     0     0      0    32      0   994      0     0      0    0   940      0     0      0
    ##    0   1148     44    0     14   702      0   1266   1018     20    0      0   1142      0     0   846      0   1230      0     0     0   1202     0      0    18      0     0      0    0     0     52     0    750
    ##    0     20      0   56   1092     0      0      0      0   1072   60   1128      0      0   964    18     38    958      0     0     0      0     0   1214     0   1138   808      0   40    12      0   744     98
    ##    0      0   1050    0      0   872      0      0     28      0    0      0      8   1232     0     0   1114      0      0     0    50      0     0    990     0     22     0   1118    0   886      0     0   1092
    ##    0     64      0    0     82     0      0      0      0      0   72      0      0      0     0     0      0      0      0     0     0      0     0      0     0      0     0      0   80     0      0    68      0
    ## 
    ## [[4]]
    ## 
    ## 
    ##                 CoverL   CoverR
    ## -------------  -------  -------
    ## site_luq_es1     10647    10647
    ## site_luq_es2     12068    12068
    ## site_luq_rb2       713      713
    ## 
    ## [[5]]
    ## 
    ## 
    ##  CoverL   CoverR
    ## -------  -------
    ##    1836     1836
    ##    3506     3506
    ##    4216     4216
    ##    4726     4726
    ##    4730     4730
    ##    4231     4231
    ##     183      183
    ## 
    ## [[6]]
    ## 
    ## 
    ##       CoverL   CoverR
    ## ---  -------  -------
    ## 10       349      349
    ## 11       616      616
    ## 12      1114     1114
    ## 13        28       28
    ## 14       996      996
    ## 15       807      807
    ## 16      1201     1201
    ## 17       633      633
    ## 18       532      532
    ## 19      1891     1891
    ## 2         66       66
    ## 20       564      564
    ## 21       575      575
    ## 22       928      928
    ## 23       487      487
    ## 24      1317     1317
    ## 25       926      926
    ## 26      1094     1094
    ## 27       550      550
    ## 28       332      332
    ## 3        459      459
    ## 30       617      617
    ## 32       297      297
    ## 34      1102     1102
    ## 37       506      506
    ## 38       584      584
    ## 4        871      871
    ## 40       559      559
    ## 5         60       60
    ## 6        919      919
    ## 7       1072     1072
    ## 8        406      406
    ## 9        970      970
