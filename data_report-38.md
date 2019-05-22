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

    ## My project metadata key is  143 !!

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
    ##                    1     10     11     12     13     14     15     16    17     18     19      2     20     21     22     23     24     25      3      4      5      6      7      8      9
    ## -------------  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## site_and_142    1381   1851   1080   1165   1171   1462   1263   1143   924   1419   1709   1577   1441   1173   1233   1267   1405   1170   1226   1561   1609   1762   1098   1349   1396
    ## 
    ## [[2]]
    ## 
    ## 
    ##                    1      2      3      4      5      6      7      8      9
    ## -------------  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## site_and_142    3686   3686   3161   3992   4213   4116   3768   3302   3911
    ## 
    ## [[3]]
    ## 
    ## 
    ##         1     2     3     4     5     6     7     8     9
    ## ---  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## 1     154   334   244   115   152    41    43    48   250
    ## 10    102   363    55   397   380   142   174    79   159
    ## 11     99   128    48    33   199    41   204   234    94
    ## 12    237    40    44   257   103    98   252    37    97
    ## 13     94    33   282   234   111   120    46   216    35
    ## 14    257   263    48    75   254   128    63   204   170
    ## 15    106    74   133   274   101   244    49   233    49
    ## 16     99    38   169   178   107   198   289    33    32
    ## 17    173    80    34   183   119    30    39    78   188
    ## 18    279   121   270   162   159   231    91    66    40
    ## 19     76   129    67    59   358   363    59   229   369
    ## 2      75   132   130   266   175   333    58    69   339
    ## 20     66   255   198    46   271   100   139    64   302
    ## 21    212    36    72   285    41   111   118    48   250
    ## 22    101    46   242   129   263   115    45   242    50
    ## 23     29   137    50   197   114   270   289   124    57
    ## 24    310   171    54   143   151   223   239    62    52
    ## 25    136    54   225   218    51    35   103   109   239
    ## 3     115   373    39   106    37   177   282    26    71
    ## 4     320   259   126   310   178    92   127    64    85
    ## 5      48    44   140    43   178   340   284   390   142
    ## 6     136    47    85    72   301   163   133   417   408
    ## 7      82   182   204    99    95   145   221    36    34
    ## 8      91   117    68    43   259   319   276    75   101
    ## 9     289   230   134    68    56    57   145   119   298
