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

    ## My project metadata key is  223 !!

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
    ##                              1    10    11    12    13    14    15    16    17    18    19     2    20    21    22    23    24    25    26    27    28    29     3    30    31    32    33    34    35    36    37    38    39     4    40    41    42    43    44    45    46    47    48    49     5    50    51    52    53    54     6     7     8     9
    ## ------------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_cdr_exp002_field_A    112   128    84   175   129   121   101   150   163    84    85   186    90   120   134   129   117    92   103   101    75   144   175   171    73    82    95   131   118   159   190   154   114   181    99   130   124    77   104   100   104   116   131   117    70   181    80    80   127   147   193   147   119    82
    ## site_cdr_exp002_field_B    129   145   167   146   160   193   226   247   110   161    92   215   157   115   119   199   225   103   121   236   202   258    93   255   179   173   226   226   243   238   104   115   103   131   249   204   235   239   196   110   219   144   171   191   256   233   261   159   140   267   177   249   238   116
    ## site_cdr_exp002_field_C    185   321   308   191   119   140   212   263   204   349   125   208   195   293   154   334   312   181   175   112   128   292   245   313   273   324   264   243   221   292   256   283   340   208   336   238   250   275   258   310   353   353   239   311   228   369   281   336   360   310   270   164   204   328
    ## 
    ## [[2]]
    ## 
    ## 
    ##                            East   West   Whole
    ## ------------------------  -----  -----  ------
    ## site_cdr_exp002_field_A       0      0    6594
    ## site_cdr_exp002_field_B     284    290    9292
    ## site_cdr_exp002_field_C     199    184   13453
    ## 
    ## [[3]]
    ## 
    ## 
    ##       East   West   Whole
    ## ---  -----  -----  ------
    ## 1        4      6     416
    ## 10       4      4     586
    ## 11       4      6     549
    ## 12      14     11     487
    ## 13       3      3     402
    ## 14       5      6     443
    ## 15      11     11     517
    ## 16      11     10     639
    ## 17       3      4     470
    ## 18      19     18     557
    ## 19       8      7     287
    ## 2        6      7     596
    ## 20       5      5     432
    ## 21       3      4     521
    ## 22      15     12     380
    ## 23      15     15     632
    ## 24       9      6     639
    ## 25       2      5     369
    ## 26       3      5     391
    ## 27       4      7     438
    ## 28      13     10     382
    ## 29       0      0     694
    ## 3        8      8     497
    ## 30       0      0     739
    ## 31       6      8     511
    ## 32       6      6     567
    ## 33      18     15     552
    ## 34       0      0     600
    ## 35       7      9     566
    ## 36      18     17     654
    ## 37      12      9     529
    ## 38      16     14     522
    ## 39       5      4     548
    ## 4       10     11     499
    ## 40      14      9     661
    ## 41      17     14     541
    ## 42      18     16     575
    ## 43       9     10     572
    ## 44      17     14     527
    ## 45       3      6     511
    ## 46       9      6     661
    ## 47       5      6     602
    ## 48      13     19     509
    ## 49      15     17     587
    ## 5        0      0     554
    ## 50       9      8     766
    ## 51      11     10     601
    ## 52      27     16     532
    ## 53       2      5     620
    ## 54       8      9     707
    ## 6       12     12     616
    ## 7       11     17     532
    ## 8       14     15     532
    ## 9        2      2     522
