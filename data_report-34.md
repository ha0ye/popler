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

    ## My project metadata key is  130 !!

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
    ##                                 A     B     C     D     E     F     G     H     I    J    K     L     M    N     O     P    Q    R     S    X   XXX
    ## ---------------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ---  ----  ----  ---  ----  ----  ---  ---  ----  ---  ----
    ## site_and_moth_abundance_        0     0     0     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0    19
    ## site_and_moth_abundance_10     28   159     0     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_11     83    50   130    62     8     8     7    10     2   11   48   100   160   60    74     0    0    0     0    0     0
    ## site_and_moth_abundance_12     17    50    52    62     6    12     4    25    69    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_13    232     6    60    88     0   190    34     2     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_14     49     4     0     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_15     29   147    40   149   368   264    87    70     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_16     10   255   292    19   101    10    15   127   111    1   82     1   194    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_17     27   111   248    16    17    35    68    77    23    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_18    262    91    48     7    29     2     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_19     43    56    48     7     7    20    65     4     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_2      72    30    18    19     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_20     21   113    33    36     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_21    149   108     5     0     6     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_22     13    47   195    81    12   106     9     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_23    528    45    72    43    21   184    59   384    67   19    0    49    12   37     0    43    0    0     0   16     0
    ## site_and_moth_abundance_24     60    67    52     8    63     1    48     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_25     26     9   221    75    50     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_26      1   240   100    86    24    23    63     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_27     76    75    28    67    46     5     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_28    271    64   206   202     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_3      13   255    81    33    66    30   144     6    16   41    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_30    162     9    13     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_31    331    48    47    33     3     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_32      5   225    12    60    37     2     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_33     67    12    47    55     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_34     98    35    10    54     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_35      2    13    46    69     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_36     61    33    59    59    93    45     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_37     10   114     7    23    69    38     8    33     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_38    169   130     8     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_39      6   205     8    19   181    28     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_5      84   141    53    28    48     3    88    78    17    8   61   232    17    8   143   172   48   50   207    0     0
    ## site_and_moth_abundance_6      74    64    78    30   108   100   118    44    53    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_7      38    10   100    29    28    27     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_8      79   104     8     0     0     0     0     0     0    0    0     0     0    0     0     0    0    0     0    0     0
    ## site_and_moth_abundance_9      15    32   215    39    11    34    87    62     9    0    3    21     9    0   275    58   56    0     0    0     0
