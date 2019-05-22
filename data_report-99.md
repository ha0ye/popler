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

    ## My project metadata key is  681 !!

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
    ##                                                        1    10    11    12    13    14    15    16    17    18    19    2    20    21    22    23    24    25    26    27    28    29    3    30    31    32    33    34    35    36    37    38    39    4    40    41    42    43    44    45    46    47    48    49    5    50    51    52    53    54    55    56    57    58    59     6    60    61    62    63    64    65    66    67    68    69     7    70    71    72    73    74    75    76    77    78    79     8    80    81    82    83    84    85    86    87    88    89     9    90    91
    ## ---------------------------------------------------  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_jrn_transect_line_intercept_alternate_control    45    71    77    92    85   102    90    92    77    73    98   39   105    80    88    58    68    58    87    59    79    96   35    75    73    89    80    85    74    94    82    83   103   46    87    96    91    88   111    86    84   116   104    94   39   111   103    87    80    90    82   108   102    98    75    75   101    60    55    63    57    65    69    72    64    61    51    61    42    56    80    62    66    79    60    90    85    84    72    69    72    92    77    70    53    58    55    85    75    83    49
    ## site_jrn_transect_line_intercept_control              68   140   232   222   217   171   223   240   228   228   213   76   208   205   185   224   214   192   224   237   243   247   87   229   216   251   281   235   239   205   227   234   229   62   230   223   217   222   249   265   257   234   209   224   62   255   248   265   281   223   236   212   212   177   198    73   178   151   159    91   125   136   175   120   103   111    92   121   159   201   242   193   253   241   228   207   231   106   176   190   188   175   186   212   197   218   203   195   136   164   179
    ## site_jrn_transect_line_intercept_treatment            75   186   249   223   191   212   222   203   251   189   188   77   204   163   207   196   241   164   186   188   174   201   85   226   214   240   226   212   209   188   180   160   149   78   188   179   258   214   243   257   239   246   220   233   74   248   220   236   225   258   259   220   216   232   166   100   151    89    58   148   143   115   136   100   126    97   146    67    53    95    52   229   198   225   202   229   187   130   244   189   172   199   197   172   183   153   215   218   130   164   174
