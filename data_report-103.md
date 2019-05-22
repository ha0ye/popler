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

    ## My project metadata key is  714 !!

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
    ##               BASN   CALI   EAST   GRAV   IBPE   NORT   RABB   SAND   SUMM   WELL   WEST
    ## -----------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----
    ## site_jrn_C       0    318      0    287      1      0      3    440      0      0      0
    ## site_jrn_G     383      0      0      1    592      0      1      0    148      0      0
    ## site_jrn_M       0      0      1      0      0    205    505      0      0    442      3
    ## site_jrn_T       0      0    478      0      0      0      0      0      0      0    283
    ## 
    ## [[2]]
    ## 
    ## 
    ##                 A     B     C     D
    ## -----------  ----  ----  ----  ----
    ## site_jrn_C    350   344    89   266
    ## site_jrn_G    302   363   459     1
    ## site_jrn_M    413   430   313     0
    ## site_jrn_T    300   461     0     0
    ## 
    ## [[3]]
    ## 
    ## 
    ##           A     B     C     D
    ## -----  ----  ----  ----  ----
    ## BASN     17   362     4     0
    ## CALI    311     5     0     2
    ## EAST     22   457     0     0
    ## GRAV     13     1    12   262
    ## IBPE    139     0   454     0
    ## NORT     23     0   182     0
    ## RABB    368    11   129     1
    ## SAND     25   337    76     2
    ## SUMM    146     0     2     0
    ## WELL     23   417     2     0
    ## WEST    278     8     0     0
    ## 
    ## [[4]]
    ## 
    ## 
    ##                 .    0    1   10   11   12   13   14   15   16    2    3    4    5    6    7    8    9
    ## -----------  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## site_jrn_C    348   27   66   47   41   50   49   44   36   40   43   45   29   38   26   34   56   30
    ## site_jrn_G    305   28   67   35   50   39   40   57   47   58   58   63   57   46   36   44   59   36
    ## site_jrn_M    286   17   64   37   42   51   67   43   85   49   58   35   60   53   47   55   62   45
    ## site_jrn_T    207   16   32   51   26   47   33   20   26   32   35   54   45   35   15   33   28   26
    ## 
    ## [[5]]
    ## 
    ## 
    ##           .    0    1   10   11   12   13   14   15   16    2    3    4    5    6    7    8    9
    ## -----  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## BASN     87    6   38    9   23   15   10   33   11   18   22   16   16   20    5   11   25   18
    ## CALI    119   11   13   17   19   11   11   17   16    8   12    8    9    9    9    5   19    5
    ## EAST     73    5   23   43   18   33   22   15   17   22   27   45   38   21   12   24   20   21
    ## GRAV    120    8   16   10    7    9   10    6    7    7   10   10    9   12    9   18    7   13
    ## IBPE    128    7   26   24   26   20   26   22   35   37   28   41   40   23   28   32   32   18
    ## NORT     63    7   17    3   10   13    7    7    5    6    4    8   12   13    3    3   14   10
    ## RABB    138    5   23   18    4   22   38   23   46   19   25   17   13   23   18   27   26   24
    ## SAND    107    8   37   20   15   30   28   21   13   25   21   27   11   17    8   11   29   12
    ## SUMM     88   15    3    2    1    4    4    2    1    3    8    6    1    3    3    1    3    0
    ## WELL     89    5   24   15   28   16   22   13   34   23   28   10   35   17   26   24   22   11
    ## WEST    134   11    9    9    8   14   11    5    9   11    9    9    7   14    3   10    8    5
    ## 
    ## [[6]]
    ## 
    ## 
    ##         .    0     1   10   11   12   13   14   15   16    2    3    4    5    6    7    8    9
    ## ---  ----  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## A     476   46    57   52   39   57   70   56   78   42   63   54   43   49   39   49   54   41
    ## B     348   22   108   77   79   84   71   71   66   80   89   87   95   66   45   65   90   55
    ## C     205   13    49   31   36   38   41   31   43   52   32   46   45   46   32   37   55   29
    ## D     117    7    15   10    5    8    7    6    7    5   10   10    8   11    8   15    6   12
