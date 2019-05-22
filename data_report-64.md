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

    ## My project metadata key is  292 !!

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
    ##                      1     2     3     4     5     6     7     8     9
    ## ----------------  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_cdr_exp142    853   416   663   805   784   644   352   494   344
    ## 
    ## [[2]]
    ## 
    ## 
    ##                      1    10    11    12    13    14    15    16     2     3     4     5     6     7     8     9
    ## ----------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_cdr_exp142    326   344   283   304   332   352   308   317   356   324   434   364   306   325   359   321
    ## 
    ## [[3]]
    ## 
    ## 
    ##   1   10   11   12   13   14   15   16    2    3    4    5    6    7    8    9
    ## ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ##  71   56   43   57   52   52   63   56   67   45   50   43   36   41   70   51
    ##  29   20   18   21   18   30   35   19   37   20   46   27   22   20   27   27
    ##  42   35   36   32   44   47   28   47   38   36   38   54   32   62   59   33
    ##  36   76   44   34   58   46   48   42   44   43   76   48   56   55   48   51
    ##  37   47   61   45   44   56   46   56   60   53   58   53   48   45   32   43
    ##  38   39   27   50   33   43   33   27   43   31   64   54   47   28   40   47
    ##  17   26   17   22   30   23   20   19   26   27   25   19   20   14   22   25
    ##  33   23   20   25   34   35   19   30   20   42   50   39   28   34   34   28
    ##  23   22   17   18   19   20   16   21   21   27   27   27   17   26   27   16
