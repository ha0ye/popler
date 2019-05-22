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

    ## My project metadata key is  781 !!

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
    ##                          banana_left_field   pasture_left_field   pasture_right_field   sugar_cane_right_field
    ## ----------------------  ------------------  -------------------  --------------------  -----------------------
    ## site_luq_EC_old_field                 1808                  543                   394                     2259
    ## 
    ## [[2]]
    ## 
    ## 
    ##                            1    10    11    12    13    14    15    16    17    18    19     2    20    21    22    23    24    25     3     4     5     6     7     8     9
    ## ----------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_luq_EC_old_field    264   224   170   199   211   188   181   175   174   197   196   246   184   152   196   191   214   218   207   193   174   193   178   244   235
    ## 
    ## [[3]]
    ## 
    ## 
    ##                             1    10   11   12   13   14   15   16   17    18    19     2   20   21   22   23    24   25    3    4    5    6    7     8     9
    ## -----------------------  ----  ----  ---  ---  ---  ---  ---  ---  ---  ----  ----  ----  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ----  ----
    ## banana_left_field         111    70   50   77   75   63   67   73   57    57    49   104   57   36   61   67    53   73   86   81   73   77   70   119   102
    ## pasture_left_field         44    32   30   25   14   16   14   17   15    14    28    43   28   17   28   18    25   19   22   18   17   16   15    14    14
    ## pasture_right_field        17    14   14   14   29   14   14   14   17    16    14    14   14   14   15   14    15   32   14   14   14   14   14    15    14
    ## sugar_cane_right_field     92   108   76   83   93   95   86   71   85   110   105    85   85   85   92   92   121   94   85   80   70   86   79    96   105
