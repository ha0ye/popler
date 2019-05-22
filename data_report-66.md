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

    ## My project metadata key is  298 !!

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
    ##            1     2     3
    ## ------  ----  ----  ----
    ## GCE1     548   548     0
    ## GCE10    516   516   160
    ## GCE2     548   548     0
    ## GCE3     548   548     0
    ## GCE4     548   548     0
    ## GCE5     548   548     0
    ## GCE6     548   548     0
    ## GCE7     548   548     0
    ## GCE8     548   548     0
    ## GCE9     548   548    64
    ## 
    ## [[2]]
    ## 
    ## 
    ##            1   11   13   16   18    2   21   23   26   28     3   31   33   36   38    4   43   46   48    5     6    7     8
    ## ------  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ----
    ## GCE1     274    0   24    0   16    8    0    0    0   24   250    0    0   16    0    8    0    8    0    0   242    0   226
    ## GCE10    298    0   16    0    0    0    0    0    0    0   282    0    0    0    0    0    0    0    0    0   298    0   298
    ## GCE2     234   16    0    0    0    0   16    8    0    0   234    8   32    0   16    0    0    0   24    0   267    7   234
    ## GCE3     274    0    0    0   24    0    0    0    0    0   274    0    0    0    0    0    0    0    0    0   274    0   250
    ## GCE4     252    0    0    0    0   16    0    0    8    0   260    0    0    8    0   23    0   24    0   14   218    7   266
    ## GCE5     270    0   16   40    8    4    0    8    0    0   243    0    0    0    0   11    0    0    0    0   230    0   266
    ## GCE6     274    0    0    0    8    0    0    0    8    0   250    0   24    0    0    0    0    0    0    0   266    0   266
    ## GCE7     274    0    0    0    0    7    0    0    0    0   267    0    0    0    0    0    0    0    0    0   274    0   274
    ## GCE8     219    8   40    0   40   14   24    0   24    8   227   16    0    0   16    7    0    0    0    0   236   14   203
    ## GCE9     258    8    0   16   24    0   16   16   24    0   250    8   16    0    0    0    8    0    0    0   243    7   266
    ## 
    ## [[3]]
    ## 
    ## 
    ##     1   11   13   16   18    2   21   23   26   28      3   31   33   36   38    4   43   46   48    5      6    7      8
    ## -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  -----
    ##  1224   24   56   56   80   42   56   32   64   32   1173   32   72   24   32   42    8   32   24    7   1137   21   1178
    ##  1347    8   40    0   40    7    0    0    0    0   1308    0    0    0    0    7    0    0    0    7   1355   14   1315
    ##    56    0    0    0    0    0    0    0    0    0     56    0    0    0    0    0    0    0    0    0     56    0     56
