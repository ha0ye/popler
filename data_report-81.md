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

    ## My project metadata key is  398 !!

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
    ##                                  Control   Pulldown
    ## ------------------------------  --------  ---------
    ## site_hfr_hurricane_experiment        239       4331
    ## 
    ## [[2]]
    ## 
    ## 
    ##                                   1   10    11   12   13   14   15   16   17   18   19    2   20   21   22   23   24   25   26   27   28   29    3   30   31   32   33   34   35   36   37   38    39    4    40   41   42   43   44   45   46    47   48   49    5   50   51   52    53   54   55   56   57   58   59    6    60    61   62   63   64   65   66   67   68   69    7   70   71   72   73   74   75   76   77   78   79    8   80    9
    ## ------------------------------  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ----  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ----  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## site_hfr_hurricane_experiment    30   13   102   38   39   26   43   17   36   43   37   40   22   49   55   59   72   40   80   92   83   75   57   59   67   39   31   67   75   63   94   95   114   72   109   82   60   96   53   80   83   102   48   42   58   34   68   59   102   64   45   63   88   57   83   57   124   104   97   37   53   29   31   69   37   85   28   24   18    9   16   43   34   64   53   38   40   13   46   21
    ## 
    ## [[3]]
    ## 
    ## 
    ##              1   10    11   12   13   14   15   16   17   18   19    2   20   21   22   23   24   25   26   27   28   29    3   30   31   32   33   34   35   36   37   38    39    4    40   41   42   43   44   45   46   47   48   49    5   50   51   52   53   54   55   56   57   58   59    6    60    61   62   63   64   65   66   67   68   69    7   70   71   72   73   74   75   76   77   78   79    8   80    9
    ## ---------  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---
    ## Control      2    0     0    6   17    1    0    0    0    0    0    8    0   14    4    8    6    4    0    4    3    0    6    2    0   16    5    9    3    4   21   11     2    3     2    2    4    0    0    9    0    3    0   18    1    7    1    0    4    5    5    7    6    5    0    0     0     0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    0    1
    ## Pulldown    28   13   102   32   22   25   43   17   36   43   37   32   22   35   51   51   66   36   80   88   80   75   51   57   67   23   26   58   72   59   73   84   112   69   107   80   56   96   53   71   83   99   48   24   57   27   67   59   98   59   40   56   82   52   83   57   124   104   97   37   53   29   31   69   37   85   28   24   18    9   16   43   34   64   53   38   40   13   46   20
