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

    ## My project metadata key is  73 !!

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
    ##             1.0    2.0    3.0    4.0    5.0   8.0    NA
    ## --------  -----  -----  -----  -----  -----  ----  ----
    ## site_73    4005   4096   3931   4049   4101     3   328
    ## 
    ## [[2]]
    ## 
    ## 
    ##             1.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0   17.0   18.0   19.0    2.0   20.0   21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0   29.0    3.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0   37.0   38.0   39.0    4.0   40.0   41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0   49.0    5.0   50.0    6.0    7.0    8.0    9.0   NA
    ## --------  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ---
    ## site_73    1929   1882     34     31     34     34     32     32     34     43     37   1897     32     38     32     38     45     41     29     35     42     34   1947     28     39     30     30     34     35     28     33     31     35   1930     31     37     32     30     37     32     33     38     33     45   1957     34   1900   1890   1894   1899    6
    ## 
    ## [[3]]
    ## 
    ## 
    ##        1.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0   17.0   18.0   19.0   2.0   20.0   21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0   29.0   3.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0   37.0   38.0   39.0   4.0   40.0   41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0   49.0   5.0   50.0   6.0   7.0   8.0   9.0   NA
    ## ----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  ----  ----  ----  ----  ---
    ## 1.0    405    379      0      0      0      0      0      0      0      0      0   410      0      0      0      0      0      0      0      0      0      0   417      0      0      0      0      0      0      0      0      0      0   402      0      0      0      0      0      0      0      0      0      0   411      0   393   383   398   407    0
    ## 2.0    389    370     29     26     27     28     26     25     28     36     28   379     27      0      0      0      0      0      0      0      0      0   390      0      0      0      0      0      0      0      0      0      0   393      0      0      0      0      0      0      0      0      0      0   393      0   376   376   370   380    0
    ## 3.0    370    369      0      0      0      0      0      0      0      0      0   357      0     32     25     32     37     34     23     29     33     27   362     24      0      0      0      0      0      0      0      0      0   383      0      0      0      0      0      0      0      0      0      0   363      0   373   363   344   351    0
    ## 4.0    374    376      0      0      0      0      0      0      0      0      0   377      0      0      0      0      0      0      0      0      0      0   380      0     35     22     25     27     28     22     28     24     29   381     25      0      0      0      0      0      0      0      0      0   367      0   370   387   400   366    6
    ## 5.0    384    381      0      0      0      0      0      0      0      0      0   367      0      0      0      0      0      0      0      0      0      0   390      0      0      0      0      0      0      0      0      0      0   366      0     32     24     25     30     27     25     32     24     36   414     28   379   373   379   385    0
    ## 8.0      0      0      0      0      0      0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0     1      0     0     0     0     2    0
    ## NA       7      7      5      5      7      6      6      7      6      7      9     7      5      6      7      6      8      7      6      6      9      7     8      4      4      8      5      7      7      6      5      7      6     5      6      5      8      5      7      5      8      6      9      9     8      6     9     8     3     8    0
