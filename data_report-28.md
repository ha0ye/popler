popler\_data\_organizatonal\_hierarchy
================
Hao Ye, Ellen Bledsoe
5/21/2019

``` r
library(tidyverse)

all_data <- readRDS("list_df_full.RDS")
df <- as_tibble(all_data[[params$dataset_index]])

cat("My project metadata key is ", 
    df$proj_metadata_key[1], "!!")
```

    ## My project metadata key is  83 !!

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
  new_name <- paste0(i, "--", as.character(df[[1, paste0("spatial_replication_level_", i, "_label")]]))
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
# make pair-wise density plots to summarize organizational structure:
# 
library(GGally)
my_bin <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_viridis_c()
}

pm <- ggpairs(data_organization, 
                      lower = list(discrete = my_bin), 
                      upper = list(discrete = "blank"), 
              cardinality_threshold = NULL) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(pm)
```

![](data_report-28_files/figure-markdown_github/unnamed-chunk-5-1.png)

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
    ##                                            1.0   10.0   100.0   101.0   102.0   103.0   104.0   105.0   106.0   107.0   108.0   109.0   11.0   110.0   111.0   112.0   113.0   114.0   115.0   116.0   117.0   118.0   119.0   12.0   120.0   121.0   122.0   123.0   124.0   13.0   14.0   15.0   16.0   17.0   18.0   19.0   2.0   20.0   21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0   29.0   3.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0   37.0   38.0   39.0   4.0   40.0   41.0   42.0   43.0   44.0   45.0   46.0   47.0   48.0   49.0   5.0   50.0   51.0   52.0   53.0   54.0   55.0   56.0   57.0   58.0   59.0   6.0   60.0   61.0   62.0   63.0   64.0   65.0   66.0   67.0   68.0   69.0   7.0   70.0   71.0   72.0   73.0   74.0   75.0   76.0   77.0   78.0   79.0   8.0   80.0   81.0   82.0   83.0   84.0   85.0   86.0   87.0   88.0   89.0   9.0   90.0   91.0   92.0   93.0   94.0   95.0   96.0   97.0   98.0   99.0   NA
    ## ----------------------------------------  ----  -----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  -----  ------  ------  ------  ------  ------  ------  ------  ------  ------  ------  -----  ------  ------  ------  ------  ------  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ---
    ## site_sgs_small_mammal_trapping_web_13NE      0      9       2       3       1       4       0       1       4       2       0       0      1       2       3       0       1       2       1       2       2       4       2      1       5       1       0       0       3      2      2      0      2      1      5      5     4      9      1      0      1      0      0      2      2      4      7     4      4      3      1      4      1      5      2      1      5      7     2      8      0      1      3      4      3      7      5      8      5     8      8      0      2      0      2      0      2      4      2      1     4      6      3      0      2      3      2      4      2      0      2     0      5      1      1      2      0      2      4      1      1      3     5      3      0      3      1      2      1      3      0      1      1     4      1      3      1      2      4      2      1      2      5      0    0
    ## site_sgs_small_mammal_trapping_web_13NW      4     11      14       1       4       6      20       3      15      11      11      11      0       6       1       5       1      11      10       4      21       5      17      5      19       1       0       4       4      1      3      2      7     17      9     10     4     15      3      5      4      4      4     16      5      4      9     7      4      3      1      3      6      3      4      7      3      5     2     12      0      2      6      3      7     14     11      6     16     8      7      2      5     10      8      9      1      8     11      9     7      6      1      6      4     10      6      1      2      6      4    15      6      3      2      9      9      6      3     10     19     29    13     40      3      8      2      7      5     15     20     15     36    25     37      1      5      4      4      4      8      3     10     22    1
    ## site_sgs_small_mammal_trapping_web_13SW      6      7      22       7       7      12      11      12      11       9      18      15      4      15       3       2       7       9      10       7      15       8       6      4       9       2       2       0       3     14      5      3      6      8      7      9     3     17      2      7      6      7      6      2      8      9     12     3     12      3      5      6      6     15     10      6     10      9     3     25      2      5      1      4      1      4     10     14      8     9     10      8      2      0      3      4     10      6     14      9    12      5      6      3      5      4      6      8      7     14     11     5     24      5      4      3      5     11     13     25     20      5     7     14      2      3      7      9     14      9     23     10     13    12     10      8      3     10     11     11     19     26     27     28    2
    ## site_sgs_small_mammal_trapping_web_24NE      2     23      20       5       5       6      10      13      10      16      18      16      4      26       1       7       8       6       8       6       6       8      16      4      22       4       2       1       3      4      4     11      6      8      4     12     3     17      5      3      1      6      7     14     17     10     15     8     17      0      9      6      7     10     20     17     24     14     5      7      3      4      8      2      9     10      7     16     11    13      5      5      4      1      9      7     19     11     21     15    14     10      4      7      4      8      6     12     20     17     13     8     11      3     10      4      2      3      7      4      8     17    16     20      6      9     10     12      8      4     13      7     12    30     24      4      7      9      6      5     13     18     12     17    5
    ## site_sgs_small_mammal_trapping_web_25NE      0      1       1       2       0       0       0       0       0       1       0       0      2       0       2       1       0       0       0       0       0       0       2      2       0       1       0       0       1      0      0      2      1      0      3      5     0      2      2      1      0      1      1      0      1      2      2     2      0      0      4      1      0      1      0      0      0      0     0      3      0      0      0      0      0      1      2      8      2     1      2      0      0      0      1      0      2      1      1      0     1      3      0      0      0      1      0      0      0      0      2     0      6      0      1      2      2      0      3      0      1      0     1      2      0      0      1      0      0      1      0      4      0     0      1      0      0      0      0      0      0      0      0      0   36
    ## site_sgs_small_mammal_trapping_web_26NW      2      5       4       0       0       0       0       2       1       0       3       0      0       3       2       2       1       1       0       0       4       1       0      0       0       0       0       1       0      3      6      0      0      5      0      5     2      2      0      1      3      2      1      1      0      0      1     0      2      1      0      5      2      2      1      4      0      1     2      1      0      0      2      0      0      3      4      0      1     0      0      0      0      1      0      2      0      3      1      4     0      1      0      0      1      1      2      2      2      1      2     0      0      1      0      3      1      2      2      0      1      1     1      0      1      2      0      1      0      0      1      4      1     0      4      0      0      0      1      1      0      1      2      0   19
    ## site_sgs_small_mammal_trapping_web_27NE      1      7       2       1       0       4       3       1       3       5       2       1      0       4       2       0       5       2       3       3       4       3       3      2       5       1       1       0       3      7      3      2      1      2      4      1     1      9      0      1      1      2      1      7      4      1      2     2      5      0      0      0      2      1      2      2      6      4     1      3      3      1      0      0      1      5      3      3      3     1      4      1      3      7      4      1      1      2      3      0     3      3      0      5      4      2      1      2      1      2      0     5      3      3      4      3      3      3      1      3      3      4     1      6      1      0      3      2      1      4     10      1      1     3      9      0      3      2      3      5      7      7     11      5    9
    ## site_sgs_small_mammal_trapping_web_31W       0      1       0       0       0       1       0       0       0       0       0       0      0       0       0       0       0       0       1       0       0       0       0      0       1       0       0       0       0      0      0      0      0      0      0      1     0      0      0      0      0      0      0      0      0      1      0     1      1      0      0      0      0      0      0      0      1      1     0      2      0      0      0      0      0      1      0      1      0     0      0      0      0      0      0      0      0      1      1      0     0      0      0      0      0      0      0      0      0      0      1     1      0      0      0      0      0      0      0      1      0      0     0      1      0      0      0      0      0      0      0      1      0     0      0      0      0      0      0      0      0      0      0      0    0
