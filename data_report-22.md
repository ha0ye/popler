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

    ## My project metadata key is  72 !!

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
    ##                            1      2      3      4      5
    ## ---------------------  -----  -----  -----  -----  -----
    ## site_sgs_esa_block_1    4225   4000   4063   4230   4100
    ## site_sgs_esa_block_2    3644   3658   3316   3561   3605
    ## 
    ## [[2]]
    ## 
    ## 
    ##                            1     10   11   12   13   14   15   16   17   18   19      2   20   21   22   23   24   25   26   27   28   29      3   30   31   32   33   34   35   36   37   38   39      4   40   41   42   43   44   45   46   47   48   49      5   50      6      7      8      9
    ## ---------------------  -----  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  -----  ---  -----  -----  -----  -----
    ## site_sgs_esa_block_1    1907   1998   47   37   45   51   41   42   51   40   48   2028   48   43   44   40   45   44   40   47   42   39   1877   41   46   41   34   54   46   43   35   43   35   1885   43   32   39   47   39   34   32   41   41   41   1866   47   1794   1770   1871   1934
    ## site_sgs_esa_block_2    1823   1655    5    4    5    5    5    4    5    6    4   1783    6    6    4    4    7    3    6    7    5    5   1753    5    8    5    6    6    6    4    4    7    4   1790    4    4    4    6    7    4    4    2    6    4   1732    6   1827   1810   1763   1646
    ## 
    ## [[3]]
    ## 
    ## 
    ##    1    10   11   12   13   14   15   16   17   18   19     2   20   21   22   23   24   25   26   27   28   29     3   30   31   32   33   34   35   36   37   38   39     4   40   41   42   43   44   45   46   47   48   49     5   50     6     7     8     9
    ## ----  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ---  ---  ---  ---  ---  ---  ---  ---  ---  ----  ---  ----  ----  ----  ----
    ##  792   798    0    0    0    0    0    0    0    0    0   805    0    0    0    0    0    0    0    0    0    0   796    0    0    0    0    0    0    0    0    0    0   799    0    0    0    0    0    0    0    0    0    0   753    0   742   790   793   801
    ##  735   710   52   41   50   56   46   46   56   46   52   738   54    0    0    0    0    0    0    0    0    0   667    0    0    0    0    0    0    0    0    0    0   746    0    0    0    0    0    0    0    0    0    0   695    0   745   700   738   685
    ##  697   703    0    0    0    0    0    0    0    0    0   705    0   49   48   44   52   47   46   54   47   44   708   46    0    0    0    0    0    0    0    0    0   710    0    0    0    0    0    0    0    0    0    0   661    0   671   688   652   707
    ##  761   736    0    0    0    0    0    0    0    0    0   834    0    0    0    0    0    0    0    0    0    0   725    0   54   46   40   60   52   47   39   50   39   704   47    0    0    0    0    0    0    0    0    0   745    0   735   674   738   665
    ##  745   706    0    0    0    0    0    0    0    0    0   729    0    0    0    0    0    0    0    0    0    0   734    0    0    0    0    0    0    0    0    0    0   716    0   36   43   53   46   38   36   43   47   45   744   53   728   728   713   722
