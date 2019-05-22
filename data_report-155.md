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

    ## My project metadata key is  864 !!

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
    ##                      Left   NA   Right
    ## ------------------  -----  ---  ------
    ## site_pie_Clubhead      79    0     222
    ## site_pie_Nelson       181    4      70
    ## site_pie_Sweeney      360    0     275
    ## site_pie_West         356    0     359
    ## 
    ## [[2]]
    ## 
    ## 
    ##                        1     2     3     4
    ## ------------------  ----  ----  ----  ----
    ## site_pie_Clubhead     88    85    82    46
    ## site_pie_Nelson       86    82    60    27
    ## site_pie_Sweeney     186   188   168    93
    ## site_pie_West        212   199   194   110
    ## 
    ## [[3]]
    ## 
    ## 
    ##            1     2     3     4
    ## ------  ----  ----  ----  ----
    ## Left     304   284   251   137
    ## NA         1     1     1     1
    ## Right    267   269   252   138
