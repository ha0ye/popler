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

    ## My project metadata key is  31 !!

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
    ##               1C   1RA   1RC   1RU   1TR    2C   2RA   2RC   2RU   2TR    3C   3RA   3RC   3RU   3TR    4C   4RA   4RC   4RU   4TR    5C   5RA   5RC   5RU   5TR
    ## ----------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## removal_1    193     0     0   279   134   168     0     0   213   132   175     0     0   254   122   180     0     0   231   134   187     0     0   230   142
    ## removal_2    184   256     0   202   127   212   206     0   217   123   198   181     0   220   121   187   211     0   200   159   170   220     0   245   136
    ## removal_3    161   264     0     0   167   194   258     0     0   173   195   247     0     0   160   197   244     0     0   172   237   282     0     0   170
    ## removal_4    262   350   245     0   145   255   285   248     0   191   343   330   283     0   210   320   279   250     0   193   315   301   323     0   177
    ## removal_5    176     0   176     0    67   179     0   215     0    88   133     0   179     0   116   178     0   128     0    90   203     0   185     0    92
    ## removal_6    197     0     0   202   143   195     0     0   213   144   222     0     0   227   174   207     0     0   174   128   155     0     0   180   119
