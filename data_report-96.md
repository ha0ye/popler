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

    ## My project metadata key is  622 !!

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
    ##                                A     B     C     D     G     H     I     J
    ## --------------------------  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_hbr_hubbard_brook       762   643   610   752     0     0     0     0
    ## site_hbr_moosilauke            0     0     0     0   320   302   352   410
    ## site_hbr_russell_pond        304   309   321   302     0     0     0     0
    ## site_hbr_stinson_mountain    250   288   290   363     0     0     0     0
    ## 
    ## [[2]]
    ## 
    ## 
    ##                              10.5   1.5   2.5   3.5   4.5   5.5   6.5   7.5   8.5   9.5
    ## --------------------------  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_hbr_hubbard_brook        270   251   274   310   270   292   293   279   261   267
    ## site_hbr_moosilauke           104   136   138   159   141   160   130   123   153   140
    ## site_hbr_russell_pond         135    93   112   109   114   113   132   121   164   143
    ## site_hbr_stinson_mountain      86   157   116   115   124   122   131   121   112   107
    ## 
    ## [[3]]
    ## 
    ## 
    ##       10.5   1.5   2.5   3.5   4.5   5.5   6.5   7.5   8.5   9.5
    ## ---  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## A      142   112   107   126   114   145   143   131   148   148
    ## B      134   121   106   114   127   115   136   122   133   132
    ## C      103   131   149   133   124   111   129   122   114   105
    ## D      112   137   140   161   143   156   148   146   142   132
    ## G       23    32    30    30    40    29    31    28    48    29
    ## H       26    42    43    41    27    31    29    22    21    20
    ## I       22    32    41    51    35    43    28    36    36    28
    ## J       33    30    24    37    39    57    42    37    48    63
