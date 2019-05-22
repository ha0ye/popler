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

    ## My project metadata key is  145 !!

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
    ##                                                       1    10    11    12    13    14    15    16    17    18    19     2    20     3     4     5     6     7     8     9
    ## -------------------------------------------------  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----
    ## site_and_mt_st_helens_butte_camp_transect_A         198     0     0     0     0     0     0     0     0     0     0     0     0   322   269     0     0     0     0     0
    ## site_and_mt_st_helens_butte_camp_transect_B         269     0     0     0     0     0     0     0     0     0     0   252     0   286   258   276   303   270     0     0
    ## site_and_mt_st_helens_butte_camp_transect_C         348     0     0     0     0     0     0     0     0     0     0   248     0   245   266     0     0     0     0     0
    ## site_and_mt_st_helens_butte_camp_transect_D         235     0     0     0     0     0     0     0     0     0     0   242     0   220   218   244     1     0     0     0
    ## site_and_mt_st_helens_lahars                        276     0     0     0     0     0     0     0     0     0     0   299     0     0   350   253   253   276   299     0
    ## site_and_mt_st_helens_pinecreek_ridge_transect_A    202     0     0     0     0     0     0     0     0     0     0   221     0   218   206     0     0     0     0     0
    ## site_and_mt_st_helens_pinecreek_ridge_transect_B    224     0     0     0     0     0     0     0     0     0     0   176     0   169    77    63     0     0     0     0
    ## site_and_mt_st_helens_plains_of_abraham              92   112     0     0     0     0     0     0     0     0     0   117     0   142   106    96    97    91   106   124
    ## site_and_mt_st_helens_pumice_plain                  171   177   205   193     0     0     0     0     0     0     0   159     0   127   170   204   170   159   176   177
    ## site_and_mt_st_helens_studebaker_ridge              219   186   207   162   189   161   151   144   180   192   166   181   175   231   126   205   292   210   246   230
    ## site_and_mt_st_helens_toutle_ridge                  135    56     0     0     0     0     0     0     0     0     0   126     0   117    81    54    63    45    63    54
