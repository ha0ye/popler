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

    ## My project metadata key is  230 !!

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
    ##                                 1     2     3     4    g     G    r     R    w     W    y     Y
    ## ---------------------------  ----  ----  ----  ----  ---  ----  ---  ----  ---  ----  ---  ----
    ## site_cdr_core_old_field_10      0     0     0     0    0   105    0   120    0   107    0   146
    ## site_cdr_core_old_field_24      0     0     0     0    2   325    0   272    0   276    0   231
    ## site_cdr_core_old_field_26      0     0     0     0    0   264    0   262    0   213    1   253
    ## site_cdr_core_old_field_28      0     0     0     0    0   172    0   222    0   205    0   187
    ## site_cdr_core_old_field_35      0     0     0     0    0   155    0   190    0   243    0   170
    ## site_cdr_core_old_field_39      0     0     0     0    0   275    0   272    0   316    0   331
    ## site_cdr_core_old_field_4       0     0     0     0    0   223    0   255    3   272    0   276
    ## site_cdr_core_old_field_41      0     0     0     0    0   245    0   274    0   267    0   259
    ## site_cdr_core_old_field_45      0     0     0     0    0   262    0   345    0   294    0   225
    ## site_cdr_core_old_field_5       0     0     0     0    0   268    0   252    0   201    0   195
    ## site_cdr_core_old_field_53      0     0     0     0    0   197    1   200    0   160    0   156
    ## site_cdr_core_old_field_70      0     0     0     0    0   290    0   224    0   351    0   519
    ## site_cdr_core_old_field_72      0     0     0     0    0   314    0   286    0   330    0   285
    ## site_cdr_core_old_field_77      0     0     0     0    0   288    0   300    0   330    0   337
    ## site_cdr_core_old_field_LS    229   274   265   240    0     0    0     0    0     0    0     0
    ## 
    ## [[2]]
    ## 
    ## 
    ##                                 1     2     3     4
    ## ---------------------------  ----  ----  ----  ----
    ## site_cdr_core_old_field_10    105   120   107   146
    ## site_cdr_core_old_field_24    327   272   276   231
    ## site_cdr_core_old_field_26    264   262   213   254
    ## site_cdr_core_old_field_28    187   205   222   172
    ## site_cdr_core_old_field_35    155   190   243   170
    ## site_cdr_core_old_field_39    331   316   272   275
    ## site_cdr_core_old_field_4     223   255   275   276
    ## site_cdr_core_old_field_41    245   274   267   259
    ## site_cdr_core_old_field_45    225   294   345   262
    ## site_cdr_core_old_field_5     195   201   252   268
    ## site_cdr_core_old_field_53    156   160   201   197
    ## site_cdr_core_old_field_70    519   351   224   290
    ## site_cdr_core_old_field_72    285   330   286   314
    ## site_cdr_core_old_field_77    288   300   330   337
    ## site_cdr_core_old_field_LS    229   274   265   240
    ## 
    ## [[3]]
    ## 
    ## 
    ##          1      2      3      4
    ## ---  -----  -----  -----  -----
    ## 1      229      0      0      0
    ## 2        0    274      0      0
    ## 3        0      0    265      0
    ## 4        0      0      0    240
    ## g        2      0      0      0
    ## G     1605      0      0   1778
    ## r        0      0      1      0
    ## R        0   1673   1801      0
    ## w        0      0      3      0
    ## W        0   1857   1708      0
    ## y        0      0      0      1
    ## Y     1898      0      0   1672
