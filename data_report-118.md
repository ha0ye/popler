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

    ## My project metadata key is  780 !!

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

![](data_report-118_files/figure-markdown_github/unnamed-chunk-5-1.png)

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
    ##                   100   106   107   109   110   112   113   115   116   123   124   126   127   129   130   132   133    139    142    145    148    151   157   158   160   161   163   164   166   167   174   175   177   178   180   181   183   184    190    193    196    199    202   208   209   211   212   214   215   217   218   225   226   228   229   231   232   234   235    241    244    247    250    253   259   260   262   263   265   266   268   269   276   277   279   280   282   283   285   286    292    295    298    301    304   310   311   313   314   316   317   319   320   327   328   330   331   333   334   336   337    343    346    349    352    355   361   362   364   365   367   368     37   370   371   378   379   381   382   384   385   387   388    394    397     40    400    403    406   412   413   415   416   418   419   421   422     43     46     49    55    56    58    59    61    62    64    65    72    73    75    76    78    79    81    82     88     91     94     97
    ## --------------  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  -----  -----  -----  -----
    ## site_luq_LFDP    3192   304   228   228   304   304   228   228   304   228   304   304   228   228   209   304   228   3211   3192   3192   3230   3211   228   304   304   228   228   304   304   228   304   228   228   304   304   228   228   304   3211   3192   3211   3211   3192   304   228   228   304   304   228   228   304   171   304   304   228   228   304   304   228   3211   3230   3230   3230   3230   209   285   304   228   228   285   304   228   285   209   228   304   304   228   228   285   3192   3211   3230   3230   3230   285   209   209   304   304   228   228   304   209   285   304   228   228   304   304   228   3211   3211   3192   3192   3192   228   304   304   228   228   304   3211   304   228   304   228   228   304   304   228   228   304   3192   3192   3211   3192   3192   3192   304   228   228   304   304   228   228   304   3192   3192   3192   228   304   304   228   228   304   285   228   304   228   209   304   304   228   228   304   3211   3211   3211   3192
