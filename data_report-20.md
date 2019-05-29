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

    ## My project metadata key is  65 !!

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

![](data_report-20_files/figure-markdown_github/unnamed-chunk-5-1.png)

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
    ##                1     2     3
    ## ----------  ----  ----  ----
    ## ESA          611   614   602
    ## ESA2          79    76    55
    ## MIDSLOPE     685   696   668
    ## OWL_CREEK    337   316   341
    ## RIDGE        639   647   623
    ## SEC25        382   381   364
    ## SWALE        626   592   620
    ## 
    ## [[2]]
    ## 
    ## 
    ##                1     2     3     4     5
    ## ----------  ----  ----  ----  ----  ----
    ## ESA          371   357   349   366   384
    ## ESA2          44    41    47    35    43
    ## MIDSLOPE     409   393   419   409   419
    ## OWL_CREEK    190   194   197   216   197
    ## RIDGE        355   407   393   391   363
    ## SEC25        226   227   218   236   220
    ## SWALE        383   371   368   362   354
    ## 
    ## [[3]]
    ## 
    ## 
    ##    1     2     3     4     5
    ## ----  ----  ----  ----  ----
    ##  668   697   656   675   663
    ##  641   636   698   687   660
    ##  669   657   637   653   657
