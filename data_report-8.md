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

    ## My project metadata key is  21 !!

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
    ##            1      2      3      4      5
    ## -----  -----  -----  -----  -----  -----
    ## BOER    1854   1907   1900   2022   1804
    ## BOGR    1722   1743   1463   1402   1628
    ## LATR     462    706    530    498    649
    ## PJ       188    213    309    328    295
    ## 
    ## [[2]]
    ## 
    ## 
    ##          108     12    132     36     60     84
    ## -----  -----  -----  -----  -----  -----  -----
    ## BOER    1604   1564   1594   1570   1676   1479
    ## BOGR    1334   1310   1405   1287   1283   1339
    ## LATR     448    658    534    381    383    441
    ## PJ       188    184    170    360    267    164
    ## 
    ## [[3]]
    ## 
    ## 
    ##  108    12   132    36    60    84
    ## ----  ----  ----  ----  ----  ----
    ##  709   616   773   729   768   631
    ##  704   851   798   669   736   811
    ##  658   724   726   712   738   644
    ##  729   761   656   780   705   619
    ##  774   764   750   708   662   718
