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

    ## My project metadata key is  813 !!

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
    ##                    Fringing   Outer 10   Outer 17
    ## ----------------  ---------  ---------  ---------
    ## site_mcr_LTER_1       10933      10585      10623
    ## site_mcr_LTER_2       10805      11318      10953
    ## site_mcr_LTER_3       11063      11245      10356
    ## site_mcr_LTER_4       10989      10424      10863
    ## site_mcr_LTER_5       10564      10433      11200
    ## site_mcr_LTER_6       10631      10948      11125
    ## 
    ## [[2]]
    ## 
    ## 
    ##                       1      2      3      4      5
    ## ----------------  -----  -----  -----  -----  -----
    ## site_mcr_LTER_1    6663   6671   5558   6598   6651
    ## site_mcr_LTER_2    6692   6710   6186   6758   6730
    ## site_mcr_LTER_3    6613   6749   5944   6647   6711
    ## site_mcr_LTER_4    6432   6593   6189   6436   6626
    ## site_mcr_LTER_5    6420   6300   6465   6407   6605
    ## site_mcr_LTER_6    6733   6417   6683   6615   6256
    ## 
    ## [[3]]
    ## 
    ## 
    ##                 1       2       3       4       5
    ## ---------  ------  ------  ------  ------  ------
    ## Fringing    13218   12813   12898   13202   12854
    ## Outer 10    13034   13148   12644   12775   13352
    ## Outer 17    13301   13479   11483   13484   13373
    ## 
    ## [[4]]
    ## 
    ## 
    ##                       1      2      3      4      5      6      7      8
    ## ----------------  -----  -----  -----  -----  -----  -----  -----  -----
    ## site_mcr_LTER_1    4160   4219   4180   4170   4191   4112   3627   3482
    ## site_mcr_LTER_2    4211   4197   4209   4184   4145   4140   4104   3886
    ## site_mcr_LTER_3    4211   4218   4167   4231   4151   3929   3820   3937
    ## site_mcr_LTER_4    4146   4096   4027   3934   4026   4138   4027   3882
    ## site_mcr_LTER_5    4091   4109   4144   4007   4055   4223   4063   3505
    ## site_mcr_LTER_6    4169   4221   4188   4157   4089   4165   4149   3566
    ## 
    ## [[5]]
    ## 
    ## 
    ##                1      2      3      4      5      6      7      8
    ## ---------  -----  -----  -----  -----  -----  -----  -----  -----
    ## Fringing    8255   8321   8307   8180   8277   8237   8049   7359
    ## Outer 10    8355   8283   8203   8082   8111   8399   7914   7606
    ## Outer 17    8378   8456   8405   8421   8269   8071   7827   7293
    ## 
    ## [[6]]
    ## 
    ## 
    ##     1      2      3      4      5      6      7      8
    ## -----  -----  -----  -----  -----  -----  -----  -----
    ##  4981   5003   4971   4855   4911   5030   4872   4930
    ##  5009   4973   4979   4938   4993   5005   5031   4512
    ##  5018   5046   5003   4928   4832   4652   4077   3469
    ##  4999   5039   4904   4933   4959   5018   4945   4664
    ##  4981   4999   5058   5029   4962   5002   4865   4683
