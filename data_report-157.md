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

    ## My project metadata key is  877 !!

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

![](data_report-157_files/figure-markdown_github/unnamed-chunk-5-1.png)

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
    ##                     1      2      3      4
    ## --------------  -----  -----  -----  -----
    ## site_pie_CC      5046   5394      0      0
    ## site_pie_EPH     3538   2030      0      0
    ## site_pie_McH1     493    638    638   1102
    ## site_pie_PUH     1624   1972   2030   1885
    ## 
    ## [[2]]
    ## 
    ## 
    ##                   0     1    10   100   101   105   106    11   110   111   115   116   120   121   125   126   130   131   135   136   140   141   145   146    15   150   151   156    16   161   166   171    20    21    25    26    30    31    35    36    40    41    45    46     5    50    51   55    56     6   60    61   65    66   70    71   75    76   80    81   85    86   90    91   95    96
    ## --------------  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----
    ## site_pie_CC      58   290    58    58   290    58   290   290    58   290    58   290    58   290    58   290    58   290    58   290    58   290    29   145    58    29   145     0   290     0     0     0    58   290    58   290    58   290    58   290    58   290    58   290    58    58   290   58   290   290   58   290   58   290   58   290   58   290   58   290   58   290   58   290   58   290
    ## site_pie_EPH      0   174     0     0   174     0   174   174     0   174     0   174     0   174     0   174     0   174     0   174     0   174     0   174     0     0   116    58   174    58    58    58     0   174     0   174     0   174     0   174     0   174     0   174     0     0   174    0   174   174    0   174    0   174    0   174    0   174    0   174    0   174    0   174    0   174
    ## site_pie_McH1     0   290   261     0     0     0     0    29     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0   261     0     0     0    29     0     0     0   261    29   261    29   261    29   261    29   232    29   145    29   261   116     0    0     0    29    0     0    0     0    0     0    0     0    0     0    0     0    0     0    0     0
    ## site_pie_PUH      0   522     0     0     0     0     0   522     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0   522     0     0     0     0   522     0   522    58   464    58   464     0   522     0   522     0     0   522    0   522   522   87   435    0   377    0   116    0   116    0   116    0     0    0     0    0     0
    ## 
    ## [[3]]
    ## 
    ## 
    ##   0     1   10   100   101   105   106    11   110   111   115   116   120   121   125   126   130   131   135   136   140   141   145   146   15   150   151   156    16   161   166   171   20    21   25    26    30    31    35    36   40    41   45    46    5   50    51   55    56     6   60    61   65    66   70    71   75    76   80    81   85    86   90    91   95    96
    ## ---  ----  ---  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ----  ---  ----  ----  ----  ----  ----  ----  ----  ---  ----  ---  ----  ----  ----  ----  ----  ---  ----  ---  ----  ---  ---  ----  ---  ----  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----  ---  ----
    ##  29   435   87    29   261    29   261   377    29   261    29   261    29   261    29   261    29   261    29   261    29   261     0   116   87     0    58     0   377     0     0     0   87   377   87   377   145   319   145   319   58   377   29   377   87   29   377   29   377   377   29   377   29   377   29   261   29   261   29   261   29   261   29   261   29   261
    ##  29   377   87    29   203    29   203   319    29   203    29   203    29   203    29   203    29   203    29   203    29   203    29   203   87    29   203    58   319    58    58    58   87   319   87   319    87   319    87   319   87   319   87   319   87   87   319   29   319   319   29   319   29   319   29   319   29   319   29   319   29   203   29   203   29   203
    ##   0   203   58     0     0     0     0   145     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0   58     0     0     0   145     0     0     0   58   145   58   145    58   145    58   145   58   145   58   145   58   58   145    0   145   145    0   145    0   145    0     0    0     0    0     0    0     0    0     0    0     0
    ##   0   261   87     0     0     0     0   174     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0     0   87     0     0     0   174     0     0     0   87   174   87   174    87   174    87   174   87   174   29   174   87    0   145    0   145   174   87    58    0     0    0     0    0     0    0     0    0     0    0     0    0     0
