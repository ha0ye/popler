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

    ## My project metadata key is  147 !!

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
    ##                                                 1.0   10.0   11.0   2.0   3.0   4.0   5.0   6.0   7.0   8.0   9.0     NA
    ## ---------------------------------------------  ----  -----  -----  ----  ----  ----  ----  ----  ----  ----  ----  -----
    ## site_and_blue_river_district_cougar_resevoir    609      0      1   569   617   765   203   143    56    49     0    927
    ## site_and_mckenzie_bridge_district_mill_creek    733     28      0   578   551   575   491   213   124   147    96   1123
    ## site_and_oakridge_district_christy_flats        612      0      0   613   612   581   642   574   358   303   104    604
    ## site_and_oakridge_district_sidewalk_creek       686      0     33   629   416   554   217   278    77    74    55    620
    ## 
    ## [[2]]
    ## 
    ## 
    ##                                                 1.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0   17.0   18.0   19.0   2.0   20.0   21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0   29.0   3.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0   37.0   4.0   5.0   6.0   7.0   8.0   9.0     NA
    ## ---------------------------------------------  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  -----
    ## site_and_blue_river_district_cougar_resevoir    206    144    163    140    125     83     81     93     90     89     62   198     76     85     82     41     35     46     60     13      9      0   186      0      0      0      0      0      0      0      0   143   175   147   132   155   153    927
    ## site_and_mckenzie_bridge_district_mill_creek    277    187    208    120    128    113    130    109    163     92    100   200     33     53     31     25     19     20      6      2      0      0   225      0      0      0      0      0      0      0      0   211   210   215   225   201   233   1123
    ## site_and_oakridge_district_christy_flats        423    213    270    216    168    133    154    119     47     13     16   297     21     26     20     21     32     26      0      0      0      0   337      0      0      0      0      0      0      0      0   329   332   299   284   315   288    604
    ## site_and_oakridge_district_sidewalk_creek       259    150    115    141    136     87     81     70     74     65     68   204     35     35     32     30     34     23      6      8      9     14   175      8     12      9     11      9     16      3      7   199   196   188   161   159   190    620
    ## 
    ## [[3]]
    ## 
    ## 
    ##         1.0   10.0   11.0   12.0   13.0   14.0   15.0   16.0   17.0   18.0   19.0   2.0   20.0   21.0   22.0   23.0   24.0   25.0   26.0   27.0   28.0   29.0   3.0   30.0   31.0   32.0   33.0   34.0   35.0   36.0   37.0   4.0   5.0   6.0   7.0   8.0   9.0     NA
    ## -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  -----  -----  ----  -----  -----  -----  -----  -----  -----  -----  -----  ----  ----  ----  ----  ----  ----  -----
    ## 1.0     218    109    141     77    135     78     95     65    108     45     97   144     34     54     34     37     27     48     13      4      4      8   162      5      6      8      8      2     10      0      0   114   176   121   150   109   194      0
    ## 10.0      9      0      0      0      0      0      0      0      0      0      0     3      0      0      0      0      0      0      0      0      0      0    12      0      0      0      0      0      0      0      0     4     0     0     0     0     0      0
    ## 11.0     10      0      0      0      0      0      0      0      0      0      0     9      0      0      0      0      0      0      0      0      0      0     6      0      0      0      0      0      0      0      0     9     0     0     0     0     0      0
    ## 2.0     139    143    122    133     86     79     75     92     49     84     40   173     42     34     51     29     46     30     12     16     13      6   121      3      6      1      3      7      6      3      7   150   104   160    72   149   103      0
    ## 3.0     177     83    135     89    125     72    126     68    102     47     56   103     20     55     31     26     21      9      6      0      1      0   133      0      0      0      0      0      0      0      0   100   126    88   148    85   164      0
    ## 4.0     151    154    115    159     93    114     79     99     70     79     38   141     61     39     49     25     26     28     41      3      0      0   111      0      0      0      0      0      0      0      0   142   132   155   107   164   100      0
    ## 5.0     178     58    104     75     75     31     51     56     33      4     15   100      8     17      0      0      0      0      0      0      0      0   150      0      0      0      0      0      0      0      0   100   128    84   102    86    98      0
    ## 6.0     107     89     64     48     26     30     20     11     12      0      0   100      0      0      0      0      0      0      0      0      0      0    91      0      0      0      0      0      0      0      0   130    92   107    79   120    82      0
    ## 7.0      81     20     40     16      8      0      0      0      0      0      0    46      0      0      0      0      0      0      0      0      0      0    54      0      0      0      0      0      0      0      0    53    69    41    86    43    58      0
    ## 8.0      45     34     35     20      9     12      0      0      0      0      0    47      0      0      0      0      0      0      0      0      0      0    47      0      0      0      0      0      0      0      0    61    58    67    29    61    48      0
    ## 9.0      50      4      0      0      0      0      0      0      0      0      0    33      0      0      0      0      0      0      0      0      0      0    36      0      0      0      0      0      0      0      0    19    28    26    29    13    17      0
    ## NA        0      0      0      0      0      0      0      0      0      0      0     0      0      0      0      0      0      0      0      0      0      0     0      0      0      0      0      0      0      0      0     0     0     0     0     0     0   3274
