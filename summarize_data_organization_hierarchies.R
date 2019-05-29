all_data <- readRDS("list_df_full.RDS")

for (i in seq(length(all_data)))
{
  rmarkdown::render(
    "popler_data_report.Rmd", params = list(
      dataset_index = i),
    output_file = paste0("data_report-", i, ".md")
  )
}







# x %>%
#   select(year, month, day, 
#          sppcode, 
#          starts_with("spatial_replication_level"), 
#          count_observation) %>%
#   mutate(date = lubridate::ymd(paste(year, month, day, sep = "-"))) %>%
#   mutate_at(vars("sppcode", starts_with("spatial_replication_level")), as.factor) %>%
#   select(-year, -month, -day) %>%
#   {.} -> xx
# 
# xx[, c("spatial_replication_level_3", "spatial_replication_level_4")] %>% table()

