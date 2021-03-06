# Downloading data from `popler` 
devtools::install_github("AldoCompagnoni/popler", build_vignettes = TRUE)
library(popler)
library(testit)
source(efficient_query_popler.R)

# find datasets by subsetting the metadata table
community_metadata <- pplr_browse(duration_years >= 10 & community == 'yes')

# # download the data using the get_data() function
# community_datasets <- pplr_get_data(community_metadata)
# community_datasets_cov <- pplr_cov_unpack(community_metadata)

# pull number column
# for loop for downloading w/ a dataframe to record any errors
# skip error function (metacom practice)

proj_keys <- community_metadata$proj_metadata_key
proj_keys_test <- c(2,3,4,12,13)

error_df <- data.frame(project_key = integer(length(proj_keys)),
                       error = character(length(proj_keys)))
list_df <- list()
list_num = 1

for (j in proj_keys) {
  
  n <- list_num
  
  init_t    <- Sys.time()
  
  if (has_error(query_get(conn, efficienty_query(j)))){
    error_df$project_key[n] <- j
    error_df$error[n] <- "error"
  } else {
    error_df$project_key[n] <- j
    error_df$error[n] <- NA
    list_df[[n]] <- query_get(conn, efficienty_query(j))
  }
  
  list_num <- n + 1
  Sys.time() - init_t
}



###############################################################
data <- pplr_get_data(proj_metadata_key == 2)

