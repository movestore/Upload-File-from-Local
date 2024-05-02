test_data <- function(test_file) {
    test_data_root_dir <- test_path("data")
    readRDS(file = file.path(test_data_root_dir, test_file))
}


toggle_rds <- function(hide = TRUE) {
  from <- "../../data/local_app_files/uploaded-app-files/rdsFile_ID/data.rds"
  to <- "../../data/local_app_files/uploaded-app-files/rdsFile_ID/data.rds-hide"
  
  if (hide) {
    file.rename(from = from, to = to)
    
  } else {
    file.rename(from = to, to = from)
  }
}


toggle_csv <- function(hide = TRUE) {
  from <- "../../data/local_app_files/uploaded-app-files/csvFile_ID/data.csv"
  to <- "../../data/local_app_files/uploaded-app-files/csvFile_ID/data.csv-hide"
  if (hide) {
    
    file.rename(from = from, to = to)
  } else {
    file.rename(from = to, to = from)
    
  }
}


toggle_both <- function(hide) {
  toggle_csv(hide = hide)
  toggle_rds(hide = hide)
}
