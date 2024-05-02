library("move2")
library("move")
library("vroom")
library("dplyr")
library("tidyr")
library("sf")
library("units")

# vroom reads in the data as is, csv from MB is with "-" in column names
# the csv file is expected to be comma delimited
# The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone
# The expected projection is a valid numeric EPSG value. E.g 4326 (EPSG:4326 is lat/lon)

########
## ToDo: check if uploaded file is .rds of csv, as in the cloud-upload-app, this will also give freedom of name of file
## ToDo: find correct way of doing this: names(rds_data) <- make.names(names(rds_data),allow_=TRUE)
#######

UNSTANDARDIZED_COLS <- c("tag.local.identifier", "sex")

get_most_recent_upload <- function(file_type) {
  if (file_type == "rds") {
    pattern <- "\\.rds$"
    root_path <- getAppFilePath("rdsFile_ID")
  } else {
    pattern <- "\\.csv$"
    root_path <- getAppFilePath("csvFile_ID")
  }

  uploaded_files_df <- file.info(list.files(
    pattern = pattern,
    path = gsub("/$", "", root_path),
    full.names = TRUE
  ))

  file_path <- uploaded_files_df |>
    arrange(mtime) |>
    rownames() |>
    last()

  return(file_path)
}

process_rds_file <- function(rds_file) {
  if (is.na(rds_file)) {
    logger.info("No new rds file uploaded.")
    return()
  }

  logger.info(paste("Reading file", rds_file, "of size:", file.info(rds_file)$size, "."))
  rds_data <- readRDS(rds_file)

  if (is.na(crs(rds_data))) {
    rds_data <- NULL
    logger.info("The uploaded rds file does not contain coordinate reference system information. This data set cannot be uploaded")
    return()
  }

  ########################
  ### if .rds is move2 ###
  ########################
  if (any(class(rds_data) == "move2")) {
    logger.info("Uploaded rds file containes a object of class move2")

    ### quality check:### cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
    ## remove empty locations
    if (!mt_has_no_empty_points(rds_data)) {
      logger.info("Your data included empty points. We remove them for you.")
      rds_data <- dplyr::filter(rds_data, !sf::st_is_empty(rds_data))
    }
    ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
    crds <- sf::st_coordinates(rds_data)
    rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
    if (length(rem) > 0) {
      rds_data <- rds_data[-rem, ]
    }
    if (nrow(rds_data) == 0) {
      logger.info("Your uploaded csv file does not contain any location data.")
    }

    if (!mt_is_track_id_cleaved(rds_data)) {
      logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
      rds_data <- rds_data |> dplyr::arrange(mt_track_id(rds_data))
    }

    if (!mt_is_time_ordered(rds_data)) {
      logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
      rds_data <- rds_data |> dplyr::arrange(mt_track_id(rds_data), mt_time(rds_data))
    }
    ## remove duplicated timestamps
    if (!mt_has_unique_location_time_records(rds_data)) {
      n_dupl <- length(which(duplicated(paste(mt_track_id(rds_data), mt_time(rds_data)))))
      logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
      ## this piece of code keeps the duplicated entry with least number of columns with NA values
      rds_data <- rds_data %>%
        mutate(n_na = rowSums(is.na(pick(everything())))) %>%
        arrange(mt_track_id(.), mt_time(.), n_na) %>%
        mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
    }
  }

  #################################################################
  ### if move2 object - fine, else transform moveStack to move2 ###
  #################################################################
  if (any(class(rds_data) == "MoveStack")) {
    rds_data <- mt_as_move2(rds_data)
    logger.info("Uploaded rds file containes a object of class moveStack that is transformed to move2.")
  }

  if (!any(class(rds_data) %in% c("move2", "MoveStack"))) {
    rds_data <- NULL
    logger.info("Uploaded rds file contains data with unexpected class, please ensure that the file contains a object of class 'move2' or 'MoveStack'.")
  }


  # make names for rds_data
  if (!is.null(rds_data)) {
    # names(rds_data) <- make.names(names(rds_data),allow_=TRUE)
    mt_track_id(rds_data) <- make.names(mt_track_id(rds_data), allow_ = TRUE)
  }

  return(rds_data)
}

process_csv_file <- function(csv_file, coords, track_attr, time_col, track_id_col, crss) {
  if (is.na(csv_file)) {
    logger.info("No new csv file uploaded.")
    return(NULL)
  }

  logger.info(paste("Reading file", csv_file, "of size:", file.info(csv_file)$size, "."))
  test <- readLines(csv_file) # to check if it is empty, only continue if not

  if (test[1] == "") {
    df2 <- NULL
    logger.info("No new csv data found. The settings of the App are not used.")
  } else {
    df2 <- vroom::vroom(csv_file, delim = ",") # alternative to read.csv, takes care of timestamps, default timezome is UTC, problems with empty table avoided above
    if (length(grep(",", coords)) %in% c(1, 2)) {
      coo <- trimws(strsplit(as.character(coords), ",")[[1]])
      logger.info(paste("You have defined as coordinate columns:", coords))
    } else {
      coo <- NULL
      logger.info("An incorrect number of coordinate columns is provided; expecting two (x,y) or three (x,y,z). Cannot transform csv data to move2.")
      df2 <- NULL
    }
    if (!is.null(df2)) {
      logger.info("New data uploaded from csv file, this file is expected to be comma delimited.")
      logger.info(paste("You have defined as datetime column: ", time_col, ".", " The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone"))
      logger.info(paste("You have defined as track ID column: ", track_id_col, "."))
      logger.info(paste("You have defined as track attributes: ", track_attr, "."))
      logger.info(paste("You have defined as projection (crs): ", crss, ".", " The expected projection is a valid numeric EPSG value. For more info see https://epsg.io/ and https://spatialreference.org/"))

      # this only if a comma in the data!
      if (length(grep(",", track_attr)) > 0) {
        tr_attr <- trimws(strsplit(as.character(track_attr), ",")[[1]])
      } else {
        tr_attr <- track_attr
      }
      
      # transform data.frame to move2 object
      df2 <- df2 |>
        tidyr::drop_na(!!track_id_col) |> # if there is a NA in the track_id col, mt_as_move2() gives error
        mutate_at(vars(one_of(UNSTANDARDIZED_COLS)), as.character)
     
      n.trcolna <- length(which(is.na(df2[[track_id_col]])))
      if (n.trcolna > 0) logger.info(paste("Your tracks contained", ntrcolna, "locations with unspecified track ID. They have been removed."))
      
      # if track_id column is different to classes  integer, integer64, character or factor, transform it to a factor
      if (!class(df2[[track_id_col]]) %in% c("integer", "integer64", "character", "factor")) {
        df2[[track_id_col]] <- as.factor(df2[[track_id_col]])
      }

      csv_data <- mt_as_move2(df2,
        time_column = time_col,
        track_id_column = track_id_col,
        track_attributes = tr_attr,
        coords = coo,
        crs = crss,
        na.fail = FALSE
      )

      ## quality check:## cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
      ## remove empty locations
      if (!mt_has_no_empty_points(csv_data)) {
        logger.info("Your data included empty points. We remove them for you.")
        csv_data <- dplyr::filter(csv_data, !sf::st_is_empty(csv_data))
      }
      ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
      crds <- sf::st_coordinates(csv_data)
      rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
      if (length(rem) > 0) {
        csv_data <- csv_data[-rem, ]
      }
      if (nrow(csv_data) == 0) {
        logger.info("Your uploaded csv file does not contain any location data.")
      }

      if (!mt_is_track_id_cleaved(csv_data)) {
        logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
        csv_data <- csv_data |> dplyr::arrange(mt_track_id(csv_data))
      }

      if (!mt_is_time_ordered(csv_data)) {
        logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
        csv_data <- csv_data |> dplyr::arrange(mt_track_id(csv_data), mt_time(csv_data))
      }
      ## remove duplicated timestamps
      if (!mt_has_unique_location_time_records(csv_data)) {
        n_dupl <- length(which(duplicated(paste(mt_track_id(csv_data), mt_time(csv_data)))))
        logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
        ## this piece of code keeps the duplicated entry with least number of columns with NA values
        csv_data <- csv_data %>%
          mutate(n_na = rowSums(is.na(pick(everything())))) %>%
          arrange(mt_track_id(.), mt_time(.), n_na) %>%
          mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
      }

      # make names for csv_data
      # names(csv_data) <- make.names(names(csv_data),allow_=TRUE)
      mt_track_id(csv_data) <- make.names(mt_track_id(csv_data), allow_ = TRUE)
    }
  }

  if (is.null(df2)) csv_data <- NULL else if (nrow(csv_data) == 0) csv_data <- NULL

  return(csv_data)
}

join_data <- function(data, rds_data, csv_data, coords, time_col, track_id_col) {
  # here is where the object data is needed
  
  if (!exists("data") | is.null(data) | length(data) == 0) { # here need to check what is possible (Clemens)
    if (is.null(rds_data)) {
      if (is.null(csv_data)) {
        result <- NULL
        logger.info("No new data in rds or csv files and no input data from previous App. Returning NULL.") # works
      } else {
        result <- csv_data
        logger.info("New data uploaded from csv file, no previous input data.") # works
      }
    } else {
      if (is.null(csv_data)) {
        result <- rds_data
        logger.info("New data uploaded from rds file, no previous input data.") # works
      } else {
        if (!st_crs(rds_data) == st_crs(csv_data)) {
          rds_data <- st_transform(rds_data, st_crs(csv_data)) ## or the other way around, not sure which makes more sense...
          logger.info(paste0("The two data sets to combine have a different projection. One has been re-projected, and now the combined data set is in the '", st_crs(csv_data)$input, "' projection."))
        }
        result <- mt_stack(rds_data, csv_data, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds and csv files, no previous input data. Both data sets are merged.") # works
      }
    }
  } else {
    if (is.null(rds_data)) {
      if (is.null(csv_data)) {
        result <- data
        logger.info("No new data in rds or csv files. Returning input data.") # works
      } else {
        if (!st_crs(data) == st_crs(csv_data)) {
          csv_data <- st_transform(csv_data, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }

        # drop units from intersecting columns that are not defined as time/loc/track_ID
        defd <- c(time_col, track_id_col, trimws(strsplit(as.character(coords), ",")[[1]]), attr(data, "sf_column"))
        overlp <- intersect(names(csv_data), names(data))
        drp <- overlp[!is.element(overlp, defd)]
        if (length(drp > 0)) {
          for (i in seq(along = drp))
          {
            dataunit <- eval(parse(text = paste("class(data$", drp[i], ")=='units'", sep = "")))
            csv_dataunit <- eval(parse(text = paste("class(csv_data$", drp[i], ")=='units'", sep = "")))

            if (dataunit & !csv_dataunit) {
              eval(parse(text = paste("data$", drp[i], "<- drop_units(data$", drp[i], ")", sep = ""))) # if only the variable in data has units, drop them
              logger.info(paste("Your uploaded file does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
            if (csv_dataunit & !dataunit) {
              eval(parse(text = paste("csv_data$", drp[i], "<- drop_units(csv_data$", drp[i], ")", sep = ""))) # if only the variable in csv_data has units, drop them
              logger.info(paste("Your App input data set does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
          }
        }

        data <- data |>
          mutate_at(vars(any_of(UNSTANDARDIZED_COLS)), as.character)
        result <- mt_stack(data, csv_data, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from csv file and appended to input data.") # works
      }
    } else {
      if (is.null(csv_data)) {
        if (!st_crs(data) == st_crs(rds_data)) {
          rds_data <- st_transform(rds_data, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }
        result <- mt_stack(data, rds_data, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds file and appended to input data.") # works for move1 and move2
      } else {
        if (!st_crs(data) == st_crs(rds_data)) {
          rds_data <- st_transform(rds_data, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }
        if (!st_crs(data) == st_crs(csv_data)) {
          csv_data <- st_transform(csv_data, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }

        data_rds_stack <- mt_stack(data, rds_data, .track_combine = "rename",
                                   .track_id_repair = "universal")

        # drop units from intersecting columns that are not defined as time/loc/track_ID
        defd <- c(time_col, track_id_col, trimws(strsplit(as.character(coords), ",")[[1]]), attr(data, "sf_column"))
        overlp <- intersect(names(csv_data), names(data_rds_stack))
        drp <- overlp[!is.element(overlp, defd)]
        if (length(drp > 0)) {
          for (i in seq(along = drp))
          {
            data_rds_stackunit <- eval(parse(text = paste("class(data_rds_stack$", drp[i], ")=='units'", sep = "")))
            csv_dataunit <- eval(parse(text = paste("class(csv_data$", drp[i], ")=='units'", sep = "")))

            if (data_rds_stackunit & !csv_dataunit) {
              eval(parse(text = paste("data_rds_stack$", drp[i], "<- drop_units(data_rds_stack$", drp[i], ")", sep = ""))) # if only the variable in data_rds_stack has units, drop them
              logger.info(paste("Your uploaded csv file does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
            if (csv_dataunit & !data_rds_stackunit) {
              eval(parse(text = paste("csv_data$", drp[i], "<- drop_units(csv_data$", drp[i], ")", sep = ""))) # if only the variable in csv_data has units, drop them
              logger.info(paste("Your App input data set and/or uploaded rds file do not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
          }
        }

        data_rds_stack <- data_rds_stack |>
          mutate_at(vars(one_of("tag.local.identifier")), as.character)
        result <- mt_stack(data_rds_stack, csv_data, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds and csv files. Both data sets are appended to input data.") # works
      }
    }
  }
  
  return(result)
}


rFunction <- function(data = NULL,
                      time_col = "timestamp",
                      track_id_col = "individual-local-identifier",
                      track_attr = "",
                      coords = "location-long,location-lat",
                      crss = 4326, ...) {
  
  rds_file <- get_most_recent_upload("rds")
  rds_data <- process_rds_file(rds_file)

  csv_file <- get_most_recent_upload(file_type = "csv")
  csv_data <- process_csv_file(csv_file, coords, track_attr, time_col, track_id_col, crss)

  result <- join_data(data, rds_data, csv_data, coords, time_col, track_id_col)

  return(result)
}