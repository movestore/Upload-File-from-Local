library("move2")
library("move")
library("vroom")
library("dplyr")
library("sf")
library("units")

# vroom reads in the data as is, csv from MB is with "-" in column names
# the csv file is expected to be comma delimited
# The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone
# The expected projection is a valid numeric EPSG value. E.g 4326 (EPSG:4326 is lat/lon)

########
## ToDo: check if uploaded file is .rds of csv, as in the cloud-upload-app, this will also give freedom of name of file
## ToDo: find correct way of doing this: names(new1) <- make.names(names(new1),allow_=TRUE)
#######

rFunction <- function(data = NULL,
                      time_col = "timestamp",
                      track_id_col = "individual-local-identifier",
                      track_attr = "",
                      coords = "location-long,location-lat",
                      crss = 4326, ...) {
  # rds file # works :)
  fileName1 <- getAuxiliaryFilePath("rdsFile_ID") # default is NULL
  logger.info(paste("Reading file", fileName1, "of size:", file.info(fileName1)$size, "."))
  new1 <- readRDS(fileName1)

  if (is.na(crs(new1))) {
    new1 <- NULL
    logger.info("The uploaded rds file does not contain coordinate reference system information. This data set cannot be uploaded")
  }

  if (is.null(new1)) {
    logger.info("No new rds data found.")
  } else {
    ########################
    ### if .rds is move2 ###
    ########################
    if (any(class(new1) == "move2")) {
      logger.info("Uploaded rds file containes a object of class move2")

      ### quality check:### cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
      ## remove empty locations
      if (!mt_has_no_empty_points(new1)) {
        logger.info("Your data included empty points. We remove them for you.")
        new1 <- dplyr::filter(new1, !sf::st_is_empty(new1))
      }
      ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
      crds <- sf::st_coordinates(new1)
      rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
      if (length(rem) > 0) {
        new1 <- new1[-rem, ]
      }
      if (nrow(new1) == 0) {
        logger.info("Your uploaded csv file does not contain any location data.")
      }

      if (!mt_is_track_id_cleaved(new1)) {
        logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
        new1 <- new1 |> dplyr::arrange(mt_track_id(new1))
      }

      if (!mt_is_time_ordered(new1)) {
        logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
        new1 <- new1 |> dplyr::arrange(mt_track_id(new1), mt_time(new1))
      }
      ## remove duplicated timestamps
      if (!mt_has_unique_location_time_records(new1)) {
        n_dupl <- length(which(duplicated(paste(mt_track_id(new1), mt_time(new1)))))
        logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
        ## this piece of code keeps the duplicated entry with least number of columns with NA values
        new1 <- new1 %>%
          mutate(n_na = rowSums(is.na(pick(everything())))) %>%
          arrange(mt_track_id(.), mt_time(.), n_na) %>%
          mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
      }
    }

    #################################################################
    ### if move2 object - fine, else transform moveStack to move2 ###
    #################################################################
    if (any(class(new1) == "MoveStack")) {
      new1 <- mt_as_move2(new1)
      logger.info("Uploaded rds file containes a object of class moveStack that is transformed to move2.")
    }
    if (!any(class(new1) %in% c("move2", "MoveStack"))) {
      new1 <- NULL
      logger.info("Uploaded rds file contains data with unexpected class, please ensure that the file contains a object of class 'move2' or 'MoveStack'.")
    }
  }
  # make names for new1
  if (!is.null(new1)) {
    # names(new1) <- make.names(names(new1),allow_=TRUE)
    mt_track_id(new1) <- make.names(mt_track_id(new1), allow_ = TRUE)
  }

  ################
  ### csv file ###
  ################
  fileName2 <- getAuxiliaryFilePath("csvFile_ID") # default is NULL
  logger.info(paste("Reading file", fileName2, "of size:", file.info(fileName2)$size, "."))
  test <- readLines(fileName2) # to check if it is empty, only continue if not
  if (test[1] == "") {
    df2 <- NULL
    logger.info("No new csv data found. The settings of the App are not used.")
  } else {
    df2 <- vroom::vroom(fileName2, delim = ",") # alternative to read.csv, takes care of timestamps, default timezome is UTC, problems with empty table avoided above
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
      df2 <- dplyr::filter(df2, !is.na(df2[[track_id_col]])) # if there is a NA in the track_id col, mt_as_move2() gives error
      n.trcolna <- length(which(is.na(df2[[track_id_col]])))
      if (n.trcolna > 0) logger.info(paste("Your tracks contained", ntrcolna, "locations with unspecified track ID. They have been removed."))
      # if track_id column is different to classes  integer, integer64, character or factor, transform it to a factor
      if (!class(df2[[track_id_col]]) %in% c("integer", "integer64", "character", "factor")) {
        df2[[track_id_col]] <- as.factor(df2[[track_id_col]])
      }
      new2 <- mt_as_move2(df2,
        time_column = time_col,
        track_id_column = track_id_col,
        track_attributes = tr_attr,
        coords = coo,
        crs = crss,
        na.fail = FALSE
      )

      ## quality check:## cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
      ## remove empty locations
      if (!mt_has_no_empty_points(new2)) {
        logger.info("Your data included empty points. We remove them for you.")
        new2 <- dplyr::filter(new2, !sf::st_is_empty(new2))
      }
      ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
      crds <- sf::st_coordinates(new2)
      rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
      if (length(rem) > 0) {
        new2 <- new2[-rem, ]
      }
      if (nrow(new2) == 0) {
        logger.info("Your uploaded csv file does not contain any location data.")
      }

      if (!mt_is_track_id_cleaved(new2)) {
        logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
        new2 <- new2 |> dplyr::arrange(mt_track_id(new2))
      }

      if (!mt_is_time_ordered(new2)) {
        logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
        new2 <- new2 |> dplyr::arrange(mt_track_id(new2), mt_time(new2))
      }
      ## remove duplicated timestamps
      if (!mt_has_unique_location_time_records(new2)) {
        n_dupl <- length(which(duplicated(paste(mt_track_id(new2), mt_time(new2)))))
        logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
        ## this piece of code keeps the duplicated entry with least number of columns with NA values
        new2 <- new2 %>%
          mutate(n_na = rowSums(is.na(pick(everything())))) %>%
          arrange(mt_track_id(.), mt_time(.), n_na) %>%
          mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
      }

      # make names for new2
      # names(new2) <- make.names(names(new2),allow_=TRUE)
      mt_track_id(new2) <- make.names(mt_track_id(new2), allow_ = TRUE)
    }
  }

  if (is.null(df2)) new2 <- NULL else if (nrow(new2) == 0) new2 <- NULL

  ######################
  ## joining objects ###
  ######################

  # here is where the object data is needed
  if (!exists("data") | is.null(data) | length(data) == 0) { # here need to check what is possible (Clemens)
    if (is.null(new1)) {
      if (is.null(new2)) {
        result <- NULL
        logger.info("No new data in rds or csv files and no input data from previous App. Returning NULL.") # works
      } else {
        result <- new2
        logger.info("New data uploaded from csv file, no previous input data.") # works
      }
    } else {
      if (is.null(new2)) {
        result <- new1
        logger.info("New data uploaded from rds file, no previous input data.") # works
      } else {
        if (!st_crs(new1) == st_crs(new2)) {
          new1 <- st_transform(new1, st_crs(new2)) ## or the other way around, not sure which makes more sense...
          logger.info(paste0("The two data sets to combine have a different projection. One has been re-projected, and now the combined data set is in the '", st_crs(new2)$input, "' projection."))
        }
        result <- mt_stack(new1, new2, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds and csv files, no previous input data. Both data sets are merged.") # works
      }
    }
  } else {
    if (is.null(new1)) {
      if (is.null(new2)) {
        result <- data
        logger.info("No new data in rds or csv files. Returning input data.") # works
      } else {
        if (!st_crs(data) == st_crs(new2)) {
          new2 <- st_transform(new2, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }

        # drop units from intersecting columns that are not defined as time/loc/track_ID
        defd <- c(time_col, track_id_col, trimws(strsplit(as.character(coords), ",")[[1]]), attr(data, "sf_column"))
        overlp <- intersect(names(new2), names(data))
        drp <- overlp[!is.element(overlp, defd)]
        if (length(drp > 0)) {
          for (i in seq(along = drp))
          {
            dataunit <- eval(parse(text = paste("class(data$", drp[i], ")=='units'", sep = "")))
            new2unit <- eval(parse(text = paste("class(new2$", drp[i], ")=='units'", sep = "")))

            if (dataunit & !new2unit) {
              eval(parse(text = paste("data$", drp[i], "<- drop_units(data$", drp[i], ")", sep = ""))) # if only the variable in data has units, drop them
              logger.info(paste("Your uploaded file does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
            if (new2unit & !dataunit) {
              eval(parse(text = paste("new2$", drp[i], "<- drop_units(new2$", drp[i], ")", sep = ""))) # if only the variable in new2 has units, drop them
              logger.info(paste("Your App input data set does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
          }
        }

        result <- mt_stack(data, new2, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from csv file and appended to input data.") # works
      }
    } else {
      if (is.null(new2)) {
        if (!st_crs(data) == st_crs(new1)) {
          new1 <- st_transform(new1, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }
        result <- mt_stack(data, new1, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds file and appended to input data.") # works for move1 and move2
      } else {
        if (!st_crs(data) == st_crs(new1)) {
          new1 <- st_transform(new1, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }
        if (!st_crs(data) == st_crs(new2)) {
          new2 <- st_transform(new2, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }

        datanew1 <- mt_stack(data, new1, .track_combine = "rename", .track_id_repair = "universal")

        # drop units from intersecting columns that are not defined as time/loc/track_ID
        defd <- c(time_col, track_id_col, trimws(strsplit(as.character(coords), ",")[[1]]), attr(data, "sf_column"))
        overlp <- intersect(names(new2), names(datanew1))
        drp <- overlp[!is.element(overlp, defd)]
        if (length(drp > 0)) {
          for (i in seq(along = drp))
          {
            datanew1unit <- eval(parse(text = paste("class(datanew1$", drp[i], ")=='units'", sep = "")))
            new2unit <- eval(parse(text = paste("class(new2$", drp[i], ")=='units'", sep = "")))

            if (datanew1unit & !new2unit) {
              eval(parse(text = paste("datanew1$", drp[i], "<- drop_units(datanew1$", drp[i], ")", sep = ""))) # if only the variable in datanew1 has units, drop them
              logger.info(paste("Your uploaded csv file does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
            if (new2unit & !datanew1unit) {
              eval(parse(text = paste("new2$", drp[i], "<- drop_units(new2$", drp[i], ")", sep = ""))) # if only the variable in new2 has units, drop them
              logger.info(paste("Your App input data set and/or uploaded rds file do not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
          }
        }

        result <- mt_stack(datanew1, new2, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from rds and csv files. Both data sets are appended to input data.") # works
      }
    }
  }
  return(result)
}
