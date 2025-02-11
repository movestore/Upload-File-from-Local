library("move2")
library("move")
library("vroom")
library("dplyr")
library("sf")
library("units")
library("bit64")

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
                      crss = "EPSG:4326", ...) {
  
  fileName <- getAuxiliaryFilePath("File_ID")
  # fileName <- "./data/raw/input3_move2loc_LatLon.rds"
  
  extn0 <- strsplit(fileName,"[.]")[[1]]
  extn <- extn0[length(extn0)]
  
  logger.info(paste("Reading",extn, "file", fileName, "of size:", file.info(fileName)$size, "bytes."))
  
  ## read in rds 
  if (extn=="rds"){
    
    newRDS <- readRDS(fileName)
    
    if (is.na(crs(newRDS))) {
      newRDS <- NULL
      logger.info("The uploaded rds file does not contain coordinate reference system information. This data set cannot be uploaded")
    }
    
    if (is.null(newRDS)) {
      logger.info("No new rds data found.")
    } else {
      ########################
      ### if .rds is move2 ###
      ########################
      if (any(class(newRDS) == "move2")) {
        logger.info("Uploaded rds file containes a object of class move2")
        
        ### quality check:### cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
        ## remove empty locations
        if (!mt_has_no_empty_points(newRDS)) {
          logger.info("Your data included empty points. We remove them for you.")
          newRDS <- dplyr::filter(newRDS, !sf::st_is_empty(newRDS))
        }
        ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
        crds <- sf::st_coordinates(newRDS)
        rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
        if (length(rem) > 0) {
          newRDS <- newRDS[-rem, ]
        }
        if (nrow(newRDS) == 0) {
          logger.info("Your uploaded csv file does not contain any location data.")
        }
        
        if (!mt_is_track_id_cleaved(newRDS)) {
          logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
          newRDS <- newRDS |> dplyr::arrange(mt_track_id(newRDS))
        }
        
        if (!mt_is_time_ordered(newRDS)) {
          logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
          newRDS <- newRDS |> dplyr::arrange(mt_track_id(newRDS), mt_time(newRDS))
        }
        ## remove duplicated timestamps
        if (!mt_has_unique_location_time_records(newRDS)) {
          n_dupl <- length(which(duplicated(paste(mt_track_id(newRDS), mt_time(newRDS)))))
          logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
          ## this piece of code keeps the duplicated entry with least number of columns with NA values
          newRDS <- newRDS %>%
            mutate(n_na = rowSums(is.na(pick(everything())))) %>%
            arrange(mt_track_id(.), mt_time(.), n_na) %>%
            mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
        }
      }
      
      #################################################################
      ### if move2 object - fine, else transform moveStack to move2 ###
      #################################################################
      if (any(class(newRDS) == "MoveStack")) {
        newRDS <- mt_as_move2(newRDS)
        logger.info("Uploaded rds file containes a object of class moveStack that is transformed to move2.")
      }
      if (!any(class(newRDS) %in% c("move2", "MoveStack"))) {
        newRDS <- NULL
        logger.info("Uploaded rds file contains data with unexpected class, please ensure that the file contains a object of class 'move2' or 'MoveStack'.")
      }
    }
    # make names for newRDS
    if (!is.null(newRDS)) {
      # names(newRDS) <- make.names(names(newRDS),allow_=TRUE)
      mt_track_id(newRDS) <- make.names(mt_track_id(newRDS), allow_ = TRUE)
    }
    newMV <- newRDS
  }
  
  if (extn=="csv"){
    ################
    ### csv file ###
    ################
    test <- readLines(fileName) # to check if it is empty, only continue if not
    if (test[1] == "") {
      df2 <- NULL
      logger.info("No new csv data found. The settings of the App are not used.")
    } else {
      df2 <- vroom::vroom(fileName, delim = ",") # alternative to read.csv, takes care of timestamps, default timezome is UTC, problems with empty table avoided above
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
        
        newCSVmv <- mt_as_move2(df2,
                                time_column = time_col,
                                track_id_column = track_id_col,
                                track_attributes = tr_attr,
                                coords = coo,
                                crs = crss,
                                na.fail = FALSE
        )
        
        ## quality check:## cleaved, time ordered, non-emtpy, non-duplicated (dupl get removed further down in the code)
        ## remove empty locations
        if (!mt_has_no_empty_points(newCSVmv)) {
          logger.info("Your data included empty points. We remove them for you.")
          newCSVmv <- dplyr::filter(newCSVmv, !sf::st_is_empty(newCSVmv))
        }
        ## for some reason, sometimes either lat or long are NA, as one still has a value it does not get removed with the excluding empty, here is what I came up with:
        crds <- sf::st_coordinates(newCSVmv)
        rem <- unique(c(which(is.na(crds[, 1])), which(is.na(crds[, 2]))))
        if (length(rem) > 0) {
          newCSVmv <- newCSVmv[-rem, ]
        }
        if (nrow(newCSVmv) == 0) {
          logger.info("Your uploaded csv file does not contain any location data.")
        }
        
        if (!mt_is_track_id_cleaved(newCSVmv)) {
          logger.info("Your data set was not grouped by individual/track. We regroup it for you.")
          newCSVmv <- newCSVmv |> dplyr::arrange(mt_track_id(newCSVmv))
        }
        
        if (!mt_is_time_ordered(newCSVmv)) {
          logger.info("Your data is not time ordered (within the individual/track groups). We reorder the measurements for you.")
          newCSVmv <- newCSVmv |> dplyr::arrange(mt_track_id(newCSVmv), mt_time(newCSVmv))
        }
        ## remove duplicated timestamps
        if (!mt_has_unique_location_time_records(newCSVmv)) {
          n_dupl <- length(which(duplicated(paste(mt_track_id(newCSVmv), mt_time(newCSVmv)))))
          logger.info(paste("Your data has", n_dupl, "duplicated location-time records. We removed here those with less info and then select the first if still duplicated."))
          ## this piece of code keeps the duplicated entry with least number of columns with NA values
          newCSVmv <- newCSVmv %>%
            mutate(n_na = rowSums(is.na(pick(everything())))) %>%
            arrange(mt_track_id(.), mt_time(.), n_na) %>%
            mt_filter_unique(criterion = "first") # this always needs to be "first" because the duplicates get ordered according to the number of columns with NA.
        }
        
        # make names for newCSVmv
        # names(newCSVmv) <- make.names(names(newCSVmv),allow_=TRUE)
        mt_track_id(newCSVmv) <- make.names(mt_track_id(newCSVmv), allow_ = TRUE)
      }
    }
    
    if (is.null(df2)) newCSVmv <- NULL else if (nrow(newCSVmv) == 0) newCSVmv <- NULL
    
    newMV <- newCSVmv
  }
  ######################
  ## joining objects ###
  ######################
  
  # here is where the object data is needed
  if (!exists("data") | is.null(data) | length(data) == 0) { # here need to check what is possible (Clemens)
    if (is.null(newMV)) {
        result <- NULL
        logger.info("No new data in rds or csv files and no input data from previous App. Returning NULL.") # works
      } else {
        result <- newMV
        logger.info("New data uploaded from rds/csv file, no previous input data.") # works
      }
  } else {
      if (is.null(newMV)) {
        result <- data
        logger.info("No new data in rds or csv files. Returning input data.") # works
      } else {
        if (!st_crs(data) == st_crs(newMV)) {
          newMV <- st_transform(newMV, st_crs(data))
          logger.info(paste0("The new data sets to combine has a different projection. It has been re-projected, and now the combined data set is in the '", st_crs(data)$input, "' projection."))
        }
        
        # drop units from intersecting columns that are not defined as time/loc/track_ID
        defd <- c(time_col, track_id_col, trimws(strsplit(as.character(coords), ",")[[1]]), attr(data, "sf_column"))
        overlp <- intersect(names(newMV), names(data))
        drp <- overlp[!is.element(overlp, defd)]
        if (length(drp > 0)) {
          for (i in seq(along = drp))
          {
            dataunit <- eval(parse(text = paste("class(data$", drp[i], ")=='units'", sep = "")))
            newMVunit <- eval(parse(text = paste("class(newMV$", drp[i], ")=='units'", sep = "")))
            
            if (dataunit & !newMVunit) {
              eval(parse(text = paste("data$", drp[i], "<- drop_units(data$", drp[i], ")", sep = ""))) # if only the variable in data has units, drop them
              logger.info(paste("Your uploaded file does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
            if (newMVunit & !dataunit) {
              eval(parse(text = paste("newMV$", drp[i], "<- drop_units(newMV$", drp[i], ")", sep = ""))) # if only the variable in newMV has units, drop them
              logger.info(paste("Your App input data set does not contain units for the attribute", drp[i], ". Make sure that its units are the same in both data sets. The units will be dropped for further analysis steps."))
            }
          }
        }
        
        result <- mt_stack(data, newMV, .track_combine = "rename", .track_id_repair = "universal")
        logger.info("New data uploaded from csv file and appended to input data.") # works
      }
        
    }
  return(result)
}
