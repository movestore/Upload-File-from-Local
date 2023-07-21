library('move2')
library('move')
library('vroom')
library('dplyr')
library('sf')

# vroom reads in the data as is, csv from MB is with "-" in column names
# the csv file is expected to be comma delimited
# The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone
# The expected projection is a valid numeric EPSG value. E.g 4326 (EPSG:4326 is lat/lon)

rFunction  <-  function(data=NULL, time_col="timestamp", track_id_col="individual-local-identifier", track_attr="",coords="location-long,location-lat",crss=4326, duplicates_handling= "first", ...){ 
  
  # rds file # works :)
  fileName1 <- paste0(getAppFilePath("rdsFile_ID"), "data.rds") #default is NULL
  logger.info(paste("Reading file", fileName1,"of size:", file.info(fileName1)$size,"."))
  new1 <- readRDS(fileName1)
  
  if(is.null(new1)){logger.info("No new rds data found.")
  } else { 
    # if .rds is move2
    if(any(class(new1)=="move2")){logger.info("Uploaded rds file containes a object of class move2")}
    # if move2 object - fine, else transform moveStack to move2
    if(any(class(new1)=="MoveStack")){
      new1 <- mt_as_move2(new1)
      logger.info("Uploaded rds file containes a object of class moveStack that is transformed to move2.")
    }
    if(!any(class(new1)%in%c("move2","MoveStack"))){
      new1 <- NULL
      logger.info("Uploaded rds file contains data with unexpected class, please ensure that the file contains a object of class 'move2' or 'MoveStack'.")}
  }
  # make names for new1
  if(!is.null(new1)){
    names(new1) <- make.names(names(new1),allow_=TRUE)
    track_id(new1) <- make.names(track_id(new1),allow_=TRUE)
  }
  
  
  # csv file
  fileName2 <- paste0(getAppFilePath("csvFile_ID"), "data.csv") #default is NULL
  logger.info(paste("Reading file", fileName2,"of size:", file.info(fileName2)$size,"."))
  try(df2 <-  vroom::vroom(fileName2, delim = ",")) # alternative to read.csv, takes care of timestamps, default timezome is UTC
  if (!exists("df2")){
    df2 <- NULL
    logger.info("No new csv data found. The settings of the App are not used.")
  } 
  if(length(grep(",",coords)) %in% c(1,2)){
    coo <- trimws(strsplit(as.character(coords),",")[[1]])
    logger.info(paste("You have defined as coordinate columns:",coords,"."))
  } else {
    coo <- NULL
    logger.info("An incorrect number of coordinate columns is provided; expecting two (x,y) or three (x,y,z). Cannot transform csv data to move2.")
    df2 <- NULL
  }
  if(!is.null(df2)){
    logger.info("New data uploaded from csv file, this file is expected to be comma delimited.")
    logger.info(paste("You have defined as datetime column: ",time_col,".", " The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone"))
    logger.info(paste("You have defined as track ID column: ",track_id_col,"."))
    logger.info(paste("You have defined as track attributes: ",track_attr,"."))
    logger.info(paste("You have defined as projection (crs): ",crss,".", " The expected projection is a valid numeric EPSG value. For more info see https://epsg.io/ and https://spatialreference.org/"))
    
    #this only if a comma in the data!
    if(length(grep(",",track_attr))>0){tr_attr <- trimws(strsplit(as.character(track_attr),",")[[1]])} else {tr_attr <- track_attr}
    
    # transform data.frame to move2 object
    df2 <- dplyr::filter(df2,!is.na(df2[[track_id_col]])) # if there is a NA in the track_id col, mt_as_move2() gives error
    new2 <- mt_as_move2(df2,
                        time_column=time_col,
                        track_id_column=track_id_col,
                        track_attributes=track_attr,
                        coords=coo,
                        crs=crss,
                        na.fail=FALSE)
    ## remove empty locations
    new2 <- new2[!sf::st_is_empty(new2),]  
    if(nrow(new2)==0){logger.info("Your uploaded csv file does not contain any location data.")}
    
    ## remove duplicated timestamps
    if(!mt_has_unique_location_time_records(new2)){
      n_dupl <- length(which(duplicated(paste(mt_track_id(new2),mt_time(new2)))))
      logger.info(paste("Your data has",n_dupl, "duplicated location-time records. We removed here those with less info and then select the",duplicates_handling,"if still duplicated."))
      if(any(c("event_id", "event-id", "event.id") %in% names(new2))){ ## event id is always different for all entries, so excluding it when looking for duplicates
        new2 <- mt_filter_unique(dplyr::select(new2, -starts_with("event")),criterion="subsets") # hope ignoring all event... columns is ok
      } else {
        new2 <- mt_filter_unique(new2,criterion="subsets")
      }
      new2 <- mt_filter_unique(new2,criterion=duplicates_handling)
    }
    
    ## ensure timestamps are ordered within tracks
    new2 <- dplyr::arrange(new2, mt_track_id(new2), mt_time(new2)) 
    
    # make names for new2
    names(new2) <- make.names(names(new2),allow_=TRUE)
    mt_track_id(new2) <- make.names(mt_track_id(new2),allow_=TRUE)
  }
  if(is.null(df2)){new2 <- NULL} #na.fail ok?
  if(nrow(new2)==0){new2 <- NULL}
  
  
  # here is where the object data is needed
  if (!exists("data") | is.null(data) | length(data)==0){ #here need to check what is possible (Clemens)
    if(is.null(new1)){
      if(is.null(new2)){
        result <- NULL
        logger.info("No new data in rds or csv files and no input data from previous App. Returning NULL.") # works
      } else {
        result <- new2
        logger.info("New data uploaded from csv file.") # works
      }
    } else {
      if(is.null(new2)){
        result <- new1
        logger.info("New data uploaded from rds file.") # works
      } else {
        result <- mt_stack(new1,new2,.track_combine="rename")
        logger.info("New data uploaded from rds and csv files. Both data sets are merged.") # works
      }   
    }
  } else {
    if(is.null(new1)){
      if(is.null(new2)){
        result <- data
        logger.info("No new data in rds or csv files. Returning input data.") # works
      } else {
        result <- mt_stack(data,new2,.track_combine="rename")
        logger.info("New data uploaded from csv file and appended to input data.") # works
      }
    } else {
      if(is.null(new2)){
        result <- mt_stack(data,new1,.track_combine="rename")
        logger.info("New data uploaded from rds file and appended to input data.") # works for move1 and move2
      } else {
        result <- mt_stack(data,new1,new2,.track_combine="rename")
        logger.info("New data uploaded from rds and csv files. Both data sets are appended to input data.") # works
      }   
    }
  }
  return(result)
}
