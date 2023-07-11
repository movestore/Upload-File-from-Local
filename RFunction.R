library('move2')
library('move')

# Showcase injecting app setting (parameter `year`)
rFunction = function(data=NULL, time_col="timestamp", track_id_col="deployment.id", track_attr="",coords="location.long,location.lat",crss="WGS84", ...) {
  
  # rds file # works :)
  fileName1 <- paste0(getAppFilePath("rdsFile_ID"), "data.rds") #default is NULL
  logger.info(paste("Reading file", fileName1,"of size:", file.info(fileName1)$size,"."))
  new1 <- readRDS(fileName1)
  
  # if move2 object - fine, else transform moveStack to move2
  if (is.null(new1)) logger.info("No new rds data found.") else logger.info("New data uploaded from rds file.")
  if (!is.null(new1) & any(class(new1)=="MoveStack")) 
  {
    new1 <- mt_as_move2(new1)
    logger.info("Uploaded rds file containes a moveStack object that is transformed to move2.")
  }
  
  # csv file
  fileName2 <- paste0(getAppFilePath("csvFile_ID"), "data.csv") #default is NULL
  logger.info(paste("Reading file", fileName2,"of size:", file.info(fileName2)$size,"."))
  try(df2 <- read.csv(fileName2,header=TRUE),silent=TRUE)
  if (!exists("df2")) df2 <- NULL
  
  if(is.null(df2)) logger.info("No new csv data found. The settings of the App are not used.") else {
    logger.info("New data uploaded from csv file.")
    logger.info(paste("You have defined as time column:",time_col,"."))
    logger.info(paste("You have defined as track ID column:",track_id_col,"."))
    logger.info(paste("You have defined as track attributes:",track_attr,"."))
    #this only if a comma in the data!
    if(length(grep(",",track_attr))>0) tr_attr <- trimws(strsplit(as.character(track_attr),",")[[1]]) else tr_attr <- track_attr
    logger.info(paste("You have defined as coordinate columns:",coords,"."))
    if(length(grep(",",coords)) %in% c(1,2)) coo <- trimws(strsplit(as.character(coords),",")[[1]]) else {
          coo <- NULL
          logger.info("An incorrect number of coordinate columns is provided; expecting two (x,y) or three (x,y,z). Cannot transform csv data to move2.")
          df2 <- NULL
    }
    logger.info(paste("You have defined as projection (crs):",crss,"."))
  }

  # transform data.frame to move2 object
  if (!is.null(df2)) new2 <- mt_as_move2(df2,time_column=time_col,track_id_column=track_id_col,track_attributes=tr_attr,coords=coo,crs=crss,na.fail=FALSE) else new2 <- NULL #na.fail ok?

  # here is where the object data is needed
  if (!exists("data") | is.null(data) | length(data)==0) #here need to check what is possible (Clemens)
  {
    if(is.null(new1))
    {
      if(is.null(new2)) 
      {
        result <- NULL
        logger.info("No new data in rds or csv files and no input data from previous App. Returning NULL.") # works
      } else
      {
        result <- new2
        logger.info("New data uploaded from csv file.") # works
      }
    } else
    {
      if(is.null(new2)) 
      {
        result <- new1
        logger.info("New data uploaded from rds file.") # works
      } else
      {
        result <- mt_stack(new1,new2,.track_combine="rename")
        logger.info("New data uploaded from rds and csv files. Both data sets are merged.") #does not work: problem with mt_stack() 
        #[INFO] You have defined as projection (crs): WGS84 .
        #[1] "ERROR:  \033[1m\033[33mError\033[39m:\033[22m\n\033[33m!\033[39m Can't combine `..1` <factor<7244e>> and `..2` <integer>.\n"
      }   
    }
  } else
  {
    if(is.null(new1))
    {
      if(is.null(new2)) 
      {
        result <- data
        logger.info("No new data in rds or csv files. Returning input data.") # works
      } else
      {
        result <- mt_stack(data,new2,.track_combine="rename")
        logger.info("New data uploaded from csv file and appended to input data.") # does not work, error in mt_stack
        #Error in (function (..., .ptype = NULL, .name_spec = NULL, .name_repair = c("minimal", :
        #Can't combine `..1` <factor<27461>> and `..2` <integer>.
      }
    } else
    {
      if(is.null(new2)) 
      {
        result <- mt_stack(data,new1,.track_combine="rename")
        logger.info("New data uploaded from rds file and appended to input data.") # works for move1 and move2
      } else
      {
        result <- mt_stack(data,new1,new2,.track_combine="rename")
        logger.info("New data uploaded from rds and csv files. Both data sets are appended to input data.") # didnt try, but if other cases dont work, likely not
      }   
    }
  }

  return(result)
}
