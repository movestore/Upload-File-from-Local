# Upload File from Local

MoveApps

Github repository: github.com/movestore/Upload-File-from-Local

## Description
Upload tracking data as moveStack, move2 (rds) and/or csv table from your local system. The data will be transformed to a move2_loc object and appended to possible App input data.

## Documentation
The App reads either an rds file and/or a csv table of tracking location data. The data are transformed to a move2_loc object so they can be analysed with many other Apps in a workflow. In case this App is not the start of a workflow and has input data, the input data and the newly read data are combined/stacked in a move2 object with renaming of all tracks if any track IDs are the same in both (or all) objects. Records with the same track ID and timestamp can cause errors, for example by requiring an animal to be in two places at the same time, and therefore are not allowed. If present in the data set, the duplicated timestamp entry with least columns containing NAs is retained.

If the data is a move2 object in an rds file, it is read in without any changes. If it is a moveStack (soon to be deprecated), then it is transformed to a move2 object.

If the data is a csv table with location data, it is required that the data contain a column defining the track, one column to define the timestamp (see expected format in 'Settings') and two/three columns defining the location. The names of these columns have to be provided in the settings of the App. Furthermore, it is necessary to specify the crs/projection of the coordinate system the locations were taken in, the default is EPSG:4326. Finally, track attributes can be specified, that will then be saved separately in the move2 object, avoiding a lot of duplicated data.


It is possible to upload simultaneously a csv and an rds file, these will be than combined into one object.

...

### Input data
none or 
move2::move2_loc object

(see settings for details of local files to upload)

### Output data
move2::move2_loc object - being the input integrated with additionally uploaded data

### Artefacts
none

### Settings 
`Name of the time column` (time_col): Column to use as the timestamp column for the transformation of the table data to a move2 object. The expected timestamp format is 'yyyy-mm-dd HH:MM:SS' and in UTC timezone. Default "timestamp".

`Name of the track ID column` (track_id_col): Column to use as the track ID column for transformation of the data table to a move2 object. Beware of possible issues if you have reused tags on different animals or used several tags on the same animal. If this is the case, create a column before uploading the data with a unique identifier for each animal and tag combination, e.g. by creating a 'animalName_TagID' column. Default "individual-local-identifier".

`Name of the attributes to become track attributes` (track_attr): List of attributes that are pure track attributes, i.e. have only one value per track. This will make working with the data easier in subsequent Apps. The names must be separated with comma. Default is the empty string "", i.e. no tack attributes.

`Names of the longitude and latitute columns` (coords): Names of the two (or three) coordinate columns in your data for correct transformation to a move2 object. The order must be x/longitude followed by y/latitute and optionally z/height. The names must be separated with comma. Default: "location-long, location-lat".

`Coordinate reference system` (crss): Coordinate reference system/ projection to useas a valid numeric EPSG value. For more info see https://epsg.io/ and https://spatialreference.org/. Default 4326 (EPSG:4326, standard longitude/latitude)

`Tracking data in csv format` (csvFile_ID): Local, comma-separated csv file of tracking data to be uploaded, called 'data.csv' (this file name is compulsory). Attribute names of key properties can be indicated in the settings above. Please take care to adapt them.

`Tracking data in rds format` (rdsFile_ID): Local rds file of moveStack or move2 object of tracking data to upload, called 'data.rds' (this file name is compulsory). Attribute names as indicated above will not be used, but taken from the file.


### Most common errors
none so far, please let me know if you encounter problems.

### Null or error handling

**Settings `column names`:** Take care that the spelling is correct, else the App will run into an error.

**Settings `crss`:** If this is not a correct EPSG number, the App wil run into an error. The default of `4326` is the correct projection for data from Movebank.

**Settings `file upload`:** Take care to rename your files according to what the app expects (data.csv or data.rds), else the App cannot find the data and will not add the tracks. Instead the original input data or NULL will be returned.
