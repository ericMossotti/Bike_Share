# Imports, Processes and Cleans ----

library(duckdb)

# Load external libraries ----
source("Scripts/unz_relocate.R")

# Need a list of file download addresses.
durls <-
    sprintf("https://divvy-tripdata.s3.amazonaws.com/%d-divvy-tripdata.zip",
            202301:202312)

# Need a directory to store the temporary files. 
dir.create("tempZips")

# Need a list of file-folder paths to store the downloaded files.
tempZipPaths <- sprintf("tempZips/%d-divvy-tripdata.zip",
                        202301:202312)

# A simple way to download and relocate the downloaded files from the working 
# directory to the file-folder paths created earlier. 
curl::multi_download(durls,
                     destfiles = tempZipPaths)

# Create tempFile directory
dir.create("tempFiles")

# Create list of tempFile directory paths
tempfile_paths <- sprintf("tempFiles/%d-divvy-tripdata.csv",
                          202301:202312)

# Create a CSV list to specify for unzipping.
fileNames <- sprintf("%d-divvy-tripdata.csv",
                     202301:202312)

# Executes sourced custom function from the unz_relocate.R file.
unz_relocate()

# To remove the directory and contents thereof after having finished using.
unlink("tempZips",
       recursive = TRUE)

# Would like to read in all unzipped files to a single dataframe.
tripTibble <- 
    purrr::map(tempfile_paths[1:12],
               arrow::read_csv_arrow) |>
    purrr::list_rbind()


# duckDB for data storage ----

# Folder to hold database files..
dir.create("db")

# A path for the initial database then processed database file 
tblPath <- "db/data.db"

# A path for the raw data
rawPath <- "db/rawData.db"

dbconn <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = tblPath,
                         read_only = FALSE)

# Write database to long term storage. 
tripTibble |> 
    as.data.frame() |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = tblPath,
                         overwrite = TRUE,
                         check_from = FALSE)

# Write a copy for the original raw data
tripTibble |>
    as.data.frame() |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = rawPath,
                         overwrite = TRUE,
                         check_from = FALSE)

# Clean up environment.
rm(tripTibble)

# Incomplete observations removed
dplyr::tbl(dbconn, tblPath) |>
    dplyr::collect() |>
    tidyr::drop_na() |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = tblPath,
                         overwrite = TRUE)

# To make use of the supplied trip interval data, record duration of trip 
# in minutes
dplyr::tbl(dbconn, tblPath) |>
    dplyr::collect() |>
    dplyr::mutate("trip_time" = lubridate::time_length(
        lubridate::interval(started_at,
                            ended_at),
        unit = "minute"), 
        .keep = "all") |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = tblPath,
                         overwrite = TRUE)

# Would like to remove  all files and sub-folders within the tempFiles folder.
unlink("tempFiles", recursive = TRUE)

