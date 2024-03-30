# To help organize the quarto project.
# This part is messy. 

# Load external libraries
source("unz_relocate.R")

durls <-
    sprintf("https://divvy-tripdata.s3.amazonaws.com/%d-divvy-tripdata.zip",
            202301:202312)

# Need some directories to store the files. 
dir.create("tempZips")

tempZipPaths <- sprintf("tempZips/%d-divvy-tripdata.zip",
                        202301:202312)

# A simple way to download and relocate several files. 
curl::multi_download(durls,
                     destfiles = tempZipPaths)

# create tempFile directory
dir.create("tempFiles")

# create list of tempFile directory paths
tempfile_paths <- sprintf("tempFiles/%d-divvy-tripdata.csv",
                          202301:202312)


# create CSV list to specify for unzipping
fileNames <- sprintf("%d-divvy-tripdata.csv",
                     202301:202312)


# review address info that was just created
#tibble::tibble("URLs" = durls,
#               "Zip File Paths" = tempZipPaths,
#               "File Names" = fileNames,
#               "Parquet File Paths" = tempfile_paths)

unz_relocate()

# To remove stored files
unlink("tempZips",
       recursive = TRUE)

tripTibble <- 
    purrr::map(tempfile_paths[1:12],
               arrow::read_csv_arrow) |>
    purrr::list_rbind()

# Need to save this count for later before I drop the incomplete obs
original_nobs <- nrow(tripTibble)

tripTibble <- tripTibble |>
    tidyr::drop_na()

# duckDB instead ----

dir.create("db")

tblPath <- "db/data.db"

# Helpful persistent db
dbconn <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = tblPath,
                         read_only = FALSE,
                         check_from = FALSE)


tripTibble |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = tblPath,
                         overwrite = TRUE,
                         check_from = FALSE)

rm(tripTibble)

# To make use of supplied trip interval data
dplyr::tbl(dbconn,
           tblPath,
           check_from = FALSE) |>
    dplyr::collect() |>
    dplyr::mutate("trip_time" = lubridate::time_length(
        lubridate::interval(started_at,
                            ended_at),
        unit = "minute"), 
        .keep = "all"
    ) |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = tblPath,
                         overwrite = TRUE,
                         check_from = FALSE)

    
# all files and folders
unlink("tempFiles", recursive = TRUE)