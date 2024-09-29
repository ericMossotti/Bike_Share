# ----
# Author: Eric Mossotti
# CC BY-SA
#  ----

library(duckdb)

# Load external scripts ----
source("Scripts/unz_relocate.R")

# Create a list of file download addresses
durls <-
    sprintf("https://divvy-tripdata.s3.amazonaws.com/%d-divvy-tripdata.zip",
            202301:202312)

# Create a directory to store the temporary files
dir.create("tempZips")

# Create a list of file-folder paths to store the downloaded files
tempZipPaths <- sprintf("tempZips/%d-divvy-tripdata.zip",
                        202301:202312)

# A simple way to download and relocate those files from the working directory to the file-folder paths created earlier. 
curl::multi_download(durls,
                     destfiles = tempZipPaths)

# Create tempFile directory
dir.create("tempFiles")

# Create list of tempFile directory paths
tempfile_paths <- sprintf("tempFiles/%d-divvy-tripdata.csv",
                          202301:202312)

# Create a CSV list to specify for unzipping
fileNames <- sprintf("%d-divvy-tripdata.csv",
                     202301:202312)

# Execute sourced custom function from the unz_relocate.R file
unz_relocate()

# To remove the directory and contents thereof after having finished using
unlink("tempZips",
       recursive = TRUE)

# Would like to read in all unzipped files to a single dataframe
dataTibble <- 
    purrr::map(tempfile_paths[1:12],
               arrow::read_csv_arrow) |>
    purrr::list_rbind()

# duckDB for data storage ----

# Folder to hold database files
dir.create("db")

# A path for the initial database then processed database file 
database_path <- "db/data.db"

# For the original data
original_path <- "db/original_data.db"

# For the complete observation data
complete_path <- "db/complete_data.db"

# Initialize a duckDB database connection
dbconn <- DBI::dbConnect(duckdb::duckdb(),
                         dbdir = database_path,
                         read_only = FALSE)

# Original data table
dataTibble |>
    as.data.frame() |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = original_path,
                         overwrite = TRUE)

# Clean data and working environment ----
# 
rm(dataTibble)

# Complete observations table
dplyr::tbl(dbconn, original_path) |>
    dplyr::collect() |>
    tidyr::drop_na() |>
    duckdb::dbWriteTable(conn = dbconn,
                         name = complete_path,
                         overwrite = TRUE)

unlink("tempFiles", recursive = TRUE)
