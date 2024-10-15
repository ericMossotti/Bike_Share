# ----
# CC BY-SA, Eric Mossotti
#  ----
# Description ----
# 
# Reads the downloaded files into a dataframe. Then writes the df to local 
# storage and cleans up the environment.
# 
# ---- 

library(duckdb)

csv_toDB <- function (
        tmpfile_dir,
        tmpfile_paths,
        db_dir,
        database_path,
        original_path,
        complete_path
) {
    # Would like to read in all unzipped files to a single dataframe
    dataTibble <-
        purrr::map(tmpfile_paths[1:12], 
                   arrow::read_csv_arrow) |>
        purrr::list_rbind()
    
    # duckDB for data storage ----
    
    # Folder to hold database files
    dir.create(db_dir)
    
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
    rm(dataTibble)
    
    # Complete observations write
    dplyr::tbl(dbconn, original_path) |>
        dplyr::collect() |>
        tidyr::drop_na() |>
        duckdb::dbWriteTable(conn = dbconn,
                             name = complete_path,
                             overwrite = TRUE)
    
    unlink(tmpfile_dir, recursive = TRUE)
}
