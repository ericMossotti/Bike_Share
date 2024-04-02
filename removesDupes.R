# Saving the data to a file to ensure we have a copy free from incomplete and duplicated obs.
removesDupes <- function (conxn = dbconn,
                          path = tblPath) {
dplyr::tbl(conxn,
           path,
           check_from = FALSE) |>
    dplyr::select(ride_id:trip_time) |>
    dplyr::distinct(started_at,
                    start_station_name,
                    ended_at,
                    end_station_name,
                    .keep_all = TRUE) |>
    dplyr::arrange(started_at) |>
    dplyr::collect() |>
    duckdb::dbWriteTable(conn = conxn,
                         name = path,
                         overwrite = TRUE,
                         check_from = FALSE)
}