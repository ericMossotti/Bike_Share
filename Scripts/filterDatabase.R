filterDatabase <- function(conxn = dbconn,
                           path1 = tblPath,
                           path2 = tblPath_fltrd) {
    dplyr::tbl(conxn, 
               path1) |>
        dplyr::filter(trip_time > 1, 
                      trip_time < 480
                      #rideable_type != "docked_bike"
                      ) |>
        dplyr::collect() |>
        
# Might as well calculate distance traveled while at it.
        dplyr::mutate(
            miles = geosphere::distGeo(
                p1 = cbind(start_lng, start_lat),
                p2 = cbind(end_lng, end_lat)
            ) / 1000 * 0.62137119,
            mph = (miles / (trip_time / 60))
        ) |>
        # Floor rationale -  less than 0.1 miles are distances easily walked
        # Speed ceiling rationale - pro cyclists average around 20 mph
        # Speed floor rationale - accounts for trips possibly spent idling
        dplyr::filter(miles >= 0.1, 
                      mph <= 20, 
                      mph >= 1) |>
        duckdb::dbWriteTable(
            conn = conxn,
            name = path2,
            overwrite = TRUE)
}