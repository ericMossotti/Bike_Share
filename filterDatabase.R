filterDatabase <- function(conxn = dbconn,
                           path1 = dupelessPath,
                           path2 = tblPath_fltrd) {
    dplyr::tbl(conxn,
               path1,
               check_from = FALSE) |>
        dplyr::filter(trip_time > 1,
                      trip_time < 480,
                      rideable_type != "docked_bike") |>
        dplyr::collect() |>
        # Might as well calculate distance traveled while at it.
        dplyr::mutate(
            miles = geosphere::distGeo(
                p1 = cbind(start_lng, start_lat),
                p2 = cbind(end_lng, end_lat)
            ) / 1000 * 0.62137119,
            mph = (miles / (trip_time / 60))
        ) |>
        # It's nonsensical to rent a bike for distances easily walked.
        dplyr::filter(miles > 0.1,
                      # Seems that pro cyclists average around 20 mph,
                      # so I set that as the ceiling.
                      mph <= 20,
                      # To account for time spent idling, stoplights and
                      # traffic.
                      mph > 1) |>
        duckdb::dbWriteTable(
            conn = conxn,
            name = path2,
            overwrite = TRUE,
            check_from = FALSE
        )
}