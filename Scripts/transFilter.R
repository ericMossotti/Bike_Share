# ----
# Author: Eric Mossotti
# CC-BY SA
# ----
# Performs data transformations and filters to enforce consistency across
# all of the analyses. It also will simplify query syntax in extended analyses.
# Filtering can be based on flexible, but sensible criteria.
# ----
transFilter <- function(conxn, oldPath, newPath) {
    dplyr::tbl(conxn, oldPath) |>
        dplyr::collect() |>
        dplyr::mutate(
            "trip_time" = lubridate::time_length(lubridate::interval(started_at, ended_at), 
                                                 unit = "minute"),
            miles = geosphere::distGeo(
                p1 = cbind(start_lng, start_lat),
                p2 = cbind(end_lng, end_lat)
            ) / 1000 * 0.62137119,
            mph = (miles / (trip_time / 60)),
            .keep = "all",
        ) |> 
        # Floor rationale -  less than 0.1 miles are distances easily walked
        # Speed ceiling rationale - pro cyclists average around 20 mph
        # Speed floor rationale - accounts for trips possibly spent idling
        # rideable_type rationale - docked_bike stopped being recorded as a distinct
        # category within the time being analyzed (2023)
        # docked_bike was phased out and not much info about what it means
        dplyr::filter(
            trip_time > 1,
            trip_time < 480,
            rideable_type != "docked_bike",
            miles >= 0.1,
            mph <= 20,
            mph >= 1
        ) |>
        duckdb::dbWriteTable(conn = conxn,
                             name = newPath,
                             overwrite = TRUE)
}
