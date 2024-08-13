# I needed the stations groups' burst buttons to fit
# the viewing window in my document and the only way I could think of is to 
#   split the stations into multiple maps. This reduces duplicate code.

mapViewer <- function(x) {
    
    nameSlice <- sampled_stations |>
        dplyr::ungroup() |>
        dplyr::distinct(station_names) |>
        dplyr::slice(x)
    
    viewMap <- sampled_stations |>
        dplyr::filter(station_names %in% nameSlice$station_names) |>
        sf::st_as_sf(coords = c(3:2), crs = 4326) |>
        mapview::mapview(
            zcol = "station_names",
            col.regions = randomColors,
            map.types = "OpenStreetMap",
            burst = TRUE,
            legend = FALSE)
    
    return(viewMap)
}