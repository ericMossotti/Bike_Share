# For easily dropping specified tables from the duck db. 

paths <- duckdb::dbListTables(dbconn)

drops_tables <- function(path) {
    duckdb::dbRemoveTable(dbconn, 
                          path)
    }

choice <- c()

cat("Remove which tables? (separate by comma)\n")
for (i in seq(paths)) {
    message(as.character(i), ". ", paths[i])
} 

choice <- readline(prompt = message("Choose\n")) |>
    stringr::str_split(",") |>
    unlist() |>
    as.integer()
    
paths[choice] |> purrr::walk(drops_tables)

message("Tables left:\n",
        duckdb::dbListTables(dbconn))