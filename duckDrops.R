# For easily dropping specified tables from the duck db. 

paths <- duckdb::dbListTables(dbconn_fltrd)

drops_tables <- function(path) {
    duckdb::dbRemoveTable(dbconn_fltrd, 
                          path)
}

choice <- c()

cat("Remove which tables? (separate by comma)\n")
for (i in seq(paths)) {
    choice[i] <- readline(prompt = message(as.character(i),
                                           ". ",
                                           paths[i]))
}

paths[choice] |> purrr::walk(drops_tables)

duckdb::dbListTables(dbconn_fltrd)