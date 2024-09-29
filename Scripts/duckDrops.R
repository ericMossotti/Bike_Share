#  ----
# Author: Eric Mossotti
# CC BY-SA
#  ----
# For easily dropping specified tables from the duck db. 
# ----
# 
paths <- duckdb::dbListTables(dbconn)

# drops_tables() ----
drops_tables <- function(path) {
    duckdb::dbRemoveTable(dbconn, 
                          path)
    }

choice <- c()

cat("Remove which tables? (separate by comma)\n")
for (i in seq(paths)) {
    message(as.character(i), ". ", paths[i])
} 

choice <- readline(prompt = message("\n Drop which tables? \n")) |>
    stringr::str_split(",") |>
    unlist() |>
    as.integer()

if (is.na(choice) == TRUE) {
    message("No tables were removed\n")
} else {
    paths[choice] |> purrr::walk(drops_tables)    
}

message("Tables left:\n")

x <- duckdb::dbListTables(dbconn)

as.matrix(x)