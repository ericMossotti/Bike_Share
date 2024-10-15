# ----
# CC BY-SA, Eric Mossotti 
# ----
# Description ----
# 
# Simplifies duckDB database export to a separate directory. Prevents the need
# for re-downloading and recreating db tables in some circumstances.

dbExporter <- function(dbdir, query) {
    
    if (dir.exists("db_exported") == FALSE ) {
        dir.create("db_exported")
    }
    
    conn <- DBI::dbConnect(duckdb::duckdb(), dbdir)
    
    DBI::dbExecute(conn, query)
    
}
