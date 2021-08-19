###### Private (non exported) functions and variables

# Checks that an object is a valid database connection (pool object) and
# that the expected weather data tables are present
.ensure_connection <- function(db) {
  if (!bom_db_is_connected(db)) stop("Database connection is closed or invalid")

  tbls <- tolower(DBI::dbListTables(db))
  Expected <- c("aws", "synoptic", "stations")
  found <- Expected %in% tbls

  if (any(!found)) {
    msg1 <- ifelse(sum(!found) == 1, "table", "tables")
    msg2 <- paste(Expected[!found], collapse=", ")
    msg <- glue::glue("Database is missing {msg1} {msg2}")
    stop(msg)
  }
}
