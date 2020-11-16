#' Get a \code{tbl} object for AWS data to use with dplyr functions
#'
#' This function takes an open connection to a database and returns a dplyr
#' \code{tbl} object for AWS data to use with dplyr functions.
#'
#' @param db A database connection pool object.
#'
#' @return A \code{tbl} object representing the AWS table to use with dplyr.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to a SQLite database...
#' DB <- pool::dbPool(RSQLite::SQLite(),
#'                    dbname = "c:/foo/bar/weather.db")
#'
#' # ...or to a PostgreSQL database
#' DB <- pool::dbPool(RPostgreSQL::PostgreSQL(),
#'                    dbname = "some_database",
#'                    host = "localhost",
#'                    user = "username",
#'                    password = "mypassword")
#'
#' # Get a tbl object for AWS data
#' taws <- bom_db_aws(DB)
#'
#' # Get the field names in the table
#' colnames(taws)
#'
#' # Use dplyr to find the maximum temperature recorded at each
#' # weather station
#'
#' library(dplyr)
#'
#' dat <- taws %>%
#'   group_by(station) %>%
#'   summarize(maxtemp = max(temperature, na.rm = TRUE)) %>%
#'
#'   # omit records for stations with no temperature values
#'   filter(!is.na(maxtemp)) %>%
#'
#'   # tell dplyr to execute this query on the database
#'   collect()
#' }
#'
bom_db_aws <- function(db) {
  .ensure_connection(db)
  dplyr::tbl(db, "AWS")
}


#' Get a \code{tbl} object for synoptic data to use with dplyr functions
#'
#' This function takes an open connection to a database and returns a dplyr
#' \code{tbl} object for synoptic data to use with dplyr functions.
#'
#' @param db A database connection pool object.
#'
#' @return A \code{tbl} object representing the synoptic table to use with
#'   dplyr.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to a SQLite database...
#' DB <- pool::dbPool(RSQLite::SQLite(),
#'                    dbname = "c:/foo/bar/weather.db")
#'
#' # ...or to a PostgreSQL database
#' DB <- pool::dbPool(RPostgreSQL::PostgreSQL(),
#'                    dbname = "some_database",
#'                    host = "localhost",
#'                    user = "username",
#'                    password = "mypassword")
#'
#' # Get a tbl object for AWS data
#' tsynoptic <- bom_db_aws(DB)
#'
#' # Get the field names in the table
#' colnames(tsynoptic)
#'
#' # Use dplyr to find the maximum temperature recorded at each
#' # weather station
#'
#' library(dplyr)
#'
#' dat <- tsynoptic %>%
#'   group_by(station) %>%
#'   summarize(maxtemp = max(temperature, na.rm = TRUE)) %>%
#'
#'   # omit records for stations with no temperature values
#'   filter(!is.na(maxtemp)) %>%
#'
#'   # tell dplyr to execute this query on the database
#'   collect()
#' }
#'
bom_db_synoptic <- function(db) {
  .ensure_connection(db)
  dplyr::tbl(db, "Synoptic")
}

