#' Check if a database connection is valid and open
#'
#' This function tests that a connection object is both valid and pointing to an
#' open connection. R packages for working with particular databases such as
#' \code{RPostgres} and \code{RSQLite}, as well as related higher level packages
#' such as \code{pool} and \code{DBI}, provide a function \code{dbIsValid} to
#' test if an object is a valid database connection. However, this function only
#' checks that an object is of the right type (i.e. it \emph{is} a database
#' connection object) and not that the connection is actually open, so
#' \code{dbIsValid(mydb)} can return \code{TRUE} even if the connection has been
#' closed or the object \code{mydb} is pointing to an incorrect database path.
#'
#' @param con A database connection object, e.g. created using
#'   \code{DBI::dbConnect()} or \code{pool::dbPool()}.
#'
#' @return \code{TRUE} if the connection is valid and currently open;
#'   \code{FALSE} otherwise.
#'
#' @export
#'
bom_db_is_connected <- function(con) {
  DBI::dbIsValid(con) &&
    tryCatch(DBI::dbGetQuery(con, "select TRUE;")[,1], # Get bool result from returned data frame
             error = function(e) FALSE)
}


#' Get daily aggregate rainfall data
#'
#' @param db A database connection object.
#'
#' @param the.table The table to query: either 'aws' or 'synoptic'
#'   (may be abbreviated).
#'
#' @param stations A vector containing one or more BOM weather station integer
#'   IDs. Any values not present in the table will be silently ignored so, for
#'   example, you could retrieve data for all (almost) NSW stations by calling
#'   the function with \code{stations = 46000:75999}.
#'
#' @param crop If \code{TRUE} (default), the returned values will not include
#'   the earliest rain date (09:01 - 09:00) in the series as this is most
#'   probably a partial day.
#'
#' @param start.date The start date (local time) for the query, provided as
#'   either a \code{Date} object or a character string in the format
#'   \code{yyyy-mm-dd}. The default is to start from the earliest data for
#'   each station.
#'
#' @param end.date The end date (local time) for the query, provided as
#'   either a \code{Date} object or a character string in the format
#'   \code{yyyy-mm-dd}. The default is to end with the latest data for each
#'   station.
#'
#' @param dry.run If \code{TRUE}, the function returns the SQL code for the
#'   query but does not run it. This can be useful if you want to further
#'   modify the query. If \code{FALSE} (default), the function runs the query
#'   and returns the resulting data.
#'
#' @return A data frame with columns: station, date_rain (09:01 - 09:00 local
#'   time), precip_daily.
#'
#' @importFrom rlang .data
#'
#' @export
#'
bom_db_get_daily_rainfall <- function(db,
                                      the.table,
                                      stations,
                                      crop = TRUE,
                                      start.date = NULL,
                                      end.date = NULL,
                                      dry.run = FALSE) {

  if (!dry.run) .ensure_connection(db)

  if (!(is.numeric(stations) && length(stations) > 0)) {
    stop("stations arg should be a vector of integer station IDs")
  }

  the.table <- match.arg(tolower(the.table), choices = c("aws", "synoptic"))

  if (is.character(start.date)) start.date <- as.Date(start.date)
  if (is.character(end.date)) end.date <- as.Date(end.date)

  # Clause to aggregate rain values for aws table (max value) and
  # synoptic table (sum of values)
  precip_daily_clause <- switch(
    the.table,
    aws = "MAX(precipitation)",
    synoptic = "SUM(precipitation)",
    stop("Unknown table name: ", the.table) )

  # Clause to select station(s).
  stations <- stats::na.omit(stations)
  stations_clause <- glue::glue("station IN ({paste(stations, collapse = ', ')})")

  # If there are a lot of stations but the IDs are contiguous,
  # create a neater version of the clause
  if (length(stations) > 10) {
    s0 <- min(stations)
    s1 <- max(stations)
    if (setequal(stations, s0:s1)) {
      stations_clause <- glue::glue("station >= {s0} AND station <= {s1}")
    }
  }

  # Clauses for start and end dates if specified.
  # Note: the glue function returns an empty string if any input args
  # are NULL but we won't rely on that behaviour in case it changes
  # in the future
  #
  if (is.null(start.date)) {
    start_clause <- ""
  } else {
    start_clause <- glue::glue("AND date_local >= '{start.date}'::date")
  }

  if (is.null(end.date)) {
    end_clause <- ""
  } else {
    end_clause <- glue::glue("AND date_local <= '{end.date}'::date")
  }

  cmd <- glue::glue(
    "SELECT station,
            date_rain,
            -- using coalesce() to replace nulls with zero
            COALESCE({precip_daily_clause}, 0) AS precip_daily
    FROM (
      SELECT station, precipitation,
        CASE
          WHEN hour_local > 9 OR (hour_local = 9 AND min_local > 0) THEN date_local
          ELSE (date_local - INTERVAL '1 day')::date
        END
        AS date_rain
      FROM {the.table}
      WHERE {stations_clause} {start_clause} {end_clause}) AS sub_daily

    GROUP BY station, date_rain
    ORDER BY station, date_rain;")

  if (dry.run) {
    # Just return composed query text
    cmd
  } else {
    # Run query and return data
    res <- DBI::dbGetQuery(db, cmd)

    if (crop) {
      # Note: using .data keyword to avoid R package check errors
      res <- res %>%
        dplyr::group_by(.data$station) %>%
        dplyr::filter(.data$date_rain > min(.data$date_rain)) %>%
        dplyr::ungroup()
    }

    res
  }
}


#' Get first and last record dates for stations
#'
#' This function performs a fast query to find the dates of the earliest and
#' the most recent record for each station.
#'
#' The speed of this query with large tables comes from the use of a LATERAL
#' join. This is a type of join provided by PostgreSQL that allows a SQL query
#' to function like a for-each loop. It also relies on the presence of an index
#' on the date_local field. The approach is adapted from an example (number 5)
#' in \href{https://stackoverflow.com/a/34715134/40246}{this StackOverflow answer.}
#'
#' @param db A database connection object.
#'
#' @param the.table The table to query: either 'aws' or 'synoptic'
#'   (may be abbreviated).
#'
#' @param dry.run If \code{TRUE}, the function returns the SQL code for the
#'   query but does not run it. This can be useful if you want to further
#'   modify the query. If \code{FALSE} (default), the function runs the query
#'   and returns the resulting data.
#'
#' @return A data frame with columns: station, earliest_date, latest_date.
#'
#' @export
#'
bom_db_get_station_dates <- function(db, the.table, dry.run = FALSE) {
  if (!dry.run) .ensure_connection(db)

  the.table <- match.arg(tolower(the.table), choices = c("aws", "synoptic"))

  # lateral query template
  cmd0 <- glue::glue("SELECT station, date_local as {{field.name}}
                     FROM
                       {the.table}_stations s,
                       LATERAL (
                        SELECT date_local
                        FROM {the.table}
                        WHERE station = s.station  -- lateral reference
                        ORDER BY date_local {{asc.desc}}  -- order by date
                        LIMIT 1  -- only return earliest or latest date
                       ) l")

  # Separate earliest and latest date queries
  cmd.earliest <- glue::glue(cmd0,
                             field.name = "earliest_date",
                             asc.desc = "ASC")

  cmd.latest <- glue::glue(cmd0,
                           field.name = "latest_date",
                           asc.desc = "DESC")

  # Combined query
  cmd.full <- glue::glue("SELECT early.station, earliest_date, latest_date
                         FROM ({cmd.earliest}) early
                         LEFT JOIN ({cmd.latest}) late
                         ON early.station = late.station
                         ORDER BY early.station;")

  if (dry.run) {
    # Just return composed query text
    cmd.full
  } else {
    # Run query and return data
    DBI::dbGetQuery(db, cmd.full)
  }
}



