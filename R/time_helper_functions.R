#' Convert integer year, month, day values to Date objects.
#'
#' This is a helper for other package functions. If year is a data frame or
#' matrix it is assumed to have columns year, month and day. Otherwise three
#' equal-length vectors are expected.
#'
#' @param year Either a vector of four digit year numbers, or a data frame or matrix with
#'   column names year, month, day (all lower case).
#'
#' @param month A vector of month numbers with the same length as \code{year}. Ignored if
#'   \code{year} is a matrix or data frame.
#'
#' @param day A vector of day numbers with the same length as \code{year}. Ignored if
#'   \code{year} is a matrix or data frame.
#'
#' @return A vector of \code{Date} objects.
#'
#' @export
#'
bom_ymd_to_date <- function(year, month, day) {
  if (inherits(year, c("data.frame", "matrix"))) {
    x <- year
    colnames(x) <- tolower(colnames(x))
    if (!all( c("year", "month", "day") %in% colnames(x) ) ) {
      stop("Matrix or data frame input must have columns: year, month, day")
    }

    year <- x[, "year"]
    month <- x[, "month"]
    day <- x[, "day"]

  } else if (inherits(year, "numeric")) {
    if(length(month) != length(year) || length(day) != length(year)) {
      stop("year, month and day vectors must be the same length")
    }
  }

  as.Date( sprintf("%4d-%02d-%02d", year, month, day) )
}


#' Convert Date objects to a data frame of integer year, month, day values
#'
#' This is a helper for other package functions.
#'
#' @param dates A vector of \code{Date} objects
#'
#' @return A data frame with columns year, month, day
#'
#' @export
#'
bom_date_to_ymd <- function(dates) {
  stopifnot(inherits(dates, "Date"))

  data.frame(
    year = as.integer(lubridate::year(dates)),
    month = as.integer(lubridate::month(dates)),
    day = lubridate::day(dates)
  )
}

