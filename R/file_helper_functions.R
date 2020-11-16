#' Convert raw BOM weather station data into a standard format
#'
#' This function takes a data frame of records read from a BOM weather station
#' data file (either AWS or synoptic) and reformats it to the column names and
#' data types used in the PostgreSQL weather database. BOM data files use
#' extremely long column names with slight differences between AWS and synoptic
#' data. The input year, month, and day columns for local and standard time are
#' combined into a date_local and date_standard columns. The input hour and
#' minute columns for local and standard time are kept separate as: hour_local,
#' hour_standard, min_local and min_standard. The mapping of input column names
#' to standard names is defined in the lookup table \code{COLUMN_LOOKUP} that is
#' part of this package.
#'
#' @param dat.raw A data frame of records in raw BOM weather station data format.
#'
#' @return A data frame in the standard format used with the CERMB PostgreSQL
#'   weather database, with the attribute \code{'datatype'} set to either 'aws'
#'   or 'synoptic'.
#'
#' @importFrom rlang .data
#'
#' @export
#'
bom_db_tidy_data <- function(dat.raw) {

  cnames <- tolower(colnames(dat.raw))

  if (any(stringr::str_detect(cnames, "maximum.windgust|aws.flag"))) {
    type <- "aws"
  } else {
    type <- "synoptic"
  }

  ii <- CERMBweather::COLUMN_LOOKUP$datatype == type
  lookup <- CERMBweather::COLUMN_LOOKUP[ii, ]
  lookupnames <- tolower(lookup[["input"]])

  ii <- match(cnames, lookupnames)
  if (anyNA(ii)) stop("Unrecognized column name(s): ", colnames(dat.raw)[is.na(ii)])

  importnames <- lookup[["importcolname"]][ii]
  ii.keep <- !is.na(importnames)

  dat <- dat.raw[, ii.keep]
  colnames(dat) <- importnames[ii.keep]


  # Combine the separate year, month, day columns for local and standard
  # time into two date columns.
  dat <- dat %>%
    # Note: using .data keyword to avoid R package check errors
    dplyr::mutate(date_local = sprintf("%4d-%02d-%02d",
                                       .data$year_local, .data$month_local, .data$day_local),
                  date_std = sprintf("%4d-%02d-%02d",
                                     .data$year_std, .data$month_std, .data$day_std))

  dat <- dat[, stats::na.omit(lookup$dbcolname)]

  attr(dat, "datatype") <- type

  coltypes <- sapply(colnames(dat), function(cn) {
    i <- match(cn, lookup[["dbcolname"]])
    if (is.na(i)) stop("Data column name missing from lookup table: ", cn)
    lookup[["dbcoltype"]][i]
  })

  attr(dat, "coltypes") <- coltypes

  # Sometimes there are non-numeric values in the measure fields
  # (e.g. whitespace or '###'). Guard against this by converting
  # each measure field to numeric. Non-numeric values will be
  # coerced to NA.
  for (i in 4:length(coltypes)) {
    suppressWarnings(
      if (coltypes[i] == "numeric") {
        if (!is.numeric(dat[[i]])) {
          dat[[i]] <- as.numeric(dat[[i]])
        }
      }
    )
  }

  dat
}


#' Extract integer weather station identifiers from file names
#'
#' Given a vector of one or more paths and file names for BOM weather station
#' dat files, this function extracts the integer station identifiers, including
#' any leading zeroes, as a vector of character strings (e.g. "067105"). The
#' function relies on BOM conventions for data file names. The returned vector
#' can be converted to a vector of integer IDs using the base R
#' \code{as.integer} function.
#'
#' @param filenames A character vector of paths and/or file names of BOM weather
#'   station data files.
#'
#' @return A character vector of weather station numbers. Each element will include
#'   any leading zeroes (e.g. "067105").
#'
#' @export
#'
bom_db_extract_station_numbers <- function(filenames) {
  x <- stringr::str_extract(filenames, "_Data_\\d+")
  stringr::str_extract(x, "\\d+")
}


#' Format BOM weather station numbers as six-character strings
#'
#' Given a vector of weather station numbers as either integers or text strings,
#' this function formats each number as a six-character string with leading
#' zeroes (if required). For example, the integer station number \code{67105}
#' will be converted to the string \code{'067105'}. This is useful when matching
#' weather station numbers to BOM data file names because it avoids ambiguity,
#' e.g. incorrectly matching an integer ID 1005 to a filename that includes
#' "041005". If the input is a character vector, the function checks that no
#' elements have more than six characters and then returns a vector with each
#' element left-padded with \code{'0'}.
#'
#' @param ids Vector (integer or character) of one or more station
#'   identifying numbers.
#'
#' @return A character vector of weather station numbers in standard format.
#'
#' @examples
#' # This will return the vector c("067105", "067108")
#' bom_db_station_id(c(67105, 67108))
#'
#' @export
#'
bom_db_station_id <- function(ids) {
  if (is.numeric(ids)) {
    ids <- sprintf("%06d", ids)
  }
  else if (is.character(ids)) {
    len <- stringr::str_length(ids)
    if ( any(len > 6) ) stop("One or more station identifiers have more than 6 characters")

    ids <- stringr::str_pad(ids, width = 6, side = "left", pad = "0")
  }

  ids
}


#' Check if a file name refers to a zip file
#'
#' This is a helper used by other package functions. It simply checks whether
#' the file name has a '.zip' extension (case-insensitive).
#'
#' @param filenames A character vector of paths and/or file names.
#'
#' @return A logical vector in which an element will be \code{TRUE} if the
#'   corresponding input file name referred to a zip file.
#'
#' @export
#'
bom_db_is_zip_file <- function(filenames) {
  path <- stringr::str_trim(filenames)
  stringr::str_detect(tolower(filenames), "\\.zip$")
}


#' Extract file names from full file paths
#'
#' This is a helper used by other package functions. It extracts the
#' file name part of each full path in the \code{paths} argument.
#'
#' @param paths A character vector of one or more file paths.
#'
#' @return A vector of file names
#'
#' @export
#'
bom_db_get_file_name <- function(paths) {
  x <- stringr::str_split(paths, "[\\\\/]+")
  sapply(x, utils::tail, 1)
}

