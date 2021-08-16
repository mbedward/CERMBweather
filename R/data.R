#' Lookup table to relate BOM data file column names to standard names
#'
#' This data frame is used by function \code{bom_tidy_data} to relate the
#' long field names used in BOM data files for AWS and synoptic weather stations
#' to the shorter column names used within this package and the CERMB PostgreSQL
#' weather database.
#'
#' @seealso \code{\link{bom_tidy_data}}
#'
"COLUMN_LOOKUP"
