## Lookup table used by the bom_db_tidy_data function

COLUMN_LOOKUP <- read.csv("data-raw/COLUMN_LOOKUP_table.csv", stringsAsFactors = FALSE)
usethis::use_data(COLUMN_LOOKUP, overwrite = TRUE)
rm(COLUMN_LOOKUP)
