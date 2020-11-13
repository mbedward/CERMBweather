# CERMBweather

A package of functions to query weather station data held in a PostgreSQL 
database. The package is developed for use by the Centre for Environmental 
Risk Management of Bushfires, University of Wollongong (Australia). The 
package functions make it easier for users to run involved queries without
needing an in-depth knowledge of SQL or dplyr. Each query function has an
option to either run the query and return the resulting data, or just 
return SQL code composed for the query.

An associated R package with functions to manage a PostgreSQL database,
import data provided by the Australian Bureau of Meteorology (BOM), and 
calculate fire weather variables can be found at https://github.com/mbedward/CERMBweatheradmin.

**Note** Feel free to look at or use the code but (a) lots of things don't
work yet, and (b) the functions here assume a particular database
structure and conventions specific to BOM weather station data.
