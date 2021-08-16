# CERMBweather

A package of functions to work with weather station data files, as provided by
the Australian Bureau of Meteorology (BOM), and query data in an associated 
PostgreSQL database. The package is developed for use by the Centre for Environmental 
Risk Management of Bushfires, University of Wollongong (Australia). 

An associated R package with functions to manage a PostgreSQL database of
data provided by BOM can be found at https://github.com/mbedward/CERMBweatheradmin.

A further R package to calculate Forest Fire Danger Index from weather records
can be found at https://github.com/mbedward/CERMBffdi.

**Note** Feel free to look at or use the code but (a) lots of things don't
work yet, and (b) the functions here assume a particular database
structure and conventions specific to BOM weather station data.
