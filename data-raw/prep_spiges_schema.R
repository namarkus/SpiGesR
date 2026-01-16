# data-raw/spiges_schema_prep.R
#
# Build internal object `spiges_schema` from metadata CSV.
# This will be stored in R/sysdata.rda as an internal dataset.

library(readr)
library(purrr)
library(tidyr)
library(dplyr)

# 1) Read metadata file
spiges_meta <- read_delim(
  file = "data-raw/SpiGes_Metadaten_v1.4.csv",
  delim = ";",
  col_types = cols(
    varnum = col_integer(),
    key = col_integer(),
    .default = col_character()
  )
)

# 2 ) extract file-/tablenames
spiges_tables <- spiges_meta |>
  distinct(filename, tablename) |>
  (\(df) setNames(df$filename, df$tablename))()


# 3) Split by tablename to create one schema table per logical table
spiges_columns <-
  spiges_meta %>%
  select(tablename, varnum, canonical, type, key, clear_name, anon_name) %>%
  arrange(tablename, varnum) |>
  nest(
    data = c(canonical, type, key, clear_name, anon_name),
    .by = tablename
  ) |>
  mutate(data = set_names(data, tablename)) |>
  pull(data)


# Pack everything into a versioned list (name "1.4")
spiges_schema <- list(
  "1.4" = list(spiges_tables = spiges_tables, spiges_columns = spiges_columns)
)
