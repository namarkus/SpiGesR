## code to prepare `DATASET` dataset goes here

source(file.path('data-raw', 'prep_spiges_schema.R'))

# save all as internal data
usethis::use_data(spiges_schema, overwrite = TRUE)
