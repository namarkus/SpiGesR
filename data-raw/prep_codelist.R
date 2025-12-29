## code to prepare variable specs for internal data
# steps:
# 1. read Excel-file with variable specs of a given spiges-version
# 2. extrace code dictionary and variable types
# 3. copy paste the constructive of these objects into file data-raw/prep_internal_data.R


# librarys and options ---------
library(readxl)
library(purrr)
library(constructive)
library(stringr)
library(tidyr)
library(dplyr)

# get data ----------------------------------------------------------------
spiges_pfad = 'C:/Users/namar/Documents/MNDD Referenzdaten/BFS_SpiGes'
variables <- read_excel(file.path(spiges_pfad, 'do-t-14.04-spiges-VL.xlsx'), sheet= 'Variablen,Variables,Variabile',
                       skip = 3)
names(variables)[1] = 'varname'

## Codelisten extrahieren -------------------------------------------------
var_codes <-
  variables %>%
  filter(!is.na(Codeliste)) %>%
  filter(grepl('^.{1,2} =', Codeliste)) %>%
  select(varname, Codeliste, `Liste des codes`, `Elenco dei codici`) %>%
  mutate(across(c(Codeliste, `Liste des codes`, `Elenco dei codici`), ~strsplit(., split = '\r\n')))

# checks
var_codes %>%
  mutate(across(2:4, ~map(., ~sub('^\\s*(\\S+?)\\s*?\\=.*$', '\\1', .)))) %>%
  mutate(across(2:4, ~map_chr(., ~paste0(., collapse=',')))) %>%
  filter(Codeliste != `Liste des codes` | Codeliste != `Elenco dei codici`)

# Dictionary erstellen
code_dict <-
  var_codes %>%
  pivot_longer(cols = c(Codeliste, `Liste des codes`, `Elenco dei codici`), names_to = 'language', values_to = 'Codeliste') %>%
  mutate(language = case_when(
    language == 'Codeliste' ~ 'de',
    language == 'Liste des codes' ~ 'fr',
    language == 'Elenco dei codici' ~ 'it')) %>%
  mutate(Codeliste = map(Codeliste, \(x) strsplit(x, split = '='))) %>%
  mutate(Codeliste = map(Codeliste, \(x) map(x, \(y) trimws(y)))) %>%
  mutate(Codeliste = map(Codeliste, \(x) {
    names <- map_chr(x, \(y) str_trim(y[1]))
    values   <- map_chr(x, \(y) str_trim(y[2]))
    set_names(values, names)
  })) %>%
  summarise(Codeliste = list(set_names(Codeliste, language)), .by = varname) %>%
  summarise(Codeliste = list(set_names(Codeliste, varname))) %>%
  pull(Codeliste) %>%
  set_names('v1.4')

constructive::construct(code_dict)
constructive::construct_clip(code_dict)
# Test
code_dict[['v1.4']][['liegeklasse']][['de']][c('1','3','9')]

## Datentypen extrahieren -------------------------------------------------
var_types_raw <-
  variables %>%
  rename(Format = `Format...8`) %>%
  mutate(vartype = case_when(Format == 'AN' ~ 'character',
                              Format == 'M' & Werte %in% c('0/1', '0 / 1') ~ 'logical',
                              Format == 'N' ~'integer',
                              grepl('^N[0-9]{1,2}\\.[0-9]{1,2}$', Format) ~ 'numeric',
                              .default = 'character')) %>%
  mutate(vartype = if_else(varname %in% names(code_dict[['v1.4']]), 'spigesvar', vartype)) %>%
  select(varname, vartype)

var_types_raw %>% count(vartype)

var_types <-
  var_types_raw %>%
  summarise(vartype = list(set_names(vartype, varname))) %>%
  pull(vartype) %>%
  set_names('v1.4')

constructive::construct(var_types)
constructive::construct_clip(var_types)

