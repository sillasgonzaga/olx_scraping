library(tidyverse)
library(DBI)

con <- DBI::dbConnect(RSQLite::SQLite(), "imoveis-olx.sqlite")

df <- tbl(con, 'anuncios') %>% 
  collect()


df %>% 
  distinct(dta_public)