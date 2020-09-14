library(tidyverse)
library(rvest)
library(curl)
library(httr)
library(RSQLite)
source('funcoes.R')

pgs <- 1:100
vec_url_lista_busca <- str_glue("https://sp.olx.com.br/sao-paulo-e-regiao/imoveis?o={pgs}&sf=1")

vec_url_anuncios <- vec_url_lista_busca %>% 
  map(parsear_pagina_lista_anuncios) %>% 
  flatten_chr()


db <- dbConnect(RSQLite::SQLite(), 'imoveis-olx.sqlite')

tstamp <- Sys.time()

#vec_url_anuncios <- read_rds('vec_url_lista_anuncios.rds') %>% as.character() %>% rev()
vec <- vec_url_anuncios

print(str_c('Rodando ', length(vec), ' iterações'))
cat('\n')

for (i in 1:length(vec)){
  if (i %% 50 == 0) print(i)
  # parsear
  out_loop <- try(
    parsear_anuncio_imovel(vec[i]),
    silent = TRUE)
  
  if(inherits(out_loop, 'try-error')) {
    next
  }
  
  # criar coluna de timestap
  out_loop <- out_loop %>% mutate(timestamp_scraping = tstamp)
  # salvar no banco
  dbWriteTable(db, 
               name = 'anuncios',
               value = out_loop,
               overwrite = FALSE,
               append = TRUE
  )
  
}

dbDisconnect(db)
