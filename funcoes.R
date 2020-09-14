parsear_pagina_lista_anuncios <- function(url){
  
  mycurl <- url %>% 
    curl(handle = curl::new_handle("useragent" = "Mozilla/5.0")) %>% 
    read_html()
  
  links <- mycurl %>%
    html_nodes(xpath = "//li") %>%
    html_nodes("a") %>%
    html_attr("href")
  
  links_anuncios <- links %>% 
    str_subset("https://sp.olx.com.br/sao-paulo-e-regiao/imoveis/") %>% 
    str_subset("[0-9]{9}")
  
  links_anuncios
}



extrair_detalhes_anuncio <- function(node){
  
  if (length(node) != 2){
    stop("Essa função não é vetorizada. Use purrr::map()")
  }
  
  nome_detalhe <- node %>% html_nodes("dt") %>% html_text()
  
  valor_detalhe_dd <- node %>% html_nodes("dd") %>% html_text()
  valor_detalhe_a <- node %>% html_nodes("a") %>% html_text()
  
  valor_detalhe <- ifelse(length(valor_detalhe_dd) == 0,
                          valor_detalhe_a,
                          valor_detalhe_dd)
  
  out <- valor_detalhe
  names(out) <- nome_detalhe
  out <- tibble::enframe(out, name = 'coluna', value = 'valor')
  out <- out %>% mutate(coluna = janitor::make_clean_names(coluna))
  return(out)
} 



parsear_anuncio_imovel <- function(url){
  # converter para objeto xml
  mycurl <- url %>% 
    curl(handle = curl::new_handle("useragent" = "Mozilla/5.0")) %>% 
    read_html()
  # extrair preco
  preco_char <- mycurl %>% html_nodes(".irrBEM") %>% html_text() 
  preco_char <- ifelse(length(preco_char) == 0, NA_character_, preco_char)
  # data de publicaco
  dta_publi <-  mycurl %>% html_nodes('.drrPdv') %>% html_text()
  dta_publi <- dta_publi[1]
  # extrair detalhes
  nodes_detalhes <- mycurl %>% html_nodes(".jyICCp")
  
  tbl_detalhes <- nodes_detalhes %>% map_dfr(extrair_detalhes_anuncio)
  # adicionar linha com o preço
  tbl_out <- tbl_detalhes %>% 
    add_row(coluna = "preco", valor = preco_char) %>% 
    mutate(url_anuncio = url,
           #       preco = preco_char,
           dta_public = dta_publi)
  return(tbl_out)
  
}

parsear_data_publicacao <- function(x){
  x <- str_remove(x, 'Publicado em ')
  x <- str_remove(x, ' às ')
  x <- str_remove(x, '/')
  ISOdatetime(
    day = substr(x, 1, 2),
    month = substr(x, 3, 4),
    year = 2020,
    hour = substr(x, 5, 6),
    min = substr(x, 8, 9),
    sec = 0
    
  )
}


