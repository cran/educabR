## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE,
  message  = FALSE,
  warning  = FALSE
)

## ----ideb---------------------------------------------------------------------
# library(educabR)
# 
# ideb <- get_ideb(
#   level  = "municipio",
#   stage  = "anos_finais",
#   metric = "indicador",
#   year   = c(2021, 2023)
# )

## ----shape--------------------------------------------------------------------
# head(ideb, 3)
# #>   uf_sigla municipio_codigo        municipio_nome      rede  ano indicador valor
# #> 1       RO          1100015 Alta Floresta D'Oeste  Estadual 2021      IDEB   4.8
# #> 2       RO          1100015 Alta Floresta D'Oeste Municipal 2021      IDEB   4.7
# #> 3       RO          1100015 Alta Floresta D'Oeste   Pública 2021      IDEB   4.8

