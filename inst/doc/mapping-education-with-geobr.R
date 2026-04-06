## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE,
  message  = FALSE,
  warning  = FALSE
)
suppressPackageStartupMessages(library(systemfonts))
suppressPackageStartupMessages(library(textshaping))

## ----setup--------------------------------------------------------------------
# library(educabR)
# library(geobr)
# library(dplyr)
# library(ggplot2)

## ----state-map----------------------------------------------------------------
# ideb_uf <-
#   get_ideb(level = "estado", stage = "anos_iniciais", metric = "indicador", year = 2023) |>
#   filter(rede == "Total", indicador == "IDEB")
# 
# states <- read_state(year = 2020, showProgress = FALSE)
# 
# states |>
#   left_join(ideb_uf, by = c("abbrev_state" = "uf_sigla")) |>
#   ggplot() +
#   geom_sf(aes(fill = valor), color = "white", linewidth = .2) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "IDEB") +
#   labs(title = "IDEB 2023 — Early elementary by state") +
#   theme_void()

## ----muni-download------------------------------------------------------------
# ideb_muni <- get_ideb(
#   level  = "municipio",
#   stage  = "anos_iniciais",
#   metric = "indicador",
#   year   = 2023
# )
# 
# # Keep only public schools and the IDEB indicator
# ideb_muni <-
#   ideb_muni |>
#   filter(grepl("blica", rede), indicador == "IDEB")
# 
# municipalities <- read_municipality(year = 2020, showProgress = FALSE)

## ----muni-map-----------------------------------------------------------------
# municipalities |>
#   mutate(code_muni = as.character(code_muni)) |>
#   left_join(ideb_muni, by = c("code_muni" = "municipio_codigo")) |>
#   ggplot() +
#   geom_sf(aes(fill = valor), color = NA) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "IDEB") +
#   labs(title = "IDEB 2023 — Early elementary by municipality (public schools)") +
#   theme_void()

## ----state-zoom---------------------------------------------------------------
# ideb_mg <-
#   ideb_muni |>
#   filter(uf_sigla == "MG")
# 
# munis_mg <- read_municipality(code_muni = "MG", year = 2020, showProgress = FALSE)
# 
# munis_mg |>
#   mutate(code_muni = as.character(code_muni)) |>
#   left_join(ideb_mg, by = c("code_muni" = "municipio_codigo")) |>
#   ggplot() +
#   geom_sf(aes(fill = valor), color = "grey90", linewidth = .1) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "IDEB") +
#   labs(title = "IDEB 2023 — Early elementary in Minas Gerais") +
#   theme_void()

## ----temporal-comparison------------------------------------------------------
# ideb_time <-
#   get_ideb(
#     level  = "estado",
#     stage  = "anos_iniciais",
#     metric = "indicador",
#     year   = c(2017, 2023)
#   ) |>
#   filter(rede == "Total", indicador == "IDEB")
# 
# states |>
#   left_join(ideb_time, by = c("abbrev_state" = "uf_sigla")) |>
#   ggplot() +
#   geom_sf(aes(fill = valor), color = "white", linewidth = .2) +
#   scale_fill_distiller(palette = "YlGn", direction = 1, name = "IDEB") +
#   facet_wrap(~ano, strip.position = "bottom") +
#   labs(title = "IDEB evolution — Early elementary (2017 vs 2023)") +
#   theme_void() +
#   theme(
#     legend.position = "bottom",
#     plot.title = element_text(hjust = 0.5),
#     strip.text = element_text(size = 11, margin = margin(t = 5))
#   )

