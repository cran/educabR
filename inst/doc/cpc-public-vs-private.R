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
# library(dplyr)
# library(tidyr)
# library(ggplot2)

## ----download-----------------------------------------------------------------
# cpc <- get_cpc(year = 2023)
# glimpse(cpc)

## ----classify-----------------------------------------------------------------
# cpc_classified <-
#   cpc |>
#   mutate(
#     sector = case_when(
#       categoria_administrativa %in% c(
#         "Publica Federal", "Publica Estadual", "Publica Municipal",
#         "P\u00fablica Federal", "P\u00fablica Estadual", "P\u00fablica Municipal"
#       ) ~ "Public",
#       .default = "Private"
#     )
#   )

## ----distribution-------------------------------------------------------------
# cpc_classified |>
#   filter(!is.na(cpc_faixa)) |>
#   count(sector, cpc_faixa) |>
#   mutate(pct = n / sum(n) * 100, .by = sector) |>
#   ggplot(aes(x = factor(cpc_faixa), y = pct, fill = sector)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "CPC Score Distribution: Public vs Private (2023)",
#     x     = "CPC Score (1-5)",
#     y     = "Percentage of Courses (%)",
#     fill  = "Sector"
#   ) +
#   theme_minimal()

## ----by-area------------------------------------------------------------------
# cpc_classified |>
#   filter(!is.na(cpc_continuo), !is.na(area_de_avaliacao)) |>
#   summarise(
#     mean_cpc = mean(cpc_continuo, na.rm = TRUE),
#     n = n(),
#     .by = c(sector, area_de_avaliacao)
#   ) |>
#   filter(n >= 10) |>
#   pivot_wider(
#     names_from  = sector,
#     values_from = c(mean_cpc, n)
#   ) |>
#   mutate(gap = mean_cpc_Public - mean_cpc_Private) |>
#   slice_max(abs(gap), n = 15) |>
#   ggplot(aes(x = reorder(area_de_avaliacao, gap), y = gap)) +
#   geom_col(aes(fill = gap > 0)) +
#   coord_flip() +
#   scale_fill_manual(
#     values = c("TRUE" = "#2a9d8f", "FALSE" = "#e76f51"),
#     labels = c("TRUE" = "Public higher", "FALSE" = "Private higher")
#   ) +
#   labs(
#     title = "CPC Gap: Public minus Private, by Knowledge Area (2023)",
#     x     = NULL,
#     y     = "CPC difference (public - private)",
#     fill  = NULL
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")

## ----igc-comparison-----------------------------------------------------------
# igc <- get_igc(year = 2023)
# 
# igc |>
#   mutate(
#     sector = case_when(
#       categoria_administrativa %in% c(
#         "Publica Federal", "Publica Estadual", "Publica Municipal",
#         "P\u00fablica Federal", "P\u00fablica Estadual", "P\u00fablica Municipal"
#       ) ~ "Public",
#       .default = "Private"
#     )
#   ) |>
#   filter(!is.na(igc_continuo)) |>
#   ggplot(aes(x = sector, y = igc_continuo, fill = sector)) +
#   geom_boxplot(alpha = 0.7) +
#   scale_fill_manual(values = c("Public" = "#2a9d8f", "Private" = "#e76f51")) +
#   labs(
#     title = "IGC Distribution: Public vs Private (2023)",
#     x     = NULL,
#     y     = "IGC (Continuous)"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")

