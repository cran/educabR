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
# library(ggplot2)

## ----censo-superior-download--------------------------------------------------
# # Institution data
# ies_2023 <- get_censo_superior(year = 2023, type = "ies")
# 
# # Course data filtered by state
# cursos_sp <- get_censo_superior(year = 2023, type = "cursos", uf = "SP")
# 
# # Faculty data with limited rows
# docentes_sample <- get_censo_superior(
#   year  = 2023,
#   type  = "docentes",
#   n_max = 10000
# )

## ----censo-superior-files-----------------------------------------------------
# # See what files are inside the downloaded ZIP
# list_censo_superior_files(2023)

## ----censo-superior-analysis--------------------------------------------------
# ies <- get_censo_superior(2023, type = "ies")
# 
# ies_summary <-
#   ies |>
#   mutate(
#     admin_type = case_when(
#       tp_categoria_administrativa == 1 ~ "Public Federal",
#       tp_categoria_administrativa == 2 ~ "Public State",
#       tp_categoria_administrativa == 3 ~ "Public Municipal",
#       tp_categoria_administrativa == 4 ~ "Private For-Profit",
#       tp_categoria_administrativa == 5 ~ "Private Non-Profit",
#       TRUE ~ "Other"
#     )
#   ) |>
#   count(admin_type, sort = TRUE)
# 
# ggplot(ies_summary, aes(x = reorder(admin_type, n), y = n)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "Higher Education Institutions by Type (2023)",
#     x     = NULL,
#     y     = "Number of Institutions"
#   ) +
#   theme_minimal() +
#   scale_y_continuous(label = scales::number_format(big.mark = ".", decimal.mark = ","))

## ----enade-download-----------------------------------------------------------
# # Download ENADE microdata
# enade_2023 <- get_enade(year = 2023)
# 
# # Sample for exploration
# enade_sample <- get_enade(year = 2023, n_max = 5000)

## ----enade-structure----------------------------------------------------------
# enade <- get_enade(2023, n_max = 5000)
# glimpse(enade)

## ----idd-download-------------------------------------------------------------
# # Download IDD data
# idd_2023 <- get_idd(year = 2023)
# 
# # Sample for exploration
# idd_sample <- get_idd(year = 2023, n_max = 5000)

## ----idd-analysis-------------------------------------------------------------
# idd <- get_idd(2023)
# 
# glimpse(idd)

## ----cpc-download-------------------------------------------------------------
# # Download CPC data (Excel format, requires readxl)
# cpc_2023 <- get_cpc(year = 2023)
# 
# # Sample for exploration
# cpc_sample <- get_cpc(year = 2023, n_max = 5000)

## ----cpc-analysis-------------------------------------------------------------
# cpc <- get_cpc(2023)
# 
# # Distribution of CPC scores
# cpc |>
#   filter(!is.na(cpc_faixa)) |>
#   count(cpc_faixa) |>
#   ggplot(aes(x = factor(cpc_faixa), y = n)) +
#   geom_col(fill = "coral") +
#   labs(
#     title = "CPC 2023 - Course Quality Distribution",
#     x     = "CPC Score (1-5)",
#     y     = "Number of Courses"
#   ) +
#   theme_minimal() +
#   scale_y_continuous(label = scales::number_format(big.mark = ".", decimal.mark = ","))

## ----igc-download-------------------------------------------------------------
# # Download IGC data (Excel format, requires readxl)
# igc_2023 <- get_igc(year = 2023)
# 
# # Sample for exploration
# igc_sample <- get_igc(year = 2023, n_max = 1000)

## ----igc-analysis-------------------------------------------------------------
# igc <- get_igc(2023)
# 
# # Top institutions by continuous IGC
# igc |>
#   filter(!is.na(igc_continuo)) |>
#   filter(!is.na(sigla_da_ies)) |>
#   arrange(desc(igc_continuo)) |>
#   head(20) |>
#   ggplot(aes(x = reorder(sigla_da_ies, igc_continuo), y = igc_continuo)) +
#   geom_col(fill = "darkblue") +
#   coord_flip() +
#   labs(
#     title = "Top 20 Institutions by IGC (2023)",
#     x     = NULL,
#     y     = "IGC (Continuous)"
#   ) +
#   theme_minimal()

## ----capes-download-----------------------------------------------------------
# # Graduate programs
# programas_2023 <- get_capes(year = 2023, type = "programas")
# 
# # Students (large dataset!)
# discentes_sample <- get_capes(year = 2023, type = "discentes", n_max = 10000)
# 
# # Theses and dissertations catalog
# catalogo_2023 <- get_capes(year = 2023, type = "catalogo")

## ----capes-analysis-----------------------------------------------------------
# programas <- get_capes(2023, type = "programas")
# 
# # Count programs by broad knowledge area
# programas |>
#   count(nm_grande_area_conhecimento, sort = TRUE) |>
#   head(10) |>
#   ggplot(aes(
#     x = reorder(nm_grande_area_conhecimento, n),
#     y = n
#   )) +
#   geom_col(fill = "purple4") +
#   coord_flip() +
#   labs(
#     title = "Graduate Programs by Knowledge Area (2023)",
#     x     = NULL,
#     y     = "Number of Programs"
#   ) +
#   theme_minimal()

## ----combined-analysis--------------------------------------------------------
# # Load CPC and IGC for the same year
# cpc <- get_cpc(2023)
# igc <- get_igc(2023)
# 
# # Compare institution-level quality
# # IGC gives the overall institution score
# # CPC gives individual course scores within each institution
# igc_summary <-
#   igc |>
#   filter(!is.na(igc_faixa)) |>
#   select(codigo_da_ies, sigla_da_ies, igc_continuo, igc_faixa)
# 
# cpc_summary <-
#   cpc |>
#   filter(!is.na(cpc_continuo)) |>
#   group_by(codigo_da_ies) |>
#   summarise(
#     n_courses = n(),
#     mean_cpc  = mean(cpc_continuo, na.rm = TRUE),
#     .groups   = "drop"
#   )
# 
# combined <- inner_join(igc_summary, cpc_summary, by = "codigo_da_ies")
# 
# ggplot(combined, aes(x = mean_cpc, y = igc_continuo, size = n_courses)) +
#   geom_point(alpha = 0.4, color = "steelblue") +
#   labs(
#     title = "IGC vs Average CPC by Institution (2023)",
#     x     = "Average CPC (Continuous)",
#     y     = "IGC (Continuous)",
#     size  = "Courses Evaluated"
#   ) +
#   theme_minimal()

