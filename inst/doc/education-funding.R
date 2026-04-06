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

## ----distribution-download----------------------------------------------------
# # All distribution data for 2023
# dist_2023 <- get_fundeb_distribution(year = 2023)
# 
# # Filter by state
# dist_sp <- get_fundeb_distribution(year = 2023, uf = "SP")
# 
# # Filter by funding source
# dist_fpm <- get_fundeb_distribution(year = 2023, source = "FPM")
# 
# # Filter by destination (states or municipalities)
# dist_estados <- get_fundeb_distribution(
#   year        = 2023,
#   destination = "estados"
# )

## ----distribution-structure---------------------------------------------------
# dist <- get_fundeb_distribution(2023, uf = "SP")
# glimpse(dist)

## ----distribution-analysis----------------------------------------------------
# dist <- get_fundeb_distribution(2023, uf = "SP")
# 
# # Total monthly transfers by funding source
# monthly <-
#   dist |>
#   mutate(mes = as.integer(format(as.Date(mes_ano), "%m"))) |>
#   group_by(origem, mes) |>
#   summarise(total = sum(valor, na.rm = TRUE), .groups = "drop")
# 
# ggplot(monthly, aes(x = factor(mes), y = total / 1e6, fill = origem)) +
#   geom_col() +
#   labs(
#     title = "FUNDEB Transfers to Sao Paulo by source (2023)",
#     x     = "Month",
#     y     = "Total (millions R$)",
#     fill  = "Source"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",")) +
#   guides(fill = guide_legend(nrow = 1))

## ----distribution-states------------------------------------------------------
# dist <- get_fundeb_distribution(2023)
# 
# # Total annual transfers by state
# by_state <-
#   dist |>
#   group_by(uf) |>
#   summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
#   arrange(desc(total)) |>
#   head(10)
# 
# ggplot(by_state, aes(x = reorder(uf, total), y = total / 1e9)) +
#   geom_col(fill = "darkgreen") +
#   coord_flip() +
#   labs(
#     title = "Top 10 States by FUNDEB Transfers (2023)",
#     x     = NULL,
#     y     = "Total (billions R$)"
#   ) +
#   theme_minimal()

## ----enrollment-download------------------------------------------------------
# # All enrollment data for 2018
# mat_2018 <- get_fundeb_enrollment(year = 2018)
# 
# # Filter by state (applied at the API level for efficiency)
# mat_sp <- get_fundeb_enrollment(year = 2018, uf = "SP")
# 
# # Limited rows for exploration
# mat_sample <- get_fundeb_enrollment(year = 2018, n_max = 5000)

## ----enrollment-structure-----------------------------------------------------
# mat <- get_fundeb_enrollment(2018, uf = "RJ")
# glimpse(mat)

## ----enrollment-analysis------------------------------------------------------
# mat <- get_fundeb_enrollment(2018, uf = "SP")
# 
# # Total enrollment by education type
# by_type <-
#   mat |>
#   group_by(descricao_tipo_educacao) |>
#   summarise(total = sum(qtd_matricula, na.rm = TRUE), .groups = "drop") |>
#   arrange(desc(total))
# 
# ggplot(by_type, aes(x = reorder(descricao_tipo_educacao, total), y = total / 1e3)) +
#   geom_col(fill = "coral", width = .5) +
#   coord_flip() +
#   labs(
#     title = "FUNDEB Enrollment by Education Type - SP (2018)",
#     x     = NULL,
#     y     = "Enrollments (thousands)"
#   ) +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))

## ----enrollment-location------------------------------------------------------
# mat <- get_fundeb_enrollment(2018)
# 
# # Compare urban vs rural
# by_location <-
#   mat |>
#   mutate(
#     location = case_when(
#       descricao_tipo_localizacao == "URBANA"     ~ "Urban",
#       descricao_tipo_localizacao == "RURAL"      ~ "Rural",
#       descricao_tipo_localizacao == "QUILOMBOLA" ~ "Quilombola",
#       grepl("GENA", descricao_tipo_localizacao)  ~ "Indigenous",
#       .default = descricao_tipo_localizacao
#     )
#   ) |>
#   group_by(uf, location) |>
#   summarise(total = sum(qtd_matricula, na.rm = TRUE), .groups = "drop")
# 
# ggplot(by_location, aes(x = uf, y = total / 1e3, fill = location)) +
#   geom_col(position = "dodge") +
#   labs(
#     title = "FUNDEB Enrollment: Urban vs Rural by State (2018)",
#     x     = "State",
#     y     = "Enrollments (thousands)",
#     fill  = "Location"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))

## ----combined-----------------------------------------------------------------
# # Total transfers by state
# dist <- get_fundeb_distribution(2018)
# transfers <-
#   dist |>
#   group_by(uf) |>
#   summarise(total_transfer = sum(valor, na.rm = TRUE), .groups = "drop")
# 
# # Total enrollment by state
# mat <- get_fundeb_enrollment(2018)
# enrollment <-
#   mat |>
#   group_by(uf) |>
#   summarise(total_students = sum(qtd_matricula, na.rm = TRUE), .groups = "drop")
# 
# # Per-student funding
# funding <-
#   inner_join(transfers, enrollment, by = "uf") |>
#   mutate(per_student = total_transfer / total_students) |>
#   arrange(desc(per_student))
# 
# ggplot(funding, aes(x = reorder(uf, per_student), y = per_student)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "FUNDEB Per-Student Funding by State (2018)",
#     x     = NULL,
#     y     = "R$ per Student"
#   ) +
#   theme_minimal() +
#   scale_y_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ","))

