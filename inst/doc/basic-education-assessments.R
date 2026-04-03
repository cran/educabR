## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----setup--------------------------------------------------------------------
# library(educabR)
# library(dplyr)
# library(ggplot2)

## ----saeb-download------------------------------------------------------------
# # Student performance data
# saeb_students <- get_saeb(year = 2023, type = "aluno")
# 
# # School questionnaire
# saeb_schools <- get_saeb(year = 2023, type = "escola")
# 
# # Use n_max for exploration
# saeb_sample <- get_saeb(year = 2023, type = "aluno", n_max = 5000)

## ----saeb-years---------------------------------------------------------------
# # 2021 data is split by education level
# saeb_fund <- get_saeb(
#   year  = 2021,
#   type  = "aluno",
#   level = "fundamental_medio"
# )
# 
# saeb_infantil <- get_saeb(
#   year  = 2021,
#   type  = "aluno",
#   level = "educacao_infantil"
# )

## ----saeb-analysis------------------------------------------------------------
# # Explore student scores
# saeb_sample <- get_saeb(2023, type = "aluno", n_max = 10000)
# 
# # Score distribution by subject
# saeb_sample |>
#   filter(!is.na(proficiencia_mt)) |>
#   ggplot(aes(x = proficiencia_mt)) +
#   geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
#   labs(
#     title = "SAEB 2023 - Mathematics Proficiency Distribution",
#     x     = "Mathematics Score",
#     y     = "Count"
#     ) +
#   theme_minimal()

## ----encceja-download---------------------------------------------------------
# # Download ENCCEJA microdata
# encceja_2023 <- get_encceja(year = 2023)
# 
# # Sample for exploration
# encceja_sample <- get_encceja(year = 2023, n_max = 5000)

## ----encceja-structure--------------------------------------------------------
# # Explore the data structure
# glimpse(encceja_sample)

## ----encceja-analysis---------------------------------------------------------
# encceja_2023 <- get_encceja(2023, n_max = 50000)
# 
# # Count participants by state
# participants_by_state <-
#   encceja_2023 |>
#   count(sg_uf_prova, sort = TRUE) |>
#   head(10)
# 
# ggplot(participants_by_state, aes(
#   x = reorder(sg_uf_prova, n),
#   y = n
# )) +
#   geom_col(fill = "darkorange") +
#   coord_flip() +
#   labs(
#     title = "ENCCEJA 2023 - Top 10 States by Participation",
#     x     = "State",
#     y     = "Number of Participants"
#     ) +
#   theme_minimal()

## ----enem-escola-download-----------------------------------------------------
# # Download all ENEM by School data (2005-2015)
# enem_escola <- get_enem_escola()
# 
# # Sample for exploration
# enem_escola_sample <- get_enem_escola(n_max = 5000)

## ----enem-escola-structure----------------------------------------------------
# glimpse(enem_escola_sample)

## ----enem-escola-analysis-----------------------------------------------------
# enem_escola <- get_enem_escola()
# 
# # Average scores over time (public vs private)
# trend <-
#   enem_escola |>
#   filter(!is.na(nu_media_tot)) |>
#   group_by(nu_ano, tp_dependencia_adm_escola) |>
#   summarise(
#     mean_score = mean(nu_media_tot, na.rm = TRUE),
#     .groups    = "drop"
#   ) |>
#   mutate(
#     admin_type = case_when(
#       tp_dependencia_adm_escola == 1 ~ "Federal",
#       tp_dependencia_adm_escola == 2 ~ "State",
#       tp_dependencia_adm_escola == 3 ~ "Municipal",
#       tp_dependencia_adm_escola == 4 ~ "Private"
#     )
#   )
# 
# ggplot(trend, aes(x = nu_ano, y = mean_score, color = admin_type)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   labs(
#     title = "ENEM Average Score by School Type (2005-2015)",
#     x     = "Year",
#     y     = "Average Total Score",
#     color = "School Type"
#     ) +
#   theme_minimal()

