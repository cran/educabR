## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
# # Install from GitHub (development version)
# # install.packages("remotes")
# remotes::install_github("SidneyBissoli/educabR")

## ----setup--------------------------------------------------------------------
# library(educabR)
# library(dplyr)
# library(ggplot2)

## ----cache--------------------------------------------------------------------
# # Check current cache directory
# get_cache_dir()
# 
# # Set a permanent directory
# set_cache_dir("~/educabR_data")
# 
# # List cached files
# list_cache()
# 
# # Clear cache (when needed)
# clear_cache()

## ----ideb-available-----------------------------------------------------------
# # See available combinations
# list_ideb_available()
# #> # A tibble: 24 x 3
# #>     year level     stage
# #>    <int> <chr>     <chr>
# #>  1  2017 escola    anos_iniciais
# #>  2  2017 escola    anos_finais
# #>  3  2017 escola    ensino_medio
# #>  4  2017 municipio anos_iniciais
# #>  ...

## ----ideb-download------------------------------------------------------------
# # IDEB by school - Early elementary (1st-5th grade)
# ideb_schools <- get_ideb(
#   year  = 2021,
#   level = "escola",
#   stage = "anos_iniciais"
# )
# 
# # IDEB by municipality - High school
# ideb_municipalities <- get_ideb(
#   year  = 2023,
#   level = "municipio",
#   stage = "ensino_medio"
# )
# 
# # Filter by state (faster)
# ideb_sp <- get_ideb(
#   year  = 2021,
#   level = "escola",
#   stage = "anos_iniciais",
#   uf    = "SP"
# )

## ----ideb-structure-----------------------------------------------------------
# # View structure
# glimpse(ideb_schools)
# #> Rows: 63,529
# #> Columns: 17
# #> $ sg_uf                   <chr> "RO", "RO", "RO", ...
# #> $ co_municipio            <dbl> 1100015, 1100015, ...
# #> $ no_municipio            <chr> "Alta Floresta D'Oeste", ...
# #> $ id_escola               <dbl> 11000023, 11000040, ...
# #> $ no_escola               <chr> "EEEE ABNAEL MACHADO DE LIMA", ...
# #> $ rede                    <chr> "Estadual", "Municipal", ...
# #> $ vl_aprovacao_2021_si_4  <dbl> 93.3, 98.5, 100, ...
# #> $ vl_indicador_rend_2021  <dbl> 0.92, 0.98, 1.00, ...
# #> $ vl_nota_matematica_2021 <dbl> 5.2, 5.8, 6.1, ...
# #> $ vl_nota_portugues_2021  <dbl> 5.4, 5.9, 6.0, ...
# #> $ vl_nota_media_2021      <dbl> 5.3, 5.85, 6.05, ...
# #> $ vl_observado_2021       <dbl> 4.9, 5.7, 6.1, ...

## ----ideb-analysis------------------------------------------------------------
# # Calculate average IDEB by state
# ideb_by_state <-
#   ideb_schools |>
#   filter(!is.na(vl_observado_2021)) |>
#   group_by(sg_uf) |>
#   summarise(
#     n_schools   = n(),
#     mean_ideb   = mean(vl_observado_2021, na.rm = TRUE),
#     median_ideb = median(vl_observado_2021, na.rm = TRUE)
#   ) |>
#   arrange(desc(mean_ideb))
# 
# # Plot
# ggplot(ideb_by_state, aes(x = reorder(sg_uf, mean_ideb), y = mean_ideb)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "Average IDEB by State - Early Elementary (2021)",
#     x     = "State",
#     y     = "Average IDEB"
#     ) +
#   theme_minimal()

## ----ideb-series--------------------------------------------------------------
# # Download historical series
# ideb_history <- get_ideb_series(
#   years = c(2017, 2019, 2021, 2023),
#   level = "municipio",
#   stage = "anos_iniciais"
# )
# 
# # National trend
# trend <-
#   ideb_history |>
#   group_by(ano_ideb) |>
#   summarise(mean_ideb = mean(vl_observado, na.rm = TRUE))
# 
# ggplot(trend, aes(x = ano_ideb, y = mean_ideb)) +
#   geom_line(color = "darkgreen", size = 1.2) +
#   geom_point(color = "darkgreen", size = 3) +
#   labs(
#     title = "IDEB Trend - Early Elementary",
#     x     = "Year",
#     y     = "National Average IDEB"
#   ) +
#   theme_minimal()

## ----enem-download------------------------------------------------------------
# # WARNING: Large files (1-3 GB)!
# # Use n_max for initial exploration
# 
# # Sample for exploration
# enem_sample <- get_enem(2023, n_max = 10000)
# 
# # Full data (takes a while!)
# # enem_full <- get_enem(2023)

## ----enem-structure-----------------------------------------------------------
# glimpse(enem_sample)
# #> Rows: 10,000
# #> Columns: 76
# #> $ nu_inscricao      <dbl> 230001234567, ...
# #> $ nu_ano            <dbl> 2023, 2023, ...
# #> $ tp_faixa_etaria   <dbl> 3, 4, 2, ...
# #> $ tp_sexo           <chr> "F", "M", "F", ...
# #> $ tp_cor_raca       <dbl> 1, 3, 2, ...
# #> $ nu_nota_cn        <dbl> 512.3, 489.1, ...
# #> $ nu_nota_ch        <dbl> 598.2, 567.4, ...
# #> $ nu_nota_lc        <dbl> 534.8, 502.1, ...
# #> $ nu_nota_mt        <dbl> 478.9, 521.3, ...
# #> $ nu_nota_redacao   <dbl> 720, 640, ...

## ----enem-analysis------------------------------------------------------------
# # Summary statistics for scores
# enem_summary(enem_sample)
# 
# # Summary by sex
# enem_summary(enem_sample, by = "tp_sexo")
# 
# # Average scores by race/ethnicity
# scores_by_race <-
#   enem_sample |>
#   filter(!is.na(nu_nota_mt)) |>
#   mutate(
#     race = case_when(
#       tp_cor_raca == 0 ~ "Not declared",
#       tp_cor_raca == 1 ~ "White",
#       tp_cor_raca == 2 ~ "Black",
#       tp_cor_raca == 3 ~ "Mixed race",
#       tp_cor_raca == 4 ~ "Asian",
#       tp_cor_raca == 5 ~ "Indigenous"
#     )
#   ) |>
#   group_by(race) |>
#   summarise(
#     n          = n(),
#     mean_math  = mean(nu_nota_mt, na.rm = TRUE),
#     mean_essay = mean(nu_nota_redacao, na.rm = TRUE)
#   )

## ----enem-itens---------------------------------------------------------------
# # Data about exam questions
# items <- get_enem_itens(2023)
# 
# glimpse(items)
# #> $ co_item        <dbl> 1, 2, 3, ...
# #> $ co_prova       <dbl> 1001, 1001, ...
# #> $ tp_lingua      <dbl> 0, 0, ...
# #> $ sg_area        <chr> "CN", "CN", ...
# #> $ co_habilidade  <dbl> 1, 2, ...

## ----censo-download-----------------------------------------------------------
# # School data
# schools_2023 <- get_censo_escolar(2023)
# 
# # Filter by state (faster)
# schools_sp <- get_censo_escolar(2023, uf = "SP")
# 
# # Sample for exploration
# schools_sample <- get_censo_escolar(2023, n_max = 1000)

## ----censo-structure----------------------------------------------------------
# # The census contains over 400 variables per school!
# glimpse(schools_2023)
# #> Rows: 217,625
# #> Columns: 408
# #> $ nu_ano_censo     <dbl> 2023, 2023, ...
# #> $ sg_uf            <chr> "RO", "RO", ...
# #> $ co_uf            <dbl> 11, 11, ...
# #> $ no_municipio     <chr> "Porto Velho", ...
# #> $ co_municipio     <dbl> 1100205, ...
# #> $ no_entidade      <chr> "EEEE ABNAEL MACHADO DE LIMA", ...
# #> $ co_entidade      <dbl> 11000023, ...
# #> $ tp_dependencia   <dbl> 2, 3, 4, ...
# #> $ tp_localizacao   <dbl> 1, 1, 1, ...

## ----censo-analysis-----------------------------------------------------------
# # Count by administrative type
# schools_by_type <-
#   schools_2023 |>
#   mutate(
#     admin_type = case_when(
#       tp_dependencia == 1 ~ "Federal",
#       tp_dependencia == 2 ~ "State",
#       tp_dependencia == 3 ~ "Municipal",
#       tp_dependencia == 4 ~ "Private"
#     )
#   ) |>
#   count(admin_type) |>
#   mutate(pct = n / sum(n) * 100)
# 
# ggplot(schools_by_type, aes(x = reorder(admin_type, n), y = n, fill = admin_type)) +
#   geom_col() +
#   geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1) +
#   coord_flip() +
#   scale_fill_brewer(palette = "Set2") +
#   labs(
#     title = "Number of Schools by Administrative Type (2023)",
#     x = NULL,
#     y = "Number of Schools"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none")

## ----censo-infrastructure-----------------------------------------------------
# # Check infrastructure availability in public schools
# infra <-
#   schools_2023 |>
#   filter(tp_dependencia %in% c(2, 3)) |>  # Public schools only
#   summarise(
#     pct_internet      = mean(in_internet == 1, na.rm = TRUE) * 100,
#     pct_library       = mean(in_biblioteca == 1, na.rm = TRUE) * 100,
#     pct_computer_lab  = mean(in_laboratorio_informatica == 1, na.rm = TRUE) * 100,
#     pct_sports_court  = mean(in_quadra_esportes == 1, na.rm = TRUE) * 100,
#     pct_accessibility = mean(in_acessibilidade == 1, na.rm = TRUE) * 100
#   )
# 
# print(infra)
# #> # A tibble: 1 x 5
# #>   pct_internet pct_library pct_computer_lab pct_sports_court pct_accessibility
# #>          <dbl>       <dbl>            <dbl>            <dbl>             <dbl>
# #> 1         78.3        42.1             35.2             48.7              32.1

## ----best-practices-cache-----------------------------------------------------
# # At the start of each project
# set_cache_dir("~/educabR_data")

## ----best-practices-sample----------------------------------------------------
# # Explore with a small sample first
# test_data <- get_enem(2023, n_max = 1000)
# 
# # Then download the full data
# # full_data <- get_enem(2023)

## ----best-practices-filter----------------------------------------------------
# # Faster than downloading everything and filtering afterwards
# data_sp <- get_ideb(2021, level = "escola", stage = "anos_iniciais", uf = "SP")

## ----best-practices-memory----------------------------------------------------
# # Large files can consume a lot of RAM
# # Use n_max or filter data after loading
# data <- get_censo_escolar(2023)
# data_filtered <-
#   data |>
#   select(co_entidade, no_entidade, sg_uf, tp_dependencia)
# rm(data)  # Free memory
# gc()

