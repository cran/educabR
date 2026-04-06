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

## ----install------------------------------------------------------------------
# # Install from GitHub (development version)
# # install.packages("remotes")
# remotes::install_github("SidneyBissoli/educabR")

## ----setup--------------------------------------------------------------------
# library(educabR)
# library(dplyr)
# library(ggplot2)
# library(kableExtra)

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

## ----ideb-available-table, echo = FALSE, eval = TRUE--------------------------
suppressPackageStartupMessages(library(educabR))
suppressPackageStartupMessages(library(kableExtra))
df <- list_ideb_available()
df[1:10, ] |>
  kbl(
    col.names = c("Level", "Stage", "Metric"),
    align = "lll"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) |>
  column_spec(1:3, width = "12em") |>
  footnote(
    general = paste0("Showing 10 of ", nrow(df), " available combinations"),
    general_title = "",
    footnote_as_chunk = TRUE
  )

## ----ideb-download------------------------------------------------------------
# # IDEB by school - Early elementary (1st-5th grade)
# ideb_schools <- get_ideb(
#   level  = "escola",
#   stage  = "anos_iniciais",
#   metric = "indicador",
#   year   = 2021
# )
# 
# # IDEB by municipality - High school (1st-4th grade)
# ideb_municipalities <- get_ideb(
#   level  = "municipio",
#   stage  = "ensino_medio",
#   metric = "indicador",
#   year   = 2023
# )
# 
# # Filter by state after downloading
# ideb_sp <-
#   ideb_schools |>
#   dplyr::filter(uf_sigla == "SP")

## ----ideb-structure-----------------------------------------------------------
# # View structure
# glimpse(ideb_schools)
# #> Rows: 194,715
# #> Columns: 9
# #> $ uf_sigla         <chr> "RO", "RO", "RO", ...
# #> $ municipio_codigo <chr> "1100015", "1100015", ...
# #> $ municipio_nome   <chr> "Alta Floresta D'Oeste", ...
# #> $ escola_id        <chr> "11024372", "11024666", ...
# #> $ escola_nome      <chr> "EMEIEF ANA NERY", "EMEIEF BOA ESPERANCA", ...
# #> $ rede             <chr> "Municipal", "Municipal", ...
# #> $ ano              <int> 2021, 2021, ...
# #> $ indicador        <chr> "IDEB", "IDEB", ...
# #> $ valor            <dbl> NA, NA, 5.9, 4.1, ...

## ----ideb-analysis------------------------------------------------------------
# # Calculate average IDEB by state (observed indicator only)
# ideb_by_state <-
#   ideb_schools |>
#   filter(indicador == "IDEB", !is.na(valor)) |>
#   group_by(uf_sigla) |>
#   summarise(
#     n_schools   = n(),
#     mean_ideb   = mean(valor, na.rm = TRUE),
#     median_ideb = median(valor, na.rm = TRUE)
#   ) |>
#   arrange(desc(mean_ideb))
# 
# # Plot
# ggplot(ideb_by_state, aes(x = reorder(uf_sigla, mean_ideb), y = mean_ideb)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(
#     title = "Average IDEB by State - Early Elementary (2021)",
#     x     = "State",
#     y     = "Average IDEB"
#   ) +
#   theme_minimal()

## ----ideb-series--------------------------------------------------------------
# # Download historical series (get_ideb() already returns long format)
# ideb_history <- get_ideb(
#   level  = "municipio",
#   stage  = "anos_iniciais",
#   metric = "indicador",
#   year   = c(2017, 2019, 2021, 2023)
# )
# 
# # National trend
# trend <-
#   ideb_history |>
#   filter(indicador == "IDEB") |>
#   group_by(ano) |>
#   summarise(mean_ideb = mean(valor, na.rm = TRUE))
# 
# ggplot(trend, aes(x = factor(ano), y = mean_ideb)) +
#   geom_col(fill = "darkgreen", width = 0.6) +
#   geom_text(
#     aes(label = sprintf("%.2f", mean_ideb)),
#     vjust = -0.5, size = 3.5
#   ) +
#   labs(
#     title = "IDEB Trend - Early Elementary",
#     x     = "Year",
#     y     = "National Average IDEB"
#   ) +
#   theme_minimal() +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

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
# #> $ nu_inscricao           <dbl> 210059085136, 210059527735, ...
# #> $ nu_ano                 <dbl> 2023, 2023, ...
# #> $ tp_faixa_etaria        <dbl> 14, 12, 6, ...
# #> $ tp_sexo                <chr> "M", "M", "F", ...
# #> $ tp_estado_civil        <dbl> 2, 2, 1, ...
# #> $ tp_cor_raca            <dbl> 1, 1, 1, ...
# #> $ sg_uf_prova            <chr> "DF", "DF", "RS", ...
# #> $ nu_nota_cn             <dbl> NA, NA, 502.0, ...
# #> $ nu_nota_ch             <dbl> NA, NA, 498.9, ...
# #> $ nu_nota_lc             <dbl> NA, NA, 475.6, ...
# #> $ nu_nota_mt             <dbl> NA, NA, 363.2, ...
# #> $ nu_nota_redacao        <dbl> NA, NA, 700, ...
# #> # ... 64 more variables

## ----enem-analysis------------------------------------------------------------
# # Summary statistics for scores
# enem_summary(enem_sample)

## ----enem-summary-table, echo = FALSE, eval = TRUE----------------------------
suppressPackageStartupMessages(library(kableExtra))
data.frame(
  Variable        = c("nu_nota_cn", "nu_nota_ch", "nu_nota_lc", "nu_nota_mt",
                       "nu_nota_comp1", "nu_nota_comp2", "nu_nota_comp3",
                       "nu_nota_comp4", "nu_nota_comp5", "nu_nota_redacao"),
  n               = 10000L,
  n_valid         = c(7281L, 7562L, 7562L, 7281L, 7562L, 7562L, 7562L, 7562L, 7562L, 7562L),
  Mean            = c(492, 529, 520, 520, 126, 147, 124, 136, 117, 649),
  SD              = c(79.8, 81.7, 70.4, 121, 32.1, 48.4, 41.0, 41.2, 60.0, 201),
  Min             = 0,
  Q25             = c(440, 480, 476, 426, 120, 120, 100, 120, 80, 520),
  Median          = c(489, 535, 524, 507, 120, 160, 120, 120, 120, 640),
  Q75             = c(541, 584, 568, 608, 160, 200, 160, 160, 160, 820),
  Max             = c(817, 823, 731, 945, 200, 200, 200, 200, 200, 980)
) |>
  kbl(align = "lrrrrrrrrr") |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) |>
  column_spec(1, width = "10em") |>
  footnote(
    general = "Based on a random sample of 10,000 observations",
    general_title = "",
    footnote_as_chunk = TRUE
  )

## ----enem-analysis-2----------------------------------------------------------
# # Summary by sex
# enem_summary(enem_sample, by = "tp_sexo")

## ----enem-summary-by-sex-table, echo = FALSE, eval = TRUE---------------------
suppressPackageStartupMessages(library(kableExtra))
data.frame(
  Sex      = c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M"),
  Variable = c("nu_nota_cn", "nu_nota_cn", "nu_nota_ch", "nu_nota_ch",
                "nu_nota_lc", "nu_nota_lc", "nu_nota_mt", "nu_nota_mt",
                "nu_nota_comp1", "nu_nota_comp1"),
  n        = c(7042L, 2958L, 7042L, 2958L, 7042L, 2958L, 7042L, 2958L, 7042L, 2958L),
  n_valid  = c(5130L, 2151L, 5328L, 2234L, 5328L, 2234L, 5130L, 2151L, 5328L, 2234L),
  Mean     = c(485, 507, 527, 535, 520, 522, 509, 547, 128, 121),
  SD       = c(76.5, 85.1, 78.6, 88.2, 68.2, 75.5, 116, 129, 30.8, 34.6),
  Min      = 0,
  Q25      = c(435, 455, 480, 483, 476, 478, 420, 447, 120, 100),
  Median   = c(481, 508, 532, 542, 523, 527, 494, 540, 120, 120),
  Q75      = c(532, 560, 579, 596, 566, 574, 589, 643, 160, 140),
  Max      = c(817, 804, 784, 823, 731, 729, 944, 945, 200, 200)
) |>
  kbl(align = "clrrrrrrrrr") |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) |>
  column_spec(1, width = "3em") |>
  column_spec(2, width = "10em") |>
  footnote(
    general = "Showing 10 of 20 rows",
    general_title = "",
    footnote_as_chunk = TRUE
  )

## ----enem-analysis-3----------------------------------------------------------
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

## ----enem-race-table, echo = FALSE, eval = TRUE-------------------------------
suppressPackageStartupMessages(library(kableExtra))
data.frame(
  Race       = c("Asian", "Black", "Indigenous", "Mixed race", "Not declared", "White"),
  n          = c(116L, 1066L, 30L, 3383L, 74L, 2612L),
  Mean_Math  = c(511, 491, 479, 507, 533, 550),
  Mean_Essay = c(660, 619, 550, 643, 643, 690)
) |>
  kbl(
    col.names = c("Race", "n", "Mean Math", "Mean Essay"),
    align = "lrrr"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  ) |>
  column_spec(1, width = "10em") |>
  footnote(
    general = "Based on a random sample of 10,000 observations",
    general_title = "",
    footnote_as_chunk = TRUE
  )

## ----enem-itens---------------------------------------------------------------
# # Data about exam questions
# items <- get_enem_itens(2023)
# 
# glimpse(items)
# #> Rows: 5,550
# #> Columns: 14
# #> $ co_posicao       <chr> "21", "20", "11", ...
# #> $ sg_area          <chr> "LC", "LC", "LC", ...
# #> $ co_item          <chr> "141283", "118144", ...
# #> $ tx_gabarito      <chr> "B", "C", "D", ...
# #> $ co_habilidade    <chr> "14", "23", "26", ...
# #> $ in_item_aban     <dbl> 0, 0, 0, ...
# #> $ nu_param_a       <dbl> 2.20, 2.43, 1.95, ...
# #> $ nu_param_b       <dbl> 0.83, 0.90, -0.13, ...
# #> $ nu_param_c       <dbl> 0.21, 0.13, 0.20, ...
# #> $ tx_cor           <chr> "AMARELA", "AMARELA", ...
# #> $ co_prova         <chr> "1286", "1286", ...
# #> # ... 3 more variables

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
# #> $ nu_ano_censo              <dbl> 2023, 2023, ...
# #> $ no_regiao                 <chr> "Norte", "Norte", ...
# #> $ co_regiao                 <chr> "1", "1", ...
# #> $ no_uf                     <chr> "Rondônia", "Rondônia", ...
# #> $ sg_uf                     <chr> "RO", "RO", ...
# #> $ co_uf                     <chr> "11", "11", ...
# #> $ no_municipio              <chr> "Porto Velho", "Porto Velho", ...
# #> $ co_municipio              <chr> "1100205", "1100205", ...
# #> $ no_regiao_geog_interm     <chr> "Porto Velho", "Porto Velho", ...
# #> $ co_regiao_geog_interm     <chr> "1101", "1101", ...
# #> # ... 398 more variables

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
#     x     = NULL,
#     y     = "Number of Schools"
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   scale_y_continuous(
#     labels = scales::label_number(big.mark = ".", decimal.mark = ","),
#     expand = expansion(mult = c(0, 0.12))
#   )

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
#     pct_ramps         = mean(in_acessibilidade_rampas == 1, na.rm = TRUE) * 100
#   )
# 
# print(infra)

## ----censo-infra-table, echo = FALSE, eval = TRUE-----------------------------
library(kableExtra)
data.frame(
  Internet     = 88.5,
  Library      = 31.2,
  Computer_Lab = 29.3,
  Sports_Court = 36.0,
  Ramps        = 53.5
) |>
  kbl(
    col.names = c("Internet", "Library", "Computer Lab", "Sports Court", "Ramps"),
    align = "ccccc",
    caption = "Infrastructure availability in public schools (%)"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )

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
# # Download once, then filter
# ideb <- get_ideb(level = "escola", stage = "anos_iniciais", metric = "indicador", year = 2021)
# ideb_sp <- ideb |> dplyr::filter(uf_sigla == "SP")

## ----best-practices-memory----------------------------------------------------
# # Large files can consume a lot of RAM
# # Use n_max or filter data after loading
# data <- get_censo_escolar(2023)
# data_filtered <-
#   data |>
#   select(co_entidade, no_entidade, sg_uf, tp_dependencia)
# rm(data)  # Free memory
# gc()

