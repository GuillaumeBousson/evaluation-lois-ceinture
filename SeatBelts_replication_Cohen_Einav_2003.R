############################################################
#  Replication — Cohen & Einav (2003) Seat Belt Laws
#  Paper: "The Effect of Mandatory Seat Belt Laws on Driving
#         Behavior and Traffic Fatalities" (REStat, 2003)
#
#  Master 2 SE — Université de Strasbourg
#
#  Auteurs du code : Bousson Guillaume / Sénéchal Maé
#  Date : 5 janvier 2026
#
#  ---------------------------------------------------------
#  ACCÈS AUX DONNÉES (récupération)
#   1) Aller sur : https://leinav.people.stanford.edu/research
#   2) Trouver l’article (avec Alma Cohen) :
#        "The Effect of Mandatory Seat Belt Laws on Driving Behavior and Traffic Fatalities"
#      (souvent en bas de la page "Research")
#   3) Cliquer sur "Data Set and Do File" pour télécharger le package
#
#  PLACEMENT DES DONNÉES (structure attendue)
#   - Dézipper le package téléchargé
#   - Placer le fichier .dta dans le dossier : data_raw/
#   - Renommer le fichier en : SB_Paper.dta (recommandé)
#
#  LANCEMENT (important)
#   - Ouvrir le fichier .Rproj du projet (pour être à la racine)
#   - Lancer les sections dans l’ordre : STEP 0 -> STEP 1 -> STEP 2 -> ...
#   - Les résultats (tables/figures) sont enregistrés dans : outputs_stepX/
############################################################


# Informations importantes : 
# - chaque partie du code pour la réplication des figures et des tables de l'article sont indépendantes
# - le nom de la figure ou de la table répliquée est indiqué à chaque début de partie.


# ==========================================================
# SEAT BELTS (Cohen & Einav) — Étape 0
# Objectif : charger les données + nettoyer + créer les variables
# (log / taux / identifiants panel) comme dans le do-file des auteurs.
# Sortie : data_step0_clean.rds + petits checks imprimés
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages
# ----------------------------
pkgs <- c("dplyr", "readr", "haven", "stringr", "janitor")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)
invisible(lapply(pkgs, library, character.only = TRUE))

# ----------------------------
# 1) Chemins 
# ----------------------------

data_dir <- "data"
out_dir  <- "data_step0"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Nom de fichier 

# Chemin de la donnée brute (.dta)
data_file <- file.path("data_raw", "SB_Paper.dta")


# ----------------------------
# 2) Lecture data (robuste .dta / .csv / .rds)
# ----------------------------
if (!file.exists(data_file)) {
  stop("Fichier introuvable : ", data_file, "\n-> Vérifie data_dir et data_file.")
}

ext <- tolower(tools::file_ext(data_file))

df <- switch(
  ext,
  "dta" = haven::read_dta(data_file),
  "csv" = readr::read_csv(data_file, show_col_types = FALSE),
  "rds" = readRDS(data_file),
  stop("Extension non gérée : .", ext, " (utilise .dta, .csv ou .rds)")
)

# Nettoyage noms de colonnes (minuscules, underscores, etc.)
df <- df %>% janitor::clean_names()

# ----------------------------
# 3) Checks rapides : dimensions / panel / variables essentielles
# ----------------------------
cat("\n--- CHECK 1 : Dimensions ---\n")
cat("N obs :", nrow(df), "\n")
cat("N var :", ncol(df), "\n")

need_vars <- c(
  "state", "fips", "year",
  "syear", "pyear", "usage",
  "ds", "dp", "dsp",
  "farsocc", "farsnocc",
  "vmtrural", "vmturban",
  "percapin", "meanage", "precentb", "precenth",
  "densrur", "densurb",
  "viopcap", "proppcap",
  "unemp", "fueltax",
  "lim65", "lim70p", "mlda21", "admin", "bac08"
)

missing <- setdiff(need_vars, names(df))
if (length(missing) > 0) {
  cat("\n⚠️ Variables manquantes :\n")
  print(missing)
  cat("\n-> On peut continuer, mais certaines régressions ne seront pas faisables.\n")
}

cat("\n--- CHECK 2 : Panel ---\n")
cat("Nb états (state) :", dplyr::n_distinct(df$state), "\n")
cat("Années min/max  :", min(df$year, na.rm = TRUE), "→", max(df$year, na.rm = TRUE), "\n")

# Doublons state-year ?
dup_key <- df %>% count(state, year) %>% filter(n > 1)
if (nrow(dup_key) > 0) {
  cat("\n⚠️ Doublons détectés sur (state, year) :\n")
  print(dup_key)
  stop("Stop : panel non unique. Il faut corriger avant de continuer.")
} else {
  cat("OK : (state, year) est unique.\n")
}

# ----------------------------
# 4) Préparation types (numériques / facteurs)
# ----------------------------
# On force les colonnes clés en types propres
df <- df %>%
  mutate(
    state = as.character(state),
    fips  = as.integer(fips),
    year  = as.integer(year),
    syear = suppressWarnings(as.integer(syear)),
    pyear = suppressWarnings(as.integer(pyear)),
    ds    = as.integer(ds),
    dp    = as.integer(dp),
    dsp   = as.integer(dsp)
  )

# ----------------------------
# 5) Création des variables comme dans le do-file
# ----------------------------

# Log "safe" : renvoie NA si x <= 0 ou x manquant
safe_log <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x) & (x > 0)
  out[ok] <- log(x[ok])
  out
}

df <- df %>%
  mutate(
    # VMT total
    vmt_total = vmtrural + vmturban,

    # Fatality rates (par VMT total) en log (do-file)
    fr_occ   = farsocc  / vmt_total,
    fr_nocc  = farsnocc / vmt_total,
    lfrsvocc  = safe_log(fr_occ),
    lfrsvnocc = safe_log(fr_nocc),     
    lfrsvnoc  = lfrsvnocc,             

    # Logs des variables explicatives (do-file)
    lusage   = safe_log(usage),
    lincome  = safe_log(percapin),
    lage     = safe_log(meanage),
    lpblack  = safe_log(precentb),
    lphisp   = safe_log(precenth),
    lvmtrura = safe_log(vmtrural),
    lvmtruba = safe_log(vmturban),
    ldensrur = safe_log(densrur),
    ldensurb = safe_log(densurb),
    lviopcap = safe_log(viopcap),
    lpropcap = safe_log(proppcap),
    lunemp   = safe_log(unemp),
    lfueltax = safe_log(fueltax),

    # FE pratiques (facteurs)
    fe_state = factor(fips),   # FE État via code FIPS (comme i.fips)
    fe_year  = factor(year)    # FE année (comme i.year)
  )

# ----------------------------
# 6) Checks log / NA créés
# ----------------------------
cat("\n--- CHECK 3 : Problèmes de log (valeurs <= 0) ---\n")

check_cols <- c(
  "lfrsvocc", "lfrsvnocc", "lusage",
  "lincome", "lage", "lpblack", "lphisp",
  "lvmtrura", "lvmtruba", "ldensrur", "ldensurb",
  "lviopcap", "lpropcap", "lunemp", "lfueltax"
)

na_counts <- sapply(df[check_cols], function(x) sum(is.na(x)))
na_tbl <- data.frame(variable = names(na_counts), n_na = as.integer(na_counts)) %>%
  arrange(desc(n_na))

print(na_tbl)

# Petit check : VMT total doit être > 0
cat("\n--- CHECK 4 : vmt_total ---\n")
cat("Nb obs avec vmt_total <= 0 :", sum(!is.na(df$vmt_total) & df$vmt_total <= 0), "\n")

# Petit check : instruments (ds/dp/dsp)
cat("\n--- CHECK 5 : Instruments (ds/dp/dsp) ---\n")
print(df %>% summarise(across(c(ds, dp, dsp), ~mean(.x, na.rm = TRUE))))

# ----------------------------
# 7) Sauvegarde dataset clean pour les étapes suivantes
# ----------------------------
saveRDS(df, file.path(out_dir, "data_step0_clean.rds"))

cat("\n✅ Étape 0 terminée.\n")
cat("Fichier sauvegardé :", file.path(out_dir, "data_step0_clean.rds"), "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — STEP 1A
# TABLE A1 — Descriptive statistics (continuous variables)
# Objectif : reproduire Table A1 du papier
# Entrée  : data_step0/data_step0_clean.rds (créée à l'étape 0)
# Sorties : outputs_step1/table_A1_numeric.csv
#           outputs_step1/table_A1_formatted.csv
#           + affichage console
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages 
# ----------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ----------------------------
# 1) Charger le dataset clean
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) {
  stop("Fichier introuvable : ", in_file,
       "\n-> Lance d'abord l'étape 0 (ou vérifie ton dossier).")
}
df <- readRDS(in_file)

# Dossier de sortie
out_dir <- "outputs_step1"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Construire les variables comme dans la Table A1
# ----------------------------
# Notes:
# - % Blacks / % Hispanics : on affiche en %
# - Seat belt usage : usage est souvent une fraction (0.45), on affiche en %
# - Fatalities per VMT : dans le do-file ils utilisent fars / (vmt_total)
#   Pour coller à l'échelle de la table (≈ 18.34), on multiplie par 1000.

VMT_RATE_SCALE <- 1000

# Chercher une éventuelle variable CDC/BRFSS si elle existe dans tes data
cdc_candidates <- c("cdcusage", "cdc_usage", "brfss", "brfss_usage", "usage_cdc", "cdcbrfss")
cdc_var <- intersect(names(df), cdc_candidates)
cdc_var <- if (length(cdc_var) > 0) cdc_var[1] else NA_character_

# Créer les variables "table"
df_tab <- df %>%
  mutate(
    # Pourcentages
    pct_black = 100 * precentb,
    pct_hisp  = 100 * precenth,

    # Crimes : la table indique "crimes per 1,000 people".

    violent_crimes_1000  = 1000 * viopcap,
    property_crimes_1000 = 1000 * proppcap,

    # Seat belt usage en %
    usage_pct = 100 * usage,

    # Fatalities per VMT 
    occ_fat_per_vmt  = VMT_RATE_SCALE * (farsocc  / vmt_total),
    nocc_fat_per_vmt = VMT_RATE_SCALE * (farsnocc / vmt_total)
  )

# Ajouter CDC usage (%) 
if (!is.na(cdc_var)) {
  df_tab <- df_tab %>%
    mutate(cdc_usage_pct = 100 * .data[[cdc_var]])
}

# ----------------------------
# 3) Fonctions stats (Mean, SD, Min, Max, Within SD, N)
# ----------------------------
within_sd <- function(x, g) {
  # Within-state SD = sd(x_it - mean_i(x_it))
  x <- as.numeric(x)
  g <- as.factor(g)
  x_demean <- x - ave(x, g, FUN = function(z) mean(z, na.rm = TRUE))
  sd(x_demean, na.rm = TRUE)
}

get_stats <- function(x, g) {
  x <- as.numeric(x)
  c(
    mean   = mean(x, na.rm = TRUE),
    sd     = sd(x, na.rm = TRUE),
    min    = min(x, na.rm = TRUE),
    max    = max(x, na.rm = TRUE),
    within = within_sd(x, g),
    n      = sum(!is.na(x))
  )
}

# ----------------------------
# 4) Liste des lignes de la Table A1 (Pour faire comme dans l'ordre du papier)
# ----------------------------
var_list <- list(
  "% Blacks"                      = "pct_black",
  "% Hispanics"                   = "pct_hisp",
  "Mean age"                      = "meanage",
  "Median income"                 = "percapin",     # (dans nos données : percapin)
  "Traffic density rural"         = "densrur",
  "Traffic density urban"         = "densurb",
  "VMT rural"                     = "vmtrural",
  "VMT urban"                     = "vmturban",
  "Unemployment rate"             = "unemp",
  "Violent crimes"                = "violent_crimes_1000",
  "Property crimes"               = "property_crimes_1000",
  "Fuel tax"                      = "fueltax",
  "Seat belt usage"               = "usage_pct"
)

# Ajouter CDC usage si dispo
if (!is.na(cdc_var)) {
  var_list <- append(var_list, list("CDC usage" = "cdc_usage_pct"), after = length(var_list))
}

# Fin de table
var_list <- append(var_list, list(
  "Occupant fatalities"           = "farsocc",
  "Nonoccupant fatalities"        = "farsnocc",
  "Occupant fatalities per VMT"   = "occ_fat_per_vmt",
  "Nonoccupant fatalities per VMT"= "nocc_fat_per_vmt"
))

# ----------------------------
# 5) Calcul des stats
# ----------------------------
g_state <- df_tab$fips  # groupement état (comme i.fips)

tab_num <- lapply(names(var_list), function(lbl) {
  vname <- var_list[[lbl]]
  if (!vname %in% names(df_tab)) stop("Variable manquante pour Table A1 : ", vname)

  s <- get_stats(df_tab[[vname]], g_state)
  data.frame(
    Variable = lbl,
    Mean = s["mean"],
    `Std. Dev.` = s["sd"],
    Min = s["min"],
    Max = s["max"],
    `Within Std. Dev.` = s["within"],
    `Number of Observations` = as.integer(s["n"]),
    check.names = FALSE
  )
})

tab_A1_numeric <- bind_rows(tab_num)

# ----------------------------
# 6) Format "propre" pour lecture 
# ----------------------------
fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")

tab_A1_formatted <- tab_A1_numeric %>%
  mutate(
    Mean  = ifelse(grepl("^%|usage", Variable), paste0(fmt(Mean, 2), "%"),
                   ifelse(Variable == "Median income", fmt0(Mean), fmt(Mean, 2))),
    `Std. Dev.` = ifelse(grepl("^%|usage", Variable), paste0(fmt(`Std. Dev.`, 2), "%"),
                         ifelse(Variable == "Median income", fmt0(`Std. Dev.`), fmt(`Std. Dev.`, 2))),
    Min = ifelse(grepl("^%|usage", Variable), paste0(fmt(Min, 2), "%"),
                 ifelse(Variable == "Median income", fmt0(Min), fmt(Min, 2))),
    Max = ifelse(grepl("^%|usage", Variable), paste0(fmt(Max, 2), "%"),
                 ifelse(Variable == "Median income", fmt0(Max), fmt(Max, 2))),
    `Within Std. Dev.` = ifelse(grepl("^%|usage", Variable), paste0(fmt(`Within Std. Dev.`, 2), "%"),
                                ifelse(Variable == "Median income", fmt0(`Within Std. Dev.`), fmt(`Within Std. Dev.`, 2))),
    `Number of Observations` = as.integer(`Number of Observations`)
  )

# ----------------------------
# 7) Export + checks rapides
# ----------------------------
write.csv(tab_A1_numeric,    file.path(out_dir, "table_A1_numeric.csv"),    row.names = FALSE)
write.csv(tab_A1_formatted,  file.path(out_dir, "table_A1_formatted.csv"),  row.names = FALSE)

cat("\n================= TABLE A1 (aperçu) =================\n")
print(tab_A1_formatted, row.names = FALSE)
cat("\nSauvegardé dans :", out_dir, "\n")
cat(" - table_A1_numeric.csv\n - table_A1_formatted.csv\n")

# Check important du papier : N usage ≈ 556 
n_usage <- sum(!is.na(df_tab$usage))
cat("\nCheck N(usage) =", n_usage, " (dans le papier ~556)\n")

# Check échelle fatalities/VMT (doit être ~18 pour occ, ~3 pour non-occ si scale ok)
cat("Check mean(occ fatalities per VMT)  =", round(mean(df_tab$occ_fat_per_vmt,  na.rm=TRUE), 2), "\n")
cat("Check mean(nocc fatalities per VMT) =", round(mean(df_tab$nocc_fat_per_vmt, na.rm=TRUE), 2), "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — STEP 1B
# FIGURE 3 — Legislation over time (stacked bars)
# Barres empilées : No law / Secondary / Primary / Switched (Sec->Prim)
# Entrée  : data_step0/data_step0_clean.rds
# Sorties : outputs_step1/figure3_legislation_over_time.png (+ .pdf)
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages 
# ----------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(dplyr)
library(ggplot2)

# ----------------------------
# 1) Charger les données clean (Step 0)
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) {
  stop("Fichier introuvable : ", in_file,
       "\n-> Lance d'abord l'étape 0 (ou vérifie le dossier).")
}
df <- readRDS(in_file)

out_dir <- "outputs_step1"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Construire le statut de loi par (state, year)
# Règle (comme l'idée du papier) :
# - Switched : dp==1 & dsp==1
# - Primary  : dp==1 & dsp==0 (ou NA)
# - Secondary: ds==1 & dp==0
# - No law   : sinon
# ----------------------------
df_leg <- df %>%
  mutate(
    ds  = as.integer(ds),
    dp  = as.integer(dp),
    dsp = as.integer(dsp)
  ) %>%
  select(state, year, ds, dp, dsp) %>%
  mutate(
    law_status = case_when(
      dp == 1 & dsp == 1                      ~ "Switched (Sec→Prim)",
      dp == 1 & (is.na(dsp) | dsp == 0)       ~ "Primary enforcement",
      (is.na(dp) | dp == 0) & ds == 1         ~ "Secondary enforcement",
      TRUE                                    ~ "No law"
    )
  )

# Ordre de pile (pour ressembler à la figure du papier : No law en haut)
df_leg$law_status <- factor(
  df_leg$law_status,
  levels = c("Secondary enforcement", "Primary enforcement", "Switched (Sec→Prim)", "No law")
)

# ----------------------------
# 3) Comptes par année (nombre d'États)
# ----------------------------
counts <- df_leg %>%
  group_by(year, law_status) %>%
  summarise(n_states = n(), .groups = "drop") %>%
  arrange(year, law_status)

# Check : somme par année doit = nombre d'États (souvent 51)
check_year <- counts %>%
  group_by(year) %>%
  summarise(total = sum(n_states), .groups = "drop")

cat("\n--- CHECK Figure 3 : total états par année ---\n")
print(check_year)

# ----------------------------
# 4) Plot (barres empilées + nombres dans les segments)
# ----------------------------
p <- ggplot(counts, aes(x = year, y = n_states, fill = law_status)) +
  geom_col(color = "black", linewidth = 0.2) +
  geom_text(
    aes(label = ifelse(n_states > 0, n_states, "")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_x_continuous(breaks = sort(unique(counts$year))) +
  labs(
    title = "Figure 3 — Legislation over time",
    x = "year",
    y = "number of states",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0, vjust = 0.5),
    legend.position = "right"
  ) +
  # Couleurs sobres (impression B/W)
  scale_fill_manual(values = c(
    "Secondary enforcement" = "grey75",
    "Primary enforcement"   = "grey25",
    "Switched (Sec→Prim)"   = "grey55",
    "No law"                = "white"
  ))

# ----------------------------
# 5) Sauvegarde
# ----------------------------
png_file <- file.path(out_dir, "figure3_legislation_over_time.png")
pdf_file <- file.path(out_dir, "figure3_legislation_over_time.pdf")

ggsave(png_file, plot = p, width = 10, height = 6, dpi = 300)
ggsave(pdf_file, plot = p, width = 10, height = 6)

cat("\n✅ Figure 3 sauvegardée :\n")
cat(" -", png_file, "\n")
cat(" -", pdf_file, "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — STEP 1C
# FIGURE 2 — Average seat belt usage over time
# - Solid line : National usage rate (equal-weight mean of usage by year)
# - Dashed line: CDC/BRFSS usage IF the variable exists in your dataset
# Entrée  : data_step0/data_step0_clean.rds
# Sorties : outputs_step1/figure2_usage_over_time.png (+ .pdf)
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages 
# ----------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# ----------------------------
# 1) Charger données clean (Step 0)
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) {
  stop("Fichier introuvable : ", in_file,
       "\n-> Lance d'abord l'étape 0 (ou vérifie le dossier).")
}
df <- readRDS(in_file)

out_dir <- "outputs_step1"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Détecter automatiquement une variable CDC/BRFSS si elle existe
# (souvent absente selon les datasets)
# ----------------------------
cdc_candidates <- c("cdcusage", "cdc_usage", "brfss", "brfss_usage", "usage_cdc", "cdcbrfss")
cdc_var <- intersect(names(df), cdc_candidates)
cdc_var <- if (length(cdc_var) > 0) cdc_var[1] else NA_character_

cat("\n--- INFO Figure 2 ---\n")
if (is.na(cdc_var)) {
  cat("Variable CDC/BRFSS : NON trouvée -> on trace seulement la ligne principale (usage).\n")
} else {
  cat("Variable CDC/BRFSS trouvée :", cdc_var, "-> on trace 2 lignes.\n")
}

# ----------------------------
# 3) Moyennes nationales (poids égaux) par année
# ----------------------------
usage_year <- df %>%
  group_by(year) %>%
  summarise(
    usage_mean = mean(usage, na.rm = TRUE),
    n_states_usage = sum(!is.na(usage)),
    .groups = "drop"
  )

# Optionnel : CDC/BRFSS
if (!is.na(cdc_var)) {
  cdc_year <- df %>%
    group_by(year) %>%
    summarise(
      cdc_mean = mean(.data[[cdc_var]], na.rm = TRUE),
      n_states_cdc = sum(!is.na(.data[[cdc_var]])),
      .groups = "drop"
    )
  plot_df <- left_join(usage_year, cdc_year, by = "year")
} else {
  plot_df <- usage_year
}

# Check utile : combien d'états contribuent chaque année ?
cat("\n--- CHECK : nombre d'états avec usage observé par année ---\n")
print(plot_df %>% select(year, n_states_usage))

if (!is.na(cdc_var)) {
  cat("\n--- CHECK : nombre d'états avec CDC/BRFSS observé par année ---\n")
  print(plot_df %>% select(year, n_states_cdc))
}

# ----------------------------
# 4) Préparer données "long" pour ggplot
# ----------------------------
plot_long <- plot_df %>%
  select(year, usage_mean, any_of("cdc_mean")) %>%
  pivot_longer(
    cols = -year,
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    series = case_when(
      series == "usage_mean" ~ "National Usage Rate",
      series == "cdc_mean"   ~ "National Usage Rate from BRFSS (CDC)",
      TRUE ~ series
    )
  )

# ----------------------------
# 5) Plot (style proche du papier)
# ----------------------------
p <- ggplot(plot_long, aes(x = year, y = value, linetype = series)) +
  geom_line(color = "black", linewidth = 1) +
  geom_point(color = "black", size = 1) +
  scale_linetype_manual(
    values = c(
      "National Usage Rate" = "solid",
      "National Usage Rate from BRFSS (CDC)" = "dashed"
    ),
    breaks = c("National Usage Rate", "National Usage Rate from BRFSS (CDC)")
  ) +
  scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, 0.1)) +
  scale_x_continuous(breaks = sort(unique(plot_long$year))) +
  labs(
    title = "Figure 2 — Average Seat Belt Usage Over Time",
    x = "year",
    y = "usage rate",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.70, 0.20),
    legend.background = element_rect(fill = "white", color = "black"),
    axis.text.x = element_text(angle = 0)
  )

# ----------------------------
# 6) Sauvegarde
# ----------------------------
png_file <- file.path(out_dir, "figure2_usage_over_time.png")
pdf_file <- file.path(out_dir, "figure2_usage_over_time.pdf")

ggsave(png_file, plot = p, width = 10, height = 6, dpi = 300)
ggsave(pdf_file, plot = p, width = 10, height = 6)

cat("\n✅ Figure 2 sauvegardée :\n")
cat(" -", png_file, "\n")
cat(" -", pdf_file, "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — STEP 1D (FIXED)
# FIGURE 1 — Total occupant & non-occupant fatalities
#            + fatalities per 100 million VMT (2 axes)
# IMPORTANT: VMT variables are in "millions of miles"
# Therefore:
#   fatalities per 100 million VMT = (total fatalities / total VMT_million) * 100
# ==========================================================

rm(list = ls())

# Packages
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) stop("Fichier introuvable : ", in_file)
df <- readRDS(in_file)

out_dir <- "outputs_step1"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# 1) National aggregation by year (totals)
agg <- df %>%
  group_by(year) %>%
  summarise(
    occ_total  = sum(farsocc,  na.rm = TRUE),
    nocc_total = sum(farsnocc, na.rm = TRUE),
    vmt_total_million = sum(vmt_total, na.rm = TRUE),  # IMPORTANT: in millions
    .groups = "drop"
  ) %>%
  arrange(year)

# 2) Fatalities per 100 million VMT (paper scale)
# Since VMT is in million: per 100 million = *100
agg <- agg %>%
  mutate(
    occ_rate_100m  = (occ_total  / vmt_total_million) * 100,
    nocc_rate_100m = (nocc_total / vmt_total_million) * 100
  )

# CHECK: should be around ~1-2 (not 1,000,000)
cat("\n--- CHECK Figure 1 (fixed) ---\n")
cat("Years:", min(agg$year), "→", max(agg$year), "\n")
cat("Mean occ_rate_100m  =", round(mean(agg$occ_rate_100m,  na.rm = TRUE), 3), "\n")
cat("Mean nocc_rate_100m =", round(mean(agg$nocc_rate_100m, na.rm = TRUE), 3), "\n")
cat("Mean occ_total      =", round(mean(agg$occ_total,      na.rm = TRUE), 0), "\n")
cat("Mean nocc_total     =", round(mean(agg$nocc_total,     na.rm = TRUE), 0), "\n")

# 3) Two axes trick: scale totals to the left axis
rate_max  <- max(c(agg$occ_rate_100m, agg$nocc_rate_100m), na.rm = TRUE)
total_max <- max(c(agg$occ_total, agg$nocc_total), na.rm = TRUE)
K <- total_max / rate_max   # so total/K is on same scale as rates (~0-2.5)

plot_df <- agg %>%
  transmute(
    year,
    `Occupant Fatalities per 100 million VMT`     = occ_rate_100m,
    `Non-Occupant Fatalities per 100 million VMT` = nocc_rate_100m,
    `Occupant Fatalities (total)`                 = occ_total  / K,
    `Non-Occupant Fatalities (total)`             = nocc_total / K
  ) %>%
  pivot_longer(-year, names_to = "series", values_to = "value") %>%
  mutate(
    ltype = ifelse(grepl("\\(total\\)", series), "dashed", "solid")
  )

# 4) Plot
p <- ggplot(plot_df, aes(x = year, y = value, linetype = series)) +
  geom_line(color = "black", linewidth = 1) +
  scale_x_continuous(breaks = sort(unique(plot_df$year))) +
  scale_y_continuous(
    name = "fatalities per 100 million VMT",
    limits = c(0, max(plot_df$value, na.rm = TRUE) * 1.05),
    sec.axis = sec_axis(~ . * K, name = "number of fatalities")
  ) +
  labs(
    title = "Figure 1 — Total Occupant and Nonoccupant Fatalities",
    x = "year",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = c(0.68, 0.55),
    legend.background = element_rect(fill = "white", color = "black")
  )

# 5) Save
png_file <- file.path(out_dir, "figure1_fatalities_totals_and_rates.png")
pdf_file <- file.path(out_dir, "figure1_fatalities_totals_and_rates.pdf")
ggsave(png_file, plot = p, width = 11, height = 6.5, dpi = 300)
ggsave(pdf_file, plot = p, width = 11, height = 6.5)

cat("\n✅ Figure 1 sauvegardée :\n -", png_file, "\n -", pdf_file, "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — TABLE 4 (paper-like)
# First stage: laws -> seat belt usage
# Output: LaTeX (.tex) + text (.txt) + csv (tidy)
# Robust SE (HC1) + stars + 3 decimals
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages
# ----------------------------
packs <- c("dplyr", "lmtest", "sandwich", "stargazer")
for (p in packs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

# ----------------------------
# 1) Load data clean
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) stop("Fichier introuvable : ", in_file)
df <- readRDS(in_file)

out_dir <- "outputs_step2"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Harmonise DV name if needed
if (!"usage" %in% names(df) && "sb_usage" %in% names(df)) {
  df <- dplyr::rename(df, usage = sb_usage)
}
if (!"usage" %in% names(df)) stop("Variable dépendante 'usage' introuvable dans les données.")

# ----------------------------
# 2) Prepare variables (logs + filters)
# ----------------------------
df2 <- df %>%
  mutate(
    lincome   = log(percapin),
    lunemp    = log(unemp),
    lage      = log(meanage),
    lpblack   = log(precentb),
    lphisp    = log(precenth),
    ldensrur  = log(densrur),
    ldensurb  = log(densurb),
    lviopcap  = log(viopcap),
    lproppcap = log(proppcap),
    lvmtrura  = log(vmtrural),
    lvmturba  = log(vmturban),
    lfueltax  = log(fueltax),

    ds  = as.integer(ds),
    dp  = as.integer(dp),
    dsp = as.integer(dsp),
    fips = as.factor(fips),
    year = as.factor(year)
  ) %>%
  filter(!is.na(usage)) %>%
  # on s'assure qu'on n'a pas de -Inf/Inf dans les logs
  filter(
    is.finite(lincome), is.finite(lunemp), is.finite(lage),
    is.finite(lpblack), is.finite(lphisp), is.finite(ldensrur),
    is.finite(ldensurb), is.finite(lviopcap), is.finite(lproppcap),
    is.finite(lvmtrura), is.finite(lvmturba), is.finite(lfueltax)
  )

cat("\n--- CHECK Table 4 ---\n")
cat("N (usage observed) =", nrow(df2), " (papier: 556)\n")

# ----------------------------
# 3) Specs (same structure as paper)
# ----------------------------
controls <- c(
  "lincome","lunemp","lage","lpblack","lphisp",
  "ldensrur","ldensurb","lviopcap","lproppcap",
  "lvmtrura","lvmturba","lfueltax",
  "lim65","lim70p","mlda21","bac08"
)

f_ols <- as.formula(
  paste("usage ~ ds + dp + dsp +", paste(controls, collapse = " + "), "+ year")
)
f_fe <- as.formula(
  paste("usage ~ ds + dp + dsp +", paste(controls, collapse = " + "), "+ year + fips")
)

m_ols <- lm(f_ols, data = df2)
m_fe  <- lm(f_fe,  data = df2)

# Robust inference (HC1) => se + p-values robustes pour étoiles
ct_ols <- lmtest::coeftest(m_ols, vcov. = sandwich::vcovHC(m_ols, type = "HC1"))
ct_fe  <- lmtest::coeftest(m_fe,  vcov. = sandwich::vcovHC(m_fe,  type = "HC1"))

se_ols <- ct_ols[, 2]
se_fe  <- ct_fe[, 2]
p_ols  <- ct_ols[, 4]
p_fe   <- ct_fe[, 4]

# ----------------------------
# 4) Joint F-test (ds, dp, dsp) with HC1
# ----------------------------
ft_ols <- lmtest::waldtest(
  m_ols, . ~ . - ds - dp - dsp,
  vcov = sandwich::vcovHC(m_ols, type = "HC1"),
  test = "F"
)
ft_fe <- lmtest::waldtest(
  m_fe, . ~ . - ds - dp - dsp,
  vcov = sandwich::vcovHC(m_fe, type = "HC1"),
  test = "F"
)

cat("\nJoint F-test (ds,dp,dsp) OLS (HC1):\n"); print(ft_ols)
cat("\nJoint F-test (ds,dp,dsp) State FE (HC1):\n"); print(ft_fe)

# ----------------------------
# 5) Export "paper-like" table with stargazer
# ----------------------------
# Labels like in the paper (order must match the covariates shown)
cov_labels <- c(
  "Secondary enforcement",
  "Primary enforcement",
  "Secondary to primary enforcement",
  "log(median income)",
  "log(unemployment rate)",
  "log(mean age)",
  "log(% blacks)",
  "log(% Hispanics)",
  "log(traffic density rural)",
  "log(traffic density urban)",
  "log(violent crimes)",
  "log(property crimes)",
  "log(VMT rural)",
  "log(VMT urban)",
  "log(fuel tax)",
  "65-mph speed limit",
  "70-mph speed limit or above",
  "MLDA of 21 years",
  "BAC = 0.08"
)

# We omit the intercept + all year FE dummies + all state FE dummies from display
omit_regex <- c("\\(Intercept\\)", "^year", "^fips")

tex_file <- file.path(out_dir, "table4_first_stage.tex")
txt_file <- file.path(out_dir, "table4_first_stage.txt")

# LaTeX output
stargazer(
  m_ols, m_fe,
  type = "latex",
  out  = tex_file,

  title = "Table 4 --- The impact of mandatory seat belt laws on seat belt usage",
  dep.var.labels = "Seat Belt Usage",
  column.labels = c("OLS", "State FE"),
  column.separate = c(1, 1),

  se = list(se_ols, se_fe),
  p  = list(p_ols,  p_fe),         # IMPORTANT: stars based on robust p-values
  digits = 3,

  omit = omit_regex,
  covariate.labels = cov_labels,

  add.lines = list(
    c("Year FE",  "Yes", "Yes"),
    c("State FE", "No",  "Yes")
  ),
  omit.stat = c("rsq", "ser", "f"), # keep N + Adj. R2 like paper
  notes = c("Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01."),
  notes.align = "l"
)

# Text output (même table en .txt)
stargazer(
  m_ols, m_fe,
  type = "text",
  out  = txt_file,

  title = "TABLE 4 — The impact of mandatory seat belt laws on seat belt usage",
  dep.var.labels = "Seat Belt Usage",
  column.labels = c("OLS", "State FE"),
  column.separate = c(1, 1),

  se = list(se_ols, se_fe),
  p  = list(p_ols,  p_fe),
  digits = 3,

  omit = omit_regex,
  covariate.labels = cov_labels,

  add.lines = list(
    c("Year FE",  "Yes", "Yes"),
    c("State FE", "No",  "Yes")
  ),
  omit.stat = c("rsq", "ser", "f"),
  notes = c("Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01.")
)

cat("\n✅ Table 4 exportée (format papier):\n")
cat(" -", tex_file, "\n")
cat(" -", txt_file, "\n")

# ----------------------------
# 6) (Optional) CSV tidy (coef/se/p + stars, arrondi 3)
# ----------------------------
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

keep_vars <- c("ds","dp","dsp", controls)
tidy_out <- data.frame(
  term = keep_vars,
  coef_OLS = coef(m_ols)[keep_vars],
  se_OLS   = se_ols[keep_vars],
  p_OLS    = p_ols[keep_vars],
  stars_OLS = stars(p_ols[keep_vars]),
  coef_FE  = coef(m_fe)[keep_vars],
  se_FE    = se_fe[keep_vars],
  p_FE     = p_fe[keep_vars],
  stars_FE = stars(p_fe[keep_vars]),
  row.names = NULL
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

csv_file <- file.path(out_dir, "table4_first_stage_tidy.csv")
write.csv(tidy_out, csv_file, row.names = FALSE)
cat(" -", csv_file, "(tidy)\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — STEP 3 (paper-like)
# TABLE 5 — Reduced form: laws -> fatalities
# 4 regressions (all include State FE + Year FE), Robust SE (HC1)
# Output: .tex + .txt + tidy .csv
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages
# ----------------------------
packs <- c("dplyr", "lmtest", "sandwich", "stargazer")
for (p in packs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(lmtest)
library(sandwich)
library(stargazer)

# ----------------------------
# 1) Load data
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) stop("Fichier introuvable : ", in_file)
df <- readRDS(in_file)

out_dir <- "outputs_step3"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Build dependent variables (Table 5)
# Notes scaling:
# vmt_total is already in MILLIONS in your dataset -> fatalities per VMT here = fatalities per MILLION VMT
# ----------------------------
if (!"vmt_total" %in% names(df)) {
  if (all(c("vmtrural","vmturban") %in% names(df))) {
    df <- df %>% mutate(vmt_total = vmtrural + vmturban)
  } else {
    stop("Impossible de construire vmt_total : variables manquantes.")
  }
}

df3 <- df %>%
  mutate(
    vmt_total_million = vmt_total,

    fr_occ  = farsocc  / vmt_total_million,
    fr_nocc = farsnocc / vmt_total_million,
    lfr_occ  = log(fr_occ),
    lfr_nocc = log(fr_nocc),

    # logs controls (as in Table 4 / do-file)
    lincome   = log(percapin),
    lunemp    = log(unemp),
    lage      = log(meanage),
    lpblack   = log(precentb),
    lphisp    = log(precenth),
    ldensrur  = log(densrur),
    ldensurb  = log(densurb),
    lviopcap  = log(viopcap),
    lproppcap = log(proppcap),
    lvmtrura  = log(vmtrural),
    lvmturba  = log(vmturban),
    lfueltax  = log(fueltax),

    ds   = as.integer(ds),
    dp   = as.integer(dp),
    dsp  = as.integer(dsp),
    fips = as.factor(fips),
    year = as.factor(year)
  ) %>%
  # For Table 5, we want the full sample (N≈765) BUT logs need positive rates:
  filter(!is.na(fr_occ), !is.na(fr_nocc), fr_occ > 0, fr_nocc > 0) %>%
  # Ensure logs/controls are finite
  filter(
    is.finite(lfr_occ), is.finite(lfr_nocc),
    is.finite(lincome), is.finite(lunemp), is.finite(lage),
    is.finite(lpblack), is.finite(lphisp), is.finite(ldensrur),
    is.finite(ldensurb), is.finite(lviopcap), is.finite(lproppcap),
    is.finite(lvmtrura), is.finite(lvmturba), is.finite(lfueltax)
  )

cat("\n--- CHECK Table 5 ---\n")
cat("N =", nrow(df3), " (papier: 765)\n")

# ----------------------------
# 3) Specs: same RHS everywhere (State FE + Year FE)
# ----------------------------
controls <- c(
  "lincome","lunemp","lage","lpblack","lphisp","ldensrur","ldensurb",
  "lviopcap","lproppcap","lvmtrura","lvmturba","lfueltax",
  "lim65","lim70p","mlda21","bac08"
)

rhs <- paste(c("ds","dp","dsp", controls, "year", "fips"), collapse = " + ")

f1 <- as.formula(paste("fr_occ  ~", rhs))
f2 <- as.formula(paste("fr_nocc ~", rhs))
f3 <- as.formula(paste("lfr_occ  ~", rhs))
f4 <- as.formula(paste("lfr_nocc ~", rhs))

m1 <- lm(f1, data = df3)  # Occupant fatalities per VMT
m2 <- lm(f2, data = df3)  # Nonoccupant fatalities per VMT
m3 <- lm(f3, data = df3)  # log(Occupant fatalities per VMT)
m4 <- lm(f4, data = df3)  # log(Nonoccupant fatalities per VMT)

# Robust inference (HC1): SE + p-values robustes (for stars)
robust_ct <- function(model) {
  ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
  list(se = ct[,2], p = ct[,4])
}

ct1 <- robust_ct(m1)
ct2 <- robust_ct(m2)
ct3 <- robust_ct(m3)
ct4 <- robust_ct(m4)

# ----------------------------
# 4) Export "paper-like" with stargazer (only ds/dp/dsp)
# IMPORTANT: keep regex must avoid ds matching dsp -> use anchors ^$
# ----------------------------
keep_regex <- c("^ds$", "^dp$", "^dsp$")

cov_labels <- c(
  "Secondary dummy",
  "Primary dummy",
  "Secondary to primary dummy"
)

tex_file <- file.path(out_dir, "table5_reduced_form.tex")
txt_file <- file.path(out_dir, "table5_reduced_form.txt")

stargazer(
  m1, m2, m3, m4,
  type = "latex",
  out  = tex_file,

  title = "Table 5 --- Reduced-form regression",
  model.numbers = FALSE,
  digits = 5,

  keep = keep_regex,
  covariate.labels = cov_labels,

  se = list(ct1$se, ct2$se, ct3$se, ct4$se),
  p  = list(ct1$p,  ct2$p,  ct3$p,  ct4$p),  # stars based on robust p-values

  column.labels = c("Occupant", "Nonoccupant", "Occupant", "Nonoccupant"),
  column.separate = c(1,1,1,1),

  add.lines = list(
    c("Dependent variable",
      "Fatalities per VMT", "Fatalities per VMT",
      "log(Fatalities per VMT)", "log(Fatalities per VMT)")
  ),

  omit.stat = c("rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c("All regressions include State FE and Year FE.",
            "Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01."),
  notes.align = "l"
)

stargazer(
  m1, m2, m3, m4,
  type = "text",
  out  = txt_file,

  title = "TABLE 5 — Reduced form: laws -> fatalities",
  model.numbers = FALSE,
  digits = 5,

  keep = keep_regex,
  covariate.labels = cov_labels,

  se = list(ct1$se, ct2$se, ct3$se, ct4$se),
  p  = list(ct1$p,  ct2$p,  ct3$p,  ct4$p),

  column.labels = c("Occupant", "Nonoccupant", "Occupant", "Nonoccupant"),
  column.separate = c(1,1,1,1),

  add.lines = list(
    c("Dependent variable",
      "Fatalities per VMT", "Fatalities per VMT",
      "log(Fatalities per VMT)", "log(Fatalities per VMT)")
  ),

  omit.stat = c("rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c("All regressions include State FE and Year FE.",
            "Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01.")
)

cat("\n✅ Table 5 exportée (format papier):\n")
cat(" -", tex_file, "\n")
cat(" -", txt_file, "\n")

# ----------------------------
# 5) (Optional) CSV tidy (ds/dp/dsp only)
# ----------------------------
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

get_triplet <- function(model, ct, model_name) {
  b <- coef(model)
  vars <- c("ds","dp","dsp")
  data.frame(
    term = vars,
    coef = b[vars],
    se   = ct$se[vars],
    p    = ct$p[vars],
    stars = stars(ct$p[vars]),
    model = model_name,
    row.names = NULL
  )
}

tidy <- rbind(
  get_triplet(m1, ct1, "Occ (Fatalities per VMT)"),
  get_triplet(m2, ct2, "Nonocc (Fatalities per VMT)"),
  get_triplet(m3, ct3, "Occ (log Fatalities per VMT)"),
  get_triplet(m4, ct4, "Nonocc (log Fatalities per VMT)")
) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

csv_file <- file.path(out_dir, "table5_reduced_form_tidy.csv")
write.csv(tidy, csv_file, row.names = FALSE)
cat(" -", csv_file, "(tidy)\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — TABLE 3 (NONOCCUPANT) — paper-like
# The effect of seat belt usage on NONOCCUPANT fatalities
#
# 6 colonnes (comme le papier) :
#   DV1: Nonocc fatalities per VMT     : OLS / State FE / IV
#   DV2: log(Nonocc fatalities per VMT): OLS / State FE / IV
#
# Year FE: YES everywhere
# State FE: NO in OLS; YES in State FE & IV
# Robust SE: HC1 (stars from robust p-values)
#
# Input  : data_step0/data_step0_clean.rds
# Output : outputs_step4/table3_nonoccupant.tex
#          outputs_step4/table3_nonoccupant.txt
#          outputs_step4/table3_nonoccupant_tidy.csv
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages
# ----------------------------
packs <- c("dplyr", "sandwich", "lmtest", "AER", "stargazer")
for (p in packs) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
library(dplyr)
library(sandwich)
library(lmtest)
library(AER)
library(stargazer)

# ----------------------------
# 1) Load data
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) stop("Fichier introuvable : ", in_file)
df <- readRDS(in_file)

out_dir <- "outputs_step4"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Harmonise DV name if needed
if (!"usage" %in% names(df) && "sb_usage" %in% names(df)) {
  df <- dplyr::rename(df, usage = sb_usage)
}
if (!"usage" %in% names(df)) stop("Variable 'usage' introuvable.")

# ----------------------------
# 2) Build variables
# ----------------------------
df3 <- df %>%
  mutate(
    vmt_total_calc = vmtrural + vmturban,

    # Dependent variables (Nonoccupant)
    fr_nocc  = farsnocc / vmt_total_calc,
    lfr_nocc = log(fr_nocc),

    # Usage
    lusage = log(usage),

    # Logs controls
    lincome   = log(percapin),
    lunemp    = log(unemp),
    lage      = log(meanage),
    lpblack   = log(precentb),
    lphisp    = log(precenth),
    ldensrur  = log(densrur),
    ldensurb  = log(densurb),
    lviopcap  = log(viopcap),
    lproppcap = log(proppcap),
    lvmtrura  = log(vmtrural),
    lvmturba  = log(vmturban),
    lfueltax  = log(fueltax),

    # FE
    fips_f = as.factor(fips),
    year_f = as.factor(year),

    # Instruments (laws dummies)
    ds  = as.integer(ds),
    dp  = as.integer(dp),
    dsp = as.integer(dsp)
  ) %>%
  # Table 3 requires usage observed -> N should be 556
  filter(!is.na(usage)) %>%
  # Ensure logs / rates valid
  filter(
    vmt_total_calc > 0,
    fr_nocc > 0, is.finite(lfr_nocc),
    is.finite(lusage),
    is.finite(lincome), is.finite(lunemp), is.finite(lage),
    is.finite(lpblack), is.finite(lphisp), is.finite(ldensrur),
    is.finite(ldensurb), is.finite(lviopcap), is.finite(lproppcap),
    is.finite(lvmtrura), is.finite(lvmturba), is.finite(lfueltax)
  )

cat("\n--- CHECK Table 3 sample ---\n")
cat("N =", nrow(df3), " (papier: 556)\n")

# ----------------------------
# 3) RHS controls
# ----------------------------
controls <- c("lincome","lunemp","lage","lpblack","lphisp","ldensrur","ldensurb",
              "lviopcap","lproppcap","lvmtrura","lvmturba","lfueltax",
              "lim65","lim70p","mlda21","bac08")

# ----------------------------
# 4) Estimate models
# ----------------------------
# Panel A: level DV
f_ols_lvl <- as.formula(paste("fr_nocc ~ usage +", paste(controls, collapse = " + "), "+ year_f"))
f_fe_lvl  <- as.formula(paste("fr_nocc ~ usage +", paste(controls, collapse = " + "), "+ year_f + fips_f"))
f_iv_lvl  <- as.formula(paste(
  "fr_nocc ~ usage +", paste(controls, collapse = " + "), "+ year_f + fips_f |",
  "ds + dp + dsp +", paste(controls, collapse = " + "), "+ year_f + fips_f"
))

m_ols_lvl <- lm(f_ols_lvl, data = df3)
m_fe_lvl  <- lm(f_fe_lvl,  data = df3)
m_iv_lvl  <- ivreg(f_iv_lvl, data = df3)

# Panel B: log DV
f_ols_log <- as.formula(paste("lfr_nocc ~ lusage +", paste(controls, collapse = " + "), "+ year_f"))
f_fe_log  <- as.formula(paste("lfr_nocc ~ lusage +", paste(controls, collapse = " + "), "+ year_f + fips_f"))
f_iv_log  <- as.formula(paste(
  "lfr_nocc ~ lusage +", paste(controls, collapse = " + "), "+ year_f + fips_f |",
  "ds + dp + dsp +", paste(controls, collapse = " + "), "+ year_f + fips_f"
))

m_ols_log <- lm(f_ols_log, data = df3)
m_fe_log  <- lm(f_fe_log,  data = df3)
m_iv_log  <- ivreg(f_iv_log, data = df3)

# ----------------------------
# 5) Robust SE + robust p-values (HC1) for stars
# ----------------------------
robust_ct <- function(model) {
  ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
  list(se = ct[,2], p = ct[,4])
}

ct_ols_lvl <- robust_ct(m_ols_lvl)
ct_fe_lvl  <- robust_ct(m_fe_lvl)
ct_iv_lvl  <- robust_ct(m_iv_lvl)

ct_ols_log <- robust_ct(m_ols_log)
ct_fe_log  <- robust_ct(m_fe_log)
ct_iv_log  <- robust_ct(m_iv_log)

# ----------------------------
# 6) Paper-like table (stargazer) — 6 columns like article
# ----------------------------
# Digits: Table 3 has tiny rates => 4 decimals is closer to paper
DIGITS_T3 <- 4  # change to 5 if you want more precision

omit_regex <- c("\\(Intercept\\)", "^year_f", "^fips_f")

# Keep variables in the right order (stargazer will display lusage row with blanks on level panel and usage row with blanks on log panel; ok)
keep_regex <- c("^usage$", "^lusage$",
                "^lincome$", "^lunemp$", "^lage$", "^lpblack$", "^lphisp$",
                "^ldensrur$", "^ldensurb$", "^lviopcap$", "^lproppcap$",
                "^lvmtrura$", "^lvmturba$", "^lfueltax$",
                "^lim65$", "^lim70p$", "^mlda21$", "^bac08$")

cov_labels <- c(
  "Seat belt usage",
  "log(seat belt usage)",
  "log(median income)",
  "log(unemployment rate)",
  "log(mean age)",
  "log(% blacks)",
  "log(% Hispanics)",
  "log(traffic density rural)",
  "log(traffic density urban)",
  "log(violent crimes)",
  "log(property crimes)",
  "log(VMT rural)",
  "log(VMT urban)",
  "log(fuel tax)",
  "65-mph speed limit",
  "70-mph speed limit or above",
  "MLDA of 21 years",
  "BAC = 0.08"
)

tex_file <- file.path(out_dir, "table3_nonoccupant.tex")
txt_file <- file.path(out_dir, "table3_nonoccupant.txt")

# LaTeX
stargazer(
  m_ols_lvl, m_fe_lvl, m_iv_lvl, m_ols_log, m_fe_log, m_iv_log,
  type = "latex",
  out  = tex_file,

  title = "Table 3 --- The effect of seat belt usage on Nonoccupant fatalities",
  model.numbers = FALSE,
  digits = DIGITS_T3,

  # headers: two blocks of 3 columns
  column.labels = c("Nonoccupant fatalities per VMT", "log(Nonoccupant fatalities per VMT)"),
  column.separate = c(3, 3),

  # robust stars
  se = list(ct_ols_lvl$se, ct_fe_lvl$se, ct_iv_lvl$se,
            ct_ols_log$se, ct_fe_log$se, ct_iv_log$se),
  p  = list(ct_ols_lvl$p,  ct_fe_lvl$p,  ct_iv_lvl$p,
            ct_ols_log$p,  ct_fe_log$p,  ct_iv_log$p),

  omit = omit_regex,
  keep = keep_regex,
  covariate.labels = cov_labels,

  add.lines = list(
    c("Year FE",  "Yes","Yes","Yes","Yes","Yes","Yes"),
    c("State FE", "No", "Yes","Yes","No", "Yes","Yes")
  ),

  # keep N + Adj R2 like paper
  omit.stat = c("rsq", "ser", "f", "ll", "aic", "bic"),

  notes = c("Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01."),
  notes.align = "l"
)

# Text
stargazer(
  m_ols_lvl, m_fe_lvl, m_iv_lvl, m_ols_log, m_fe_log, m_iv_log,
  type = "text",
  out  = txt_file,

  title = "TABLE 3 — Effect of seat belt usage on NONOCCUPANT fatalities",
  model.numbers = FALSE,
  digits = DIGITS_T3,

  column.labels = c("Nonocc per VMT", "log(Nonocc per VMT)"),
  column.separate = c(3, 3),

  se = list(ct_ols_lvl$se, ct_fe_lvl$se, ct_iv_lvl$se,
            ct_ols_log$se, ct_fe_log$se, ct_iv_log$se),
  p  = list(ct_ols_lvl$p,  ct_fe_lvl$p,  ct_iv_lvl$p,
            ct_ols_log$p,  ct_fe_log$p,  ct_iv_log$p),

  omit = omit_regex,
  keep = keep_regex,
  covariate.labels = cov_labels,

  add.lines = list(
    c("Year FE",  "Yes","Yes","Yes","Yes","Yes","Yes"),
    c("State FE", "No", "Yes","Yes","No", "Yes","Yes")
  ),

  omit.stat = c("rsq", "ser", "f", "ll", "aic", "bic"),
  notes = c("Robust standard errors (HC1) in parentheses.",
            "* p<0.10, ** p<0.05, *** p<0.01.")
)

cat("\n✅ Table 3 exportée (format papier):\n")
cat(" -", tex_file, "\n")
cat(" -", txt_file, "\n")

# ----------------------------
# 7) Optional: tidy csv (coef/se/p/stars for key rows)
# ----------------------------
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

extract_one <- function(model, ct, model_name, terms) {
  b <- coef(model)
  data.frame(
    term  = terms,
    coef  = b[terms],
    se    = ct$se[terms],
    p     = ct$p[terms],
    stars = stars(ct$p[terms]),
    model = model_name,
    row.names = NULL
  )
}

terms_all <- c("usage","lusage", controls)
tidy <- rbind(
  extract_one(m_ols_lvl, ct_ols_lvl, "OLS_lvl", terms_all),
  extract_one(m_fe_lvl,  ct_fe_lvl,  "FE_lvl",  terms_all),
  extract_one(m_iv_lvl,  ct_iv_lvl,  "IV_lvl",  terms_all),
  extract_one(m_ols_log, ct_ols_log, "OLS_log", terms_all),
  extract_one(m_fe_log,  ct_fe_log,  "FE_log",  terms_all),
  extract_one(m_iv_log,  ct_iv_log,  "IV_log",  terms_all)
) %>%
  mutate(across(where(is.numeric), ~ round(.x, DIGITS_T3)))

csv_file <- file.path(out_dir, "table3_nonoccupant_tidy.csv")
write.csv(tidy, csv_file, row.names = FALSE)
cat(" -", csv_file, "(tidy)\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — TABLE 2
# The effect of seat belt usage on OCCUPANT fatalities
#
# 6 colonnes (comme le papier) :
#   A) Occupant fatalities per VMT      : OLS / State FE / IV
#   B) log(Occupant fatalities per VMT) : OLS / State FE / IV
#
# FE année : OUI partout
# FE Etat  : NON en OLS ; OUI en State FE et IV
# SE robust HC1 (heteroskedasticity), comme Stata "robust"
#
# Entrée  : data_step0/data_step0_clean.rds
# Sorties : outputs_step5/table2_occupant_level.csv
#           outputs_step5/table2_occupant_log.csv
#           outputs_step5/table2_occupant.txt
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages (légers)
# ----------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("sandwich", quietly = TRUE)) install.packages("sandwich")
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")

library(dplyr)
library(sandwich)
library(AER)

# ----------------------------
# 1) Charger data
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) {
  stop("Fichier introuvable : ", in_file,
       "\n=> Vérifie ton working directory (getwd()) ou le chemin du fichier.")
}
df <- readRDS(in_file)

out_dir <- "outputs_step5"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Checks variables requises
# ----------------------------
need <- c("year","fips","usage","percapin","unemp","meanage","precentb","precenth",
          "densrur","densurb","viopcap","proppcap","vmtrural","vmturban","fueltax",
          "lim65","lim70p","mlda21","bac08","ds","dp","dsp","farsocc")
miss <- setdiff(need, names(df))
if (length(miss) > 0) stop("Variables manquantes dans df : ", paste(miss, collapse = ", "))

# ----------------------------
# 3) Préparer variables (log + rates) (comme do-file)
# ----------------------------
df2 <- df %>%
  mutate(
    # VMT total
    vmt_total_calc = if ("vmt_total" %in% names(.)) vmt_total else (vmtrural + vmturban),

    # Occupant fatalities per VMT
    fr_occ  = farsocc / vmt_total_calc,
    lfr_occ = log(fr_occ),

    # Usage (niveau + log)
    lusage = log(usage),

    # Logs contrôles (comme Stata do-file)
    lincome   = log(percapin),
    lunemp    = log(unemp),
    lage      = log(meanage),
    lpblack   = log(precentb),
    lphisp    = log(precenth),
    ldensrur  = log(densrur),
    ldensurb  = log(densurb),
    lviopcap  = log(viopcap),
    lproppcap = log(proppcap),
    lvmtrura  = log(vmtrural),
    lvmturba  = log(vmturban),
    lfueltax  = log(fueltax),

    # FE (facteurs)
    year_f = as.factor(year),
    fips_f = as.factor(fips),

    # Instruments (lois)
    ds  = as.integer(ds),
    dp  = as.integer(dp),
    dsp = as.integer(dsp)
  ) %>%
  # Table 2 : N=556 car usage requis (années early manquantes)
  filter(!is.na(usage)) %>%
  # Sécurité pour logs (doit être OK si données cohérentes)
  filter(is.finite(lusage), vmt_total_calc > 0, fr_occ > 0, is.finite(lfr_occ)) %>%
  filter(is.finite(lincome), is.finite(lunemp), is.finite(lage), is.finite(lpblack), is.finite(lphisp),
         is.finite(ldensrur), is.finite(ldensurb), is.finite(lviopcap), is.finite(lproppcap),
         is.finite(lvmtrura), is.finite(lvmturba), is.finite(lfueltax))

cat("\n--- CHECK Table 2 sample ---\n")
cat("N =", nrow(df2), " (papier: 556)\n")

# ----------------------------
# 4) RHS (contrôles) — ordre papier
# ----------------------------
controls <- c("lincome","lunemp","lage","lpblack","lphisp","ldensrur","ldensurb",
              "lviopcap","lproppcap","lvmtrura","lvmturba","lfueltax",
              "lim65","lim70p","mlda21","bac08")

# Robust SE HC1
robust_vcov <- function(model) vcovHC(model, type = "HC1")
robust_se   <- function(model) sqrt(diag(robust_vcov(model)))

# p-values robust + étoiles
stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}
robust_pvals <- function(model) {
  V <- robust_vcov(model)
  b <- coef(model)
  se <- sqrt(diag(V))
  t  <- b / se
  df_res <- tryCatch(df.residual(model), error = function(e) NA)
  if (is.na(df_res)) {
    # fallback
    p <- 2 * (1 - pnorm(abs(t)))
  } else {
    p <- 2 * (1 - pt(abs(t), df = df_res))
  }
  p
}

# helper extraction
extract_one <- function(model, term) {
  b <- coef(model)
  se <- robust_se(model)
  p  <- robust_pvals(model)
  out_b  <- if (term %in% names(b))  b[term]  else NA
  out_se <- if (term %in% names(se)) se[term] else NA
  out_p  <- if (term %in% names(p))  p[term]  else NA
  list(b = out_b, se = out_se, p = out_p)
}

# ----------------------------
# 5) Estimations — Panel A (DV = fr_occ)
# ----------------------------
f_ols_lvl <- as.formula(paste("fr_occ ~ usage +", paste(controls, collapse=" + "), "+ year_f"))
m_ols_lvl <- lm(f_ols_lvl, data = df2)

f_fe_lvl  <- as.formula(paste("fr_occ ~ usage +", paste(controls, collapse=" + "), "+ year_f + fips_f"))
m_fe_lvl  <- lm(f_fe_lvl, data = df2)

f_iv_lvl  <- as.formula(paste(
  "fr_occ ~ usage +", paste(controls, collapse=" + "), "+ year_f + fips_f |",
  "ds + dp + dsp +", paste(controls, collapse=" + "), "+ year_f + fips_f"
))
m_iv_lvl  <- ivreg(f_iv_lvl, data = df2)

# ----------------------------
# 6) Estimations — Panel B (DV = lfr_occ)
# ----------------------------
f_ols_log <- as.formula(paste("lfr_occ ~ lusage +", paste(controls, collapse=" + "), "+ year_f"))
m_ols_log <- lm(f_ols_log, data = df2)

f_fe_log  <- as.formula(paste("lfr_occ ~ lusage +", paste(controls, collapse=" + "), "+ year_f + fips_f"))
m_fe_log  <- lm(f_fe_log, data = df2)

f_iv_log  <- as.formula(paste(
  "lfr_occ ~ lusage +", paste(controls, collapse=" + "), "+ year_f + fips_f |",
  "ds + dp + dsp +", paste(controls, collapse=" + "), "+ year_f + fips_f"
))
m_iv_log  <- ivreg(f_iv_log, data = df2)

# ----------------------------
# 7) Export CSV séparés (plus propre)
# ----------------------------
# Panel A (level) : usage + contrôles
rows_lvl <- c("usage", controls)
tab_lvl <- data.frame(term = rows_lvl, stringsAsFactors = FALSE)

mods_lvl <- list(OLS = m_ols_lvl, FE = m_fe_lvl, IV = m_iv_lvl)
for (nm in names(mods_lvl)) {
  m <- mods_lvl[[nm]]
  tab_lvl[[paste0("coef_", nm)]] <- sapply(rows_lvl, function(tt) extract_one(m, tt)$b)
  tab_lvl[[paste0("se_", nm)]]   <- sapply(rows_lvl, function(tt) extract_one(m, tt)$se)
  tab_lvl[[paste0("p_", nm)]]    <- sapply(rows_lvl, function(tt) extract_one(m, tt)$p)
}

csv_lvl <- file.path(out_dir, "table2_occupant_level.csv")
write.csv(tab_lvl, csv_lvl, row.names = FALSE)

# Panel B (log) : lusage + contrôles
rows_log <- c("lusage", controls)
tab_log <- data.frame(term = rows_log, stringsAsFactors = FALSE)

mods_log <- list(OLS = m_ols_log, FE = m_fe_log, IV = m_iv_log)
for (nm in names(mods_log)) {
  m <- mods_log[[nm]]
  tab_log[[paste0("coef_", nm)]] <- sapply(rows_log, function(tt) extract_one(m, tt)$b)
  tab_log[[paste0("se_", nm)]]   <- sapply(rows_log, function(tt) extract_one(m, tt)$se)
  tab_log[[paste0("p_", nm)]]    <- sapply(rows_log, function(tt) extract_one(m, tt)$p)
}

csv_log <- file.path(out_dir, "table2_occupant_log.csv")
write.csv(tab_log, csv_log, row.names = FALSE)

# ----------------------------
# 8) Export TXT format "papier" (coef + étoiles ; SE dessous)
# ----------------------------
fmt_coef <- function(b, p) {
  if (is.na(b)) return("")
  paste0(formatC(b, digits = 4, format = "f"), stars(p))
}
fmt_se <- function(se) {
  if (is.na(se)) return("")
  paste0("(", formatC(se, digits = 4, format = "f"), ")")
}

# ordre “papier”
row_names <- c(
  "Seat belt usage",
  "log(seat belt usage)",
  "log(median income)",
  "log(unemployment rate)",
  "log(mean age)",
  "log(% blacks)",
  "log(% Hispanics)",
  "log(traffic density rural)",
  "log(traffic density urban)",
  "log(violent crimes)",
  "log(property crimes)",
  "log(VMT rural)",
  "log(VMT urban)",
  "log(fuel tax)",
  "65-mph speed limit",
  "70-mph speed limit or above",
  "MLDA of 21 years",
  "BAC = 0.08"
)

# map “label -> variable”
map_lvl <- c(
  "Seat belt usage" = "usage",
  "log(seat belt usage)" = NA,
  "log(median income)" = "lincome",
  "log(unemployment rate)" = "lunemp",
  "log(mean age)" = "lage",
  "log(% blacks)" = "lpblack",
  "log(% Hispanics)" = "lphisp",
  "log(traffic density rural)" = "ldensrur",
  "log(traffic density urban)" = "ldensurb",
  "log(violent crimes)" = "lviopcap",
  "log(property crimes)" = "lproppcap",
  "log(VMT rural)" = "lvmtrura",
  "log(VMT urban)" = "lvmturba",
  "log(fuel tax)" = "lfueltax",
  "65-mph speed limit" = "lim65",
  "70-mph speed limit or above" = "lim70p",
  "MLDA of 21 years" = "mlda21",
  "BAC = 0.08" = "bac08"
)

map_log <- c(
  "Seat belt usage" = NA,
  "log(seat belt usage)" = "lusage",
  "log(median income)" = "lincome",
  "log(unemployment rate)" = "lunemp",
  "log(mean age)" = "lage",
  "log(% blacks)" = "lpblack",
  "log(% Hispanics)" = "lphisp",
  "log(traffic density rural)" = "ldensrur",
  "log(traffic density urban)" = "ldensurb",
  "log(violent crimes)" = "lviopcap",
  "log(property crimes)" = "lproppcap",
  "log(VMT rural)" = "lvmtrura",
  "log(VMT urban)" = "lvmturba",
  "log(fuel tax)" = "lfueltax",
  "65-mph speed limit" = "lim65",
  "70-mph speed limit or above" = "lim70p",
  "MLDA of 21 years" = "mlda21",
  "BAC = 0.08" = "bac08"
)

get_row_vals <- function(model, varname) {
  if (is.na(varname)) return(list(b = NA, se = NA, p = NA))
  extract_one(model, varname)
}

txt_file <- file.path(out_dir, "table2_occupant.txt")
sink(txt_file)

cat("============================================================\n")
cat("TABLE 2 — Effect of seat belt usage on OCCUPANT fatalities\n")
cat("Robust SE (HC1). Year FE everywhere. State FE in FE and IV.\n")
cat("Sample requires usage => N should be 556.\n")
cat("============================================================\n\n")

cat(sprintf("%-32s %12s %12s %12s | %12s %12s %12s\n",
            "", "OLS", "State FE", "IV", "OLS", "State FE", "IV"))
cat(sprintf("%-32s %12s %12s %12s | %12s %12s %12s\n",
            "", "Occ/VMT", "Occ/VMT", "Occ/VMT", "log(Occ/VMT)", "log(Occ/VMT)", "log(Occ/VMT)"))
cat(paste(rep("-", 108), collapse = ""), "\n", sep = "")

for (lab in row_names) {

  v_lvl <- map_lvl[lab]
  v_log <- map_log[lab]

  a_ols <- get_row_vals(m_ols_lvl, v_lvl); a_fe <- get_row_vals(m_fe_lvl, v_lvl); a_iv <- get_row_vals(m_iv_lvl, v_lvl)
  b_ols <- get_row_vals(m_ols_log, v_log); b_fe <- get_row_vals(m_fe_log, v_log); b_iv <- get_row_vals(m_iv_log, v_log)

  # ligne coef
  cat(sprintf("%-32s %12s %12s %12s | %12s %12s %12s\n",
              lab,
              fmt_coef(a_ols$b, a_ols$p), fmt_coef(a_fe$b, a_fe$p), fmt_coef(a_iv$b, a_iv$p),
              fmt_coef(b_ols$b, b_ols$p), fmt_coef(b_fe$b, b_fe$p), fmt_coef(b_iv$b, b_iv$p)))

  # ligne SE
  cat(sprintf("%-32s %12s %12s %12s | %12s %12s %12s\n",
              "",
              fmt_se(a_ols$se), fmt_se(a_fe$se), fmt_se(a_iv$se),
              fmt_se(b_ols$se), fmt_se(b_fe$se), fmt_se(b_iv$se)))
}

cat(paste(rep("-", 108), collapse = ""), "\n", sep = "")

# Infos bas de tableau (N + R2 adj si dispo)
safe_adjR2 <- function(m) {
  out <- tryCatch(summary(m)$adj.r.squared, error = function(e) NA)
  out
}
safe_adjR2_iv <- function(m) {
  out <- tryCatch(summary(m)$adj.r.squared, error = function(e) NA)
  out
}

cat("\nYear FE : Yes everywhere\n")
cat("State FE: OLS = No ; State FE & IV = Yes\n\n")

info <- data.frame(
  model = c("OLS_lvl","FE_lvl","IV_lvl","OLS_log","FE_log","IV_log"),
  N = c(nobs(m_ols_lvl), nobs(m_fe_lvl), nobs(m_iv_lvl),
        nobs(m_ols_log), nobs(m_fe_log), nobs(m_iv_log)),
  adj_R2 = c(safe_adjR2(m_ols_lvl), safe_adjR2(m_fe_lvl), safe_adjR2_iv(m_iv_lvl),
             safe_adjR2(m_ols_log), safe_adjR2(m_fe_log), safe_adjR2_iv(m_iv_log))
)
print(info, row.names = FALSE)

cat("\nNotes: *, **, *** = 10%, 5%, 1% (p-values robust HC1)\n")

sink()

cat("\n✅ Table 2 exportée :\n")
cat(" -", csv_lvl, "\n")
cat(" -", csv_log, "\n")
cat(" -", txt_file, "\n")

# QUICK CHECK (coeffs principaux du papier)
cat("\n--- QUICK CHECK Table 2 (coeffs clés) ---\n")
cat("Level DV (usage): OLS | FE | IV =",
    coef(m_ols_lvl)["usage"], "|", coef(m_fe_lvl)["usage"], "|", coef(m_iv_lvl)["usage"], "\n")
cat("Log DV (lusage): OLS | FE | IV =",
    coef(m_ols_log)["lusage"], "|", coef(m_fe_log)["lusage"], "|", coef(m_iv_log)["lusage"], "\n")


# ==========================================================
# SEAT BELTS (Cohen & Einav) — TABLE 1 
# "A Summary of the Laws in All States"
#
# Objectif :
#  - retrouver (par Etat) l'année d'adoption du secondaire (ds)
#  - retrouver l'année d'adoption du primaire (dp ou dsp)
#  - calculer les usages :
#       * Before law              : usage à (année_law - 1)
#       * Immediately after law   : usage à (année_law)
#       * Before change in law    : usage à (année_primary - 1) si switch
#       * Immediately after change: usage à (année_primary) si switch
#       * In 1998                 : usage en 1998 (sinon dernière année dispo)
#
# Entrée  : data_step0/data_step0_clean.rds
# Sorties : outputs_step6/table1_laws_summary.csv
#           outputs_step6/table1_laws_summary.txt
# ==========================================================

rm(list = ls())

# ----------------------------
# 0) Packages (léger)
# ----------------------------
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

# ----------------------------
# 1) Charger data
# ----------------------------
in_file <- "data_step0/data_step0_clean.rds"
if (!file.exists(in_file)) {
  stop("Fichier introuvable : ", in_file,
       "\n=> Vérifie ton working directory avec getwd()")
}
df <- readRDS(in_file)

out_dir <- "outputs_step6"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 2) Harmoniser noms de variables si besoin
# ----------------------------
# usage
if (!"usage" %in% names(df)) {
  if ("sb_usage" %in% names(df)) df <- df %>% rename(usage = sb_usage)
  if ("sb_useage" %in% names(df)) df <- df %>% rename(usage = sb_useage)
}

# year / fips (identifiants)
need <- c("year", "fips", "usage", "ds", "dp", "dsp")
miss <- setdiff(need, names(df))
if (length(miss) > 0) stop("Variables manquantes : ", paste(miss, collapse = ", "))

# ----------------------------
# 3) Construire un identifiant Etat lisible (AK, AL, …)
# ----------------------------
# Si tu as déjà "state" en 2 lettres, on l'utilise.
# Sinon on mappe fips -> abréviation (inclut DC).
if ("state" %in% names(df) && is.character(df$state) && nchar(df$state[!is.na(df$state)][1]) <= 2) {
  df <- df %>% mutate(state_abbr = state)
} else {
  fips_map <- data.frame(
    fips = c(1,2,4,5,6,8,9,10,11,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
             30,31,32,33,34,35,36,37,38,39,40,41,42,44,45,46,47,48,49,50,51,53,54,55,56),
    state_abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA",
                   "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM",
                   "NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
                   "WV","WI","WY"),
    stringsAsFactors = FALSE
  )
  df <- df %>%
    left_join(fips_map, by = "fips")
}

# ----------------------------
# 4) Nettoyage minimal + année en numérique
# ----------------------------
df0 <- df %>%
  mutate(
    year_num = suppressWarnings(as.integer(as.character(year))),
    ds  = as.integer(ds),
    dp  = as.integer(dp),
    dsp = as.integer(dsp)
  ) %>%
  filter(!is.na(state_abbr), !is.na(year_num))

# Quelle année pour la colonne "In 1998" ?
target_year <- if (any(df0$year_num == 1998)) 1998 else max(df0$year_num, na.rm = TRUE)
if (target_year != 1998) {
  cat("NOTE: 1998 absent du dataset => colonne 'In 1998' remplacée par In ", target_year, "\n", sep="")
}

# Petite fonction pour récupérer usage(state, year)
get_usage <- function(dat, st, yy) {
  x <- dat$usage[dat$state_abbr == st & dat$year_num == yy]
  if (length(x) == 0) return(NA_real_)
  # normalement 1 obs par (state,year). si plusieurs, on prend la moyenne.
  mean(x, na.rm = TRUE)
}

# ----------------------------
# 5) Détecter les années d'adoption de lois
# ----------------------------
# définitions pratiques :
#  - sec_year  = 1ère année où ds==1
#  - prim_year = 1ère année où (dp==1 ou dsp==1)
#  - first_law = min(sec_year, prim_year)
#  - switch (= table "before/after change") seulement si sec_year existe et prim_year existe et prim_year > sec_year
laws <- df0 %>%
  group_by(state_abbr) %>%
  summarise(
    sec_year  = ifelse(any(ds == 1, na.rm = TRUE),  min(year_num[ds == 1], na.rm = TRUE), NA_integer_),
    prim_year = ifelse(any((dp == 1 | dsp == 1), na.rm = TRUE), min(year_num[(dp == 1 | dsp == 1)], na.rm = TRUE), NA_integer_),
    .groups = "drop"
  ) %>%
  mutate(
    first_law_year = pmin(sec_year, prim_year, na.rm = TRUE),
    first_law_year = ifelse(is.infinite(first_law_year), NA_integer_, first_law_year),
    switched = ifelse(!is.na(sec_year) & !is.na(prim_year) & prim_year > sec_year, TRUE, FALSE)
  )

# ----------------------------
# 6) Construire Table 1 (usages aux moments clés)
# ----------------------------
tab1 <- laws %>%
  rowwise() %>%
  mutate(
    # Usage autour de la 1ère loi (secondaire OU primaire direct)
    usage_before_law = get_usage(df0, state_abbr, first_law_year - 1),
    usage_after_law  = get_usage(df0, state_abbr, first_law_year),

    # Usage autour du switch (si switch)
    usage_before_change = ifelse(switched, get_usage(df0, state_abbr, prim_year - 1), NA_real_),
    usage_after_change  = ifelse(switched, get_usage(df0, state_abbr, prim_year), NA_real_),

    # Usage en 1998 (ou dernière année dispo)
    usage_in_1998 = get_usage(df0, state_abbr, target_year)
  ) %>%
  ungroup()

# Mettre en % comme le papier (ex: 0.57 -> 57%)
to_pct <- function(x) ifelse(is.na(x), NA_real_, 100 * x)

tab1 <- tab1 %>%
  mutate(
    before_law_pct        = to_pct(usage_before_law),
    after_law_pct         = to_pct(usage_after_law),
    before_change_pct     = to_pct(usage_before_change),
    after_change_pct      = to_pct(usage_after_change),
    in_1998_pct           = to_pct(usage_in_1998)
  ) %>%
  select(
    State = state_abbr,
    Secondary_Enforcement_Year = sec_year,
    Primary_Enforcement_Year   = prim_year,
    Before_Law_pct             = before_law_pct,
    Immediately_After_Law_pct  = after_law_pct,
    Before_Change_pct          = before_change_pct,
    Immediately_After_Change_pct = after_change_pct,
    In_1998_pct                = in_1998_pct
  ) %>%
  arrange(State)

# ----------------------------
# 7) Totaux / Moyennes (ligne finale)
# ----------------------------
tot_sec  <- sum(!is.na(tab1$Secondary_Enforcement_Year))
tot_prim <- sum(!is.na(tab1$Primary_Enforcement_Year))

avg_row <- data.frame(
  State = "Average",
  Secondary_Enforcement_Year = tot_sec,
  Primary_Enforcement_Year = tot_prim,
  Before_Law_pct = mean(tab1$Before_Law_pct, na.rm = TRUE),
  Immediately_After_Law_pct = mean(tab1$Immediately_After_Law_pct, na.rm = TRUE),
  Before_Change_pct = mean(tab1$Before_Change_pct, na.rm = TRUE),
  Immediately_After_Change_pct = mean(tab1$Immediately_After_Change_pct, na.rm = TRUE),
  In_1998_pct = mean(tab1$In_1998_pct, na.rm = TRUE)
)

tab1_out <- bind_rows(tab1, avg_row)

# ----------------------------
# 8) Export CSV
# ----------------------------
csv_file <- file.path(out_dir, "table1_laws_summary.csv")
write.csv(tab1_out, csv_file, row.names = FALSE)

# ----------------------------
# 9) Export TXT (lisible)
# ----------------------------
fmtp <- function(x) {
  if (is.na(x)) return("NA")
  paste0(round(x, 0), "%")
}
fmty <- function(x) {
  if (is.na(x)) return("NA")
  as.character(x)
}

txt_file <- file.path(out_dir, "table1_laws_summary.txt")
sink(txt_file)

cat("============================================================\n")
cat("TABLE 1 — A Summary of the Laws in All States (ADAPTED)\n")
cat("Dates are reconstructed as YEARS from ds/dp/dsp (no month/day).\n")
cat("Usage columns use usage(t-1) / usage(t) around adoption years.\n")
cat("============================================================\n\n")

cat(sprintf("%-4s | %-6s | %-6s | %-10s | %-12s | %-14s | %-16s | %-8s\n",
            "St", "SecYr", "PrimYr", "BeforeLaw", "AfterLaw", "BeforeChange", "AfterChange", "In1998"))
cat(paste(rep("-", 98), collapse=""), "\n", sep="")

for (i in 1:nrow(tab1)) {
  r <- tab1[i, ]
  cat(sprintf("%-4s | %-6s | %-6s | %-10s | %-12s | %-14s | %-16s | %-8s\n",
              r$State,
              fmty(r$Secondary_Enforcement_Year),
              fmty(r$Primary_Enforcement_Year),
              fmtp(r$Before_Law_pct),
              fmtp(r$Immediately_After_Law_pct),
              fmtp(r$Before_Change_pct),
              fmtp(r$Immediately_After_Change_pct),
              fmtp(r$In_1998_pct)))
}

cat(paste(rep("-", 98), collapse=""), "\n", sep="")
cat("\nTotal states with Secondary Enforcement:", tot_sec, "\n")
cat("Total states with Primary Enforcement  :", tot_prim, "\n")
cat("Target year for 'In 1998' column       :", target_year, "\n")

sink()

cat("\n✅ Table 1 exportée :\n")
cat(" -", csv_file, "\n")
cat(" -", txt_file, "\n")
