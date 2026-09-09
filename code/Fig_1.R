# ==============================================================================
# 00_data_prep.R
# Préparation des données fonctionnelles partagées par tous les scripts
# À sourcer APRÈS 00_common_style.R et APRÈS avoir chargé TAB_FINALE, DATAT, CARACT
#
# Produit :
#   func_exp  — une ligne par expérience (numéro) avec composition fonctionnelle
#               colonnes : n_WE, n_WNE, n_HE, n_HNE, n_Med, dom_group, ...
#   datat_func — DATAT enrichi avec le groupe fonctionnel de chaque espèce
# ==============================================================================

# ── 0. Harmoniser le type de numéro dans les deux tables ─────────────────────
# Évite les NA causés par un type mismatch character vs numeric
TAB_FINALE <- TAB_FINALE %>% mutate(numéro = as.character(numéro))
DATAT      <- DATAT      %>% mutate(numéro = as.character(numéro))

# Diagnostic : numéros présents dans DATAT mais pas dans TAB_FINALE
orphans <- anti_join(DATAT %>% distinct(numéro), TAB_FINALE %>% distinct(numéro), by = "numéro")
if (nrow(orphans) > 0) {
  cat("⚠ ", nrow(orphans), "numéro(s) dans DATAT absents de TAB_FINALE (exclus de func_exp) :\n")
  print(orphans)
  cat("\n")
}

# ── 1. Table de traits espèces ─────────────────────────────────────────────────
caract_key <- CARACT %>%
  select(NAME, Woodiness, Alim, Medicinal) %>%
  distinct() %>%
  mutate(
    NAME      = str_to_sentence(str_trim(NAME)),
    is_woody  = str_to_lower(Woodiness) %in% c("woody", "w"),
    is_edible = !is.na(Alim) & Alim != "" & Alim != "0",
    is_medic  = !is.na(Medicinal) & Medicinal != "" & Medicinal != "0",
    # Groupe fonctionnel à 4 niveaux
    func_group = case_when(
      is_woody  &  is_edible ~ "WE",
      is_woody  & !is_edible ~ "WNE",
      !is_woody &  is_edible ~ "HE",
      TRUE                   ~ "HNE"
    )
  )

cat("Functional groups from CARACT:\n")
print(table(caract_key$func_group, useNA = "ifany"))
cat("\n")

# ── 2. Enrichir DATAT avec le groupe fonctionnel ──────────────────────────────
datat_func <- DATAT %>%
  mutate(species_std = str_to_sentence(str_trim(`species treatment`))) %>%
  left_join(
    caract_key %>% select(NAME, func_group, is_woody, is_edible, is_medic),
    by = c("species_std" = "NAME")
  ) %>%
  mutate(
    func_group = replace_na(func_group, "Unknown")
  )

n_unmatched <- sum(datat_func$func_group == "Unknown")
cat("Species not matched in CARACT:", n_unmatched,
    sprintf("(%.1f%%)\n\n", 100 * n_unmatched / nrow(datat_func)))

# ── 3. Résumé par expérience ───────────────────────────────────────────────────
func_exp <- datat_func %>%
  group_by(numéro) %>%
  summarise(
    n_spp   = n_distinct(species_std),
    n_WE    = n_distinct(species_std[func_group == "WE"]),
    n_WNE   = n_distinct(species_std[func_group == "WNE"]),
    n_HE    = n_distinct(species_std[func_group == "HE"]),
    n_HNE   = n_distinct(species_std[func_group == "HNE"]),
    n_Med   = n_distinct(species_std[is_medic == TRUE]),
    n_edible = n_distinct(species_std[is_edible == TRUE]),
    n_woody  = n_distinct(species_std[is_woody  == TRUE]),
    # Groupe dominant (hors HNE qui est souvent le control)
    dom_group = {
      tbl <- table(func_group[func_group != "HNE"])
      if (length(tbl) == 0) NA_character_ else names(which.max(tbl))
    },
    .groups = "drop"
  ) %>%
  # Correspondance avec Concatenation de TAB_FINALE
  inner_join(TAB_FINALE %>% select(numéro, Concatenation, country,
                                   latitude, longitude,
                                   `farm type`, Intervention_reclass),
             by = "numéro")   # inner = exclut les numéros sans ligne TAB_FINALE

cat("func_exp summary (", nrow(func_exp), "experiments):\n")
print(summary(func_exp[, c("n_spp","n_WE","n_WNE","n_HE","n_Med")]))
cat("\n")

# ── 4. Validation : comparer func_group dérivé vs Concatenation TAB_FINALE ───
# Concatenation encode déjà WE/WNE/HE — on vérifie la cohérence
val <- func_exp %>%
  mutate(
    has_WE_concat  = str_detect(Concatenation, "WE"),
    has_WNE_concat = str_detect(Concatenation, "WNE"),
    has_HE_concat  = str_detect(Concatenation, "HE"),
    match_WE  = (n_WE  > 0) == has_WE_concat,
    match_WNE = (n_WNE > 0) == has_WNE_concat,
    match_HE  = (n_HE  > 0) == has_HE_concat
  )

cat("Consistency check (derived vs Concatenation column):\n")
cat("  WE  match:", round(100 * mean(val$match_WE,  na.rm=TRUE), 1), "%\n")
cat("  WNE match:", round(100 * mean(val$match_WNE, na.rm=TRUE), 1), "%\n")
cat("  HE  match:", round(100 * mean(val$match_HE,  na.rm=TRUE), 1), "%\n\n")

cat("00_data_prep.R done. Objects created: caract_key, datat_func, func_exp\n")

# ==============================================================================
# 5. Classification 4 groupes à l'échelle de l'expérience
#    Basé sur Concatenation (TAB_FINALE) — déjà encodé WE/WNE/HE
#
#    WNE + WE    : woody non-edible + woody edible (pas de HE)
#    WNE + HE    : woody non-edible + herbaceous edible (pas de WE)
#    Edible only : edible seulement (WE et/ou HE, sans WNE)
#    Multi-type  : combinaison WNE + WE + HE (ou autre non classifiable)
# ==============================================================================
exp_groups <- TAB_FINALE %>%
  distinct(numéro, Concatenation) %>%
  mutate(
    has_WNE = str_detect(Concatenation, "WNE"),
    has_WE  = str_detect(Concatenation, "\\bWE\\b"),   # \b évite de matcher WNE
    has_HE  = str_detect(Concatenation, "\\bHE\\b"),
    exp_type = case_when(
      has_WNE &  has_WE &  has_HE ~ "Multi-type",
      has_WNE &  has_WE & !has_HE ~ "WNE + WE",
      has_WNE & !has_WE &  has_HE ~ "WNE + HE",
      !has_WNE                    ~ "Edible only",
      TRUE                        ~ "Other"
    ),
    exp_type = factor(exp_type,
                      levels = c("WNE + WE", "WNE + HE", "Edible only", "Multi-type", "Other"))
  )

cat("\nExperiment-level functional groups (from Concatenation):\n")
print(table(exp_groups$exp_type, useNA = "ifany"))
cat(sprintf("  → %.1f%% WNE+WE | %.1f%% WNE+HE | %.1f%% Edible only | %.1f%% Multi-type\n",
            100*mean(exp_groups$exp_type=="WNE + WE"),
            100*mean(exp_groups$exp_type=="WNE + HE"),
            100*mean(exp_groups$exp_type=="Edible only"),
            100*mean(exp_groups$exp_type=="Multi-type")))

# Joindre à func_exp
func_exp <- func_exp %>%
  left_join(exp_groups %>% select(numéro, exp_type, has_WNE, has_WE, has_HE),
            by = "numéro") %>%
  filter(!is.na(exp_type))

length(func_exp$numéro)
