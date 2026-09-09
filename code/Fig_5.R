# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 1 — RICHESSE & COMPOSITION
# ═══════════════════════════════════════════════════════════════════════════════

sp_treat   <- unique(tolower(DATAT$`species treatment`))
sp_control <- unique(tolower(DATAC$`species control`))

cat("Species — Agroforestry:", length(sp_treat),
    "/ Control:", length(sp_control),
    "/ Fold:", round(length(sp_treat)/length(sp_control), 2), "\n")

fam_treat   <- na.omit(unique(DATAT$Name_matched_accepted_family))
fam_control <- na.omit(unique(DATAC$Name_matched_accepted_family))

# Vérifier la couverture IUCN
cat("Total agroforestry species:", length(sp_treat), "\n")
cat("With IUCN assessment:", nrow(iucn_t), "\n")
cat("Coverage:", round(100*nrow(iucn_t)/length(sp_treat), 1), "%\n")

# Les espèces avec évaluation IUCN sont-elles représentatives ?
FF_treat_iucn <- FF_treat %>%
  mutate(has_iucn = !is.na(IUCN) & !IUCN %in% c("","NE","DD","LC","NA"))

# Biais woodiness
cat("── Woodiness bias ──\n")
FF_treat_iucn %>%
  group_by(woodiness_low, has_iucn) %>%
  summarise(n = n(), .groups="drop") %>%
  group_by(woodiness_low) %>%
  mutate(pct_with_iucn = round(100*n/sum(n),1)) %>%
  filter(has_iucn) %>%
  select(woodiness_low, n, pct_with_iucn) %>%
  print()

# Biais edibility
cat("\n── Edibility bias ──\n")
FF_treat_iucn %>%
  group_by(is_edible, has_iucn) %>%
  summarise(n = n(), .groups="drop") %>%
  group_by(is_edible) %>%
  mutate(pct_with_iucn = round(100*n/sum(n),1)) %>%
  filter(has_iucn) %>%
  select(is_edible, n, pct_with_iucn) %>%
  print()



cat("Families — Agroforestry:", length(fam_treat),
    "/ Control:", length(fam_control), "\n")

# ── Woody / herbaceous
FF_treat <- data.frame(NAME = sp_treat) %>%
  left_join(CARACT, by = "NAME") %>%
  mutate(woodiness_low = tolower(Woodiness))

woody_n <- sum(FF_treat$woodiness_low %in% c("woody"), na.rm = TRUE)
herb_n  <- sum(FF_treat$woodiness_low %in% c("herbaceous"), na.rm = TRUE)
cat("Woody:", woody_n, "/ Herbaceous:", herb_n, "\n")

# ── Edible / non-edible / medicinal — classification propre
FF_treat <- FF_treat %>%
  mutate(
    is_edible    = !is.na(Alim) & Alim == "Alim",
    is_medicinal = !is.na(Medicinal) & Medicinal != "NA",
    is_woody     = woodiness_low == "woody",
    category = case_when(
      is_edible & is_medicinal  ~ "Edible + Medicinal",
      is_edible & !is_medicinal ~ "Edible only",
      !is_edible & is_medicinal ~ "Medicinal only",
      TRUE                      ~ "Unknown/Other"
    )
  )

cat("\n── Species use categories ──\n")
print(table(FF_treat$category))

cat("\nEdible species (total):", sum(FF_treat$is_edible), "\n")
cat("Non-edible (medicinal only + other):",
    sum(!FF_treat$is_edible & (FF_treat$is_medicinal |
                                 !is.na(FF_treat$woodiness_low))), "\n")
cat("Edible & woody:", sum(FF_treat$is_edible & FF_treat$is_woody, na.rm=TRUE), "\n")
cat("% edible that are woody:",
    round(100 * mean(FF_treat$is_woody[FF_treat$is_edible], na.rm=TRUE), 1), "\n")

# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 2 — FOLD-CHANGE FAMILLES
# ═══════════════════════════════════════════════════════════════════════════════

FAMILY_T <- DATAT %>%
  distinct(`species treatment`, Name_matched_accepted_family) %>%
  count(Name_matched_accepted_family, name = "n_treat") %>%
  rename(family = Name_matched_accepted_family)

FAMILY_C <- DATAC %>%
  distinct(`species control`, Name_matched_accepted_family) %>%
  count(Name_matched_accepted_family, name = "n_control") %>%
  rename(family = Name_matched_accepted_family)

FAMILY <- full_join(FAMILY_T, FAMILY_C, by = "family") %>%
  mutate(
    n_treat   = replace_na(n_treat,   0),
    n_control = replace_na(n_control, 0),
    fold      = round(n_treat / pmax(n_control, 1), 2)
  ) %>%
  filter(!is.na(family)) %>%
  arrange(desc(n_treat))

cat("\n── Fold-change key families ──\n")
FAMILY %>%
  filter(family %in% c("Fabaceae","Poaceae","Rosaceae")) %>%
  select(family, n_treat, n_control, fold) %>%
  print()

cat("\n── Top 10 families by n_treat (most studied) ──\n")
FAMILY %>%
  slice_head(n = 10) %>%
  select(family, n_treat, n_control, fold) %>%
  print()

cat("\n── Top 10 families by fold-change (min 5 spp in agroforestry) ──\n")
FAMILY %>%
  filter(n_treat >= 5) %>%
  arrange(desc(fold)) %>%
  slice_head(n = 10) %>%
  select(family, n_treat, n_control, fold) %>%
  print()

# Proportion de familles enrichies en agroforesterie
cat("\nFamilies with fold > 1:",
    round(100 * mean(FAMILY$fold > 1, na.rm=TRUE), 1), "%\n")

cat("\n── Families present in BOTH systems ──\n")
FAMILY_BOTH <- FAMILY %>%
  filter(n_treat > 0, n_control > 0)

cat("N families present in both:", nrow(FAMILY_BOTH), "\n")

FAMILY_BOTH %>%
  mutate(direction = case_when(
    fold > 1 ~ "Richer in agroforestry",
    fold == 1 ~ "Equal",
    fold < 1 ~ "Richer in control"
  )) %>%
  count(direction) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(desc(pct)) %>%
  print()

cat("\n── Families exclusive to agroforestry (not in control) ──\n")
cat(sum(FAMILY$n_treat > 0 & FAMILY$n_control == 0), "\n")

cat("\n── Families exclusive to control (not in agroforestry) ──\n")
cat(sum(FAMILY$n_treat == 0 & FAMILY$n_control > 0), "\n")


DATAC %>%
  distinct(`species control`, IUCN, Name_matched_accepted_family) %>%
  filter(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"),
         !is.na(Name_matched_accepted_family)) %>%
  count(Name_matched_accepted_family, sort = TRUE) %>%
  slice_head(n = 6)

# Confirmer : Meliaceae dans les contrôles, threatened ou pas ?
DATAC %>%
  distinct(`species control`, IUCN, Name_matched_accepted_family) %>%
  filter(Name_matched_accepted_family == "Meliaceae") %>%
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 3 — WORKING FARM vs EXPERIMENTAL (richesse par famille)
# NB: comparer les ESPÈCES DISTINCTES, pas les occurrences
# ═══════════════════════════════════════════════════════════════════════════════

farm_family <- DATAT %>%
  mutate(farm_type = case_when(
    str_detect(tolower(`farm type`), "experimental") ~ "Experimental",
    TRUE ~ "Working farm"
  )) %>%
  distinct(farm_type, `species treatment`, Name_matched_accepted_family) %>%
  filter(!is.na(Name_matched_accepted_family)) %>%
  group_by(farm_type, Name_matched_accepted_family) %>%
  summarise(n_sp = n_distinct(`species treatment`), .groups = "drop") %>%
  pivot_wider(names_from = farm_type, values_from = n_sp, values_fill = 0) %>%
  mutate(direction = case_when(
    `Working farm` > Experimental ~ "higher",
    `Working farm` == Experimental ~ "similar",
    TRUE ~ "lower"
  ))

cat("\n── Farm type × family direction ──\n")
farm_family %>%
  count(direction) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

cat("Total species — Experimental:",
    n_distinct(DATAT$`species treatment`[
      str_detect(tolower(DATAT$`farm type`), "experimental")]),
    "/ Working farm:",
    n_distinct(DATAT$`species treatment`[
      !str_detect(tolower(DATAT$`farm type`), "experimental")]), "\n")


FAMILLE <- CARACT %>%
  select(NAME, Name_matched_accepted_family) %>%
  rename(`species treatment` = NAME)

ratio_result <- DATAT %>%
  filter(`farm type` != "Large areas") %>%
  group_by(`farm type`, Name_matched_accepted_family) %>%
  summarise(n = n_distinct(`species treatment`), .groups = "drop") %>%
  pivot_wider(names_from = `farm type`,
              values_from = n,
              values_fill = 0) %>%
  rename(exp = `experimental site`, wf = `working farm`) %>%
  mutate(
    total = exp + wf,
    group = case_when(
      exp > wf ~ "Exp > Working",
      exp == wf ~ "Exp = Working",
      TRUE      ~ "Exp < Working"
    )
  ) %>%
  filter(wf > 0, !is.na(Name_matched_accepted_family))

# ── Chiffres TOUTES familles ──────────────────────────────────────────────────
cat("── All families with working farm presence ──\n")
ratio_result %>%
  count(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ── Chiffres familles bien représentées (>2 espèces total) ───────────────────
cat("\n── Families with >2 species total ──\n")
ratio_result %>%
  filter(total > 2) %>%
  count(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ── Chiffres familles bien représentées (>5 espèces total) ───────────────────
cat("\n── Families with >5 species total ──\n")
ratio_result %>%
  filter(total > 5) %>%
  count(group) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 4 — IUCN ESPÈCES À RISQUE
# ═══════════════════════════════════════════════════════════════════════════════

iucn_clean <- function(df, sp_col) {
  df %>%
    distinct({{ sp_col }}, IUCN) %>%
    filter(!is.na(IUCN), !IUCN %in% c("","NE","DD","LC")) %>%
    mutate(at_risk = str_detect(IUCN, "CR|EN|VU|NT"))
}

iucn_t <- iucn_clean(DATAT, `species treatment`)
iucn_c <- iucn_clean(DATAC, `species control`)

cat("\n── IUCN coverage ──\n")
cat("Agroforestry:", nrow(iucn_t), "spp assessed /",
    sum(iucn_t$at_risk), "at-risk (",
    round(100*mean(iucn_t$at_risk),1), "%)\n")
cat("Control     :", nrow(iucn_c), "spp assessed /",
    sum(iucn_c$at_risk), "at-risk (",
    round(100*mean(iucn_c$at_risk),1), "%)\n")
cat("Fold at-risk:", round(sum(iucn_t$at_risk)/sum(iucn_c$at_risk), 2), "\n")

cat("\n── Top families (at-risk species, named only) ──\n")
DATAT %>%
  distinct(`species treatment`, IUCN, Name_matched_accepted_family) %>%
  filter(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"),
         !is.na(Name_matched_accepted_family)) %>%
  count(Name_matched_accepted_family, sort = TRUE) %>%
  slice_head(n = 6) %>%
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 5 — AT-RISK AU NIVEAU EXPÉRIENCE
# Métrique : has_1plus (cohérente, robuste aux petits n_iucn)
# ═══════════════════════════════════════════════════════════════════════════════

exp_atrisk <- function(df) {
  df %>%
    group_by(numéro) %>%
    summarise(
      n_atrisk  = sum(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT")),
      has_1plus = as.integer(n_atrisk >= 1),
      .groups   = "drop"
    )
}

exp_t <- exp_atrisk(DATAT) %>% mutate(system = "Agroforestry")
exp_c <- exp_atrisk(DATAC) %>% mutate(system = "Control")

cat("\n── Experiments with ≥1 at-risk species ──\n")
bind_rows(exp_t, exp_c) %>%
  group_by(system) %>%
  summarise(
    n_exp        = n(),
    n_with_risk  = sum(has_1plus),
    pct_has_1plus = round(100 * mean(has_1plus), 1)
  ) %>% print()

# Effet richesse — utiliser TAB_FINALE pour NB_sp
cat("\n── By richness class ──\n")
exp_t %>%
  left_join(
    TAB_FINALE %>%
      distinct(numéro, NB_sp) %>%
      mutate(richness_class = case_when(
        NB_sp == 2 ~ "2",
        NB_sp == 3 ~ "3",
        NB_sp >= 4 ~ "≥4"
      ) %>% factor(levels = c("2","3","≥4"))),
    by = "numéro"
  ) %>%
  filter(!is.na(richness_class)) %>%
  group_by(richness_class) %>%
  summarise(
    n_exp         = n(),
    pct_has_1plus = round(100 * mean(has_1plus), 1)
  ) %>% print()

# Farm type — raw proportions
cat("\n── By farm type (raw) ──\n")
DATAT %>%
  mutate(farm_type = case_when(
    str_detect(tolower(`farm type`), "experimental") ~ "Experimental site",
    TRUE ~ "Working farm"
  )) %>%
  group_by(numéro, farm_type) %>%
  summarise(
    has_1plus = as.integer(
      any(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"))),
    .groups = "drop"
  ) %>%
  group_by(farm_type) %>%
  summarise(
    n_exp         = n(),
    pct_has_1plus = round(100 * mean(has_1plus), 1)
  ) %>% print()


# Dédupliquer avant le join
richness_lookup <- TAB_FINALE %>%
  distinct(numéro, NB_sp) %>%
  group_by(numéro) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(richness_class = factor(case_when(
    NB_sp == 2 ~ "2",
    NB_sp == 3 ~ "3",
    NB_sp >= 4 ~ "≥4"
  ), levels = c("2","3","≥4")))

cat("\n── By farm type × richness class (raw) ──\n")
DATAT %>%
  mutate(farm_type = case_when(
    str_detect(tolower(`farm type`), "experimental") ~ "Experimental site",
    TRUE ~ "Working farm"
  )) %>%
  left_join(richness_lookup) %>%
  filter(!is.na(richness_class)) %>%
  group_by(farm_type, richness_class, numéro) %>%
  summarise(
    has_1plus = as.integer(
      any(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"))),
    .groups = "drop"
  ) %>%
  group_by(farm_type, richness_class) %>%
  summarise(
    n_exp         = n(),
    pct_has_1plus = round(100 * mean(has_1plus), 1),
    .groups = "drop"
  ) %>%
  print()


# VERSION COHÉRENTE AVEC sum_dat — niveau New ID
exp_farmtype <- dat_treat %>%
  group_by(`New ID`, `farm type`, richness_class) %>%
  summarise(
    has_1plus = as.integer(sum(risk_presence, na.rm=TRUE) >= 1),
    .groups = "drop"
  ) %>%
  mutate(farm_type = case_when(
    str_detect(tolower(`farm type`), "experimental") ~ "Experimental site",
    TRUE ~ "Working farm"
  )) %>%
  filter(farm_type != "Large areas") %>%
  group_by(farm_type, richness_class) %>%
  summarise(
    n_studies     = n(),
    pct_has_1plus = round(100 * mean(has_1plus), 1),
    .groups = "drop"
  )

print(exp_farmtype)

# ── Dataset modèle — niveau New ID (cohérent avec sum_dat) ───────────────

# ═══════════════════════════════════════════════════════════════════════════════
# MODÈLES MIXTES — niveau New ID (cohérent avec sum_dat)
# ═══════════════════════════════════════════════════════════════════════════════

ctrl_bobyqa <- glmerControl(optimizer = "bobyqa",
                            optCtrl   = list(maxfun = 5e5))
ctrl_nelder  <- glmerControl(optimizer = "Nelder_Mead",
                             optCtrl   = list(maxfun = 5e5))
ctrl_nlopt   <- glmerControl(optimizer = "nloptwrap",
                             optCtrl   = list(algorithm = "NLOPT_LN_BOBYQA",
                                              maxeval   = 5e5))

# ── M0 — richesse seule ───────────────────────────────────────────────────────
m0 <- glmer(has_1plus ~ richness_class + (1 | country),
            data = dat_model_filt, family = binomial, control = ctrl_bobyqa)

# ── M1 — + farm type : tester 3 optimiseurs ──────────────────────────────────
m1_a <- glmer(has_1plus ~ richness_class + farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_bobyqa)

m1_b <- glmer(has_1plus ~ richness_class + farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_nelder)

m1_c <- glmer(has_1plus ~ richness_class + farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_nlopt)

cat("\n── M1 coefficient stability across optimizers ──\n")
rbind(
  bobyqa       = round(exp(fixef(m1_a)), 3),
  nelder_mead  = round(exp(fixef(m1_b)), 3),
  nlopt_bobyqa = round(exp(fixef(m1_c)), 3)
)

# Garder nlopt si bobyqa diverge (comme précédemment)
m1 <- m1_a

# ── M2 — + interaction richness × farm type ───────────────────────────────────
m2_a <- glmer(has_1plus ~ richness_class * farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_bobyqa)

m2_b <- glmer(has_1plus ~ richness_class * farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_nelder)

m2_c <- glmer(has_1plus ~ richness_class * farm_type + (1 | country),
              data = dat_model_filt, family = binomial, control = ctrl_nlopt)

cat("\n── M2 coefficient stability ──\n")
rbind(
  bobyqa       = round(exp(fixef(m2_a)), 3),
  nelder_mead  = round(exp(fixef(m2_b)), 3),
  nlopt_bobyqa = round(exp(fixef(m2_c)), 3)
)

m2 <- m2_a

# ── Comparaison des modèles ───────────────────────────────────────────────────
cat("\n── Model comparison ──\n")
anova(m0, m1, m2, test = "Chisq") %>% print()

# ── Odds ratios M1 ────────────────────────────────────────────────────────────
results_m1 <- data.frame(
  term      = names(fixef(m1)),
  estimate  = exp(fixef(m1)),
  conf.low  = exp(confint(m1, parm = "beta_", method = "Wald")[, 1]),
  conf.high = exp(confint(m1, parm = "beta_", method = "Wald")[, 2]),
  p.value   = summary(m1)$coefficients[, 4]
) %>% mutate(across(where(is.numeric), ~round(.x, 3)))

cat("\n── Fixed effects M1 (OR, 95% CI) ──\n")
print(results_m1)

# ── Odds ratios M2 ────────────────────────────────────────────────────────────
results_m2 <- data.frame(
  term      = names(fixef(m2)),
  estimate  = exp(fixef(m2)),
  conf.low  = exp(confint(m2, parm = "beta_", method = "Wald")[, 1]),
  conf.high = exp(confint(m2, parm = "beta_", method = "Wald")[, 2]),
  p.value   = summary(m2)$coefficients[, 4]
) %>% mutate(across(where(is.numeric), ~round(.x, 3)))

cat("\n── Fixed effects M2 (OR, 95% CI) ──\n")
print(results_m2)

# ── ICC ───────────────────────────────────────────────────────────────────────
cat("\nICC (country, M1):", round(icc(m1)$ICC_adjusted, 3), "\n")




cat("\n── Mixed model: farm type effect after country control ──\n")
cat("ICC =", round(icc(m1)$ICC_adjusted, 3), "\n")
print(results_m2 %>% filter(str_detect(term, "farm|richness")))

DATAT %>%
  left_join(country_lookup, by = "New ID") %>%
  mutate(country_low = tolower(trimws(country)),
         region_code = region_map[country_low],
         region      = region_labels[region_code]) %>%
  filter(!is.na(region), str_detect(replace_na(IUCN,""), "CR|EN|VU|NT")) %>%
  count(region, Name_matched_accepted_family, sort = TRUE) %>%
  group_by(region) %>%
  slice_head(n = 3)

# Vérifier : quelle espèce Rubiaceae est concernée ?
DATAT %>%
  filter(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"),
         Name_matched_accepted_family == "Rubiaceae") %>%
  distinct(`species treatment`, IUCN) %>%
  count(`species treatment`, IUCN, sort = TRUE) %>%
  print(n = 20)

# Et quelle proportion du signal total vient de Rubiaceae ?
total_atrisk_exp <- sum(exp_t$has_1plus)

rubiac_exp <- DATAT %>%
  filter(Name_matched_accepted_family == "Rubiaceae",
         str_detect(replace_na(IUCN,""), "CR|EN|VU|NT")) %>%
  distinct(numéro) %>%
  pull(numéro)

cat("Experiments with at-risk Rubiaceae:", length(rubiac_exp), "\n")
cat("Total experiments with ≥1 at-risk:", total_atrisk_exp, "\n")
cat("Rubiaceae share:", round(100*length(rubiac_exp)/total_atrisk_exp, 1), "%\n")

# Sans Rubiaceae, quel est le signal ?
exp_t_noRubiac <- DATAT %>%
  filter(!Name_matched_accepted_family %in% c("Rubiaceae") |
           is.na(Name_matched_accepted_family)) %>%
  group_by(numéro) %>%
  summarise(
    has_1plus = as.integer(
      any(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"))),
    .groups = "drop"
  )

cat("\npct_has_1plus WITHOUT Rubiaceae:",
    round(100*mean(exp_t_noRubiac$has_1plus), 1), "%\n")


# Signal contrôle sans Rubiaceae
exp_c_noRubiac <- DATAC %>%
  filter(!Name_matched_accepted_family %in% c("Rubiaceae") |
           is.na(Name_matched_accepted_family)) %>%
  group_by(numéro) %>%
  summarise(
    has_1plus = as.integer(
      any(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"))),
    .groups = "drop"
  )

cat("pct_has_1plus control WITHOUT Rubiaceae:",
    round(100*mean(exp_c_noRubiac$has_1plus), 1), "%\n")

# Est-ce que coffea arabica est aussi dans les contrôles ?
DATAC %>%
  filter(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"),
         Name_matched_accepted_family == "Rubiaceae") %>%
  distinct(`species control`, IUCN) %>%
  print()

# ═══════════════════════════════════════════════════════════════════════════════
# BLOC 6 — RÉGIONAL (has_1plus, métrique cohérente)
# ═══════════════════════════════════════════════════════════════════════════════

region_map <- c(
  "brazil"="SA","colombia"="SA","peru"="SA","ecuador"="SA",
  "bolivia"="SA","argentina"="SA","chile"="SA","venezuela"="SA",
  "paraguay"="SA","uruguay"="SA","suriname"="SA","guyana"="SA",
  "costa rica"="CAC","honduras"="CAC","guatemala"="CAC",
  "mexico"="CAC","nicaragua"="CAC","panama"="CAC","cuba"="CAC",
  "haiti"="CAC","dominican republic"="CAC","jamaica"="CAC","belize"="CAC",
  "india"="SAS","bangladesh"="SAS","sri lanka"="SAS",
  "nepal"="SAS","pakistan"="SAS","bhutan"="SAS",
  "kenya"="EAF","tanzania"="EAF","uganda"="EAF","ethiopia"="EAF",
  "rwanda"="EAF","malawi"="EAF","zambia"="EAF","mozambique"="EAF",
  "madagascar"="EAF","somalia"="EAF","eritrea"="EAF","drc"="EAF",
  "cameroon"="WAF","ghana"="WAF","nigeria"="WAF","benin"="WAF",
  "ivory coast"="WAF","senegal"="WAF","togo"="WAF","mali"="WAF",
  "burkina faso"="WAF","guinea"="WAF","congo"="WAF",
  "indonesia"="SEA","vietnam"="SEA","thailand"="SEA",
  "philippines"="SEA","malaysia"="SEA","myanmar"="SEA",
  "laos"="SEA","cambodia"="SEA","timor-leste"="SEA",
  "china"="EAS","japan"="EAS","south korea"="EAS","taiwan"="EAS"
)

region_labels <- c(
  SA  = "South America",
  CAC = "Central America & Caribbean",
  EAF = "Eastern & Middle Africa",
  WAF = "Western Africa",
  SEA = "South-East Asia",
  EAS = "East Asia",
  SAS = "Southern Asia"
)

regional_final <- DATAT %>%
  left_join(country_lookup, by = "New ID") %>%
  mutate(
    country_low = tolower(trimws(country)),
    region_code = region_map[country_low],
    region      = region_labels[region_code]
  ) %>%
  filter(!is.na(region)) %>%
  group_by(region, numéro) %>%
  summarise(
    has_1plus = as.integer(
      any(str_detect(replace_na(IUCN,""), "CR|EN|VU|NT"))),
    .groups = "drop"
  ) %>%
  group_by(region) %>%
  summarise(
    n_exp         = n(),
    pct_has_1plus = round(100 * mean(has_1plus), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_has_1plus))

cat("\n── Regional threatened species frequency ──\n")
print(regional_final)


#####
# =============================================================================
# Figure biodiversité / conservation — Nature Communications standard
# 3 panels: A = has_1plus global
#            B = has_1plus par région (agro vs control)
#            C = threatened spp par famille top 10 (dumbbell)
# Specs: 183 mm wide, 300 dpi, palette colorblind-safe
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

# ── Palette & tailles ────────────────────────────────────────────────────────
col_agro  <- "#1F6B3A"
col_ctrl  <- "#A8A8A8"
base_size <- 9
pt_size   <- 2.5
lw_ci     <- 0.5
lw_seg    <- 0.4

# ── Bootstrap CI au niveau New ID ────────────────────────────────────────────
boot_ci <- function(dat, B = 2000, conf = 0.95) {
  ids              <- unique(dat[["New ID"]])
  n_ids            <- length(ids)
  id_prop_lookup   <- tapply(dat$has_th, dat[["New ID"]], mean)
  point_est        <- mean(id_prop_lookup)
  props <- vapply(seq_len(B), function(i) {
    mean(id_prop_lookup[sample(ids, n_ids, replace = TRUE)])
  }, numeric(1))
  alpha <- (1 - conf) / 2
  data.frame(prop = point_est,
             lo   = quantile(props, alpha,     names = FALSE),
             hi   = quantile(props, 1 - alpha, names = FALSE))
}

set.seed(42)

# ── Lookup pays ──────────────────────────────────────────────────────────────
country_lookup <- DATA %>%
  select(`New ID`, country) %>%
  group_by(`New ID`) %>% slice(1) %>% ungroup()

# =============================================================================
# PANEL A — has_1plus global
# =============================================================================

make_has <- function(dat, sp_col) {
  dat %>%
    group_by(`New ID`, numéro) %>%
    summarise(has_th = as.integer(any(str_detect(replace_na(IUCN, ""), "CR|EN|VU|NT"))),
              .groups = "drop")
}

has_agro_all <- make_has(DATAT, "species treatment")
has_ctrl_all <- make_has(DATAC, "species control")

dat_A <- bind_rows(
  boot_ci(has_agro_all) %>% mutate(system = "Agroforestry"),
  boot_ci(has_ctrl_all) %>% mutate(system = "Control")
) %>%
  mutate(
    system = factor(system, levels = c("Agroforestry", "Control")),
    y_num  = ifelse(system == "Agroforestry", 2, 1)
  )

pA <- ggplot(dat_A, aes(x = prop, y = y_num, colour = system))  +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = lw_ci) +
  geom_point(size = pt_size) +
  scale_y_continuous(breaks = c(1, 2),
                     labels = c("Control", "Agroforestry"),
                     limits = c(0.4, 2.6)) +
  guides(colour = guide_legend(reverse = FALSE)) +
  scale_colour_manual(values = c("Agroforestry" = col_agro, "Control" = col_ctrl),
                      name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.30),
                     breaks = seq(0, 0.30, 0.05)) +
  labs(x = "Studies with ≥1 threatened taxon", y = NULL, tag = "A") +
  theme_classic(base_size = base_size) +
  theme(
    axis.line.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    plot.tag      = element_text(face = "bold", size = base_size + 1),
    plot.margin   = margin(4, 8, 2, 4, "mm")
  )

# =============================================================================
# PANEL B — has_1plus par région
# =============================================================================

region_map <- c(
  "Mexico"="CAC","Guatemala"="CAC","Honduras"="CAC","Costa Rica"="CAC",
  "Panama"="CAC","Nicaragua"="CAC","El Salvador"="CAC","Cuba"="CAC",
  "Dominican Republic"="CAC","Haiti"="CAC","Jamaica"="CAC",
  "Trinidad and Tobago"="CAC","Guadeloupe"="CAC","Martinique"="CAC",
  "Puerto Rico"="CAC","Belize"="CAC",
  "Brazil"="SA","Colombia"="SA","Peru"="SA","Ecuador"="SA",
  "Bolivia"="SA","Venezuela"="SA","Argentina"="SA","Chile"="SA",
  "Paraguay"="SA","Uruguay"="SA","Guyana"="SA","Suriname"="SA","French Guiana"="SA",
  "Kenya"="EAF","Tanzania"="EAF","Uganda"="EAF","Ethiopia"="EAF",
  "Rwanda"="EAF","Burundi"="EAF","Malawi"="EAF","Zambia"="EAF",
  "Zimbabwe"="EAF","Mozambique"="EAF","Madagascar"="EAF","Comoros"="EAF",
  "Ghana"="WAF","Ivory Coast"="WAF","Cameroon"="WAF","Nigeria"="WAF",
  "Senegal"="WAF","Mali"="WAF","Burkina Faso"="WAF","Togo"="WAF",
  "Benin"="WAF","Guinea"="WAF","Sierra Leone"="WAF","Liberia"="WAF",
  "Congo"="WAF","Democratic Republic of the Congo"="WAF","Gabon"="WAF",
  "Equatorial Guinea"="WAF","Central African Republic"="WAF","Cote d'Ivoire"="WAF",
  "Indonesia"="SEA","Philippines"="SEA","Vietnam"="SEA","Thailand"="SEA",
  "Malaysia"="SEA","Myanmar"="SEA","Laos"="SEA","Cambodia"="SEA","Timor-Leste"="SEA",
  "China"="EAS","Japan"="EAS","South Korea"="EAS","Taiwan"="EAS",
  "India"="SAS","Bangladesh"="SAS","Sri Lanka"="SAS","Nepal"="SAS",
  "Pakistan"="SAS","Afghanistan"="SAS"
)

region_labels <- c(
  "CAC"="Cent. Am. & Caribbean", "SA"="South America",
  "EAF"="East Africa",           "WAF"="West Africa",
  "SEA"="Southeast Asia",        "EAS"="East Asia",
  "SAS"="South Asia"
)

region_order <- c("CAC","SA","EAF","WAF","SEA","EAS","SAS")

make_has_reg <- function(dat) {
  dat %>%
    left_join(country_lookup, by = "New ID") %>%
    mutate(region = region_map[country]) %>%
    filter(!is.na(region)) %>%
    group_by(`New ID`, numéro, region) %>%
    summarise(has_th = as.integer(any(str_detect(replace_na(IUCN, ""), "CR|EN|VU|NT"))),
              .groups = "drop")
}

has_agro_reg_raw <- make_has_reg(DATAT)
has_ctrl_reg_raw <- make_has_reg(DATAC)

boot_ci_reg <- function(dat_reg, sys_label) {
  map_dfr(split(dat_reg, dat_reg$region), function(d) {
    boot_ci(d) %>% mutate(region = unique(d$region), n_ids = length(unique(d[["New ID"]])))
  }) %>% mutate(system = sys_label)
}

has_agro_reg <- boot_ci_reg(has_agro_reg_raw, "Agroforestry")
has_ctrl_reg <- boot_ci_reg(has_ctrl_reg_raw, "Control")

dodge_B <- 0.32

dat_B <- bind_rows(has_agro_reg, has_ctrl_reg) %>%
  mutate(
    region     = factor(region, levels = rev(region_order)),
    region_lab = factor(region_labels[as.character(region)],
                        levels = rev(region_labels[region_order])),
    system     = factor(system, levels = c("Agroforestry", "Control")),
    y_base     = as.numeric(region_lab),
    y_dodge    = y_base + ifelse(system == "Agroforestry", dodge_B / 2, -dodge_B / 2)
  )

seg_B <- dat_B %>%
  group_by(region_lab, y_base) %>%
  summarise(x0 = prop[system == "Control"],
            x1 = prop[system == "Agroforestry"],
            .groups = "drop")

n_annot_B <- has_agro_reg %>%
  mutate(region_lab = factor(region_labels[region],
                             levels = rev(region_labels[region_order])),
         y_base = as.numeric(region_lab),
         label  = paste0("n=", n_ids))

pB <- ggplot(dat_B, aes(x = prop, y = y_dodge, colour = system)) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0, linewidth = lw_ci) +
  geom_point(size = pt_size) +
  geom_text(data = n_annot_B,
            aes(x = 0.66, y = y_base, label = label),
            colour = "grey50", size = 2.5, hjust = 0, inherit.aes = FALSE) +
  scale_y_continuous(breaks = seq_len(nlevels(dat_B$region_lab)),
                     labels = levels(dat_B$region_lab)) +
  scale_colour_manual(values = c("Agroforestry" = col_agro, "Control" = col_ctrl),
                      name = NULL) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.72),
                     breaks = seq(0, 0.6, 0.2)) +
  labs(x = "Studies with ≥1 threatened taxon", y = NULL, tag = "B") +
  theme_classic(base_size = base_size) +
  theme(
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.tag     = element_text(face = "bold", size = base_size + 1),
    plot.margin  = margin(2, 8, 2, 4, "mm")
  )

# =============================================================================
# PANEL C — Threatened species par famille top 10 (dumbbell)
# =============================================================================

threat_agro_fam <- DATAT %>%
  filter(str_detect(replace_na(IUCN, ""), "CR|EN|VU|NT")) %>%
  distinct(`species treatment`, Name_matched_accepted_family) %>%
  count(Name_matched_accepted_family, name = "n_agro")

threat_ctrl_fam <- DATAC %>%
  filter(str_detect(replace_na(IUCN, ""), "CR|EN|VU|NT")) %>%
  distinct(`species control`, Name_matched_accepted_family) %>%
  count(Name_matched_accepted_family, name = "n_ctrl")

dat_C <- full_join(threat_agro_fam, threat_ctrl_fam,
                   by = "Name_matched_accepted_family") %>%
  filter(!is.na(Name_matched_accepted_family)) %>%
  replace_na(list(n_agro = 0, n_ctrl = 0)) %>%
  arrange(desc(n_agro)) %>%
  slice_head(n = 10) %>%
  mutate(family = factor(Name_matched_accepted_family,
                         levels = rev(Name_matched_accepted_family))) %>%
  pivot_longer(c(n_agro, n_ctrl), names_to = "system", values_to = "n") %>%
  mutate(system = recode(system, n_agro = "Agroforestry", n_ctrl = "Control"),
         system = factor(system, levels = c("Agroforestry", "Control")))

seg_C <- dat_C %>%
  pivot_wider(id_cols = family, names_from = system, values_from = n) %>%
  rename(agro = Agroforestry, ctrl = Control)

pC <- ggplot(dat_C, aes(x = n, y = family, colour = system)) +
  geom_segment(data = seg_C,
               aes(x = ctrl, xend = agro, y = family, yend = family),
               colour = "grey80", linewidth = lw_seg, inherit.aes = FALSE) +
  geom_point(size = pt_size) +
  scale_colour_manual(values = c("Agroforestry" = col_agro, "Control" = col_ctrl),
                      name = NULL) +
  scale_x_continuous(breaks = scales::breaks_width(1),
                     minor_breaks = NULL) +
  labs(x = "Threatened species (n)", y = NULL, tag = "C") +
  theme_classic(base_size = base_size) +
  theme(
    axis.text.y  = element_text(face = "italic"),
    axis.line.y  = element_blank(),
    axis.ticks.y = element_blank(),
    plot.tag     = element_text(face = "bold", size = base_size + 1),
    plot.margin  = margin(2, 8, 4, 4, "mm")
  )
# =============================================================================
# ASSEMBLAGE patchwork
# =============================================================================

fig <- (pA / pB / pC) +
  plot_layout(heights = c(0.7, 1.6, 1.8), guides = "collect") &
  theme(
    legend.position    = "bottom",
    legend.key.size    = unit(4, "mm"),
    legend.text        = element_text(size = base_size),
    legend.title       = element_blank(),
    panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.25)
  )

# Supprimer les légendes dupliquées de B et C — ne garder que celle de A
fig[[2]] <- fig[[2]] + theme(legend.position = "none")
fig[[3]] <- fig[[3]] + theme(legend.position = "none")

# =============================================================================
# EXPORT
# =============================================================================

w_mm <- 183
h_mm <- 200

cairo_pdf("fig_biodiv_natcomm.pdf",
          width = w_mm / 25.4, height = h_mm / 25.4,
          family = "Helvetica")
print(fig)
dev.off()

ragg::agg_png("fig_biodiv_natcomm.png",
              width = w_mm, height = h_mm, units = "mm",
              res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_biodiv_natcomm.pdf + .png — ", w_mm, "×", h_mm, " mm")

