# ==============================================================================
# FIGURE 4 — FAO GROUPS CO-OCCURRENCE (style IPCC/IPBES)
# Unité d'analyse : expérience (numéro), filtré sur TAB_FINALE
# Prérequis : DATAT, CARACT, TAB_FINALE déjà en mémoire
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(stringr)

ipcc_palette <- c("#94a3b8", "#0f766e", "#ca8a04", "#b91c1c")
color_dark   <- "#1e293b"

# ==============================================================================
# 1. CONSTRUCTION DE TAB_FAO
#    DATAT est déjà filtré sur TAB_FINALE$numéro → dénominateur correct
# ==============================================================================

FAO_ref <- CARACT %>%
  select(NAME, FAO_GROUPII) %>%
  rename(`species treatment` = NAME)

TAB_FAO <- DATAT %>%
  left_join(FAO_ref, by = "species treatment") %>%
  select(`species treatment`, FAO_GROUPII, numéro) %>%
  mutate(FAO_GROUPII = str_replace_all(FAO_GROUPII, c("and" = ",", "/" = ","))) %>%
  separate_rows(FAO_GROUPII, sep = ",") %>%
  mutate(FAO_GROUPII = tolower(str_trim(FAO_GROUPII)))

# ==============================================================================
# 2. STANDARDISATION DES CATÉGORIES FAO
# ==============================================================================

category_mapping <- c(
  "spices"             = "spices",
  "spice"              = "spices",
  "aromatic"           = "aromatic",
  "essential oil"      = "aromatic",
  "root"               = "roots",
  "tuber"              = "tubers",
  "no_fao_group"       = "other",
  "cereals"            = "cereals",
  "leguminous crops"   = "legumes",
  "pulses"             = "legumes",
  "stimulant"          = "stimulant",
  "tobacco"            = "stimulant",
  "fuelwood"           = "forest products",
  "firewood"           = "forest products",
  "timber"             = "forest products",
  "rubber"             = "forest products",
  "materials"          = "forest products",
  "gum"                = "forest products",
  "oil crop"           = "oil crops",
  "oilcrops"           = "oil crops",
  "oleaginous fruits"  = "oil crops",
  "oilseed crops"      = "oil crops",
  "fruits"             = "fruits",
  "fruit"              = "fruits",
  "tropical fresh nes" = "fruits",
  "nuts"               = "nuts/seed",
  "seed"               = "nuts/seed",
  "seeds"              = "nuts/seed",
  "pods"               = "nuts/seed",
  "vegetables"         = "vegetables",
  "vegetable"          = "vegetables",
  "fodder"             = "forage and fodder",
  "fooder"             = "forage and fodder",
  "forage"             = "forage and fodder",
  "forages"            = "forage and fodder",
  "fibres"             = "industrial crops",
  "fiber"              = "industrial crops",
  "industrial crop"    = "industrial crops",
  "sugar crops"        = "industrial crops",
  "other crops"        = "other",
  "other"              = "other",
  "others"             = "other",
  "windbreaks"         = "other",
  "colorant"           = "other",
  "industrial crops"   = "other",
  "erosion control"    = "other",
  "multipurpose"       = "other"
)

TAB_FAO <- TAB_FAO %>%
  mutate(FAO_GROUPII = recode(FAO_GROUPII, !!!category_mapping))

# ==============================================================================
# 3. GROUPES À CONSERVER (≥ 10 espèces uniques, hors "other")
# ==============================================================================

fao_summary <- TAB_FAO %>%
  filter(!is.na(FAO_GROUPII), FAO_GROUPII != "") %>%
  distinct(`species treatment`, FAO_GROUPII) %>%
  count(FAO_GROUPII, name = "Count") %>%
  mutate(FAO_GROUPII = ifelse(Count < 10, "other", FAO_GROUPII)) %>%
  group_by(FAO_GROUPII) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  arrange(desc(Count))

fao_groups_keep <- fao_summary %>%
  filter(FAO_GROUPII != "other") %>%
  pull(FAO_GROUPII)

cat("Groupes FAO conservés :", length(fao_groups_keep), "\n")
print(fao_summary)

# ==============================================================================
# 4. MATRICE BINAIRE expérience × groupe FAO
# ==============================================================================

fao_binary <- TAB_FAO %>%
  filter(FAO_GROUPII %in% fao_groups_keep) %>%
  distinct(numéro, FAO_GROUPII) %>%
  mutate(present = 1L) %>%
  pivot_wider(id_cols     = numéro,
              names_from  = FAO_GROUPII,
              values_from = present,
              values_fill = 0L)

# Vérification du dénominateur
cat("\nn expériences dans fao_binary :", nrow(fao_binary))
cat("\nn expériences dans TAB_FINALE :", n_distinct(TAB_FINALE$numéro), "\n")
# La différence = expériences sans aucune espèce dans les groupes conservés (normal)

# Ordre des groupes : du plus fréquent au moins fréquent
fao_groups <- intersect(fao_groups_keep, names(fao_binary))
group_freq  <- colSums(fao_binary[, fao_groups])
fao_groups  <- names(sort(group_freq, decreasing = TRUE))

mat <- as.matrix(fao_binary[, fao_groups])

# ==============================================================================
# 5. PANEL A — MATRICE DE CO-OCCURRENCE
# ==============================================================================

co_mat <- crossprod(mat)
diag(co_mat) <- 0

co_df <- as.data.frame(as.table(co_mat)) %>%
  rename(Group_1 = Var1, Group_2 = Var2, Co_occurrence = Freq) %>%
  mutate(
    Group_1 = factor(Group_1, levels = fao_groups),
    Group_2 = factor(Group_2, levels = fao_groups)
  ) %>%
  filter(as.integer(Group_1) > as.integer(Group_2),
         Co_occurrence > 0)

panel_a <- ggplot(co_df, aes(x = Group_1, y = Group_2)) +
  geom_hline(yintercept = seq_along(fao_groups), color = "#f1f5f9", linewidth = 0.4) +
  geom_vline(xintercept = seq_along(fao_groups), color = "#f1f5f9", linewidth = 0.4) +
  geom_point(aes(size = Co_occurrence, color = Co_occurrence), alpha = 0.9) +
  geom_text(aes(label = Co_occurrence), size = 2.5, color = "white", fontface = "bold") +
  scale_size_continuous(range = c(4, 14), guide = "none") +
  scale_color_gradientn(colors = c("#e2e8f0", ipcc_palette), name = "Experiments") +
  theme_minimal(base_family = "Arial") +
  labs(x = NULL, y = NULL,
       title = "a  Pairwise Co-occurrences of FAO Groups") +
  theme(
    text            = element_text(color = color_dark),
    plot.title      = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y     = element_text(size = 9, face = "bold"),
    panel.grid      = element_blank(),
    legend.position = "right",
    legend.title    = element_text(size = 9, face = "bold"),
    legend.text     = element_text(size = 8)
  )

# ==============================================================================
# 6. PANEL B — DIVERSITÉ FAO PAR EXPÉRIENCE
# ==============================================================================

complexity_fao <- fao_binary %>%
  mutate(N_groups = rowSums(across(all_of(fao_groups)))) %>%
  mutate(Class = case_when(
    N_groups == 1 ~ "1 group",
    N_groups == 2 ~ "2 groups",
    N_groups == 3 ~ "3 groups",
    N_groups >= 4 ~ "4+ groups"
  )) %>%
  mutate(Class = factor(Class, levels = c("1 group", "2 groups",
                                          "3 groups", "4+ groups"))) %>%
  count(Class, name = "Count") %>%
  mutate(Percentage = Count / sum(Count) * 100)

cat("\nDistribution FAO par expérience :\n")
print(complexity_fao)
cat("Total :", sum(complexity_fao$Count), "\n")

panel_b <- ggplot(complexity_fao, aes(x = Class, y = Percentage, fill = Class)) +
  geom_col(width = 0.6, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")),
    vjust = -0.3, size = 3, fontface = "bold", color = color_dark, lineheight = 0.9
  ) +
  scale_fill_manual(values = c(
    "1 group"   = "#94a3b8",
    "2 groups"  = "#0f766e",
    "3 groups"  = "#ca8a04",
    "4+ groups" = "#b91c1c"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  theme_minimal(base_family = "Arial") +
  labs(x = NULL, y = "Percentage of Experiments (%)",
       title = "b  FAO Group Diversity per Experiment") +
  theme(
    text               = element_text(color = color_dark),
    plot.title         = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.x        = element_text(size = 9, face = "bold", color = color_dark),
    axis.title.y       = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.line.y        = element_line(color = "black", linewidth = 0.4),
    legend.position    = "none"
  )

# ==============================================================================
# 7. ASSEMBLAGE ET EXPORT
# ==============================================================================

combined_plot <- panel_a / panel_b +
  plot_layout(heights = c(2, 1))

# PDF
ggsave("FAO_Groups_Figure4.pdf",
       plot = combined_plot,
       width = 180, height = 220, units = "mm",
       dpi = 300, device = "pdf")

# PNG
if (requireNamespace("ragg", quietly = TRUE)) {
  library(ragg)
  agg_png("FAO_Groups_Figure4.png",
          width = 180, height = 220, units = "mm",
          res = 300, background = "white")
  print(combined_plot)
  dev.off()
} else {
  ggsave("FAO_Groups_Figure4.png",
         plot = combined_plot,
         width = 180, height = 220, units = "mm", dpi = 300)
}

message("Figure 4 générée (PDF + PNG).")
