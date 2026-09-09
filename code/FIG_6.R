# =============================================================================
# Figure ecosystem services — Nature Communications standard
# Panel A : fréquence individuelle des services, coloré par catégorie
# Panel B : UpSet top 12 combinaisons, barres colorées par nb de services
# Specs : 183 mm wide, 300 dpi, 9 pt base
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(ComplexUpset)
library(ragg)

# ── Palette catégories (colorblind-safe) ─────────────────────────────────────
col_prov <- "#E69F00"   # Provisioning — orange
col_reg  <- "#56B4E9"   # Regulating   — bleu ciel
col_supp <- "#009E73"   # Supporting   — vert
base_size <- 9

# ── Noms propres des services ─────────────────────────────────────────────────
service_cols <- c(
  "Carbon Storage", "Habitat/Biodiversity", "Pest Regulation",
  "Food Provision", "Water regulation", "Healthy Soils",
  "Nutrient Cycling", "Photosynthesis", "Materials", "Natural Medecine"
)

# Catégorie par service
service_cat <- tribble(
  ~service,               ~category,
  "Food Provision",       "Provisioning",
  "Materials",            "Provisioning",
  "Natural Medecine",     "Provisioning",
  "Carbon Storage",       "Regulating",
  "Pest Regulation",      "Regulating",
  "Water regulation",     "Regulating",
  "Habitat/Biodiversity", "Supporting",
  "Healthy Soils",        "Supporting",
  "Nutrient Cycling",     "Supporting",
  "Photosynthesis",       "Supporting"
) %>%
  mutate(category = factor(category, levels = c("Provisioning", "Regulating", "Supporting")))

cat_colors <- c("Provisioning" = col_prov, "Regulating" = col_reg, "Supporting" = col_supp)

# ── Préparer final_results ────────────────────────────────────────────────────
fr <- final_results %>%
  mutate(across(all_of(service_cols), ~ as.integer(. > 0)))

n_studies <- nrow(fr)

# =============================================================================
# PANEL A — Fréquence individuelle des services
# =============================================================================

dat_A <- fr %>%
  summarise(across(all_of(service_cols), ~ sum(.) / n_studies * 100)) %>%
  pivot_longer(everything(), names_to = "service", values_to = "pct") %>%
  left_join(service_cat, by = "service") %>%
  arrange(pct) %>%
  mutate(service = factor(service, levels = service))

pA <- ggplot(dat_A, aes(x = pct, y = service, fill = category)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = paste0(round(pct, 0), "%")),
            hjust = -0.15, size = 2.8, colour = "grey30") +
  scale_fill_manual(values = cat_colors, name = NULL) +
  scale_x_continuous(limits = c(0, 60),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.01))) +
  labs(x = "Studies assessing each service (%)", y = NULL, tag = "A") +
  theme_classic(base_size = base_size) +
  theme(
    axis.line.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    legend.position = c(0.72, 0.18),
    legend.key.size = unit(3.5, "mm"),
    legend.text     = element_text(size = 7.5),
    plot.tag        = element_text(face = "bold", size = base_size + 1),
    plot.margin     = margin(4, 6, 2, 4, "mm"),
    panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.25)
  )

# =============================================================================
# PANEL B — UpSet manuel ggplot2
# Top 12 combinaisons, barres colorées par nb de services
# =============================================================================

col_multi <- c("Single"        = "grey60",
               "Two"           = "#6BAED6",
               "Three or more" = "#2171B5")

# Préparer les données
fr_bin <- fr %>%
  select(all_of(service_cols)) %>%
  mutate(nb_services = rowSums(across(all_of(service_cols)))) %>%
  filter(nb_services > 0) %>%
  mutate(multifunc = case_when(
    nb_services == 1 ~ "Single",
    nb_services == 2 ~ "Two",
    nb_services >= 3 ~ "Three or more"
  ),
  multifunc = factor(multifunc, levels = c("Single", "Two", "Three or more")),
  combo = apply(across(all_of(service_cols)), 1,
                function(x) paste(service_cols[x == 1], collapse = "|")))

# Top 12 combinaisons
top_combos <- fr_bin %>%
  count(combo, multifunc) %>%
  arrange(desc(n)) %>%
  slice_head(n = 12) %>%
  mutate(rank = row_number())

# Matrice service × combinaison
mat <- top_combos %>%
  mutate(services = strsplit(combo, "\\|")) %>%
  unnest(services) %>%
  mutate(present = 1)

# Ordre des services : par fréquence décroissante (même ordre que Panel A)
service_order <- dat_A %>% arrange(desc(pct)) %>% pull(service) %>% as.character()

mat <- mat %>%
  mutate(services = factor(services, levels = rev(service_order)))

# Labels x : numéro de rang
rank_labels <- top_combos %>%
  mutate(label = paste0(n)) %>%
  select(rank, label, n, multifunc)

# ── Barre haute ──────────────────────────────────────────────────────────────
p_bar <- ggplot(top_combos, aes(x = rank, y = n, fill = multifunc)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = n), vjust = -0.4, size = 2.5, colour = "grey30") +
  scale_fill_manual(values = col_multi, name = "Services assessed") +
  scale_x_continuous(breaks = 1:12, expand = expansion(add = 0.6)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(y = "Studies (n)", x = NULL) +
  theme_classic(base_size = base_size) +
  theme(
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    axis.line.x       = element_blank(),
    panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.25),
    legend.position   = c(0.75, 0.75),
    legend.key.size   = unit(3.5, "mm"),
    legend.text       = element_text(size = 7.5),
    plot.margin       = margin(4, 4, 0, 4, "mm")
  )

# ── Matrice de points ─────────────────────────────────────────────────────────
# Points pleins = présent, vides = absent
all_cells <- expand.grid(rank = 1:12,
                         services = factor(service_order, levels = rev(service_order)),
                         stringsAsFactors = FALSE) %>%
  left_join(mat %>% select(rank, services, present), by = c("rank", "services")) %>%
  mutate(present = replace_na(present, 0))

# Segments verticaux reliant les points présents par combinaison
seg_data <- mat %>%
  group_by(rank) %>%
  summarise(
    y_min = min(as.numeric(services)),
    y_max = max(as.numeric(services)),
    .groups = "drop"
  ) %>%
  filter(y_max > y_min)

p_mat <- ggplot(all_cells, aes(x = rank, y = services)) +
  # Bandes alternées
  annotate("rect",
           xmin = seq(0.5, 11.5, 2), xmax = seq(1.5, 12.5, 2),
           ymin = -Inf, ymax = Inf, fill = "grey97", alpha = 0.8) +
  # Segments verticaux
  geom_segment(data = seg_data,
               aes(x = rank, xend = rank,
                   y = y_min, yend = y_max),
               colour = "grey30", linewidth = 0.5, inherit.aes = FALSE) +
  # Points absents
  geom_point(data = filter(all_cells, present == 0),
             shape = 21, size = 3, fill = "grey88", colour = "grey75", stroke = 0.3) +
  # Points présents
  geom_point(data = filter(all_cells, present == 1),
             shape = 21, size = 3, fill = "grey25", colour = "grey25") +
  scale_x_continuous(breaks = 1:12, expand = expansion(add = 0.6)) +
  scale_y_discrete() +
  labs(x = "Combination rank", y = NULL, tag = "B") +
  theme_classic(base_size = base_size) +
  theme(
    axis.line.y   = element_blank(),
    axis.ticks.y  = element_blank(),
    axis.text.x   = element_blank(),
    axis.ticks.x  = element_blank(),
    axis.line.x   = element_blank(),
    plot.tag      = element_text(face = "bold", size = base_size + 1),
    plot.margin   = margin(0, 4, 4, 4, "mm")
  )

pB <- p_bar / p_mat + plot_layout(heights = c(1.2, 1))

# =============================================================================
# ASSEMBLAGE & EXPORT
# =============================================================================

fig <- pA | pB +
  plot_layout(widths = c(1, 1.6))

w_mm <- 183
h_mm <- 130

cairo_pdf("fig_ecosystem_services.pdf",
          width = w_mm / 25.4, height = h_mm / 25.4,
          family = "Helvetica")
print(fig)
dev.off()

agg_png("fig_ecosystem_services.png",
        width = w_mm, height = h_mm, units = "mm",
        res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_ecosystem_services.png — ", w_mm, "×", h_mm, " mm")



# N total
n_tot <- nrow(fr)

# % par service
fr %>%
  summarise(across(all_of(service_cols), ~ round(mean(.) * 100, 1))) %>%
  pivot_longer(everything(), names_to = "service", values_to = "pct") %>%
  arrange(desc(pct))

# % par catégorie (au moins 1 service de la catégorie)
fr %>% mutate(
  provisioning = as.integer((`Food Provision` + Materials + `Natural Medecine`) > 0),
  regulating   = as.integer((`Carbon Storage` + `Pest Regulation` + `Water regulation`) > 0),
  supporting   = as.integer((`Habitat/Biodiversity` + `Healthy Soils` + `Nutrient Cycling` + Photosynthesis) > 0)
) %>%
  summarise(across(c(provisioning, regulating, supporting), ~ round(mean(.) * 100, 1)))

# % études par nb de services
fr %>%
  mutate(nb = rowSums(across(all_of(service_cols)))) %>%
  count(nb) %>%
  mutate(pct = round(n / n_tot * 100, 1))
