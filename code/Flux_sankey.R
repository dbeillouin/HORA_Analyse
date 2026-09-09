library(tidyverse)
library(ggalluvial)

# -----------------------------
# 1. Data
# -----------------------------
alluv_data <- heat_data %>%

  mutate(
    horti_c = pmin(horti, 10),
    woody_c = pmin(woody_species, 10),
    total_c = pmin(horti + woody_species, 10)
  )


# ============================================================
# ALLUVIAL — couleur par segment de transition + gradient n
# Nature Communications quality
# ============================================================
# Logique clé : geom_flow() dessine les flux SEGMENT PAR SEGMENT
#   (contrairement à geom_alluvium qui fait une seule bande continue)
#   → after_stat(x) vaut 1 pour le flux horti→woody
#                        2 pour le flux woody→total
# ============================================================

library(ggplot2)
library(ggalluvial)
library(dplyr)
library(scales)
# install.packages("ggnewscale")   # décommenter si besoin
library(ggnewscale)

# ── Thème commun Nature ──────────────────────────────────────
theme_natcomm <- function() {
  theme_classic(base_size = 11, base_family = "Helvetica") %+replace%
    theme(
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA),
      plot.title        = element_text(face = "bold", size = 11, hjust = 0,
                                       margin = margin(b = 4)),
      plot.caption      = element_text(color = "grey50", size = 7.5,
                                       hjust = 0, margin = margin(t = 6)),
      plot.margin       = margin(12, 14, 8, 10),
      axis.text.x       = element_text(face = "bold", size = 10,
                                       color = "grey10", lineheight = 1.2),
      axis.text.y       = element_text(size = 9, color = "grey30"),
      axis.title.y      = element_text(size = 9.5, color = "grey20",
                                       angle = 90, margin = margin(r = 6)),
      axis.ticks.x      = element_blank(),
      axis.line         = element_line(color = "grey70", linewidth = 0.4),
      panel.grid        = element_blank(),
      legend.text       = element_text(size = 9),
      legend.title      = element_text(size = 9, face = "bold"),
      legend.key.size   = unit(4, "mm"),
      legend.box        = "vertical",
      legend.margin     = margin(0, 0, 0, 6)
    )
}

# Couche strata commune (gris neutre fixe, hors aes → pas de conflit d'échelle)
strata_layer <- list(
  geom_stratum(
    fill      = "grey90",
    color     = "grey40",
    width     = 1 / 10,
    linewidth = 0.35
  ),
  geom_label(
    stat          = "stratum",
    aes(label     = after_stat(stratum)),
    size          = 2.8,
    fontface      = "bold",
    color         = "grey10",
    fill          = scales::alpha("white", 0.88),
    label.size    = 0,
    label.padding = unit(0.15, "lines")
  )
)

# Axes X/Y communs
axes_scales <- list(
  scale_x_discrete(
    limits = c("horti_c", "woody_c", "total_c"),
    labels = c("Horticultural\nrichness",
               "Woody\nrichness",
               "Total\nrichness"),
    expand = c(0.13, 0.13)
  ),
  scale_y_continuous(
    name   = "Number of systems",
    expand = expansion(mult = c(0, 0.05))
  )
)

aes_base <- aes(axis1 = horti_c, axis2 = woody_c, axis3 = total_c, y = 1)

# ── PALETTE ─────────────────────────────────────────────────
# Bleus séquentiels (ColorBrewer Blues)  : horti → woody
blues   <- c("#c6dbef", "#4292c6", "#08306b")
# Oranges-rouges séquentiels (Oranges)   : woody → total
oranges <- c("#fdd0a2", "#e6550d", "#7f2704")


# ════════════════════════════════════════════════════════════
# OPTION A – Simple : couleur discrète par segment +
#            dégradé alpha proportionnel à n
# ════════════════════════════════════════════════════════════
#   ✓ Sans package supplémentaire
#   ✓ Une seule légende claire
#   ✓ Flot large = couleur plus opaque (= dégradé de fait)

p_A <- ggplot(alluv_data, aes_base) +

  geom_flow(
    aes(
      fill  = after_stat(factor(x)),   # "1" = seg1, "2" = seg2
      alpha = after_stat(count)        # n → opacité (gradient perceptuel)
    ),
    stat     = "flow",
    width    = 1 / 10,
    knot.pos = 0.4
  ) +

  strata_layer +

  scale_fill_manual(
    values = c("1" = "#2166AC",   # bleu  : horti → woody
               "2" = "#B2182B"),  # rouge : woody → total
    labels = c("1" = "Horticultural → Woody",
               "2" = "Woody → Total"),
    name  = "Transition",
    guide = guide_legend(
      override.aes = list(alpha = 0.85),
      keywidth = unit(8, "mm"), keyheight = unit(4, "mm")
    )
  ) +

  scale_alpha_continuous(
    range = c(0.20, 0.90),
    name  = "N systems",
    guide = guide_colourbar(
      barwidth  = unit(0.4, "cm"),
      barheight = unit(2.5, "cm")
    )
  ) +

  axes_scales +

  labs(
    title   = "Structural composition of horticultural agroforestry systems",
    caption = "Flow width and opacity both proportional to number of systems."
  ) +

  theme_natcomm() +
  theme(legend.position = "right")


# ════════════════════════════════════════════════════════════
# OPTION B – Avancée : deux vrais dégradés de couleur
#            (nécessite ggnewscale)
#
#   Principe : deux couches geom_flow superposées.
#   Chaque couche = un segment rendu opaque,
#   l'autre masqué par alpha = 0.
#   new_scale_fill() permet deux échelles fill indépendantes.
#
#   after_stat(x) == 1 → flux horti→woody
#   after_stat(x) == 2 → flux woody→total
# ════════════════════════════════════════════════════════════

p_B <- ggplot(alluv_data, aes_base) +

  strata_layer +

  # ── Flux 1 : horti → woody (dégradé bleu) ──────────────
  geom_flow(
    aes(
      fill  = after_stat(count),
      # alpha = 1 si segment 1, 0 si segment 2 (masqué)
      alpha = after_stat(as.numeric(round(x) == 1))
    ),
    stat     = "flow",
    width    = 1 / 10,
    knot.pos = 0.4
  ) +

  scale_fill_gradientn(
    colours = blues,
    name    = "N systems\n(Horti → Woody)",
    guide   = guide_colourbar(
      barwidth  = unit(0.4, "cm"),
      barheight = unit(2.2, "cm"),
      title.position = "top"
    )
  ) +
  scale_alpha_identity(guide = "none") +  # alpha 0/1, pas de légende

  new_scale_fill() +   # <-- reset de l'échelle fill

  # ── Flux 2 : woody → total (dégradé orange-rouge) ──────
  geom_flow(
    aes(
      fill  = after_stat(count),
      alpha = after_stat(as.numeric(round(x) == 2))
    ),
    stat     = "flow",
    width    = 1 / 10,
    knot.pos = 0.4
  ) +

  scale_fill_gradientn(
    colours = oranges,
    name    = "N systems\n(Woody → Total)",
    guide   = guide_colourbar(
      barwidth  = unit(0.4, "cm"),
      barheight = unit(2.2, "cm"),
      title.position = "top"
    )
  ) +
  scale_alpha_identity(guide = "none") +

  axes_scales +

  labs(
    title   = "Structural composition of horticultural agroforestry systems",
    caption = "Flow width and colour intensity proportional to number of systems.\nBlue: horticultural → woody transition. Orange–red: woody → total transition."
  ) +

  theme_natcomm() +
  theme(legend.position = "right")


# ── Affichage ────────────────────────────────────────────────
print(p_A)
print(p_B)


# ── Export ──────────────────────────────────────────────────
for (nm in c("A", "B")) {
  p_obj <- get(paste0("p_", nm))

  ggsave(
    filename = paste0("sankey_natcomm_opt", nm, ".pdf"),
    plot     = p_obj,
    width    = 180, height = 120, units = "mm",
    device   = cairo_pdf
  )
  ggsave(
    filename    = paste0("sankey_natcomm_opt", nm, ".tiff"),
    plot        = p_obj,
    width       = 180, height = 120, units = "mm",
    dpi         = 600,
    compression = "lzw"
  )
}

message("✓ Exports Option A et B générés (PDF + TIFF 600 dpi).")




library(dplyr)

# Total des systèmes dans ton jeu de données
N_total <- nrow(alluv_data)

# ── 1. POURCENTAGES : Transition 1 (Horti ➔ Woody) ─────────────────
flux_step1 <- alluv_data %>%
  group_by(horti_c, woody_c) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    pct = (count / N_total) * 100,
    label_pct = paste0(round(pct, 1), "%")
  ) %>%
  # On filtre pour ne garder que ceux > 5%
 # filter(pct > 5) %>%
  arrange(desc(pct))

# ── 2. POURCENTAGES : Transition 2 (Woody ➔ Total) ─────────────────
flux_step2 <- alluv_data %>%
  group_by(woody_c, total_c) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(
    pct = (count / N_total) * 100,
    label_pct = paste0(round(pct, 1), "%")
  ) %>%
  # On filtre pour ne garder que ceux > 5%
 # filter(pct > 5) %>%
  arrange(desc(pct))

# ── 3. Affichage des résultats dans la console ─────────────────────
message("--- TRANSITION 1 : Horticultural Richness ➔ Woody Richness (> 5%) ---")
print(flux_step1)

message("\n--- TRANSITION 2 : Woody Richness ➔ Total Richness (> 5%) ---")
print(flux_step2)

