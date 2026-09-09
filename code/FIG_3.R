# =============================================================================
# Figure complexity — Panel A (bubble) + Panel B (stacked bar)
# =============================================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)
library(ragg)

# =============================================================================
# 1. CARACT key (fix is_edible : NA = non-comestible)
# =============================================================================

caract_key <- CARACT %>%
  select(NAME, Woodiness, Alim, Medicinal) %>%   # ajouter Medicinal ici
  distinct() %>%
  mutate(
    is_woody  = Woodiness %in% c("Woody", "woody"),
    is_edible = !is.na(Alim)
  )

# =============================================================================
# 2. df — Panel A (bubble plot)
# =============================================================================

MAX_DISP <- 6

df <- DATAT %>%
  left_join(caract_key, by = c("species treatment" = "NAME")) %>%
  group_by(numéro) %>%
  summarise(
    n_edible = n_distinct(`species treatment`[is_edible == TRUE]),
    n_woody  = n_distinct(`species treatment`[is_woody  == TRUE]),
    .groups = "drop"
  ) %>%
  mutate(
    horti_disp = pmin(n_edible, MAX_DISP),
    woody_disp = pmin(n_woody,  MAX_DISP)
  ) %>%
  filter(horti_disp >= 1, woody_disp >= 1) %>%
  count(horti_disp, woody_disp, name = "count") %>%
  mutate(pct = count / sum(count) * 100)

# =============================================================================
# 3. FIG1 — Panel B (stacked bar)
# =============================================================================

FIG1 <- DATAT %>%
  left_join(caract_key, by = c("species treatment" = "NAME")) %>%
  group_by(numéro) %>%
  summarise(
    NB_sp        = n_distinct(`species treatment`),
    has_WNE      = any( is_woody & !is_edible,  na.rm = TRUE),
    has_WE       = any( is_woody &  is_edible,  na.rm = TRUE),
    has_HE       = any(!is_woody &  is_edible,  na.rm = TRUE),
    has_medicinal = any(!is.na(Medicinal),       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Concatenation = case_when(
      has_WNE & has_WE  & !has_HE                        ~ "WNE + WE",
      has_WNE & has_HE  & !has_WE                        ~ "WNE + HE",
      has_WNE & !has_WE & !has_HE & has_medicinal        ~ "WNE + HE",  # medicinal → WNE+HE
      (has_WE | has_HE) & !has_WNE                       ~ "Edible only",
      TRUE                                                ~ "Multi / HNE"
    ),
    Concatenation = factor(Concatenation,
                           levels = c("WNE + WE", "WNE + HE",
                                      "Edible only", "Multi / HNE")),
    NB_sp_lab = ifelse(NB_sp >= 6, "≥6", as.character(NB_sp)),
    NB_sp_lab = factor(NB_sp_lab, levels = c("2","3","4","5","≥6"))
  ) %>%
  group_by(NB_sp_lab, Concatenation) %>%
  summarise(n = n(), .groups = "drop")

# Check
cat("df total:", sum(df$count), "\n")
cat("FIG1 total:", sum(FIG1$n), "\n")
FIG1 %>% count(Concatenation, wt = n) %>% print()

# =============================================================================
# 4. PANEL A — Bubble plot
# =============================================================================

base_size <- 9

grid_bg <- expand.grid(horti_disp = 1:6, woody_disp = 1:6) %>%
  mutate(
    richness = pmin(horti_disp + woody_disp, 7),
    richness  = factor(richness)
  )

rich_pal <- c(
  "2" = "#D6EAF8", "3" = "#AED6F1", "4" = "#5DADE2",
  "5" = "#2E86C1", "6" = "#1A5276", "7" = "#0D2137"
)

pA <- ggplot() +
  geom_tile(data = grid_bg,
            aes(x = horti_disp, y = woody_disp, fill = richness),
            width = 1, height = 1, alpha = 0.85) +
  scale_fill_manual(values = rich_pal, name = "Total richness",
                    labels = c("2","3","4","5","6","≥6")) +
  geom_abline(intercept = c(1, 2, 3, 4), slope = -1,
              linetype = "dashed", colour = "white", linewidth = 0.3) +
  geom_point(data = df,
             aes(x = horti_disp, y = woody_disp, size = count),
             colour = "grey10", fill = "grey15", shape = 21, alpha = 0.8) +
  geom_text(data = filter(df, pct >= 3),
            aes(x = horti_disp, y = woody_disp,
                label = paste0(round(pct, 1), "%")),
            colour = "white", size = 2.4, fontface = "bold") +
  scale_size_area(max_size = 14, name = "Experiments (n)",
                  breaks = c(100, 500, 1000, 5000)) +
  scale_x_continuous(breaks = 1:6, labels = c(1:5, "≥6"),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = 1:6, labels = c(1:5, "≥6"),
                     expand = expansion(add = 0.5)) +
  labs(x = "Edible crop species", y = "Woody species", tag = "a") +
  guides(fill = guide_legend(order = 1, override.aes = list(size = 4)),
         size = guide_legend(order = 2)) +
  theme_classic(base_size = base_size) +
  theme(
    plot.tag        = element_text(face = "bold"),
    legend.key.size = unit(3.5, "mm"),
    legend.text     = element_text(size = 7),
    legend.title    = element_text(size = 7.5),
    plot.margin     = margin(4, 6, 4, 4, "mm")
  )

# =============================================================================
# 5. PANEL B — Stacked bar
# =============================================================================

func_pal <- c(
  "WNE + WE"    = "#2E7FA3",
  "WNE + HE"    = "#79B7D3",
  "Edible only" = "#E69F00",
  "Multi / HNE" = "#B0B0B0"
)

pB <- ggplot(FIG1, aes(x = NB_sp_lab, y = n, fill = Concatenation)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = func_pal, name = "Functional\ncomposition") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = comma) +
  labs(x = "Total richness", y = "Experiments (n)", tag = "b") +
  theme_classic(base_size = base_size) +
  theme(
    plot.tag           = element_text(face = "bold"),
    legend.key.size    = unit(3.5, "mm"),
    legend.text        = element_text(size = 7),
    legend.title       = element_text(size = 7.5),
    plot.margin        = margin(4, 4, 4, 2, "mm"),
    panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.25)
  )

# =============================================================================
# 6. ASSEMBLAGE & EXPORT
# =============================================================================

fig <- pA + pB + plot_layout(widths = c(1.6, 1))

w_mm <- 183; h_mm <- 100

agg_png("fig_complexity.png",
        width = w_mm, height = h_mm, units = "mm",
        res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_complexity.png")




# =============================================================================
# PANEL C — Distribution richesse totale (horizontal bar)
# =============================================================================

df_rich <- DATAT %>%
  mutate(
    NB_sp_class = case_when(
      NB_sp == 2 ~ "2",
      NB_sp == 3 ~ "3",
      NB_sp == 4 ~ "4",
      NB_sp == 5 ~ "5",
      NB_sp >= 6 ~ "≥6",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(NB_sp_class) %>%
  summarise(
    n = n_distinct(numéro),
    .groups = "drop"
  ) %>%
  mutate(
    pct = 100 * n / sum(n),
    NB_sp_class = factor(
      NB_sp_class,
      levels = c("2", "3", "4", "5", "≥6")
    )
  )

pC <- ggplot(df_rich, aes(y = NB_sp_class, x = pct)) +
  geom_col(aes(fill = NB_sp_class), width = 0.62, color = NA) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            hjust = -0.2, size = 2.2, color = "grey25") +
  scale_fill_manual(
    values = c("2"="#D0ECF7","3"="#8EC9E0",
               "4"="#4FA3C5","5"="#1F7FAD","≥6"="#0D4C6E"),
    guide  = "none"
  ) +
  scale_x_continuous(
    name   = "Experiments (%)",
    labels = function(x) paste0(x, "%"),
    limits = c(0, 60), expand = c(0, 0)
  ) +
  labs(y = "Total species richness", tag = "c") +
  theme_minimal(base_size = base_size) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "grey92", linewidth = 0.3),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.35),
    axis.ticks.x       = element_line(color = "grey30", linewidth = 0.35),
    axis.ticks.length  = unit(2.5, "pt"),
    axis.text          = element_text(size = 7, color = "grey20"),
    axis.title         = element_text(size = 8, color = "grey10"),
    plot.tag           = element_text(size = 9, face = "bold"),
    plot.margin        = margin(6, 12, 6, 6, "pt")
  )

# =============================================================================
# PANEL D — Composition fonctionnelle par classe de richesse (proportions)
# =============================================================================

df_func <- FIG1 %>%
  mutate(
    richness = factor(
      NB_sp_lab,   # NB_sp_lab est déjà "2","3","4","5","≥6"
      levels = c("2","3","4","5","≥6"),
      labels = c("2 spp.","3 spp.","4 spp.","5 spp.","≥6 spp.")
    )
  ) %>%
  group_by(richness, Concatenation) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(richness) %>%
  mutate(
    prop  = n / sum(n),
    n_tot = sum(n)
  ) %>%
  ungroup()

df_ann <- df_func %>% distinct(richness, n_tot)

pD <- ggplot(df_func, aes(x = richness, y = prop, fill = Concatenation)) +
  geom_col(width = 0.62, color = "white", linewidth = 0.3) +
  geom_text(data = df_ann,
            aes(x = richness, y = 1.04,
                label = paste0("n=", comma(n_tot))),
            inherit.aes = FALSE, size = 2.1, color = "grey40") +
  scale_fill_manual(
    values = func_pal,
    name   = "Functional\ncomposition",
    labels = c(
      "WNE + WE"    = "WNE + WE",
      "WNE + HE"    = "WNE + HE",
      "Edible only" = "Edible only\n(HE + WE, WE)",
      "Multi / HNE" = "Multi-type\n(≥3 groups or HNE)"
    ),
    guide = guide_legend(title.position = "top", ncol = 1,
                         keywidth = unit(8, "pt"), keyheight = unit(8, "pt"))
  ) +
  scale_y_continuous(
    name   = "Proportion of experiments",
    labels = percent_format(accuracy = 1),
    limits = c(0, 1.1), breaks = seq(0, 1, 0.25), expand = c(0, 0)
  ) +
  labs(x = "System species richness", tag = "d") +
  theme_minimal(base_size = base_size) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_line(color = "grey92", linewidth = 0.3),
    axis.line.x        = element_line(color = "grey30", linewidth = 0.35),
    axis.ticks.x       = element_line(color = "grey30", linewidth = 0.35),
    axis.ticks.length  = unit(2.5, "pt"),
    axis.text          = element_text(size = 7, color = "grey20"),
    axis.title         = element_text(size = 8, color = "grey10"),
    legend.position    = "right",
    legend.title       = element_text(size = 6.5, face = "bold"),
    legend.text        = element_text(size = 6.2),
    plot.tag           = element_text(size = 9, face = "bold"),
    plot.margin        = margin(6, 6, 6, 4, "pt")
  )

# =============================================================================
# ASSEMBLAGE 4 panels
# =============================================================================

fig <- (pA + pB) / (pC + pD) +
  plot_layout(heights = c(1, 1))

w_mm <- 183; h_mm <- 160

agg_png("fig_complexity.png",
        width = w_mm, height = h_mm, units = "mm",
        res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_complexity.png")


fig <- (pA + pB) / (pC + pD) +
  plot_layout(heights = c(1, 1))

w_mm <- 183; h_mm <- 160

agg_png("fig_complexity.png",
        width = w_mm, height = h_mm, units = "mm",
        res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_complexity.png")



#####

fig <- pA + pD + plot_layout(widths = c(1.5, 1))

w_mm <- 300; h_mm <- 100

agg_png("fig_complexity.png",
        width = w_mm, height = h_mm, units = "mm",
        res = 300, background = "white")
print(fig)
dev.off()

message("✓ fig_complexity.png")
