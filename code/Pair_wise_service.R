# ==============================================================================
# FIGURE MULTI-PANEL HAUT-IMPACT (STYLE IPCC / IPBES)
# ==============================================================================

if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(patchwork)) install.packages("patchwork") # Pour assembler les figures

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# 0. Configuration des données communes
services <- c(
  "Carbon Storage", "Habitat/Biodiversity", "Pest Regulation",
  "Food Provision", "Water regulation", "Healthy Soils",
  "Nutrient Cycling", "Photosynthesis", "Materials", "Natural Medecine"
)

# Charte couleur IPBES/IPCC commune
ipcc_palette <- c("#94a3b8", "#0f766e", "#ca8a04", "#b91c1c")
color_dark <- "#1e293b"

# ------------------------------------------------------------------------------
# PANEL A : MATRICE DE SYNERGIES (PAIRES)
# ------------------------------------------------------------------------------
mat_data <- apply(as.matrix(final_results[, services]), 2, as.numeric)
co_mat <- crossprod(mat_data)
diag(co_mat) <- 0

co_df <- as.data.frame(as.table(co_mat)) %>%
  rename(Service_1 = Var1, Service_2 = Var2, Co_occurrence = Freq) %>%
  mutate(Service_1 = factor(Service_1, levels = services),
         Service_2 = factor(Service_2, levels = services)) %>%
  filter(as.integer(Service_1) > as.integer(Service_2)) %>%
  filter(Co_occurrence > 0)

panel_a <- ggplot(co_df, aes(x = Service_1, y = Service_2)) +
  geom_hline(yintercept = 1:length(services), color = "#f1f5f9", linewidth = 0.5) +
  geom_vline(xintercept = 1:length(services), color = "#f1f5f9", linewidth = 0.5) +
  geom_point(aes(size = Co_occurrence, color = Co_occurrence), alpha = 0.9) +
  geom_text(aes(label = Co_occurrence), size = 2.8, color = "white", fontface = "bold") +
  scale_size_continuous(range = c(4, 13), guide = "none") +
  scale_color_gradientn(colors = c("#e2e8f0", ipcc_palette), name = "Articles") +
  theme_minimal(base_family = "Arial") +
  labs(x = NULL, y = NULL, title = "a  Pairwise Co-occurrences") +
  theme(
    text = element_text(color = color_dark),
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, face = "bold"),
    axis.text.y = element_text(size = 9, face = "bold"),
    panel.grid = element_blank(),
    legend.position = "none" # On enlève la légende ici pour la mettre de façon globale
  )

# ------------------------------------------------------------------------------
# PANEL B : DISTRIBUTION DE LA COMPLEXITÉ (1, 2, 3+ services)
# ------------------------------------------------------------------------------
# On compte le nombre de services étudiés par article (somme des lignes)
final_results$Num_Services <- rowSums(as.matrix(final_results[, services]) == 1)

# On groupe les résultats (ex: 1 service, 2 services, 3 services, 4+ services)
complexity_df <- final_results %>%
  count(Num_Services) %>%
  mutate(
    Class = case_when(
      Num_Services == 1 ~ "1 Service (In Silo)",
      Num_Services == 2 ~ "2 Services (Pairwise)",
      Num_Services == 3 ~ "3 Services (Triple)",
      Num_Services >= 4 ~ "4+ Services (Complex)"
    ),
    Class = factor(Class, levels = c("1 Service (In Silo)", "2 Services (Pairwise)", "3 Services (Triple)", "4+ Services (Complex)"))
  ) %>%
  group_by(Class) %>%
  summarise(Count = sum(n)) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)

panel_b <- ggplot(complexity_df, aes(x = Class, y = Percentage, fill = Class)) +
  geom_col(width = 0.6, alpha = 0.9) +
  # Ajout du pourcentage exact et du nombre d'articles au-dessus des barres
  geom_text(aes(label = paste0(round(Percentage, 1), "%\n(n=", Count, ")")),
            vjust = -0.3, size = 3, fontface = "bold", color = color_dark, lineheight = 0.9) +
  scale_fill_manual(values = c("1 Service (In Silo)" = "#94a3b8",
                               "2 Services (Pairwise)" = "#0f766e",
                               "3 Services (Triple)" = "#ca8a04",
                               "4+ Services (Complex)" = "#b91c1c")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Laisse de la place au texte en haut
  theme_minimal(base_family = "Arial") +
  labs(
    x = NULL,
    y = "Percentage of Articles (%)",
    title = "b  Research Complexity Depth"
  ) +
  theme(
    text = element_text(color = color_dark),
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 10)),
    axis.text.x = element_text(size = 9, face = "bold", color = color_dark),
    axis.title.y = element_text(size = 10, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.y = element_line(color = "black", linewidth = 0.4),
    legend.position = "none"
  )

# ------------------------------------------------------------------------------
# ASSEMBLAGE DES PANELS (Le secret de fabrication "Nature")
# ------------------------------------------------------------------------------
# On met la matrice en haut (ou à gauche) et la distribution en bas (ou à droite)
combined_plot <- panel_a / panel_b +
  plot_layout(heights = c(2, 1)) # Donne plus d'espace vertical à la matrice

# Sauvegarde Haute Définition
ggsave("Nature_Comm_Combined_Figure.pdf", plot = combined_plot,
       width = 180, height = 220, units = "mm", dpi = 300, device = "pdf")

print("La figure combinée Panel A + Panel B a été générée en PDF !")



library(ragg)
agg_png("Nature_Comm_Combined_Figure.png",
        width = 250, height = 220, units = "mm",
        res = 300, background = "white")
print(combined_plot)
dev.off()
