# ── Region mapping (UN scheme) ────────────────────────────────
region_map2 <- c(
  # Sub-Saharan Africa
  "Kenya"="Sub-Saharan Africa","Tanzania"="Sub-Saharan Africa",
  "Uganda"="Sub-Saharan Africa","Ethiopia"="Sub-Saharan Africa",
  "Rwanda"="Sub-Saharan Africa","Burundi"="Sub-Saharan Africa",
  "Malawi"="Sub-Saharan Africa","Zambia"="Sub-Saharan Africa",
  "Zimbabwe"="Sub-Saharan Africa","Mozambique"="Sub-Saharan Africa",
  "Madagascar"="Sub-Saharan Africa","Ghana"="Sub-Saharan Africa",
  "Ivory Coast"="Sub-Saharan Africa","Cote d'Ivoire"="Sub-Saharan Africa",
  "Cameroon"="Sub-Saharan Africa","Nigeria"="Sub-Saharan Africa",
  "Senegal"="Sub-Saharan Africa","Mali"="Sub-Saharan Africa",
  "Burkina Faso"="Sub-Saharan Africa","Togo"="Sub-Saharan Africa",
  "Benin"="Sub-Saharan Africa","Guinea"="Sub-Saharan Africa",
  "Sierra Leone"="Sub-Saharan Africa","Liberia"="Sub-Saharan Africa",
  "Congo"="Sub-Saharan Africa","Democratic Republic of the Congo"="Sub-Saharan Africa",
  "Gabon"="Sub-Saharan Africa","Equatorial Guinea"="Sub-Saharan Africa",
  "Central African Republic"="Sub-Saharan Africa",
  "Comoros"="Sub-Saharan Africa","South Africa"="Sub-Saharan Africa",
  "Angola"="Sub-Saharan Africa","Namibia"="Sub-Saharan Africa",

  # Southern Asia
  "India"="Southern Asia","Bangladesh"="Southern Asia",
  "Sri Lanka"="Southern Asia","Nepal"="Southern Asia",
  "Pakistan"="Southern Asia","Afghanistan"="Southern Asia",

  # South & Central America
  "Brazil"="South & Central America","Colombia"="South & Central America",
  "Peru"="South & Central America","Ecuador"="South & Central America",
  "Bolivia"="South & Central America","Venezuela"="South & Central America",
  "Argentina"="South & Central America","Chile"="South & Central America",
  "Paraguay"="South & Central America","Uruguay"="South & Central America",
  "Guyana"="South & Central America","Suriname"="South & Central America",
  "French Guiana"="South & Central America",
  "Mexico"="South & Central America","Guatemala"="South & Central America",
  "Honduras"="South & Central America","Costa Rica"="South & Central America",
  "Panama"="South & Central America","Nicaragua"="South & Central America",
  "El Salvador"="South & Central America","Cuba"="South & Central America",
  "Dominican Republic"="South & Central America","Haiti"="South & Central America",
  "Jamaica"="South & Central America","Trinidad and Tobago"="South & Central America",
  "Guadeloupe"="South & Central America","Martinique"="South & Central America",
  "Puerto Rico"="South & Central America","Belize"="South & Central America",

  # Eastern Asia
  "China"="Eastern Asia","Japan"="Eastern Asia",
  "South Korea"="Eastern Asia","Taiwan"="Eastern Asia",

  # South-Eastern Asia
  "Indonesia"="South-Eastern Asia","Philippines"="South-Eastern Asia",
  "Vietnam"="South-Eastern Asia","Thailand"="South-Eastern Asia",
  "Malaysia"="South-Eastern Asia","Myanmar"="South-Eastern Asia",
  "Laos"="South-Eastern Asia","Cambodia"="South-Eastern Asia",
  "Timor-Leste"="South-Eastern Asia",

  # Northern America
  "United States"="Northern America","Canada"="Northern America",
  "USA"="Northern America","United States of America"="Northern America",

  # Europe
  "France"="Europe","Germany"="Europe","Spain"="Europe",
  "Italy"="Europe","United Kingdom"="Europe","Netherlands"="Europe",
  "Belgium"="Europe","Portugal"="Europe","Sweden"="Europe",
  "Norway"="Europe","Denmark"="Europe","Switzerland"="Europe",
  "Austria"="Europe","Poland"="Europe","Greece"="Europe",
  "Turkey"="Europe","Finland"="Europe",

  # Northern Africa
  "Morocco"="Northern Africa","Algeria"="Northern Africa",
  "Tunisia"="Northern Africa","Libya"="Northern Africa",
  "Egypt"="Northern Africa","Sudan"="Northern Africa",

  # Oceania
  "Australia"="Oceania","New Zealand"="Oceania",
  "Papua New Guinea"="Oceania","Fiji"="Oceania",

  # Other Asia
  "Iran"="Other Asia","Iraq"="Other Asia","Saudi Arabia"="Other Asia",
  "Israel"="Other Asia","Jordan"="Other Asia","Lebanon"="Other Asia",
  "Syria"="Other Asia","Yemen"="Other Asia"
)

# ── numéro → country → region ─────────────────────────────────
numéro_region <- DATA %>%
  select(numéro, country) %>%
  distinct() %>%
  group_by(numéro) %>% slice(1) %>% ungroup() %>%
  mutate(region = region_map2[country]) %>%
  filter(!is.na(region))

# ── Joindre avec Concatenation ────────────────────────────────
# FIG1_detail doit exister (cf. code précédent)
dat_reg <- FIG1_detail %>%
  select(numéro, Concatenation) %>%
  left_join(numéro_region, by = "numéro") %>%
  filter(!is.na(region)) %>%
  count(region, Concatenation, name = "n") %>%
  group_by(region) %>%
  mutate(
    n_total = sum(n),
    prop    = n / n_total * 100
  ) %>%
  ungroup() %>%
  mutate(
    Concatenation = factor(Concatenation,
                           levels = c("WNE + WE","WNE + HE",
                                      "Edible only","Multi / HNE")),
    region_lab = paste0(region, " (", n_total, ")")
  )

# Ordre des régions : par n_total décroissant (en haut = plus grand)
region_lev <- dat_reg %>%
  distinct(region, region_lab, n_total) %>%
  arrange(n_total) %>%          # ascending → haut de barre = plus grand
  pull(region_lab)

dat_reg <- dat_reg %>%
  mutate(region_lab = factor(region_lab, levels = region_lev))

# ── Plot ──────────────────────────────────────────────────────
p_reg <- ggplot(dat_reg,
                aes(x = prop, y = region_lab, fill = Concatenation)) +
  geom_col(width = 0.65) +
  scale_fill_manual(
    values = func_pal,
    name   = "Functional\ncomposition",
    labels = c("WNE + WE"    = "WNE + WE",
               "WNE + HE"    = "WNE + HE",
               "Edible only" = "Edible only",
               "Multi / HNE" = "Multi / HNE"),
    guide  = guide_legend(title.position = "top",
                          keywidth = unit(8,"pt"),
                          keyheight = unit(8,"pt"))
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 101),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(x = "Proportion of experiments (%)", y = NULL, tag = "a") +
  theme_classic(base_size = base_size) +
  theme(
    axis.line.y        = element_blank(),
    axis.ticks.y       = element_blank(),
    panel.grid.major.x = element_line(colour = "grey93", linewidth = 0.25),
    legend.position    = "right",
    legend.title       = element_text(size = 7.5, face = "plain"),
    legend.text        = element_text(size = 7),
    legend.key.size    = unit(3.5, "mm"),
    plot.tag           = element_text(face = "bold"),
    plot.margin        = margin(4, 4, 4, 4, "mm")
  )

# ── Export ────────────────────────────────────────────────────
agg_png("fig_region_func.png",
        width = 130, height = 100, units = "mm",
        res = 300, background = "white")
print(p_reg)
dev.off()

message("✓ fig_region_func.png")
