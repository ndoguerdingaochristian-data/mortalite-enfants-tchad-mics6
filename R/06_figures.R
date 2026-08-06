# =============================================================================
# 06_figures.R  -  Figures ggplot2 (axes en clair, palette de marque)
# A lancer APRES l'analyse : m2, design et mC doivent etre en mémoire.
# Sorties : PNG haute resolution dans outputs/figures/.
# =============================================================================
library(ggplot2); library(dplyr); library(broom); library(survey); library(lme4)

col_ink <- "#0F1E2E"; col_ochre <- "#C75B12"; col_teal <- "#0E7C7B"; col_grey <- "#4A5568"

theme_cn <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title   = element_text(face = "bold", color = col_ink, size = 15),
      plot.subtitle = element_text(color = col_grey, size = 10, margin = margin(b = 8)),
      plot.caption = element_text(color = col_grey, size = 8, hjust = 0),
      axis.title = element_text(color = col_ink), axis.text = element_text(color = col_ink),
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 20, 12, 15),
      legend.position = "top", legend.title = element_blank()
    )
}
cap <- "Source : MICS6 Tchad 2019 (INSEED / UNICEF). Analyse : Ndoguerdingao Christian."

# --- Libelles en clair des modalites (références : 1re catégorie) ------------
lib <- c(
  "milieu2" = "Rural",
  "educ_mere1" = "Mère : primaire", "educ_mere2" = "Mère : secondaire ou +",
  "quintile2" = "Bien-être : second", "quintile3" = "Bien-être : moyen",
  "quintile4" = "Bien-être : quatrieme", "quintile5" = "Bien-être : le plus riche",
  "relig1" = "Animiste", "relig2" = "Catholique", "relig3" = "Protestant", "relig4" = "Musulman",
  "relig5" = "Sans religion", "relig6" = "Autre religion",
  "statut_mat2" = "Union libre", "statut_mat3" = "Pas en union",
  "sexe2" = "Fille",
  "parite_intervalle1" = "Rang 1",
  "parite_intervalle2" = "Rang 2-3, intervalle court",
  "parite_intervalle3" = "Rang 2-3, intervalle long",
  "parite_intervalle4" = "Rang 4+, intervalle court",
  "parite_intervalle5" = "Rang 4+, intervalle long",
  "parite_intervalle6" = "Parité-intervalle : autre",
  "age_mere2" = "Mère : 20-34 ans", "age_mere3" = "Mère : 35-49 ans",
  "type_naiss2" = "Naissance multiple",
  "tranche_age2" = "Âge : 12-23 mois", "tranche_age3" = "Âge : 24-35 mois",
  "tranche_age4" = "Âge : 36-47 mois", "tranche_age5" = "Âge : 48-59 mois")

prov_noms <- c(
  "1"="Batha","2"="Borkou","3"="Chari-Baguirmi","4"="Guéra","5"="Hadjer-Lamis",
  "6"="Kanem","7"="Lac","8"="Logone Occidental","9"="Logone Oriental","10"="Mandoul",
  "11"="Mayo-Kebbi Est","12"="Mayo-Kebbi Ouest","13"="Moyen-Chari","14"="Ouaddaï",
  "15"="Salamat","16"="Tandjilé","17"="Wadi Fira","18"="N'Djamena","19"="Barh-el-Gazel",
  "20"="Ennedi Ouest","21"="Sila","22"="Tibesti","23"="Ennedi Est")

# --- Figure 1 : Forest plot des odds ratios (hors province et ethnie) -------
or <- broom::tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>%
  filter(term != "(Intercept)", !grepl("province|ethnie", term)) %>%
  mutate(effet = ifelse(estimate < 1, "Risque accru de décès", "Effet protecteur"),
         code  = gsub("factor\\(|\\)", "", term),
         label = ifelse(code %in% names(lib), lib[code], code))

fig1 <- ggplot(or, aes(x = estimate, y = reorder(label, estimate), color = effet)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = col_grey) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high), linewidth = 0.6) +
  geom_point(size = 2.4) +
  scale_x_log10() +
  scale_color_manual(values = c("Effet protecteur" = col_teal, "Risque accru de décès" = col_ochre)) +
  labs(title = "Déterminants de la survie de l'enfant",
       subtitle = "Odds ratios (échelle log) du logit pondéré design-based",
       x = "Odds ratio (IC 95 %)", y = NULL, caption = cap) +
  theme_cn()
ggsave("outputs/figures/forest_odds_ratios.png", fig1, width = 9, height = 9, dpi = 300)

# --- Figure 2 : Mortalité par province (proportion pondérée de DECES) -------
d_prov <- update(design, deces = as.numeric(survie == 0))
prov <- svyby(~deces, ~province, d_prov, svymean, na.rm = TRUE)
prov <- data.frame(province = rownames(prov), taux = 100 * as.numeric(coef(prov)))
prov$nom <- ifelse(prov$province %in% names(prov_noms), prov_noms[prov$province], prov$province)
write.csv(data.frame(province = prov$nom, taux_deces = round(prov$taux, 2)),
          "outputs/tables/mortalite_par_province.csv", row.names = FALSE)

fig2 <- ggplot(prov, aes(x = taux, y = reorder(nom, taux))) +
  geom_col(fill = col_ochre, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", taux)), hjust = -0.15, size = 3, color = col_ink) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Mortalité des enfants de moins de 5 ans par province",
       subtitle = "Proportion pondérée de décès", x = "Taux (%)", y = NULL, caption = cap) +
  theme_cn()
ggsave("outputs/figures/mortalite_par_province.png", fig2, width = 8, height = 8, dpi = 300)

# --- Figure 3 : Effets aléatoires provinciaux (chenille) --------------------
re_obj <- ranef(mC, condVar = TRUE)$province
re <- data.frame(province = rownames(re_obj), effet = re_obj[, 1])
re$se  <- sqrt(attr(re_obj, "postVar")[1, 1, ])
re$nom <- ifelse(re$province %in% names(prov_noms), prov_noms[re$province], re$province)

fig3 <- ggplot(re, aes(x = effet, y = reorder(nom, effet))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = col_grey) +
  geom_linerange(aes(xmin = effet - 1.96 * se, xmax = effet + 1.96 * se),
                 color = col_teal, linewidth = 0.6) +
  geom_point(color = col_ink, size = 2.2) +
  labs(title = "Hétérogénéité provinciale de la survie",
       subtitle = "Effet aléatoire de chaque province (écart à la moyenne, échelle logit)",
       x = "Effet aléatoire provincial", y = NULL, caption = cap) +
  theme_cn()
ggsave("outputs/figures/effets_provinciaux.png", fig3, width = 8, height = 8, dpi = 300)

message("3 figures (axes en clair) enregistrees dans outputs/figures/.")
