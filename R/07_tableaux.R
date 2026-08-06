# =============================================================================
# 07_tableaux.R  -  Tableaux de résultats au format RTF (pour l'article)
# A lancer APRES analyse_complete.R : m1, m2, m0w, mC doivent etre en mémoire.
# Produit : tableau_M1_M2_OR.rtf, Tableau_AME_M2.rtf, Tableau_Multiniveau.rtf,
#           Tableau_Robustesse.rtf, + une version CSV labellisee des OR de M2.
# =============================================================================
for (pk in c("modelsummary", "marginaleffects", "flextable", "dplyr"))
  if (!requireNamespace(pk, quietly = TRUE)) install.packages(pk)
library(dplyr); library(modelsummary)

# --- Libelles en clair de toutes les modalites (extraits des .dta) ----------
prov_noms <- c("1"="Batha","2"="Borkou","3"="Chari-Baguirmi","4"="Guéra","5"="Hadjer-Lamis",
  "6"="Kanem","7"="Lac","8"="Logone Occidental","9"="Logone Oriental","10"="Mandoul",
  "11"="Mayo-Kebbi Est","12"="Mayo-Kebbi Ouest","13"="Moyen-Chari","14"="Ouaddaï","15"="Salamat",
  "16"="Tandjilé","17"="Wadi Fira","18"="N'Djamena","19"="Barh-el-Gazel","20"="Ennedi Ouest",
  "21"="Sila","22"="Tibesti","23"="Ennedi Est")

etiq <- list(
  milieu    = c("2"="Rural"),
  educ_mere = c("1"="Mère : primaire","2"="Mère : secondaire ou +"),
  quintile  = c("2"="Bien-être : second","3"="Bien-être : moyen","4"="Bien-être : quatrieme","5"="Bien-être : le plus riche"),
  relig     = c("1"="Animiste","2"="Catholique","3"="Protestant","4"="Musulman","5"="Sans religion","6"="Autre religion"),
  ethnie    = c("1"="Ethnie : Gorane","2"="Ethnie : Arabe","3"="Ethnie : Baguirmi","4"="Ethnie : Kanembou","5"="Ethnie : Boulala",
                "6"="Ethnie : Ouaddaï","7"="Ethnie : Zaghawa","8"="Ethnie : Dadjo","9"="Ethnie : Bidio",
                "10"="Ethnie : Moundang","11"="Ethnie : Massa","12"="Ethnie : Toupouri","13"="Ethnie : Sara",
                "14"="Ethnie : Peul","15"="Ethnie : Tama","16"="Ethnie : Gabri","17"="Ethnie : Marba",
                "18"="Ethnie : Mesmedje","19"="Ethnie : Karo","96"="Ethnie : autres","99"="Ethnie : manquant"),
  statut_mat = c("2"="Union libre","3"="Pas en union"),
  sexe       = c("2"="Fille"),
  parite_intervalle = c("1"="Rang 1","2"="Rang 2-3, interv. court","3"="Rang 2-3, interv. long",
                        "4"="Rang 4+, interv. court","5"="Rang 4+, interv. long","6"="Parité-interv. : autre"),
  age_mere   = c("2"="Mère : 20-34 ans","3"="Mère : 35-49 ans"),
  type_naiss = c("2"="Naissance multiple"),
  tranche_age = c("2"="Âge : 12-23 mois","3"="Âge : 24-35 mois","4"="Âge : 36-47 mois","5"="Âge : 48-59 mois"),
  province   = prov_noms)

coef_map <- c("(Intercept)" = "Constante",
  unlist(lapply(names(etiq), function(v)
    setNames(etiq[[v]], paste0(v, names(etiq[[v]]))))))

# --- Tableau 1 : Odds ratios M1 et M2 cote a cote ---------------------------
modelsummary(
  list("Modèle 1 (distaux)" = m1, "Modèle 2 (complet)" = m2),
  output = "outputs/tables/tableau_M1_M2_OR.rtf",
  exponentiate = TRUE, statistic = "conf.int",
  coef_map = coef_map, gof_omit = "AIC|BIC|Log|F|RMSE",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = "Odds ratios de la survie de l'enfant (logit pondéré), MICS6 Tchad 2019",
  notes = c("Source : MICS6-Tchad 2019 (INSEED/UNICEF, 2020). Pseudo-maximum de vraisemblance pondéré (svy: logit). OR > 1 = effet protecteur ; IC 95 % entre crochets.",
            "Ref. : Urbain, N'Djamena, Aucun/Préscolaire, Plus pauvre, Musulman, Sara, Mariée, Garçon, moins de 20 ans, Singulier, 0-11 mois, Rang 4+/interv. long."))

# Version CSV labellisee des OR de M2
or_csv <- broom::tidy(m2, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(modalite = ifelse(term %in% names(coef_map), coef_map[term], term)) %>%
  transmute(modalite, OR = round(estimate, 3),
            IC_bas = round(conf.low, 3), IC_haut = round(conf.high, 3),
            p = signif(p.value, 3))
write.csv(or_csv, "outputs/tables/OR_M2_complet.csv", row.names = FALSE)

# --- Tableau 2 : Effets marginaux moyens (AME) de M2 ------------------------
# Construit avec flextable (modelsummary gere mal les contrastes multiples).
# NB : statut_mat n'est pas dans m2 (uniquement dans le multiniveau), donc exclu.
library(marginaleffects); library(flextable)
ame <- avg_slopes(m2, variables = c("sexe","educ_mere","quintile","milieu",
              "parite_intervalle","age_mere","type_naiss","tranche_age","relig"))
noms_var <- c(sexe="Sexe (fille)", educ_mere="Education de la mere", quintile="Bien-être",
              milieu="Milieu (rural)", parite_intervalle="Parité-intervalle",
              age_mere="Âge de la mere", type_naiss="Type de naissance (multiple)",
              tranche_age="Âge de l'enfant", relig="Religion")
ame_df  <- as.data.frame(ame)
ame_tab <- data.frame(
  Variable = ifelse(ame_df$term %in% names(noms_var), noms_var[ame_df$term], ame_df$term),
  Modalite = ame_df$contrast,
  AME      = round(ame_df$estimate, 4),
  IC_95    = sprintf("[%.4f ; %.4f]", ame_df$conf.low, ame_df$conf.high),
  p        = signif(ame_df$p.value, 3))
ft_ame <- flextable(ame_tab)
ft_ame <- set_caption(ft_ame, "Effets marginaux moyens sur la probabilité de survie (M2)")
save_as_rtf(ft_ame, path = "outputs/tables/Tableau_AME_M2.rtf")

# --- Tableau 3 : Modèle multiniveau (nul et complet), OR ---------------------
cci <- function(m) { v <- as.data.frame(lme4::VarCorr(m))$vcov[1]; v/(v+pi^2/3) }
add <- data.frame(
  term = c("Variance province", "CCI", "PCV"),
  `Nul`     = c(sprintf("%.4f", as.data.frame(lme4::VarCorr(m0w))$vcov[1]),
                sprintf("%.1f %%", 100*cci(m0w)), ""),
  `Complet` = c(sprintf("%.4f", as.data.frame(lme4::VarCorr(mC))$vcov[1]),
                sprintf("%.1f %%", 100*cci(mC)),
                sprintf("%.1f %%", 100*(cci(m0w)-cci(mC))/cci(m0w))),
  check.names = FALSE)
modelsummary(list("Nul" = m0w, "Complet" = mC),
  output = "outputs/tables/Tableau_Multiniveau.rtf",
  exponentiate = TRUE, statistic = "conf.int", stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_map = coef_map, gof_omit = ".*", add_rows = add,
  title = "Modèle multiniveau logistique a effets aléatoires provinciaux",
  notes = "OR exponentiels. CCI = correlation intra-classe ; PCV = part de variance provinciale expliquee.")

# --- Tableau 4 : Robustesse (validation croisee R / Stata) ------------------
library(flextable)
rob <- data.frame(
  Indicateur = c("CCI, modèle nul", "CCI, modèle complet", "PCV",
                 "Ajustement (Archer-Lemeshow)", "Test de Mundlak"),
  `Stata (référence)` = c("5,55 %", "1,45 %", "75,0 %", "F(9,718)=0,62 ; p=0,78", "p=0,068"),
  `R`                 = c("6,03 %", "1,57 %", "75,2 %", "F(9,718)=0,69 ; p=0,72", "p=0,17"),
  check.names = FALSE)
save_as_rtf(flextable(rob), path = "outputs/tables/Tableau_Robustesse.rtf")

message("4 tableaux RTF + CSV labellise enregistres dans outputs/tables/.")
