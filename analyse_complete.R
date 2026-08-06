# =============================================================================
# analyse_complete.R
# Déterminants de la mortalité des enfants de moins de 5 ans au Tchad (MICS6 2019)
# PIPELINE R COMPLET, du debut a la fin :
#   preparation -> plan de sondage -> logit pondéré -> ajustement (Archer-Lemeshow)
#   -> modèle multiniveau (glmer) -> CCI/PCV -> test de Mundlak.
# Prerequis : placer bh.dta, wm.dta dans data/raw/  (licence MICS, voir data/README.md)
# =============================================================================

## 0. Paquets ----------------------------------------------------------------
paquets <- c("haven", "dplyr", "survey", "broom", "lme4", "ggplot2")
a_installer <- paquets[!paquets %in% installed.packages()[, "Package"]]
if (length(a_installer)) install.packages(a_installer)
invisible(lapply(paquets, require, character.only = TRUE))

## 1. PREPARATION ------------------------------------------------------------
bh <- read_dta("data/raw/bh.dta")
wm <- read_dta("data/raw/wm.dta")

bh <- bh %>%
  mutate(mois_avant_enquete = WDOI - BH4C) %>%
  filter(mois_avant_enquete >= 0, mois_avant_enquete < 60) %>%
  mutate(
    tranche_age = as.integer(cut(mois_avant_enquete, c(-1,11,23,35,47,59), labels = 1:5)),
    survie   = case_when(BH5 == 1 ~ 1L, BH5 == 2 ~ 0L, TRUE ~ NA_integer_),
    milieu   = HH6, province = HH7,
    educ_mere = if_else(welevel == 3, 2, as.numeric(welevel)),
    quintile = windex5, relig = religion, ethnie = ethnicity,
    sexe = BH3, rang = brthord, intervalle = birthint,
    age_mere = magebrt, type_naiss = BH2,
    parite_intervalle = case_when(
      rang == 1                               ~ 1L,
      rang %in% 2:3 & intervalle == 1         ~ 2L,
      rang %in% 2:3 & intervalle %in% c(2,3)  ~ 3L,
      rang >= 4     & intervalle == 1         ~ 4L,
      rang >= 4     & intervalle %in% c(2,3)  ~ 5L,
      !is.na(rang)                            ~ 6L, TRUE ~ NA_integer_),
    psu_id = PSU, strate = stratum)

wm_fusion <- wm %>%
  mutate(statut_mat = case_when(MSTATUS == 1 ~ 1L, MSTATUS == 2 ~ 2L,
                                MSTATUS %in% c(3,4) ~ 3L, TRUE ~ NA_integer_)) %>%
  select(HH1, HH2, WM3, statut_mat) %>%
  distinct(HH1, HH2, WM3, .keep_all = TRUE)

vars_modele <- c("survie","milieu","province","educ_mere","quintile","relig","ethnie",
                 "statut_mat","sexe","parite_intervalle","age_mere","type_naiss",
                 "wmweight","psu_id","strate")

base <- bh %>%
  left_join(wm_fusion, by = c("HH1","HH2","WM3")) %>%
  filter(if_all(all_of(vars_modele), ~ !is.na(.))) %>%
  mutate(across(where(~inherits(., "haven_labelled")), as.numeric)) %>%
  as.data.frame()
# Facteurs avec références alignees sur l'article
base <- base %>% mutate(
  milieu            = relevel(factor(milieu), "1"),
  province          = relevel(factor(province), "18"),
  educ_mere         = relevel(factor(educ_mere), "0"),
  quintile          = relevel(factor(quintile), "1"),
  relig             = relevel(factor(relig), "4"),
  ethnie            = relevel(factor(ethnie), "13"),
  statut_mat        = relevel(factor(statut_mat), "1"),
  sexe              = relevel(factor(sexe), "1"),
  parite_intervalle = relevel(factor(parite_intervalle), "5"),
  age_mere          = relevel(factor(age_mere), "1"),
  type_naiss        = relevel(factor(type_naiss), "1"),
  tranche_age       = relevel(factor(tranche_age), "1"))
cat("Echantillon analytique :", nrow(base), "enfants.\n")

## 2. PLAN DE SONDAGE + DESCRIPTIF DESIGN-BASED ------------------------------
options(survey.lonely.psu = "certainty")
design <- svydesign(ids = ~psu_id, strata = ~strate, weights = ~wmweight,
                    data = base, nest = TRUE)

cat("\nPrevalence pondérée du décès :\n")
print(svyciprop(~I(survie == 0), design, method = "logit"))

cat("\nTests de Rao-Scott (association bivariee) :\n")
for (v in c("milieu","educ_mere","quintile","relig","ethnie","province",
            "sexe","parite_intervalle","age_mere","type_naiss","tranche_age")) {
  cat("---", v, ":\n")
  print(svychisq(as.formula(paste0("~", v, "+survie")), design, statistic = "F"))
}

## 3. LOGIT PONDERE DESIGN-BASED (M1 puis M2), ODDS RATIOS -------------------
X1 <- "milieu+province+educ_mere+quintile+relig+ethnie"
X2 <- "sexe+parite_intervalle+age_mere+type_naiss+tranche_age"
m1 <- svyglm(as.formula(paste("survie ~", X1)),          design = design, family = quasibinomial())
m2 <- svyglm(as.formula(paste("survie ~", X1, "+", X2)), design = design, family = quasibinomial())

or_table <- function(m)
  broom::tidy(m, conf.int = TRUE, exponentiate = TRUE)[, c("term","estimate","conf.low","conf.high","p.value")]
cat("\nOdds ratios M2 (logit pondéré) :\n")
print(or_table(m2), n = 100)
write.csv(or_table(m2), "outputs/tables/OR_M2_complet.csv", row.names = FALSE)

## 4. AJUSTEMENT : TEST D'ARCHER-LEMESHOW DESIGN-BASED -----------------------
al_gof <- function(model, design, ngroups = 10) {
  p <- as.numeric(fitted(model)); resid <- as.numeric(model$y) - p
  brks <- unique(quantile(p, seq(0, 1, length.out = ngroups + 1), na.rm = TRUE, type = 2))
  grp  <- cut(p, brks, include.lowest = TRUE); G <- nlevels(grp)
  d2 <- update(design, .resid = resid, .grp = grp)
  m  <- svyby(~.resid, ~.grp, d2, svymean, covmat = TRUE)
  idx <- seq_len(G - 1); b <- coef(m)[idx]; V <- vcov(m)[idx, idx, drop = FALSE]
  W  <- as.numeric(t(b) %*% solve(V) %*% b); f <- degf(design)
  Fs <- ((f - G + 2) / (f * (G - 1))) * W
  cat(sprintf("\nArcher-Lemeshow (equivalent estat gof) : F(%d, %d) = %.2f | p = %.4f\n",
              G - 1, f - G + 2, Fs, pf(Fs, G - 1, f - G + 2, lower.tail = FALSE)))
}
al_gof(m2, design, 10)

## 5. MODELE MULTINIVEAU (glmer) : POIDS ELKASABI, CCI, PCV, MUNDLAK ---------
s_h <- 25; alpha <- 0.5
base <- base %>%
  group_by(strate) %>% mutate(a_c_h = n_distinct(psu_id)) %>% ungroup() %>% mutate(A_h = a_c_h) %>%
  group_by(psu_id) %>% mutate(n_per_psu = n()) %>% ungroup() %>%
  group_by(strate) %>% mutate(M_h_bar = mean(n_per_psu)) %>% ungroup() %>%
  mutate(d_denorm = wmweight,
         f_var   = d_denorm / ((A_h/a_c_h) * (M_h_bar/s_h)),
         wt2_elk = (A_h/a_c_h) * (f_var^alpha),
         wt1_elk = d_denorm / wt2_elk) %>%
  group_by(province) %>% mutate(wt2_prov = mean(wt2_elk)) %>% ungroup() %>% as.data.frame()

ctrl <- glmerControl(optimizer = "bobyqa")
form <- survie ~ milieu+educ_mere+quintile+relig+ethnie+statut_mat+sexe+
  parite_intervalle+age_mere+type_naiss+tranche_age+(1|province)

# Modèle nul PONDÉRÉ -> CCI (comparable au melogit de Stata)
m0w <- glmer(survie ~ 1 + (1|province), data = base, family = binomial,
             weights = wt1_elk, nAGQ = 0, control = ctrl)
vu  <- as.data.frame(VarCorr(m0w))$vcov[1]
cat(sprintf("\nNul pondéré : var = %.4f | CCI = %.2f%%\n", vu, 100*vu/(vu+pi^2/3)))

# Modèle complet PONDÉRÉ -> CCI, PCV
mC  <- glmer(form, data = base, family = binomial, weights = wt1_elk, nAGQ = 0, control = ctrl)
print(summary(mC))
vu2 <- as.data.frame(VarCorr(mC))$vcov[1]
cat(sprintf("Complet : var = %.4f | CCI = %.2f%% | PCV = %.1f%%\n",
            vu2, 100*vu2/(vu2+pi^2/3), 100*(vu-vu2)/vu))

# Test de Mundlak (rapport de vraisemblance)
vm <- c("milieu","educ_mere","quintile","statut_mat","parite_intervalle","age_mere")
base <- base %>% group_by(province) %>%
  mutate(across(all_of(vm), ~mean(as.numeric(as.character(.x)), na.rm=TRUE), .names="mpm_{.col}")) %>%
  ungroup() %>% as.data.frame()
mM <- glmer(update(form, . ~ . + mpm_milieu+mpm_educ_mere+mpm_quintile+
                     mpm_statut_mat+mpm_parite_intervalle+mpm_age_mere),
            data = base, family = binomial, weights = wt1_elk, nAGQ = 0, control = ctrl)
cat("\nTest de Mundlak (LRT) : p>0.05 -> effets aléatoires justifies\n")
print(anova(mC, mM))

cat("\n=== FIN DU PIPELINE. Comparer CCI / PCV / GOF / Mundlak aux sorties Stata. ===\n")

## 6. FIGURES (ggplot2) -----------------------------------------------------
source("R/06_figures.R")   # figures (m2, design, mC en mémoire)
source("R/07_tableaux.R")  # tableaux RTF (m1, m2, m0w, mC en mémoire)
