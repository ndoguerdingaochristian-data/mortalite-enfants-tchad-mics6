# =============================================================================
# 03_logit_pondere.R
# Regression logistique pondérée design-based (equivalent svy: logit ... , or)
# Modèles hierarchiques : X1 (distaux) puis X1 + X2 (proches), cadre Mosley-Chen
# =============================================================================
library(survey)
library(broom)
library(dplyr)

design <- readRDS("data/design_svy.rds")

# --- Blocs de variables (références identiques au do-file Stata) ------------
# X1 : déterminants distaux ; X2 : déterminants proches
X1 <- c("relevel(factor(milieu), ref='1')",
        "relevel(factor(province), ref='18')",
        "relevel(factor(educ_mere), ref='0')",
        "relevel(factor(quintile), ref='1')",
        "relevel(factor(relig), ref='4')",
        "relevel(factor(ethnie), ref='13')")

X2 <- c("relevel(factor(sexe), ref='1')",
        "relevel(factor(parite_intervalle), ref='3')",
        "relevel(factor(age_mere), ref='1')",
        "relevel(factor(type_naiss), ref='1')",
        "relevel(factor(tranche_age), ref='1')")

f1 <- as.formula(paste("survie ~", paste(X1, collapse = " + ")))
f2 <- as.formula(paste("survie ~", paste(c(X1, X2), collapse = " + ")))

# --- Modèle M1 (distaux seuls) ----------------------------------------------
m1 <- svyglm(f1, design = design, family = quasibinomial())

# --- Modèle M2 (distaux + proches) ------------------------------------------
m2 <- svyglm(f2, design = design, family = quasibinomial())

# --- Odds ratios + intervalles de confiance ---------------------------------
or_table <- function(mod) {
  broom::tidy(mod, conf.int = TRUE, exponentiate = TRUE) %>%
    select(term, OR = estimate, IC_bas = conf.low, IC_haut = conf.high, p = p.value)
}
write.csv(or_table(m1), "outputs/tables/OR_M1_distaux.csv", row.names = FALSE)
write.csv(or_table(m2), "outputs/tables/OR_M2_complet.csv", row.names = FALSE)
print(or_table(m2))

# --- Qualite d'ajustement ---------------------------------------------------
# Le test d'ajustement design-based (Archer-Lemeshow, equivalent du estat gof
# de Stata, qui tient compte du plan de sondage) est dans le script suivant :
#   R/05_gof_archer_lemeshow.R

saveRDS(list(m1 = m1, m2 = m2), "data/modeles_logit.rds")
message("Logit pondéré estime (M1 et M2).")
