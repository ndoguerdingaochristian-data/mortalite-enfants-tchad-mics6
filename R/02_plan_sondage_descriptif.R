# =============================================================================
# 02_plan_sondage_descriptif.R
# Plan de sondage complexe + statistiques descriptives design-based
# Equivalent R de : svyset psu_id [pweight=wmweight], strata(strate)
#                   svy: tab ... , pearson  (tests de Rao-Scott)
# =============================================================================
library(survey)
library(dplyr)

base <- readRDS("data/base_analytique.rds")

# --- Declaration du plan de sondage -----------------------------------------
# ids     = grappes de premier degre (ZD)
# strata  = strates (province x milieu)
# weights = pondération femmes
# nest    = TRUE : PSU emboitees dans les strates
options(survey.lonely.psu = "certainty")   # equivaut a singleunit(certainty)

design <- svydesign(
  ids     = ~psu_id,
  strata  = ~strate,
  weights = ~wmweight,
  data    = base,
  nest    = TRUE
)

# --- Prévalence pondérée de la mortalité (avec IC design-based) --------------
prev <- svyciprop(~I(survie == 0), design, method = "logit")
print(prev)

# --- Tests d'association de Rao-Scott (equivalent svy: tab ..., pearson) -----
# Le test du chi2 corrige pour le plan de sondage (statistic = "F")
vars_distales <- c("milieu", "educ_mere", "quintile", "relig", "ethnie", "province")
vars_proches  <- c("sexe", "parite_intervalle", "age_mere", "type_naiss", "tranche_age")

for (v in c(vars_distales, vars_proches)) {
  cat("\n---", v, "x survie (Rao-Scott) ---\n")
  f <- as.formula(paste0("~", v, "+survie"))
  print(svychisq(f, design, statistic = "F"))
}

# --- Repartition pondérée par province (ligne %) ----------------------------
tab_prov <- svyby(~I(survie == 0), ~province, design, svymean, na.rm = TRUE)
write.csv(tab_prov, "outputs/tables/mortalite_par_province.csv", row.names = FALSE)

saveRDS(design, "data/design_svy.rds")
message("Plan de sondage declare et descriptifs produits.")
