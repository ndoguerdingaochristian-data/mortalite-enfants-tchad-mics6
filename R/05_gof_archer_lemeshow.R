# =============================================================================
# 05_gof_archer_lemeshow.R
# Test d'ajustement d'ARCHER-LEMESHOW (F-adjusted mean residual test) pour
# regression logistique estimee sur données d'enquête a plan de sondage complexe.
#
# C'est l'equivalent R du "estat gof" après "svy: logit" sous Stata.
# Contrairement au Hosmer-Lemeshow classique, ce test tient compte des
# pondérations, des grappes et des strates.
#
# Références :
#   Archer, K. J. & Lemeshow, S. (2006). Goodness-of-fit test for a logistic
#     regression model fitted using survey sample data. Stata Journal 6(1),97-105.
#   Archer, K. J., Lemeshow, S. & Hosmer, D. W. (2007). Goodness-of-fit tests
#     for logistic regression models when data are collected using a complex
#     sampling design. Computational Statistics & Data Analysis 51, 4450-4464.
# =============================================================================
library(survey)

# -----------------------------------------------------------------------------
# Fonction : test du residu moyen ajuste par F (Archer-Lemeshow)
#   model   : modèle svyglm binomial/quasibinomial déjà estime
#   design  : objet svydesign ayant servi a estimer le modèle
#   ngroups : nombre de groupes de risque (defaut 10, comme Hosmer-Lemeshow)
# -----------------------------------------------------------------------------
al_gof <- function(model, design, ngroups = 10) {

  p <- as.numeric(fitted(model))          # probabilités ajustees
  y <- as.numeric(model$y)                # variable dependante 0/1
  resid <- y - p                          # residu brut

  # Verification d'alignement (aucune ligne supprimee par NA cote modèle)
  if (length(p) != nrow(design$variables))
    stop("Le modèle et le design n'ont pas le meme nombre d'observations : ",
         "verifier qu'aucune ligne n'a ete supprimee (NA) a l'estimation.")

  # Groupes par deciles de risque (quantiles des probabilités ajustees)
  brks <- quantile(p, probs = seq(0, 1, length.out = ngroups + 1),
                   na.rm = TRUE, type = 2)
  brks <- unique(brks)
  grp  <- cut(p, breaks = brks, include.lowest = TRUE)
  G    <- nlevels(grp)
  if (G < 3) stop("Trop peu de groupes distincts (probabilités trop concentrees).")

  # Ajouter residus et groupes au design (meme ordre que les données)
  d2 <- update(design, .resid = resid, .grp = grp)

  # Moyenne design-based du residu par groupe, avec covariance COMPLETE
  m <- svyby(~.resid, ~.grp, d2, svymean, covmat = TRUE)
  b <- coef(m)
  V <- vcov(m)

  # On retire un groupe : la somme pondérée des residus est ~0 (modèle avec
  # constante), donc la matrice des G moyennes est singuliere. G-1 suffit.
  idx <- seq_len(G - 1)
  b1  <- b[idx]
  V1  <- V[idx, idx, drop = FALSE]

  # Statistique de Wald (chi2 a G-1 ddl)
  W <- as.numeric(t(b1) %*% solve(V1) %*% b1)

  # Ajustement F du second ordre avec les degres de liberte du plan
  f     <- degf(design)                   # nb de PSU - nb de strates
  Fstat <- ((f - G + 2) / (f * (G - 1))) * W
  pval  <- pf(Fstat, df1 = G - 1, df2 = f - G + 2, lower.tail = FALSE)

  structure(list(
    statistic = c(F = Fstat),
    parameter = c(ndf = G - 1, ddf = f - G + 2),
    p.value   = pval,
    method    = paste0("Archer-Lemeshow (F-adjusted mean residual GOF), ",
                       G, " groupes"),
    data.name = paste0("Wald = ", round(W, 3), " | df du plan = ", f)
  ), class = "htest")
}

# --- Execution sur le modèle complet M2 -------------------------------------
design <- readRDS("data/design_svy.rds")
mods   <- readRDS("data/modeles_logit.rds")   # sauvegarde par 03_logit_pondere.R

res_gof <- al_gof(mods$m2, design, ngroups = 10)
print(res_gof)

cat("\nLecture : p > 0.05  => aucune preuve de mauvais ajustement (le modèle s'ajuste bien).",
    "\n         p < 0.05  => l'ajustement est rejete.\n")

# Sauvegarde du résultat pour le rapport / l'article
saveRDS(res_gof, "outputs/tables/gof_archer_lemeshow.rds")
