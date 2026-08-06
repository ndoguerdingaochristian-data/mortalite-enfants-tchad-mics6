# =============================================================================
# 04_multiniveau.R  -  Modèle multiniveau logistique + test de Mundlak
# Moteur principal : lme4::glmer (robuste).
# Note : la version PONDÉRÉE design-based de référence (poids de niveau 1 et 2,
#        méthode Elkasabi) est produite sous Stata (melogit ... pweight()).
#        WeMix est tente en option ; s'il echoue, le pipeline continue.
# =============================================================================
library(dplyr)
library(lme4)

base <- readRDS("data/base_analytique.rds")

# --- Nettoyage des formats haven + facteur de groupe + data.frame simple ----
base <- base %>% mutate(across(where(~inherits(., "haven_labelled")), as.numeric))
base$province <- factor(base$province)
base <- as.data.frame(base)

# --- [C2] Poids Elkasabi (a = 0.5) : construction (documentation methodo) ----
s_h <- 25; alpha <- 0.5
base <- base %>%
  group_by(strate) %>% mutate(a_c_h = n_distinct(psu_id)) %>% ungroup() %>%
  mutate(A_h = a_c_h) %>%
  group_by(psu_id) %>% mutate(n_per_psu = n()) %>% ungroup() %>%
  group_by(strate) %>% mutate(M_h_bar = mean(n_per_psu)) %>% ungroup() %>%
  mutate(
    d_denorm = wmweight,
    f_var    = d_denorm / ((A_h / a_c_h) * (M_h_bar / s_h)),
    wt2_elk  = (A_h / a_c_h) * (f_var ^ alpha),
    wt1_elk  = d_denorm / wt2_elk
  ) %>%
  group_by(province) %>% mutate(wt2_prov = mean(wt2_elk)) %>% ungroup() %>%
  as.data.frame()

ctrl <- glmerControl(optimizer = "bobyqa")  # optimiseur robuste

# --- Modèle NUL : hétérogénéité provinciale et CCI --------------------------
m0 <- glmer(survie ~ 1 + (1 | province), data = base, family = binomial,
            weights = wt1_elk, nAGQ = 0, control = ctrl)   # nul PONDÉRÉ (comparable a Stata)
var_u <- as.data.frame(VarCorr(m0))$vcov[1]
CCI   <- var_u / (var_u + pi^2 / 3)
cat(sprintf("Modèle nul : variance province = %.4f | CCI = %.4f (%.2f%%)\n",
            var_u, CCI, 100 * CCI))

# --- Modèle COMPLET (pondéré par le poids individuel, pseudo-vraisemblance) --
form_complet <- survie ~ factor(milieu) + factor(educ_mere) + factor(quintile) +
  factor(relig) + factor(ethnie) + factor(statut_mat) + factor(sexe) +
  factor(parite_intervalle) + factor(age_mere) + factor(type_naiss) +
  factor(tranche_age) + (1 | province)

m_complet <- glmer(form_complet, data = base, family = binomial,
                   weights = wt1_elk, nAGQ = 0, control = ctrl)
print(summary(m_complet))

var_u2 <- as.data.frame(VarCorr(m_complet))$vcov[1]
CCI2   <- var_u2 / (var_u2 + pi^2 / 3)
PCV    <- (var_u - var_u2) / var_u
cat(sprintf("Complet : CCI = %.4f (%.2f%%) | PCV = %.1f%%\n",
            CCI2, 100 * CCI2, 100 * PCV))
saveRDS(m_complet, "data/modele_multiniveau.rds")

# --- Tentative WeMix (design-based) : optionnelle, n'interrompt pas ----------
ok_wemix <- FALSE
try({
  m_wemix <- WeMix::mix(survie ~ 1 + (1 | province), data = base,
                        weights = c("wt1_elk", "wt2_prov"),
                        family = binomial(link = "logit"))
  print(summary(m_wemix)); ok_wemix <- TRUE
}, silent = TRUE)
if (!ok_wemix)
  message("WeMix indisponible ici : version design-based de référence = Stata (melogit).")

# =============================================================================
# TEST DE MUNDLAK  (effets aléatoires vs fixes)  -  par rapport de vraisemblance
# =============================================================================
vars_mundlak <- c("milieu", "educ_mere", "quintile", "statut_mat",
                  "parite_intervalle", "age_mere")
base <- base %>%
  group_by(province) %>%
  mutate(across(all_of(vars_mundlak), ~ mean(.x, na.rm = TRUE),
                .names = "mpm_{.col}")) %>%
  ungroup() %>% as.data.frame()

m_mundlak <- glmer(
  update(form_complet, . ~ . + mpm_milieu + mpm_educ_mere + mpm_quintile +
           mpm_statut_mat + mpm_parite_intervalle + mpm_age_mere),
  data = base, family = binomial, weights = wt1_elk, nAGQ = 0, control = ctrl)

cat("\n--- Test de Mundlak (rapport de vraisemblance) ---\n")
print(anova(m_complet, m_mundlak))
cat("p > 0.05 -> effets aléatoires preferes | p < 0.05 -> effets fixes.\n")
