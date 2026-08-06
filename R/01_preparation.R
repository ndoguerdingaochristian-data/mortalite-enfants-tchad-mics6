# =============================================================================
# 01_preparation.R  -  Construction de la base analytique (MICS6 Tchad 2019)
# Traduction fidele de article_propre_REVISE.do (parties 2 a 4)
# NB : verifier la casse exacte des variables avec names(bh) / names(wm).
# =============================================================================
library(haven)
library(dplyr)

bh <- read_dta("data/raw/bh.dta")
wm <- read_dta("data/raw/wm.dta")

# --- Filtre temporel : enfants nes 0-59 mois avant l'enquête ----------------
bh <- bh %>%
  mutate(mois_avant_enquete = WDOI - BH4C) %>%
  filter(mois_avant_enquete >= 0, mois_avant_enquete < 60)

# --- Tranche d'age (1=0-11, 2=12-23, 3=24-35, 4=36-47, 5=48-59) --------------
bh <- bh %>%
  mutate(tranche_age = cut(mois_avant_enquete,
            breaks = c(-1, 11, 23, 35, 47, 59),
            labels = 1:5) %>% as.integer())

# --- Variable dependante : survie (1=vivant, 0=decede) ----------------------
bh <- bh %>%
  mutate(survie = case_when(BH5 == 1 ~ 1L, BH5 == 2 ~ 0L, TRUE ~ NA_integer_))

# --- Niveau I : déterminants distaux ----------------------------------------
bh <- bh %>%
  mutate(
    milieu    = HH6,                              # ref 1 = urbain
    province  = HH7,                              # ref 18 = N'Djamena
    educ_mere = if_else(welevel == 3, 2, as.numeric(welevel)), # 3 regroupe avec 2
    quintile  = windex5,                          # ref 1 = plus pauvre
    relig     = religion,                         # ref 4 = musulman
    ethnie    = ethnicity                         # ref 13 = Sara/Ngambaye
  )

# --- Niveau II : déterminants intermediaires --------------------------------
bh <- bh %>%
  mutate(
    sexe       = BH3,        # ref 1 = garcon
    rang       = brthord,    # rang de naissance
    intervalle = birthint,   # 1 = <24 mois ; 2,3 = >=24 mois ; 0/97/99 = manquant
    age_mere   = magebrt,    # ref 1 = <20 ans
    type_naiss = BH2         # ref 1 = singulier
  )

# --- Composite parité-intervalle [C1] (référence = modalite 5) ---------------
bh <- bh %>%
  mutate(parite_intervalle = case_when(
    rang == 1                                   ~ 1L,
    rang %in% 2:3 & intervalle == 1             ~ 2L,
    rang %in% 2:3 & intervalle %in% c(2, 3)     ~ 3L,
    rang >= 4     & intervalle == 1             ~ 4L,
    rang >= 4     & intervalle %in% c(2, 3)     ~ 5L,
    !is.na(rang)                                ~ 6L,   # residuel
    TRUE                                        ~ NA_integer_
  ))

# --- Variables de plan de sondage -------------------------------------------
bh <- bh %>% mutate(psu_id = PSU, strate = stratum)

# --- Statut matrimonial depuis wm.dta ---------------------------------------
wm_fusion <- wm %>%
  mutate(statut_mat = case_when(
    MSTATUS == 1            ~ 1L,   # mariee
    MSTATUS == 2            ~ 2L,   # union libre
    MSTATUS %in% c(3, 4)    ~ 3L,   # pas en union
    TRUE                    ~ NA_integer_
  )) %>%
  select(HH1, HH2, WM3, statut_mat) %>%
  distinct(HH1, HH2, WM3, .keep_all = TRUE)

base <- bh %>% left_join(wm_fusion, by = c("HH1", "HH2", "WM3"))

# --- Suppression listwise sur les variables des modèles M1 et M2 ------------
vars_modele <- c("survie", "milieu", "province", "educ_mere", "quintile",
                 "relig", "ethnie", "statut_mat", "sexe", "parite_intervalle",
                 "age_mere", "type_naiss", "wmweight", "psu_id", "strate")

base <- base %>% filter(if_all(all_of(vars_modele), ~ !is.na(.)))

# Retirer les etiquettes haven (évite les erreurs cote modélisation)
base <- base %>% mutate(across(where(~inherits(., "haven_labelled")), as.numeric))

message("Echantillon analytique final : ", nrow(base), " enfants.")
saveRDS(base, "data/base_analytique.rds")
