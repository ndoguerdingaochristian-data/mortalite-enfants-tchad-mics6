# Installe les paquets necessaires au projet
paquets <- c("haven", "dplyr", "tidyr", "survey", "broom",
             "lme4", "ggplot2", "modelsummary", "marginaleffects", "flextable")
a_installer <- paquets[!paquets %in% installed.packages()[, "Package"]]
if (length(a_installer)) install.packages(a_installer)
invisible(lapply(paquets, require, character.only = TRUE))
