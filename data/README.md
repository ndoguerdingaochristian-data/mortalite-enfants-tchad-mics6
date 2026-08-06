# Données

Ce projet s'appuie sur l'enquête **MICS6 Tchad (2019)**, produite par l'INSEED
avec l'appui de l'UNICEF. Les données sont **gratuites mais sous licence**, et
**ne peuvent pas être redistribuées**. Elles ne figurent donc pas dans ce dépôt.

## Comment les obtenir

1. Créer un compte sur https://mics.unicef.org (rubrique *Surveys*).
2. Demander l'accès au jeu de données **Chad MICS6 2019**.
3. Une fois l'accès accordé, télécharger les fichiers Stata et les placer ici :

```
data/raw/bh.dta   # historique des naissances (Birth History)
data/raw/wm.dta   # femmes 15-49 ans (Women)
data/raw/hh.dta   # ménages (Household)
data/raw/ch.dta   # enfants de moins de 5 ans (Children)
```

Les scripts du dossier `R/` reconstruisent ensuite la base analytique.
