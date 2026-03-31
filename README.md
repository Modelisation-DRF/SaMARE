[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0) [![R-CMD-check](https://github.com/Modelisation-DRF/SaMARE/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Modelisation-DRF/SaMARE/actions/workflows/R-CMD-check.yaml)

## Le package SimulateurSaMARE

## Introduction
Un package pour la simulation de l'accroissement, de la mortalité et du recrutement des arbres pour les forêts feuillues et mixtes du Québec (SaMARE).


## Documentation et références
Non disponibles pour l'instant.

## Dépendences
Ce package dépend des packages OutilsDRF, BillonnagePetro et ExtractMap.

OutilsDRF est disponible ici: https://github.com/Modelisation-DRF/OutilsDRF

BillonnagePetro est disponible ici: https://github.com/Modelisation-DRF/BillonnagePetro

ExtractMap est disponible ici: https://github.com/Modelisation-DRF/ExtractMap


## Comment installer le package dans R
```{r eval=FALSE, echo=FALSE, message=FALSE, warning=FALSE}
require(remotes)
install_github("https://github.com/Modelisation-DRF/SaMARE")
```

## Historique des versions
| Date |  Version  | Issues |      Détails     |
|:-----|:---------:|:-------|:-----------------|
| 2024-04-05 |	2.1.0 |		Version stable |
| 2025-02-04 |  2.3.1 |     Version incluant les corrections et améliorations effectuées suites aux validations des chercheurs sans la validation statistique du modèle (calcul biais et erreurs)
| 2025-04-16 |	2.3.2 |		Version avec limitation de la variable temps après coupe |
| 2025-04-16 |	2.3.3 |		Correction de la prévision du nombre de gaules de hêtre de 6 et 8 cm |
| 2025-05-09 |	2.4.0 |		Version optimisée |
| 2025-06-04 |	2.4.1 |		Ajout de verification sur les colonnes et l'utilisation du package ExtractMap, ajout des gaules au temps 0 |
| 2025-08-19 |	2.4.2 |		Ajout de la colonne reg_eco dans le fichier retourné par SimulSamare et renommer les colonnes Prod et Vig dans SortieBillonage |
| 2025-10-21 |	2.4.3 |	Ménage dans Imports et Depends et ajout de package.R	 |
| 2026-02-11 |	2.4.4 |	Modification de SortieBillonnage() pour prendre en compte les modifications apportées au package BillonnagePetro	 |
| 2026-03-31 |	2.4.5 |	Ajout du simulateur de coupe |
