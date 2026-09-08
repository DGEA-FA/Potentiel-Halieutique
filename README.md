# Calcul du potentiel halieutique

Application R Shiny d'aide à la gestion de la pêche récréative en eaux
intérieures. Elle estime le **quota théorique de récolte** d'un lac à partir de
sa morphométrie et de sa communauté de poissons, selon plusieurs modèles de
rendement publiés, et met le résultat en regard des **statistiques de récolte
observées**.

Trois espèces sont couvertes : touladi, doré jaune et omble de fontaine. Chacune
a ses propres modèles, ses propres intrants et son propre modèle de référence —
l'outil ne transpose jamais l'approche d'une espèce à une autre.

<!-- À compléter : direction responsable, personne-ressource, licence -->

## Statut

En développement actif. Trois réserves à connaître avant usage en production :

- **Les exports Excel et PDF ne sont pas implémentés.** Les boutons existent et
  produisent un fichier contenant un message d'attente.
- **Le modèle recommandé pour l'omble de fontaine est provisoire**, en attente
  d'un rapport préliminaire. La cascade actuelle place Valin et Vaillancourt
  1998 en tête, mais ce choix est susceptible d'être révisé.
- **La source des données climatiques par région reste à confirmer** du côté du
  MELCCFP.

## Espèces et modèles

Pour chaque espèce, les modèles sont évalués dans l'ordre de la cascade. Le
premier qui dispose de tous ses intrants devient le **modèle de référence** :
c'est celui qui pilote le quota estimé. Les autres restent affichés dans le
tableau de comparaison.

| Espèce | Cascade de référence | Intrants du modèle de tête |
|---|---|---|
| Touladi | Lester 2021 → Shuter 1998 → Valin 1998 → IME/Ryder | superficie, prof. max, prof. moyenne, T° air, longueur asymptotique, thermocline |
| Doré jaune | Lester et coll. 2002 → Valin/Vaillancourt 1998 → IME/Ryder | superficie, TDS (conductivité), degrés-jours, habitat thermo-optique |
| Omble de fontaine | Valin et Vaillancourt 1998 → Archambault 1988/2009 → Vézina 1978 | prof. moyenne, espèces présentes, pH, oxygène, tributaire, chalets |

Le modèle IME (indice morpho-édaphique de Ryder) estime un rendement
**communautaire**, partitionné ensuite vers l'espèce visée selon OMNR 1982 —
25 % pour le touladi, 32 % pour le doré. Il n'est pas utilisé pour l'omble,
faute de partition sourcée dans la littérature consultée.

Des **grilles régionales** de référence (Nord-du-Québec, Mauricie, Laurentides)
s'ajoutent au tableau de comparaison selon l'espèce. Elles ne participent pas à
la cascade : ce sont des points de repère, pas des modèles calculés.

Un quota estimé correspond, par défaut, à **80 % du rendement maximal
théorique** du modèle de référence — le « pretty good yield » de Hilborn (2010).
Ce taux est modifiable dans l'interface.

## Données d'entrée

### 1. Fichier « Potentiel halieutique » (.xlsx, 4 onglets)

| Onglet | Contenu utilisé |
|---|---|
| `Lacs` | numéro et nom du plan d'eau, latitude, longitude, superficie, profondeurs max et moyenne, périmètre |
| `Profil` | profils thermiques et d'oxygène dissous par inventaire (thermocline, réduction O₂) |
| `Parametre` | conductivité (CD), transparence Secchi (TR), pH (PH) |
| `Specimens` | longueurs totales, pour l'estimation de la longueur asymptotique |

La reconnaissance des colonnes se fait par motif, insensible à la casse et aux
accents — les variantes de libellés sont tolérées.

### 2. Fichier d'exploitation (.xlsx)

Colonnes clés : numéro et nom du plan d'eau, année, code d'espèce, nombre de
captures, nombre pesés, masse mesurée (kg), effort total (jours-pêcheurs).

Alimente le rendement observé et l'onglet d'analyse.

### 3. Rasters climatiques (.tif) — optionnel

Grilles annuelles Info-Climat (MELCCFP) : `DJC5` (degrés-jours au-dessus de
5 °C) et `TMOY` (température moyenne annuelle de l'air). Voir
[`donnees/climat/LISEZMOI.md`](donnees/climat/LISEZMOI.md) pour le nommage et la
procédure de mise à jour.

Les fichiers présents dans `donnees/climat` sont chargés au démarrage.
L'importation manuelle dans l'interface permet de les compléter ou de les
remplacer ponctuellement. Sans raster ni saisie manuelle, la température de
l'air est simplement absente : elle n'est **jamais** déduite d'une autre
variable.

## Fonctionnalités

**Onglet « Rendements théoriques »** — quota estimé en kg/an et son écart avec
le quota en vigueur ; barre de positionnement en kg/ha situant le rendement
retenu, le maximum théorique et le rendement observé sur trois zones
(conservateur, recommandé, élevé) ; tableau comparatif de tous les modèles, avec
la raison de l'indisponibilité de chacun le cas échéant.

**Onglet « Analyse des données d'exploitation »** — séries historiques de
récolte, d'effort et de rendement, avec tendance estimée par la pente de
Theil-Sen (robuste aux valeurs aberrantes et à l'espacement irrégulier des
années).

**Contrôle qualité** — un bandeau signale, dès la sélection du lac, les données
manquantes ou douteuses et les modèles qu'elles rendent indisponibles. L'outil
ne substitue jamais silencieusement une valeur par défaut à une donnée absente.

## Structure du dépôt

```
app.R                        application complète (UI + serveur)
config_especes.R             registre des espèces, modèles et cascades
donnees/climat/              rasters climatiques (.tif) + LISEZMOI
manifest.json                dépendances, pour le déploiement Posit Connect
```

`config_especes.R` est le point d'entrée pour ajouter ou retirer un modèle :
il déclare, pour chaque espèce, la liste des modèles, la fonction de calcul,
les intrants requis et l'ordre de la cascade. Les fonctions de calcul
elles-mêmes vivent dans `app.R`.

## Prérequis

```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "readxl",
                   "DT", "plotly", "scales", "terra"))
```

`terra` n'est nécessaire que pour l'extraction climatique ; l'application reste
pleinement fonctionnelle sans lui, avec saisie manuelle de la température de
l'air et des degrés-jours. Il dépend de GDAL, PROJ et GEOS au niveau système.

## Exécution locale

```r
shiny::runApp()
```

## Déploiement — Posit Connect Cloud

Le déploiement se fait depuis ce dépôt GitHub.

1. Générer le manifeste depuis le répertoire de l'application :
   `rsconnect::writeManifest()`. À relancer chaque fois qu'une dépendance change.
2. Committer et pousser `manifest.json` avec le code.
3. Dans Connect, lier le dépôt (URL, branche, répertoire cible).

Trois points à garder en tête. Connect Cloud n'offre **aucun système de fichiers
partagé** : seuls les fichiers du dépôt sont disponibles à l'exécution, et tout
ce qui est écrit pendant l'exécution est perdu au redémarrage — d'où les rasters
versionnés dans `donnees/climat`. Un contenu déployé depuis Git ne peut plus
être mis à jour autrement qu'en poussant sur la branche suivie. Et Git LFS n'est
pas pris en charge.

<!-- À compléter : URL de l'application déployée -->

## Limites connues

Les modèles supposent des conditions climatiques moyennes ; la fenêtre
climatique par défaut n'utilise qu'une seule année. Les moyennes sur 5 ou 10 ans
sont disponibles dans l'interface et s'en approchent davantage.

L'extraction climatique se fait au plus proche voisin sur une grille d'environ
10 km, sans interpolation. L'emprise des rasters s'arrête vers 62,1° N : les
lacs plus au nord n'auront pas de valeur.

Le palier « ménés et/ou catostomes seuls » du modèle Valin et Vaillancourt est
fixé à 60 %, milieu d'une plage documentée de 50 à 70 %. Cette valeur n'est pas
sourcée et reste à valider.

La colonne « À 80 % » du tableau comparatif est toujours calculée au taux
recommandé, indépendamment du taux saisi en haut de l'onglet — c'est voulu, et
l'en-tête le dit.

## Références

- Hilborn, R. (2010) — concept de *pretty good yield*
- Lester, N. et coll. (2002) — doré jaune, habitat thermo-optique
- Lester, N. et coll. (2021) — touladi
- Shuter, B. et coll. (1983, 1998) — thermocline théorique, touladi
- Ryder, R. — indice morpho-édaphique ; OMNR (1982) — partition par espèce
- Vézina (1978), Archambault (1988/2009), Valin (1998), Valin et Vaillancourt
  (1998) — omble de fontaine

<!-- À compléter : références complètes, liens intranet -->
