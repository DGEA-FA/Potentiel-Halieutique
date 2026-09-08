# =============================================================================
# app_quotas.R
# Calculateur de quotas de pêche — Touladi / Doré jaune / Omble de fontaine
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)
library(plotly)

# Taille maximale d'upload (défaut Shiny : 5 Mo — trop petit pour le fichier
# Potentiel halieutique à 4 onglets, surtout pour les grandes régions).
# Les rasters climatiques ne pèsent qu'une centaine de Ko chacun (grille
# 164 x 199 en LZW) : ils tiennent largement dans cette limite.
options(shiny.maxRequestSize = 50 * 1024^2)  # 50 Mo


# =============================================================================
# PALETTE PAR ESPÈCE
# =============================================================================
COULEURS <- list(
  touladi = list(
    primaire   = "#2C3E50",
    accent     = "#4A6FA5",
    tab_actif  = "#74B9E8",
    bar        = "#243342",   # bande espèces (sombre)
    tint        = "#EBF2FA"   # fond clair des cartes accentuées
  ),
  dore = list(
    primaire   = "#B07D1A",
    accent     = "#E6A817",
    tab_actif  = "#F6C84B",
    bar        = "#5C4108",
    tint        = "#FBF3DF"
  ),
  omble = list(
    primaire   = "#8B2252",
    accent     = "#B83D74",
    tab_actif  = "#E07FAD",
    bar        = "#5A1636",
    tint        = "#F7E6EE"
  )
)

ESPECE_ACTIVE <- "touladi"
COL <- COULEURS[[ESPECE_ACTIVE]]

# Couleurs des trois zones de la fourchette
# (source unique : figure, légende, cartes KPI, badges d'état)
ZONES <- list(
  conservateur = list(fill = "#D3D1C7", trait = "#8A8F94", nom = "Conservateur"),
  recommande   = list(fill = "#C0DD97", trait = "#3B6D11", nom = "Recommandé"),
  eleve        = list(fill = "#FAC775", trait = "#B97A0B", nom = "Élevé")
)

# Seuil du rendement recommandé : 80 % du maximum théorique (pretty good yield, Hilborn 2010)
PCT_RECOMMANDE <- 80L


# =============================================================================
# FONCTIONS UTILITAIRES
# =============================================================================

#' Résoudre Linf : valeur observée ou formule théorique Lester éq. 1
#' @param Linf_obs  Linf mesuré (mm) — NA si absent
#' @param A         Superficie (ha)
resolve_linf <- function(Linf_obs, A) {
  if (is.na(Linf_obs) || Linf_obs <= 0)
    return(list(value  = 957 * (1 - exp(-0.14 * (1 + log(A)))),
                source = "théorique"))
  list(value = Linf_obs, source = "observé")
}

#' Conductivité (µS/cm) → TDS (mg/L)
conductivite_vers_tds <- function(cond_uS) cond_uS * 0.666


# =============================================================================
# DONNÉES CLIMATIQUES — extraction ponctuelle depuis des rasters GeoTIFF
#
# Source : grilles annuelles Info-Climat (MELCCFP) — DJC5 (degrés-jours au-dessus
# de 5 °C) et TMOY (température moyenne annuelle de l'air), un fichier par année.
# Convention de nommage attendue : VARIABLE_ANNEE.tif (ex. DJC5_2024.tif).
#
# Grille provinciale 164 x 199 cellules d'environ 10 km, Float32 compressé LZW,
# EPSG:32198 (NAD83 / Québec Lambert), emprise s'arrêtant vers 62,1° N —
# une centaine de Ko par fichier. Dix années des deux variables tiennent donc
# dans le dépôt Git sans difficulté : c'est la source par défaut, et
# l'importation manuelle sert de complément ponctuel.
#
# Méthode (script fourni par la DFA) : le point est construit en EPSG:4326,
# projeté dans le CRS du raster, puis la valeur est lue avec terra::extract()
# au plus proche voisin. Pas d'interpolation bilinéaire : la grille Info-Climat
# est déjà un produit interpolé, et le plus proche voisin préserve la
# correspondance exacte avec les valeurs de référence de la DFA. L'écart entre
# les deux méthodes reste de l'ordre de 1 à 3 % sur le rendement.
#
# Remplace la régression T_air = -11,55 + 0,00909 x G, retirée : la température
# est désormais lue directement plutôt que déduite des degrés-jours.
# =============================================================================

# Répertoire du dépôt contenant les rasters, lu au démarrage. Chemin RELATIF au
# répertoire de l'application : c'est ce qui permet au même code de fonctionner
# en local et sur Posit Connect Cloud, où le contenu du dépôt GitHub est copié
# tel quel (aucun système de fichiers partagé n'y est disponible).
CLIMAT_REPERTOIRE <- "donnees/climat"

CLIMAT_PATRON_FICHIER <- "^(DJC5|TMOY)_(\\d{4})\\.tif$"

CLIMAT_FENETRES <- c("Année la plus récente" = "1",
                     "Moyenne 5 dernières"   = "5",
                     "Moyenne 10 dernières"  = "10")

#' terra est-il disponible ? Vérifié à l'appel plutôt qu'au démarrage : l'outil
#' doit rester pleinement utilisable sans le paquet, la fonctionnalité climatique
#' étant la seule à en dépendre.
terra_disponible <- function() requireNamespace("terra", quietly = TRUE)

#' Catalogue à partir d'une liste de noms et de chemins.
#' Un fichier dont le nom ne suit pas la convention est ÉCARTÉ et signalé —
#' jamais rattaché à une année ou à une variable par défaut.
catalogue_depuis_noms <- function(noms, chemins, origine) {
  if (length(noms) == 0) return(list(catalogue = NULL, ignores = character(0)))

  m  <- regmatches(noms, regexec(CLIMAT_PATRON_FICHIER, noms, ignore.case = TRUE))
  ok <- vapply(m, function(x) length(x) == 3L, logical(1))
  if (!any(ok)) return(list(catalogue = NULL, ignores = noms))

  cat_df <- data.frame(
    variable = toupper(vapply(m[ok], `[`, character(1), 2L)),
    annee    = as.integer(vapply(m[ok], `[`, character(1), 3L)),
    chemin   = chemins[ok],
    nom      = noms[ok],
    origine  = origine,
    stringsAsFactors = FALSE
  )
  list(catalogue = cat_df, ignores = noms[!ok])
}

#' Rasters présents dans le dépôt. Lu à chaque appel plutôt que mis en cache :
#' l'opération est un simple list.files() et le répertoire ne change pas en
#' cours de session, mais ça évite un état global à invalider.
catalogue_repertoire <- function(chemin = CLIMAT_REPERTOIRE) {
  if (!nzchar(chemin) || !dir.exists(chemin)) return(NULL)
  f <- list.files(chemin, pattern = "\\.tif$", ignore.case = TRUE, full.names = TRUE)
  if (length(f) == 0) return(NULL)
  catalogue_depuis_noms(basename(f), f, "dépôt")$catalogue
}

#' Catalogue complet : dépôt + fichiers téléversés (TMOY et DJC5 séparément).
#' En cas de doublon variable + année, le fichier TÉLÉVERSÉ l'emporte sur celui
#' du dépôt — l'import manuel sert précisément à corriger ou compléter le dépôt.
catalogue_rasters <- function(files_tmoy = NULL, files_djc5 = NULL) {
  depot <- catalogue_repertoire()

  lots <- list()
  ignores <- character(0)
  for (fd in list(files_tmoy, files_djc5)) {
    if (is.null(fd) || nrow(fd) == 0) next
    res <- catalogue_depuis_noms(fd$name, fd$datapath, "téléversé")
    if (!is.null(res$catalogue)) lots[[length(lots) + 1L]] <- res$catalogue
    ignores <- c(ignores, res$ignores)
  }
  televerses <- if (length(lots) > 0) do.call(rbind, lots) else NULL

  # rbind(téléversés, dépôt) puis suppression des doublons en gardant la
  # PREMIÈRE occurrence : les téléversés priment.
  cat_df <- rbind(televerses, depot)
  if (is.null(cat_df) || nrow(cat_df) == 0)
    return(list(catalogue = NULL, ignores = ignores))

  cat_df <- cat_df[!duplicated(cat_df[, c("variable", "annee")]), ]
  cat_df <- cat_df[order(cat_df$variable, -cat_df$annee), ]

  list(catalogue = cat_df, ignores = ignores)
}

#' Valeur d'un raster à un point (lat/lon en degrés décimaux, EPSG:4326).
#' Retourne NA assorti d'un motif plutôt que de lever une erreur : un fichier
#' illisible, un point hors emprise ou une cellule sans valeur ne doivent pas
#' interrompre le calcul.
extraire_valeur_raster <- function(lat, lon, chemin) {
  if (!terra_disponible())
    return(list(valeur = NA_real_, motif = "paquet terra non installé"))
  if (is.na(lat) || is.na(lon))
    return(list(valeur = NA_real_, motif = "coordonnées du lac inconnues"))

  res <- tryCatch({
    # Shiny ne garantit pas l'extension du fichier temporaire d'un téléversement,
    # et certains pilotes GDAL s'y fient. On ne recopie donc QUE dans ce cas :
    # les fichiers du dépôt, déjà nommés en .tif, sont ouverts sur place.
    src <- if (grepl("\\.tif$", chemin, ignore.case = TRUE)) {
      chemin
    } else {
      tmp <- tempfile(fileext = ".tif")
      file.copy(chemin, tmp, overwrite = TRUE)
      tmp
    }

    r  <- terra::rast(src)
    pt <- terra::vect(data.frame(x = lon, y = lat), geom = c("x", "y"),
                      crs = "EPSG:4326")
    pt <- terra::project(pt, terra::crs(r))

    # Distinguer « hors emprise » de « cellule sans valeur » : les deux donnent
    # NA, mais appellent des corrections différentes de la part de l'utilisateur.
    xy <- terra::crds(pt)
    e  <- as.vector(terra::ext(r))
    dedans <- xy[1, 1] >= e[["xmin"]] && xy[1, 1] <= e[["xmax"]] &&
              xy[1, 2] >= e[["ymin"]] && xy[1, 2] <= e[["ymax"]]

    v <- terra::extract(r, pt)
    # extract() renvoie un data.frame : colonne 1 = ID, colonne 2 = valeur
    list(val = as.numeric(v[1, 2]), dedans = dedans)
  }, error = function(e) e)

  if (inherits(res, "error"))
    return(list(valeur = NA_real_, motif = conditionMessage(res)))
  if (length(res$val) == 0 || is.na(res$val))
    return(list(valeur = NA_real_,
                motif = if (isTRUE(res$dedans))
                  "cellule sans valeur (NoData) à cet emplacement"
                else
                  "hors de l'emprise du raster (couverture jusqu'à ~62° N)"))
  list(valeur = res$val, motif = NA_character_)
}

#' Valeur climatique d'un lac sur une fenêtre de n années.
#' La moyenne porte sur les valeurs extraites année par année : l'extraction
#' ponctuelle étant une opération linéaire, c'est strictement équivalent à
#' extraire d'un raster moyen, sans charger n rasters simultanément.
#' Si moins de n années sont disponibles, les années présentes sont utilisées
#' et le nombre réel est retourné — aucune complétion implicite.
extraire_climat <- function(lat, lon, catalogue, variable, n_annees) {
  vide <- function(motif) list(valeur = NA_real_, annees = integer(0), motif = motif)
  if (is.null(catalogue) || nrow(catalogue) == 0) return(vide("aucun raster disponible"))

  sub <- catalogue[catalogue$variable == variable, , drop = FALSE]
  if (nrow(sub) == 0) return(vide(paste0("aucun raster ", variable, " disponible")))

  sub <- sub[order(-sub$annee), , drop = FALSE]
  sub <- utils::head(sub, n_annees)

  vals <- numeric(0); annees <- integer(0); motifs <- character(0)
  for (i in seq_len(nrow(sub))) {
    ex <- extraire_valeur_raster(lat, lon, sub$chemin[i])
    if (!is.na(ex$valeur)) {
      vals   <- c(vals, ex$valeur)
      annees <- c(annees, sub$annee[i])
    } else {
      motifs <- c(motifs, paste0(sub$annee[i], " : ", ex$motif))
    }
  }
  if (length(vals) == 0)
    return(vide(if (length(motifs) > 0) motifs[1] else "aucune valeur extraite"))

  list(valeur = mean(vals),
       annees = sort(annees),
       motif  = if (length(motifs) > 0)
                  paste0(length(motifs), " année(s) sans valeur") else NA_character_)
}

# URL vers la référence Lester et coll. 2002 (à remplir avec le lien intranet).
# Laissée vide (""), aucun lien ne s'affiche dans le tableau des modèles.
LIEN_LESTER_SAVI <- ""

#' Premier modèle applicable de la cascade de référence de l'espèce active.
#'
#' Le nom affiché est lu DANS LE REGISTRE (config_especes.R) plutôt que dans une
#' table interne. L'ancienne version ne connaissait que les clés du Touladi
#' (lester / shuter / ime) : les clés du Doré (lester_savi, valin) et de l'Omble
#' (omble_valin, archambault, vezina) retournaient donc NA. La
#' VALEUR du rendement était correcte — seul le nom affiché était brisé (carte
#' KPI « Modèle : NA » et étiquette de la figure). Corrigé 2026-09 : tout modèle
#' ajouté au registre hérite désormais automatiquement de son nom, sans
#' modification ici.
#'
#' @param r        liste retournée par results()
#' @param cascade  ordre de priorité — config()$cascade_reference
#' @param modeles  liste des modèles de l'espèce active — config()$modeles
#' @return         list(val, nom, cle) ou NULL si aucun modèle de la cascade n'aboutit
modele_reference <- function(r, cascade, modeles = NULL) {
  ok <- function(x) !is.null(x) && !is.na(x) && is.finite(x)
  if (is.null(r)) return(NULL)
  for (k in cascade) {
    res <- r[[k]]
    if (!is.null(res) && ok(res$rendement_ha)) {
      # Repli sur la clé brute si le registre n'est pas fourni : on préfère
      # afficher "lester_savi" qu'un NA silencieux.
      nom <- if (!is.null(modeles) && !is.null(modeles[[k]]$nom)) modeles[[k]]$nom else k
      return(list(val = res$rendement_ha, nom = nom, cle = k))
    }
  }
  NULL
}

#' Formater un nombre : virgule décimale + espace pour les milliers (convention québécoise)
fmt_nb <- function(x, dec = 2) {
  ifelse(is.na(x), "—",
         formatC(round(as.numeric(x), dec), format = "f", digits = dec,
                 big.mark = " ", decimal.mark = ","))
}

#' Formater un entier : espace pour les milliers
fmt_int <- function(x) {
  ifelse(is.na(x), "—",
         formatC(round(as.numeric(x)), format = "d", big.mark = " "))
}

#' Repli sur une valeur par défaut si l'input n'a jamais été rendu (NULL).
#' Nécessaire parce que les champs d'une espèce inactive n'existent pas côté
#' client : is.na(NULL) renvoie logical(0), ce qui fait planter un if().
`%||%` <- function(x, y) if (is.null(x)) y else x

# Rapport Lmax/Linf — Ricker 1975 (Lmax ≈ 0,95 × Linf pour les salmonidés)
LMAX_LINF_RATIO <- 0.95

#' Linf (Janošík) : longueur asymptotique estimée à partir de spécimens mesurés.
#'
#' Coupe les 5 % supérieurs par RANG (pas par valeur < q95) pour éviter
#' d'éliminer une grappe d'ex aequo au sommet, ce qui sous-estimerait Linf.
#'
#' @param lt_vec  Vecteur de longueurs totales (mm), NA exclus en amont
#' @return        Linf (mm) ou NA_real_ si trop peu de spécimens
calc_linf_janoscik <- function(lt_vec) {
  lt_vec <- lt_vec[!is.na(lt_vec) & lt_vec > 0]
  n      <- length(lt_vec)
  if (n < 10L) return(NA_real_)

  # Coupe par rang : retire les ceil(5 % × n) plus grands individus
  n_cut  <- max(1L, ceiling(n * 0.05))
  lt_sub <- sort(lt_vec, decreasing = TRUE)[-seq_len(n_cut)]
  if (length(lt_sub) < 5L) return(NA_real_)

  lmax <- mean(head(sort(lt_sub, decreasing = TRUE), 5L))
  lmax / LMAX_LINF_RATIO   # Lmax ≈ LMAX_LINF_RATIO × Linf
}


#' Détection de thermocline — méthode normalisée MELCCFP (gradient -ΔT/Δz ≥ 1 °C/m).
#'
#' Pipeline :
#'   1. Agrégation : température moyenne par profondeur
#'   2. Interpolation linéaire à pas ~1 m (précision de borne, robustesse du seuil)
#'   3. Gradient sur la grille interpolée
#'   4. Bloc contigu contenant le gradient maximal → z_hypo = bas de ce bloc
#'      (protège contre un segment raide parasite près du fond)
#'
#' @param profil      data.frame avec colonnes `prof_mes` et `temp`
#' @param seuil_grad  Gradient minimal (°C/m) pour définir la thermocline (défaut 1)
#' @return            data.frame(z_thermo_top, z_thermo_bot, z_hypo, epaisseur,
#'                               statut, raison) — colonnes NA si non stratifié
detect_thermocline_norm <- function(profil, seuil_grad = 1) {

  # Structure de sortie vide
  vide <- function(raison) {
    data.frame(z_thermo_top = NA_real_, z_thermo_bot = NA_real_,
               z_hypo = NA_real_, epaisseur = NA_real_,
               statut = "non_stratifie", raison = raison,
               stringsAsFactors = FALSE)
  }

  # 1. Agrégation et tri
  p <- profil %>%
    filter(!is.na(prof_mes), !is.na(temp)) %>%
    group_by(prof_mes) %>%
    summarise(temp = mean(temp, na.rm = TRUE), .groups = "drop") %>%
    arrange(prof_mes)

  if (nrow(p) < 5L)                return(vide("moins de 5 points valides"))
  if (any(diff(p$prof_mes) <= 0))  return(vide("profondeurs non strictement croissantes"))

  # 2. Interpolation linéaire à pas ~1 m
  z_min  <- ceiling(min(p$prof_mes))
  z_max  <- floor(max(p$prof_mes))
  if (z_max - z_min < 2) return(vide("plage de profondeur insuffisante (< 2 m)"))
  z_grid <- seq(z_min, z_max, by = 1)
  t_grid <- approx(p$prof_mes, p$temp, xout = z_grid)$y

  # 3. Gradient sur la grille (°C/m, positif quand T décroît avec la prof.)
  dz   <- diff(z_grid)
  dT   <- diff(t_grid)
  ok   <- dz > 0 & !is.na(dz) & !is.na(dT)
  if (!any(ok)) return(vide("gradient impossible à calculer"))
  grad <- -dT / dz                            # positif si T↓ avec prof↑

  idx_thermo <- which(ok & grad >= seuil_grad)
  if (length(idx_thermo) == 0L) return(vide("aucun segment ≥ seuil (profil non stratifié)"))

  # 4. Bloc contigu contenant le gradient maximal
  idx_max  <- which.max(grad[idx_thermo])
  anchor   <- idx_thermo[idx_max]             # segment avec le gradient max

  # Étendre vers le haut et le bas tant que grad ≥ seuil et contigu
  sup_idx  <- anchor
  while (sup_idx - 1L >= 1L && (sup_idx - 1L) %in% idx_thermo) sup_idx <- sup_idx - 1L
  inf_idx  <- anchor
  while (inf_idx + 1L <= length(grad) && (inf_idx + 1L) %in% idx_thermo) inf_idx <- inf_idx + 1L

  z_thermo_top <- z_grid[sup_idx]
  z_thermo_bot <- z_grid[inf_idx + 1L]       # bas du dernier segment

  data.frame(
    z_thermo_top = z_thermo_top,
    z_thermo_bot = z_thermo_bot,
    z_hypo       = z_thermo_bot,              # Dth = bas de la thermocline (début hypolimnion)
    epaisseur    = z_thermo_bot - z_thermo_top,
    statut       = "stratifie",
    raison       = "",
    stringsAsFactors = FALSE
  )
}


#' Detection automatique de la reduction O2 (Omble, methode Valin 1998) --
#' meme patron que detect_thermocline_norm() : interpolation lineaire sur
#' grille 1 m (approx()), puis application des regles Valin (bascule <=10 m
#' / >10 m -- voir calc_reduction_o2_valin()).
#' @param profil    data.frame(prof_mes, do) -- issu de ifa$profils[[inv_key]]
#' @param prof_max  Profondeur maximale du lac (m)
#' @return data.frame(m_sous_5ppm, statut, raison)
detect_o2_sous_5ppm <- function(profil, prof_max) {
  vide <- function(raison) {
    data.frame(m_sous_5ppm = NA_real_, statut = "indisponible", raison = raison,
               stringsAsFactors = FALSE)
  }

  if (is.na(prof_max) || prof_max <= 0) return(vide("profondeur maximale manquante"))

  p <- profil %>%
    filter(!is.na(prof_mes), !is.na(do)) %>%
    arrange(prof_mes)

  if (nrow(p) < 2L) return(vide("moins de 2 lectures d'oxygène valides"))
  if (any(diff(p$prof_mes) <= 0)) return(vide("profondeurs non strictement croissantes"))

  # Grille 1 m, bornee a la limite pertinente (10 m si lac profond selon
  # Valin, sinon prof_max) ET a la plage reellement couverte par les points
  # mesures -- ne jamais extrapoler au-dela des donnees disponibles.
  limite <- min(prof_max, 10)
  z_min  <- ceiling(min(p$prof_mes))
  z_max  <- floor(min(max(p$prof_mes), limite))
  if (z_max < z_min) return(vide("profil ne couvre pas la plage 0-10 m (ou 0-prof_max)"))

  z_grid  <- seq(z_min, z_max, by = 1)
  do_grid <- approx(p$prof_mes, p$do, xout = z_grid)$y
  m_sous  <- sum(do_grid < 5, na.rm = TRUE)

  data.frame(m_sous_5ppm = m_sous, statut = "ok", raison = "", stringsAsFactors = FALSE)
}


#' Étiquette accompagnée d'une infobulle (pictogramme « i » au survol)
info_tip <- function(label, texte) {
  bslib::tooltip(
    span(label, span(class = "info-ic", "ⓘ")),
    texte,
    placement = "top"
  )
}

#' Normaliser les noms de colonnes d'un export IFA vers les noms internes
#' Utilise grep (insensible à la casse) pour être robuste aux accents et variantes
normaliser_colonnes_ifa <- function(df) {
  noms <- names(df)

  trouver <- function(pattern) {
    idx <- grep(pattern, noms, ignore.case = TRUE, perl = TRUE)
    if (length(idx) >= 1) idx[1] else NA_integer_
  }

  correspondances <- list(
    territoire       = trouver("territ"),
    nolac            = trouver("no[^m].*plan|num.*plan"),   # "No plan" mais pas "Nom plan"
    nom_plan_eau     = trouver("nom.*plan"),
    annee            = trouver("ann"),
    type_peche       = trouver("type.*p.che|p.che.*sport"),
    type_recolte     = trouver("type.*r.colte|r.colte"),
    espece_code      = trouver("esp.*code|code.*esp"),
    nb_captures      = trouver("nombre.*capt"),
    nb_peses         = trouver("nombre.*pes"),
    masse_mesuree_kg = trouver("masse.*mes"),
    effort_jp        = trouver("effort.*total")
  )

  for (nom_interne in names(correspondances)) {
    idx <- correspondances[[nom_interne]]
    if (!is.na(idx)) names(df)[idx] <- nom_interne
  }

  df
}


# -----------------------------------------------------------------------------
# Import — fichier « Rshiny_PotHal » (4 onglets : Lacs, Profil, Parametre, Specimen)
# -----------------------------------------------------------------------------

#' Associer des noms internes à des colonnes via regex (insensible casse/accents)
#' @param df        data.frame source (noms déjà en minuscules)
#' @param patterns  liste nommée : nom_interne = "regex"
appliquer_correspondances <- function(df, patterns) {
  noms <- names(df)
  for (nom_interne in names(patterns)) {
    idx <- grep(patterns[[nom_interne]], noms, ignore.case = TRUE, perl = TRUE)
    if (length(idx) >= 1) names(df)[idx[1]] <- nom_interne
  }
  df
}

#' Normaliser un identifiant de lac (nolac) pour comparaison robuste aux
#' zéros non significatifs — ex. "06370" et "6370" désignent le même lac,
#' mais l'un des fichiers les conserve en texte (avec zéro) et l'autre les
#' convertit en nombre (zéro perdu). Retombe sur la chaîne nettoyée si non
#' numérique (identifiants alphanumériques, rares mais possibles).
normaliser_nolac <- function(x) {
  x <- trimws(as.character(x))
  n <- suppressWarnings(as.numeric(x))
  ifelse(is.na(n), x, as.character(n))
}

#' Clé de lac sûre pour comparaison : NULL / "" / NA -> NA_character_
#' Évite le piège d'un `if (cond && ...)` sur un vecteur de longueur nulle
#' (input non encore rendu) et centralise la normalisation.
cle_lac <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_character_)
  v <- normaliser_nolac(x[1])
  if (is.na(v) || !nzchar(v)) NA_character_ else v
}

#' Convertir une colonne de dates issue d'un export Excel.
#'
#' Selon le format de la cellule, readxl retourne : un POSIXct/Date (cellule
#' formatée en date — cas des fichiers réels), un nombre (numéro de série Excel,
#' jours depuis 1899-12-30) ou du texte. L'ancienne version appliquait
#' systématiquement as.Date(as.numeric(x), origin = "1899-12-30") : sur un
#' POSIXct, cela convertissait des SECONDES depuis 1970 en JOURS depuis 1899 et
#' produisait des dates de l'ordre de l'an 4 000 000. L'ordre chronologique
#' restait bon (la transformation est monotone), si bien que la sélection du
#' paramètre le plus récent fonctionnait par chance — mais toute date affichée
#' (étiquette d'inventaire) était fausse. Corrigé 2026-09.
convertir_date_excel <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  n   <- suppressWarnings(as.numeric(x))
  res <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")

  # Numéro de série Excel
  num_ok <- !is.na(n)
  if (any(num_ok)) res[num_ok] <- as.Date(n[num_ok], origin = "1899-12-30")

  # Texte ISO ("2010-09-20") — format explicite : renvoie NA sans lever d'erreur
  # sur une chaîne non conforme (contrairement à as.Date() sans format).
  txt_ok <- is.na(n) & !is.na(x)
  if (any(txt_ok))
    res[txt_ok] <- as.Date(trimws(as.character(x[txt_ok])), format = "%Y-%m-%d")

  res
}

#' Extraire une année entière d'une colonne pouvant contenir une vraie date,
#' un numéro de série Excel, ou déjà une année.
#' Le fichier réel stocke « Anné début inventaire » comme une DATE (2013-01-01) :
#' as.numeric() y renvoyait ~1,36 milliard au lieu de 2013 (corrigé 2026-09).
extraire_annee <- function(x) {
  if (inherits(x, c("Date", "POSIXt")))
    return(as.integer(format(as.Date(x), "%Y")))

  n <- suppressWarnings(as.numeric(x))
  a <- rep(NA_integer_, length(x))

  # Déjà une année (plage plausible) : conserver telle quelle — ne surtout pas
  # la réinterpréter comme un numéro de série Excel (2013 donnerait 1905).
  est_annee    <- !is.na(n) & n >= 1800 & n <= 2200
  a[est_annee] <- as.integer(n[est_annee])

  reste <- !est_annee
  if (any(reste))
    a[reste] <- as.integer(format(convertir_date_excel(x[reste]), "%Y"))

  a
}


PATRONS_LACS <- list(
  ue_min    = "ue.*min",
  nolac     = "no[^m].*plan|num.*plan",
  nomlac    = "nom.*plan",
  lat       = "^lat|latitude",
  lon       = "^lon|longitude",
  sup       = "superficie",
  prof_max  = "prof.*max",
  prof_moy  = "prof.*moy",
  perimetre = "p.rim.tre",
  t_air_moy = "temp.*air|air.*moy"
)

PATRONS_PROFIL <- list(
  nolac    = "no[^m].*plan|num.*plan",
  nomlac   = "nom.*plan",
  date     = "^date",
  no_inv   = "no.*inventaire",
  station  = "no.*station",
  prof_mes = "profondeur",
  temp     = "temp.rature",
  do       = "oxyg.ne",
  ph       = "^ph$|\\bph\\b",
  cond     = "conductivit"
)

PATRONS_PARAMETRE <- list(
  nolac      = "no[^m].*plan|num.*plan",
  nomlac     = "nom.*plan",
  date       = "^date",
  station    = "no.*station",
  nom_param  = "nom.*param",
  code_param = "param.*physico|physico.*param",
  resultat   = "r.sultat"
)

PATRONS_SPECIMENS <- list(
  nolac       = "no[^m].*plan|num.*plan",
  nomlac      = "nom.*plan",
  annee       = "ann",
  station     = "no.*station",
  espece_code = "esp.*code|code.*esp",
  long_totale = "long.*totale|total.*max"
)

#' Remplacer les marqueurs de valeurs manquantes par NA (colonnes texte)
#' @param marqueurs  vecteur de chaînes considérées comme manquantes
nettoyer_na_ifa <- function(df, marqueurs = c("NULL", "null", "NA", "")) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) col[trimws(col) %in% marqueurs] <- NA_character_
    col
  })
  df
}

#' Dédupliquer l'onglet Lacs par lac (nolac) — priorité IPE > OG > autre.
#' Plusieurs UE peuvent exister pour un même lac (ex. "08-00001-OG" et
#' "08-00001-IPE") ; on retient l'inventaire pêche expérimental (IPE) en
#' priorité, sinon l'observation générale (OG), sinon la première ligne.
dedup_lacs_par_ue <- function(df_lacs) {
  if (!("nolac" %in% names(df_lacs))) return(df_lacs)
  if (!("ue_min" %in% names(df_lacs))) {
    return(df_lacs %>% dplyr::filter(!is.na(nolac)) %>%
             dplyr::distinct(nolac, .keep_all = TRUE))
  }
  suffixe <- toupper(trimws(sub(".*-", "", df_lacs$ue_min)))
  rang    <- dplyr::case_when(
    suffixe == "IPE" ~ 1L,
    suffixe == "OG"  ~ 2L,
    TRUE             ~ 3L
  )
  df_lacs$.rang_ue <- rang
  df_lacs %>%
    dplyr::filter(!is.na(nolac)) %>%
    dplyr::arrange(nolac, .rang_ue) %>%
    dplyr::group_by(nolac) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.rang_ue)
}

#' Pivoter l'onglet Parametre (format long) : un scalaire par (nolac, code),
#' en retenant le résultat de la date la plus récente.
#' @return  data.frame(nolac, code_param, resultat, date) ou NULL si colonnes manquantes
pivoter_parametre <- function(df_param) {
  req_cols <- c("nolac", "code_param", "resultat")
  if (!all(req_cols %in% names(df_param))) return(NULL)

  df_param$resultat   <- suppressWarnings(as.numeric(df_param$resultat))
  df_param$code_param <- toupper(trimws(df_param$code_param))
  # Date utilisée uniquement pour retenir le résultat le plus récent par
  # (nolac, code_param) — voir arrange(desc(date)) ci-dessous.
  df_param$date <- if ("date" %in% names(df_param)) {
    convertir_date_excel(df_param$date)
  } else {
    as.Date(rep(NA_real_, nrow(df_param)), origin = "1970-01-01")
  }

  df_param %>%
    dplyr::filter(!is.na(nolac), !is.na(code_param), !is.na(resultat)) %>%
    dplyr::arrange(nolac, code_param, dplyr::desc(date)) %>%
    dplyr::group_by(nolac, code_param) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(nolac, code_param, resultat, date)
}

#' Extraire la valeur pivotée d'un paramètre pour un lac donné (ex. "CD", "TR")
#' @return  valeur numérique ou NA_real_ si absente
extraire_param <- function(df_param_pivot, nolac_val, code) {
  if (is.null(df_param_pivot) || length(nolac_val) != 1L || is.na(nolac_val))
    return(NA_real_)
  # Comparaison sur clé normalisée des deux côtés (normaliser_nolac() est
  # idempotente) : robuste aux zéros non significatifs ET aux identifiants
  # alphanumériques (C0762, 04301B07...).
  cle   <- normaliser_nolac(nolac_val)
  ligne <- df_param_pivot[normaliser_nolac(df_param_pivot$nolac) == cle &
                          df_param_pivot$code_param == code, ]
  if (nrow(ligne) == 0) return(NA_real_)
  ligne$resultat[1]
}


# =============================================================================
# MODÈLES DE CALCUL
# =============================================================================

# -----------------------------------------------------------------------------
# Lester et coll. 2021 — Touladi (Salvelinus namaycush)
#
# Chaîne de calcul :
#   1. Morphométrie & habitat  → DR, Dth, pVhy, pVeb, S_habitat
#   2. Biologie                → Winf
#   3. Biomasse cible (Bmsy)   → B_rms  [kg/ha]
#   4. Mortalité naturelle     → M      [/an]
#   5. Quota MSY               → MSY = B_rms × M  [kg/ha/an]
#
# Références : Lester et al. 2021 (éq. 33, App. 2) + app.R SANA existante
# -----------------------------------------------------------------------------
#' @param A     Superficie (ha)
#' @param Dmax  Profondeur maximale (m)
#' @param Dmn   Profondeur moyenne (m)
#' @param T_air   Température moyenne de l'air (°C)
#' @param Linf    Longueur asymptotique (mm) — utiliser resolve_linf() en amont
#' @param Dth_obs Profondeur de thermocline observée (m) — NA → formule Shuter
calc_lester_touladi <- function(A, Dmax, Dmn, T_air, Linf, Dth_obs = NA) {

  # Garde : profondeur moyenne manquante ou incohérente (>= prof. max).
  # On ne fabrique JAMAIS une valeur de repli ici — le modèle est simplement
  # inapplicable (voir model_availability() pour le message affiché à
  # l'utilisateur ; le rejet est signalé, pas masqué par une valeur inventée).
  if (is.na(Dmn) || is.na(Dmax) || Dmn <= 0 || Dmax <= 0 || Dmn >= Dmax) return(NULL)

  # --- 1. Morphométrie et habitat -------------------------------------------
  DR <- Dmax / Dmn

  # Profondeur de thermocline : observée si disponible, sinon formule Shuter
  if (is.finite(Dth_obs) && Dth_obs > 0 && Dth_obs < Dmax) {
    Dth        <- Dth_obs
    Dth_source <- "observé"
  } else {
    Dth        <- 3.26 * (A^0.109) * (Dmn^0.213) * exp(-0.0263 * T_air)
    Dth_source <- "théorique (Shuter)"
  }
  term_geo  <- max(0.00001, 1 - (Dth / Dmax))
  pVhy      <- term_geo^DR
  pVeb      <- exp(-4.63 * pVhy)
  S_habitat <- 1 / (1 + exp(2.47 + 0.386 * T_air - 16.8 * pVhy))

  # --- 2. Biologie -----------------------------------------------------------
  Winf <- (Linf / 451)^3.2          # poids asymptotique (kg)

  # --- 3. Biomasse cible au RMS (Bmsy) --------------------------------------
  B_rms <- (8.47 * Dmn * pVeb * S_habitat) / (Winf^1.33)   # kg/ha

  # --- 4. Mortalité naturelle -----------------------------------------------
  # M = 91.8 × e^(0.021×T_air + 0.0004×T_air²) / Linf^0.96
  # T = température moyenne de l'air (°C) — source : Lester et al. 2021, éq. 5
  M <- 91.8 * exp(0.021 * T_air + 0.0004 * T_air^2) / (Linf^0.96)

  # --- 5. Rendement maximal soutenu (MSY = quota) ---------------------------
  MSY <- B_rms * M     # kg/ha/an

  # Garde : valeurs non-finies (ex. Linf → 0 ou lac infime) → modèle inapplicable
  if (!is.finite(B_rms) || !is.finite(MSY)) return(NULL)

  list(
    rendement_ha = MSY,
    B_rms        = B_rms,
    M            = M,
    Dth          = Dth,
    Dth_source   = Dth_source,
    pVhy         = pVhy,
    pVeb         = pVeb,
    S_habitat    = S_habitat,
    Winf         = Winf,
    note         = ""
  )
}


# -----------------------------------------------------------------------------
# Shuter et al. 1998 — Touladi (Salvelinus namaycush)
#
# Équation réduite calibrée à partir du modèle complet de Shuter 1998,
# intégrant les effets conjoints de la superficie et du TDS.
# Validée contre l'éq. 6 du papier (accord à ~1% à TDS = 32 mg/L).
#
# Source : équation institutionnelle MRNF dérivée du modèle Shuter et al.
#   (1998) Can. J. Fish. Aquat. Sci. 55:2161–2177
# Plage de validité : A = 25–450 000 ha ; TDS = 15–180 mg/L
#
# @param A    Superficie (ha)
# @param TDS  Total Dissolved Solids (mg/L) — depuis conductivite_vers_tds()
calc_shuter_1998 <- function(A, TDS) {

  # Avertissement si hors plage de calibration
  note <- ""
  if (TDS < 15 || TDS > 180)
    note <- paste0("SDT (", round(TDS, 1), " mg/L) hors plage 15–180 mg/L")
  if (A < 25 || A > 450000)
    note <- paste0(note, if (nchar(note) > 0) " | " else "",
                   "Superficie hors plage 25–450 000 ha")

  MSY <- 10^(0.594
             - 0.239 * log10(A)
             - 0.066 * log10(TDS)
             + 0.046 * log10(A) * log10(TDS))

  list(rendement_ha = MSY, note = note)
}


# -----------------------------------------------------------------------------
# IME — Ryder 1965 + OMNR 1982 — Touladi (Salvelinus namaycush)
#
# Étape 1 (Ryder 1965) : rendement total toutes espèces via l'indice
#   morphoédaphique (MEI = TDS / Dmn), formule puissance k = 1.4, b = 0.45.
# Étape 2 (OMNR 1982)  : part Touladi = 25 % du rendement total
#   (fourchette typique 16–33 % selon la communauté).
#
# Références :
#   Ryder 1965 — A method for estimating the potential fish production
#     of north-temperate lakes. Trans. Am. Fish. Soc. 94:214–218.
#   OMNR 1982  — Partitioning yields estimated from the morphoedaphic
#     index into individual species yields. Report of SPOF.
#
# @param TDS  Solides dissous totaux (mg/L) — depuis conductivite_vers_tds()
# @param Dmn  Profondeur moyenne (m)
# -----------------------------------------------------------------------------
calc_ime_ryder <- function(TDS, Dmn, partition = 0.25) {

  MEI       <- TDS / Dmn              # mg·L⁻¹·m⁻¹
  MSY_total <- 1.4 * MEI^0.45         # rendement total toutes espèces (kg/ha/an)

  if (is.na(partition)) {
    MSY  <- MSY_total                 # partition à déterminer → rendement IME total
    note <- "Partition par espèce à déterminer — rendement IME total affiché"
  } else {
    MSY  <- MSY_total * partition     # part de l'espèce — OMNR 1982
    note <- if (MEI > 10)
      paste0("IME élevé (", round(MEI, 1), ") — lac atypique") else ""
  }

  list(rendement_ha = MSY, MEI = round(MEI, 3),
       MSY_total = round(MSY_total, 3), partition = partition, note = note)
}


# -----------------------------------------------------------------------------
# Lester et al. 2002 (éq. 6 / 14) — Doré jaune (Sander vitreus)
#
# Chaîne : forme du bassin (éq. A1/A3) → zone épibenthique si lac stratifié
#   (éq. A4/A5) → proportion du lac au-dessus du thermocline (éq. A6) →
#   Secchi relatif (éq. 8) → habitat thermo-optique TOHA (éq. 13, forme gamma
#   non normalisée) → rendement maximal soutenu (éq. 14).
#
# Thermocline : observée (Dth_obs, partagée avec le Touladi) si valide, sinon
#   repli théorique (éq. A9 — Shuter et al. 1983, formule identique à celle
#   déjà utilisée pour le Touladi) si T_air est fourni, sinon le lac est
#   traité comme NON STRATIFIÉ (TOHA = habitat optique complet, cf. texte
#   principal : « when there is no thermocline, TOHA is same as OHA »).
#   T_air n'intervient donc qu'en aide au repli — jamais un intrant bloquant.
#
# Validé numériquement contre le lac Balsam (Tableau 1, Lester et al. 2002),
#   thermocline observée = 9,9 m :
#   s ≈ 1,008 | z_E ≈ 4,15 m (obs. 4,2) | s' ≈ 1,348 (obs. 1,35) |
#   P_T ≈ 0,886 (obs. 0,89) | z_rel ≈ 0,491 (obs. 0,49) | MSY ≈ 1,44 kg/ha.
#
# CORRECTIF (voir échange du 2026-07-xx) : la formule de P_T (éq. A6) a été
#   corrigée de 1-(1-u)^(2s) à 1-(1-u^s)^2 — les deux formes donnaient un
#   résultat presque identique sur Balsam (0,886 vs 0,889, indiscernable à
#   2 décimales : les deux arrondissent à 0,89) mais divergeaient fortement
#   sur un lac de forme plus plate (s ≈ 0,53), où seule la version corrigée
#   reproduit un calcul de référence externe (0,776 attendu vs 0,309 avec
#   l'ancienne formule — facteur ~2,5x sur le rendement final). Toute la
#   géométrie stratifiée/épibenthique (éq. A4/A5/A9) restait correcte ;
#   seul P_T était en cause.
#
# Référence : Lester, Ryan, Kushneriuk, Dextrase, Rawson (2002) — The Effect
#   of Water Clarity on Walleye Habitat and Yield. Percid Community Synthesis,
#   OMNR.
#
# @param A        Superficie (ha)
# @param Dmax     Profondeur maximale (m)
# @param Dmn      Profondeur moyenne (m)
# @param TDS      Solides dissous totaux (mg/L)
# @param G        Degrés-jours (base 5 °C) / 1000
# @param z_sec    Profondeur de Secchi (m)
# @param Dth_obs  Profondeur de thermocline observée (m) — NA → repli théorique / non stratifié
# @param T_air    Température moyenne de l'air (°C) — utilisée seulement pour le repli théorique (éq. A9)
calc_lester_dore <- function(A, Dmax, Dmn, TDS, G, z_sec, Dth_obs = NA, T_air = NA) {

  # Garde : morphométrie et intrants de base — jamais fabriqués, modèle
  # simplement inapplicable si l'un d'eux est absent ou incohérent.
  if (is.na(Dmn) || is.na(Dmax) || Dmn <= 0 || Dmax <= 0 || Dmn >= Dmax) return(NULL)
  if (is.na(TDS) || TDS <= 0 || is.na(G) || G <= 0 || is.na(z_sec) || z_sec <= 0) return(NULL)

  # --- 1. Forme du bassin du lac (éq. A1/A3) --------------------------------
  r <- Dmn / Dmax
  s <- (3 * r + sqrt(r^2 + 8 * r)) / (4 * (1 - r))
  if (!is.finite(s) || s <= 0) return(NULL)

  # --- 2. Thermocline : observée > théorique (éq. A9) > non stratifié -------
  Dth <- NA_real_; Dth_source <- "aucune (non stratifié)"; stratifie <- FALSE
  if (is.finite(Dth_obs) && Dth_obs > 0 && Dth_obs < Dmax) {
    Dth <- Dth_obs; Dth_source <- "observé"; stratifie <- TRUE
  } else if (is.finite(T_air)) {
    Dth_theo <- 3.26 * (A^0.109) * (Dmn^0.213) * exp(-0.0263 * T_air)
    if (is.finite(Dth_theo) && Dth_theo > 0 && Dth_theo < Dmax) {
      Dth <- Dth_theo; Dth_source <- "théorique (Shuter et coll. 1983)"; stratifie <- TRUE
    }
  }

  # --- 3. Forme + profondeur de la zone épibenthique (éq. A4/A5), si stratifié ---
  if (stratifie) {
    u  <- Dth / Dmax
    zE <- Dth * (1 - 2 / ((s + 1) * (2 - u)) + (u^s) / ((2 * s + 1) * (2 - u)))  # éq. A5
    if (!is.finite(zE) || zE <= 0 || zE >= Dth) return(NULL)
    r2    <- zE / Dth
    s_eff <- (3 * r2 + sqrt(r2^2 + 8 * r2)) / (4 * (1 - r2))                     # éq. A4
    if (!is.finite(s_eff) || s_eff <= 0) return(NULL)
    z_max_eff <- Dth
    P_T       <- 1 - (1 - u^s)^2                                                # éq. A6 (corrigée — voir note)
  } else {
    s_eff     <- s
    z_max_eff <- Dmax
    P_T       <- 1
  }

  # --- 4. Secchi relatif (éq. 8) et TOHA (éq. 13, forme gamma non normalisée) ---
  z_rel <- z_sec / (z_max_eff * (1 - exp(-s_eff)))
  if (!is.finite(z_rel) || z_rel <= 0) return(NULL)
  P_TOHA <- P_T * z_rel * exp(-z_rel / 0.27)          # b6 = 0,27 (Secchi relatif optimal)

  # --- 5. Rendement maximal soutenu (éq. 14) --------------------------------
  MSY <- 0.97 * P_TOHA * (TDS^0.52) * (G^1.30)
  if (!is.finite(MSY)) return(NULL)

  list(
    rendement_ha   = MSY,
    P_TOHA         = round(P_TOHA, 4),
    P_T            = round(P_T, 3),
    z_rel          = round(z_rel, 3),
    s              = round(s, 3),
    s_epibenthique = round(s_eff, 3),
    Dth            = round(Dth, 2),
    Dth_source     = Dth_source,
    stratifie      = stratifie,
    note           = if (!stratifie)
      "Aucune thermocline disponible — lac traité comme non stratifié (TOHA = habitat optique complet)."
      else ""
  )
}


# -----------------------------------------------------------------------------
# Valin / Vaillancourt 1998 — Doré jaune (Saguenay–Lac-Saint-Jean)
#
# Rendement = 0,66 × IME^0,466 × 32 % (partition Doré, identique à OMNR 1982
#   mais référence distincte de M1/IME — coïncidence du partage de partition,
#   pas une fusion des deux modèles).
# Conductivité réputée déjà corrigée à 25 °C (pas de restandardisation ici —
#   décision validée ; contrairement à la formule brute du document original).
# Aucune réduction physico-chimique ou par espèces présentes (celles-ci sont
#   propres à l'Omble de fontaine dans le document source).
#
# Repli explicite (jamais silencieux) : si la conductivité ou la profondeur
#   moyenne manquent, rendement de base = 0,60 kg/ha, applicable uniquement
#   aux lacs ≥ 20 ha (au-delà, le modèle est simplement indisponible).
#
# @param TDS  Solides dissous totaux (mg/L)
# @param Dmn  Profondeur moyenne (m)
# @param A    Superficie (ha) — utilisée seulement pour le repli (seuil 20 ha)
calc_valin_dore <- function(TDS, Dmn, A) {

  donnees_ok <- !is.na(TDS) && TDS > 0 && !is.na(Dmn) && Dmn > 0

  if (!donnees_ok) {
    if (is.na(A) || A < 20) return(NULL)   # repli non applicable sous 20 ha
    return(list(
      rendement_ha = 0.60,
      IME          = NA_real_,
      note         = "Valeur de base (repli) — donnée insuffisante (conductivité ou profondeur moyenne manquante)."
    ))
  }

  IME <- TDS / Dmn
  MSY <- 0.66 * IME^0.466 * 0.32

  list(rendement_ha = MSY, IME = round(IME, 3), note = "")
}


# -----------------------------------------------------------------------------
# Valin 1998 -- Touladi (Salvelinus namaycush)
#
# Meme derivation Valin que calc_valin_dore() (coefficients IME reajustes
# 0,66 / 0,466, au lieu des 1,4 / 0,45 du IME/Ryder generique), mais avec la
# partition Touladi (25 %, OMNR 1982) plutot que celle du Dore (32 %).
# Rendement = 0,66 x IME^0,466 x 25 %.
#
# Cle de registre nommee "touladi_valin" (et non "valin") pour eviter toute
# correspondance partielle avec la cle "valin" du Dore via l'operateur $
# (meme categorie de bug que celui corrige pour Omble en 2026-07) et pour ne
# pas reutiliser l'infobulle/note du Dore, qui mentionne "part Dore (32 %)"
# et un repli a 0,60 kg/ha propres a ce modele-la.
#
# AUCUN repli sourcE pour le Touladi (contrairement au Dore) : le document
# source (Valin_et_Vaillancourt.pdf, section Touladi) ne documente pas de
# valeur de repli pour donnee insuffisante -- modele simplement indisponible
# dans ce cas, pas de valeur fabriquee.
#
# @param TDS  Solides dissous totaux (mg/L)
# @param Dmn  Profondeur moyenne (m)
calc_valin_touladi <- function(TDS, Dmn) {
  if (is.na(TDS) || TDS <= 0 || is.na(Dmn) || Dmn <= 0) return(NULL)

  IME <- TDS / Dmn
  MSY <- 0.66 * IME^0.466 * 0.25

  list(rendement_ha = MSY, IME = round(IME, 3), note = "")
}


# -----------------------------------------------------------------------------
# Table de rendement Omble de fontaine < 40 ha (Houde 1982, adaptation
# Archambault 1988/2009) -- Evaluation_PotentielPeche_2009_V2_Archambault.pdf,
# section 1.1.a. Rendement en kg/ha/an, superficie en ha (1 a 40).
# -----------------------------------------------------------------------------
TABLE_ARCHAMBAULT_SAFO <- data.frame(
  superficie_ha = 1:40,
  allopatrie = c(6.00, 6.50, 6.33, 6.25, 6.20, 6.17, 6.14, 6.13, 6.11, 6.00,
                 6.00, 5.92, 5.92, 5.86, 5.80, 5.75, 5.71, 5.67, 5.63, 5.60,
                 5.57, 5.50, 5.48, 5.46, 5.40, 5.38, 5.33, 5.29, 5.28, 5.23,
                 5.19, 5.16, 5.12, 5.09, 5.03, 5.00, 4.97, 4.92, 4.90, 5.58),
  semotilus  = c(4.00, 4.00, 3.67, 3.75, 3.80, 3.67, 3.71, 3.63, 3.67, 3.60,
                 3.64, 3.58, 3.54, 3.50, 3.47, 3.44, 3.41, 3.39, 3.37, 3.35,
                 3.33, 3.32, 3.30, 3.29, 3.24, 3.23, 3.19, 3.18, 3.17, 3.13,
                 3.13, 3.09, 3.06, 3.06, 3.03, 3.00, 2.97, 2.95, 2.95, 3.35),
  catostomus = c(2.00, 2.50, 2.67, 2.50, 2.40, 2.50, 2.43, 2.50, 2.44, 2.40,
                 2.36, 2.33, 2.38, 2.36, 2.33, 2.31, 2.29, 2.28, 2.26, 2.25,
                 2.24, 2.18, 2.17, 2.17, 2.16, 2.15, 2.15, 2.11, 2.10, 2.10,
                 2.06, 2.06, 2.06, 2.03, 2.00, 2.00, 2.00, 1.97, 1.95, 2.23)
)


# -----------------------------------------------------------------------------
# Vezina 1978 -- Omble de fontaine (Salvelinus fontinalis)
#
# Regression puissance-exponentielle du rendement optimal en fonction de la
# seule profondeur moyenne (83 lacs, parc des Laurentides + reserves de
# Portneuf, Mastigouche, St-Maurice). Exposant confirme par ajustement
# numerique contre la table officielle (Annexe 1, methode Valin) -- ecart
# residuel max 0.03 kg/ha sur 240 valeurs (valide avec le collegue -- juillet 2026).
#
# log10(Rendement, lbs/acre) = 0.73766 * (Dmn_pi)^0.2293 * (0.95632)^Dmn_pi
# Rendement (kg/ha) = 10^(ce log) * 1.120851
# Dmn_pi = Dmn_m * 3.2808
#
# Non valide sous ~2 m de profondeur moyenne (table source marque "NON
# VALABLE" a 1 m).
#
# @param Dmn Profondeur moyenne (m)
calc_vezina_omble <- function(Dmn) {
  if (is.na(Dmn) || Dmn < 2) return(NULL)

  Dmn_pi <- Dmn * 3.2808
  log_rendement_lbs_acre <- 0.73766 * (Dmn_pi^0.2293) * (0.95632^Dmn_pi)
  rendement_lbs_acre <- 10^log_rendement_lbs_acre
  MSY <- rendement_lbs_acre * 1.120851

  list(rendement_ha = MSY, note = "")
}


# -----------------------------------------------------------------------------
# Classification des especes presentes (Omble) -- groupes utilises par
# Archambault, Valin/Vaillancourt et les grilles regionales.
#
# Groupements confirmes avec le collegue (2026-07) :
#   - "touladi_equiv" (salmonides predateurs, traites comme le touladi) :
#         touladi, moulac/lacmou (hybride touladi x omble), truite
#         arc-en-ciel, truite brune
#   - "cyprins"    (dit "Semotilus" dans Vezina/Archambault/Valin) :
#         mulet a cornes (Semotilus atromaculatus), mulet perle (Semotilus
#         margarita), menes (cyprinides generiques)
#   - "catostomes" (dit "Catostome"/"castotomes" dans les memes documents) :
#         meuniers (Catostomus commersonii)
#   - "epineux"    ("poissons epineux" dans Valin) : dore, perchaude, achigan
#         a petite bouche, barbotte brune -- VALIN/VAILLANCOURT UNIQUEMENT.
#         Archambault ne couvre PAS achigan/barbotte (confirme avec le
#         collegue) -- ces deux especes restent hors categorie pour ce
#         modele (voir classer_categorie_archambault()).
#   - "piscivores" : dore, brochet -- override fixe dans Archambault
#         (0,5 kg/ha/an) ; brochet seul = 100 % dans Valin (voir
#         calc_pct_reduction_valin())
#
# grp$brutes conserve la selection brute -- chaque modele peut y calculer
# lui-meme ses propres especes "non couvertes" (par difference avec ses
# propres categories), plutot que de dependre d'une liste globale unique
# qui ne refleterait pas correctement la couverture different par modele.
# -----------------------------------------------------------------------------
ESPECES_TOULADI_EQUIV_OMBLE <- c("touladi", "moulac", "arc_en_ciel", "truite_brune")
ESPECES_CYPRINS_OMBLE       <- c("mulet_cornes", "mulet_perle", "menes")
ESPECES_CATOSTOMES_OMBLE    <- c("meunier")
ESPECES_PERCHAUDE_OMBLE     <- c("perchaude")
ESPECES_EPINEUX_OMBLE       <- c("dore", "perchaude", "achigan", "barbotte")  # Valin/Vaillancourt uniquement
ESPECES_PISCIVORES_OMBLE    <- c("dore", "brochet")                          # categorie Archambault

# Especes reconnues par Archambault (pour le calcul du "non couvert" propre a ce modele)
ESPECES_COUVERTES_ARCHAMBAULT <- c(ESPECES_PISCIVORES_OMBLE, ESPECES_PERCHAUDE_OMBLE,
                                    ESPECES_CATOSTOMES_OMBLE, ESPECES_CYPRINS_OMBLE)

# Especes reconnues par Valin/Vaillancourt (pour le calcul du "non couvert" propre a ces modeles)
ESPECES_COUVERTES_VALIN <- c(ESPECES_TOULADI_EQUIV_OMBLE, ESPECES_CYPRINS_OMBLE,
                              ESPECES_CATOSTOMES_OMBLE, ESPECES_EPINEUX_OMBLE, "brochet")

especes_groupes_omble <- function(especes) {
  if (is.null(especes)) especes <- character(0)
  list(
    brutes        = especes,
    allopatrie    = ("allopatrie" %in% especes) || length(especes) == 0,
    touladi       = length(intersect(especes, ESPECES_TOULADI_EQUIV_OMBLE)) > 0,
    cyprins       = length(intersect(especes, ESPECES_CYPRINS_OMBLE))       > 0,
    catostomes    = length(intersect(especes, ESPECES_CATOSTOMES_OMBLE))    > 0,
    perchaude     = length(intersect(especes, ESPECES_PERCHAUDE_OMBLE))     > 0,
    brochet       = "brochet" %in% especes,
    epineux       = length(intersect(especes, ESPECES_EPINEUX_OMBLE))       > 0,  # Valin/Vaillancourt : dore, perchaude, achigan, barbotte
    piscivores    = length(intersect(especes, ESPECES_PISCIVORES_OMBLE))    > 0   # Archambault : dore OU brochet
  )
}

# Categorie Archambault : mutuellement exclusive, priorite descendante
# (piscivores > perchaude > catostomes > cyprins > allopatrie). Retourne
# NA si des especes sont presentes mais qu'aucune categorie connue ne
# s'applique (ex. achigan/barbotte seuls, ou touladi/moulac/truites seuls --
# non couverts par Archambault) -- le modele est alors juge indisponible,
# jamais silencieusement traite comme allopatrique.
classer_categorie_archambault <- function(grp) {
  if (grp$allopatrie)  return("allopatrie")
  if (grp$piscivores)  return("piscivores")   # -> override 0,5 kg/ha/an fixe
  if (grp$perchaude)   return("perchaude")
  if (grp$catostomes)  return("catostome")
  if (grp$cyprins)     return("semotilus")
  NA_character_
}


# -----------------------------------------------------------------------------
# Archambault 1988 / mise a jour 2009 -- Omble de fontaine
#   Source : Evaluation_PotentielPeche_2009_V2_Archambault.pdf, section 1.1
#   (remplace l'ancien document PADE 1988/1990, desormais desuet).
#
# a) < 40 ha : table exacte (Houde 1982, adaptation Archambault), interpolee
#    lineairement (approx()) entre hectares entiers 1 a 40.
# b) >= 40 ha : formule fermee, rendement de base multiplie par un pourcentage
#    selon categorie :
#      Rendement_base = 1 / (0.001711 * A + 0.1108)
#      allopatrie 100 % | semotilus 60 % | catostome 40 % | perchaude 20 %
# c) Association avec dore ou brochet (n'importe quelle superficie) :
#    rendement fixe a 0,5 kg/ha/an (remplace a et b).
#
# @param A   Superficie (ha)
# @param grp Liste de booleens -- voir especes_groupes_omble()
calc_archambault_omble <- function(A, grp) {
  if (is.na(A) || A <= 0) return(NULL)

  categorie <- classer_categorie_archambault(grp)
  if (is.na(categorie)) {
    non_couv <- setdiff(grp$brutes, c(ESPECES_COUVERTES_ARCHAMBAULT, "allopatrie"))
    return(list(
      rendement_ha = NA_real_,
      note = paste0("Combinaison d'espèces non couverte par Archambault (",
                     paste(non_couv, collapse = ", "),
                     ") — modèle indisponible, aucune valeur estimée.")
    ))
  }

  # c) Override piscivores -- prioritaire, toutes superficies
  if (categorie == "piscivores") {
    return(list(rendement_ha = 0.5, categorie = categorie,
                note = "Association avec doré ou brochet — valeur fixe (Archambault)."))
  }

  if (A < 40) {
    col <- switch(categorie, allopatrie = "allopatrie", semotilus = "semotilus",
                  catostome = "catostomus", perchaude = NA_character_)
    if (is.na(col)) {
      # La categorie "perchaude" n'existe pas dans la table < 40 ha
      # (uniquement documentee pour le regime >= 40 ha) -- pas de reduction
      # sourcee ici, modele indisponible plutot que d'inventer une valeur.
      return(list(rendement_ha = NA_real_, categorie = categorie,
                  note = "Catégorie \"perchaude\" non documentée pour les lacs < 40 ha (Archambault) — modèle indisponible."))
    }
    y <- approx(TABLE_ARCHAMBAULT_SAFO$superficie_ha, TABLE_ARCHAMBAULT_SAFO[[col]], xout = A)$y
    return(list(rendement_ha = y, categorie = categorie,
                note = paste0("Table Archambault < 40 ha, catégorie \"", categorie, "\".")))
  }

  # >= 40 ha : formule fermee
  base <- 1 / (0.001711 * A + 0.1108)
  pct  <- switch(categorie, allopatrie = 1.00, semotilus = 0.60,
                 catostome = 0.40, perchaude = 0.20)
  list(rendement_ha = base * pct, categorie = categorie,
       note = paste0("Formule Archambault ≥ 40 ha, catégorie \"", categorie, "\" (", pct * 100, " % du rendement de base)."))
}


# -----------------------------------------------------------------------------
# Pourcentage a soustraire du rendement de base -- especes presentes
# (methode Valin 1998, table p.1). Priorite descendante : le premier cas
# applicable (le plus severe) l'emporte -- reproduit l'ordre du document.
#
# NOTE : les lignes impliquant "touladi" (75 %/90 %) ne peuvent jamais se
# declencher -- "touladi" n'est pas une case a cocher du formulaire Omble
# actuel (voir especes_groupes_omble()).
#
# Retourne NA si des especes sont presentes mais qu'aucune combinaison
# documentee ne s'applique (ex. dore seul, sans menes/catostomes) --
# le modele est alors juge indisponible plutot que de fabriquer un %.
# Conversion du profil d'oxygene en % de reduction (methode Valin 1998, p.2).
#   Lacs de profondeur max <= 10 m : % = (m sous 5 ppm O2) / (profondeur max)
#   Lacs de profondeur max >  10 m : % = 10 % * (m sous 5 ppm O2 dans les 10
#     premiers metres de la colonne d'eau)
# Retourne NA si l'une des deux mesures manque (aucune reduction appliquee
# faute de donnee, jamais une reduction "0 %" fabriquee par defaut -- voir
# calc_valin_vaillancourt_omble(), qui ignore alors cette etape).
#
# @param prof_max     Profondeur maximale du lac (m)
# @param m_sous_5ppm  Nombre de metres ou l'oxygene dissous est < 5 ppm
#   (dans les 10 premiers metres si prof_max > 10 m -- a mesurer/saisir ainsi)
calc_reduction_o2_valin <- function(prof_max, m_sous_5ppm) {
  if (is.na(prof_max) || prof_max <= 0 || is.na(m_sous_5ppm) || m_sous_5ppm < 0) return(NA_real_)
  if (prof_max <= 10) {
    pct <- (m_sous_5ppm / prof_max) * 100
  } else {
    pct <- 10 * m_sous_5ppm
  }
  min(max(pct, 0), 100)
}


# Pourcentage a soustraire (methode Valin 1998, table p.1). La table source
# ne documente les paliers touladi/epineux QU'en combinaison avec
# menes/catostomes -- mais on assume desormais leur presence implicite des
# qu'un touladi (ou equivalent salmonide) ou un poisson epineux est coche,
# meme seul (confirme avec le collegue, 2026-07) : un salmonide predateur ou
# un poisson epineux n'existe pas dans un vide alimentaire, la presence de
# proies (menes/catostomes) est jugee implicite. Priorite descendante :
#   brochet seul (100 %) > touladi+epineux (90 %) > touladi seul (75 %) >
#   epineux seul (75 %) > menes/catostomes seuls, sans touladi ni epineux (50 %).
calc_pct_reduction_valin <- function(grp) {
  if (grp$allopatrie) return(0)

  # "Grand Brochet" (100 %) : critere autonome, prioritaire sur toute
  # combinaison -- un brochet a lui seul justifie la reduction maximale,
  # peu importe les autres especes presentes.
  if (grp$brochet)                    return(100)
  if (grp$touladi && grp$epineux)     return(90)
  if (grp$touladi)                    return(75)
  if (grp$epineux)                    return(75)
  if (grp$cyprins || grp$catostomes)  return(50)
  NA_real_
}


# -----------------------------------------------------------------------------
# Valin et Vaillancourt 1998 -- Omble de fontaine (Saguenay - Lac-Saint-Jean)
#   Sources : Valin.pdf ; Valin_et_Vaillancourt.pdf ("Partie modifiee de la
#   methode Valin", 30 juillet 1998).
#
#   Methode unique. La version Vaillancourt EST la methode Valin, avec deux
#   ajustements documentes. Les deux etaient implementees separement et
#   retournaient exactement le meme nombre pour tout lac de moins de 25,9 m
#   dont la combinaison d'especes ne declenchait pas le palier
#   menes/catostomes -- fusionnees ici (2026-09).
#
#   Ce qui vient de Vaillancourt plutot que de Valin :
#     - domaine de validite explicite : prof. moyenne 2,0 a 25,9 m. Vezina
#       bloque deja sous 2 m ; la borne SUPERIEURE est donc le seul ajout
#       reel -- au-dela, modele indisponible plutot qu'extrapolation de la
#       table Vezina.
#     - palier "menes et/ou catostomes seuls" : plage documentee 50-70 %
#       ("selon l'importance relative du recrutement ou severite de
#       l'infestation") au lieu de 50 % fixe.
#       pct_menes_catostomes = 60 est le MILIEU DE LA PLAGE, pas une valeur
#       sourcee -- A VALIDER.
#   Pour retrouver le comportement Valin strict : pct_menes_catostomes = 50
#   et retrait du test de domaine ci-dessous.
#
#   Cascade de reductions successives, chacune appliquee au resultat de
#   l'etape precedente (jamais au rendement de base directement) :
#     1. Base = Vezina 1978 (calc_vezina_omble)
#     2. Reduction especes presentes (calc_pct_reduction_valin)
#     3. Prof. moyenne < 2 m : -50 % (jamais atteint -- Vezina bloque en amont ;
#        incoherence documentaire mineure, signalee mais non bloquante)
#     4. pH < 5 : -50 %
#     5. Oxygene dissous : reduction fournie directement en %, calculee en amont
#        (voir calc_reduction_o2_valin()) -- garde cette fonction independante
#        de la source des donnees (saisie manuelle ou profil IFA)
#     6. Absence tributaire/emissaire permanent : -25 %
#     7. Camps/chalets : -1 % x (nb chalets / 10 ha)
#
#   Chaque etape optionnelle (pH, O2, tributaire, chalets) n'est appliquee que
#   si la donnee correspondante est fournie -- une donnee manquante n'est
#   jamais traitee comme "condition non remplie" (silencieusement favorable) :
#   elle est ignoree et l'omission apparait dans le controle qualite.
#
# @param Dmn               Profondeur moyenne (m)
# @param grp               Liste de booleens -- especes_groupes_omble()
# @param pH                pH (NA si non mesure)
# @param o2_pct_reduction  % a soustraire du profil d'oxygene (NA si non calcule)
# @param tributaire_absent TRUE = absence confirmee de tributaire/emissaire
#   permanent, NA = inconnu
# @param nb_chalets        Nombre de chalets/camps (NA ou 0 = aucun)
# @param A_optionnel       Superficie (ha) -- ratio chalets/10 ha uniquement.
#   Nommee differemment de "A" (Archambault) pour ne PAS etre un intrant
#   bloquant : le modele reste utilisable sans elle, la reduction chalets est
#   alors simplement ignoree.
# @param pct_menes_catostomes % du palier "menes et/ou catostomes seuls"
#   (plage documentee 50-70, defaut 60 -- non sourcee)
calc_valin_vaillancourt_omble <- function(Dmn, grp, pH = NA, o2_pct_reduction = NA,
                                          tributaire_absent = NA, nb_chalets = NA,
                                          A_optionnel = NA, pct_menes_catostomes = 60) {

  if (is.na(Dmn) || Dmn < 2.0 || Dmn > 25.9) {
    return(list(rendement_ha = NA_real_,
                note = "Hors domaine de validité (prof. moyenne 2,0 à 25,9 m) — modèle indisponible."))
  }

  base <- calc_vezina_omble(Dmn)
  if (is.null(base)) return(NULL)

  pct_especes <- calc_pct_reduction_valin(grp)
  if (is.na(pct_especes)) {
    non_couv <- setdiff(grp$brutes, c(ESPECES_COUVERTES_VALIN, "allopatrie"))
    return(list(rendement_ha = NA_real_,
                note = paste0("Combinaison d'espèces non couverte (",
                              paste(non_couv, collapse = ", "),
                              ") — modèle indisponible.")))
  }

  # calc_pct_reduction_valin() ne retourne 50 que pour la branche
  # cyprins/catostomes ; le test par valeur identifie donc ce palier sans
  # ambiguite avec les autres (100, 90, 75, 0).
  palier_ajustable <- (pct_especes == 50)
  if (palier_ajustable) pct_especes <- pct_menes_catostomes

  rendement <- base$rendement_ha
  notes <- character(0)

  rendement <- rendement * (1 - pct_especes / 100)
  if (palier_ajustable) {
    notes <- c(notes, paste0("espèces présentes −", pct_especes,
                             " % (plage documentée 50 à 70 % selon l'importance ",
                             "relative des ménés/catostomes)"))
  } else if (pct_especes > 0) {
    notes <- c(notes, paste0("espèces présentes −", pct_especes, " %"))
  }

  if (!is.na(pH) && pH < 5) {
    rendement <- rendement * 0.5
    notes <- c(notes, "pH < 5 −50 %")
  }

  if (!is.na(o2_pct_reduction) && o2_pct_reduction > 0) {
    rendement <- rendement * (1 - o2_pct_reduction / 100)
    notes <- c(notes, paste0("oxygène −", round(o2_pct_reduction, 1), " %"))
  }

  if (!is.na(tributaire_absent) && isTRUE(tributaire_absent)) {
    rendement <- rendement * 0.75
    notes <- c(notes, "absence de tributaire/émissaire −25 %")
  }

  if (!is.na(nb_chalets) && !is.na(A_optionnel) && A_optionnel > 0 && nb_chalets > 0) {
    pct_chalets <- nb_chalets / (A_optionnel / 10)
    rendement <- rendement * (1 - pct_chalets / 100)
    notes <- c(notes, paste0("chalets −", round(pct_chalets, 1), " %"))
  }

  list(rendement_ha = rendement,
       base_vezina  = round(base$rendement_ha, 3),
       pct_especes  = pct_especes,
       note = if (length(notes) > 0)
                paste("Réductions appliquées :", paste(notes, collapse = ", "))
              else
                # Sans aucune réduction, la cascade se réduit à sa base : le
                # résultat est par construction celui de Vézina 1978. Le dire
                # explicitement évite que les deux lignes identiques du tableau
                # passent pour une anomalie.
                "Aucune réduction applicable — résultat identique à Vézina 1978 (lac allopatrique, sans facteur limitant déclaré).")
}


# -----------------------------------------------------------------------------
# Modèles régionaux de référence — rendements tabulés (kg/ha), par espèce
#   Valeurs fixes utilisées comme référence régionale dans le tableau des
#   modèles. Structure par espèce, car la forme des grilles diffère :
#     - "binaire" (Touladi) : une valeur seul / une valeur mixte
#     - "grille"  (Doré, Laurentides/Mauricie) : classe de superficie × mono/multi
#     - "fixe"    (Doré, Nord-du-Québec) : valeur unique, indépendante
#   Sources : NDQ, Mauricie, Laurentides, Valin (littérature grise MRNF).
# -----------------------------------------------------------------------------
MODELES_REGIONAUX <- list(

  touladi = list(
    ndq = list(
      nom = "Nord-du-Québec", type = "binaire",
      val_seul = 0.30, val_mixte = 0.25,
      note_seul  = "Rendement régional fixe — Nord-du-Québec, Touladi seul",
      note_mixte = "Rendement régional fixe — Nord-du-Québec, communauté mixte (autres espèces présentes)"
    ),
    mau = list(
      nom = "Mauricie", type = "binaire",
      val_seul = 0.40, val_mixte = 0.30,
      note_seul  = "Rendement régional fixe — Mauricie, Touladi seul / omble marginale",
      note_mixte = "Rendement régional fixe — Mauricie, communauté mixte compétitive (autres espèces présentes)"
    ),
    lau = list(
      nom = "Laurentides", type = "binaire",
      val_seul = 0.50, val_mixte = 0.30,
      note_seul  = "Rendement régional fixe — Laurentides, Touladi seul / avec omble",
      note_mixte = "Rendement régional fixe — Laurentides, communauté mixte compétitive (autres espèces présentes)"
    )
  ),

  dore = list(
    lau = list(
      nom = "Laurentides", type = "grille",
      classes   = c("0-20", "21-50", "51-100", "101+"),
      seuils_ha = c(20, 50, 100, Inf),
      mono  = c(2.0, 2.0, 1.5, 1.0),
      multi = c(1.5, 1.5, 1.3, 0.9)
    ),
    mau = list(
      nom = "Mauricie", type = "grille",
      classes   = c("0-20", "21-50", "51-100", "101+"),
      seuils_ha = c(20, 50, 100, Inf),
      mono  = c(1.5, 1.5, 1.5, 0.86),
      multi = c(1.25, 1.25, 1.0, 0.86)
    ),
    ndq = list(
      nom  = "Nord-du-Québec", type = "fixe",
      val  = 0.35,
      note = "Rendement régional fixe — Nord-du-Québec (OMNR, réserve AMW) — indépendant de la communauté et de la superficie."
    )
  ),

  # Omble de fontaine — grilles catégorielles (pas de mono/multi binaire) :
  # chaque région distingue plusieurs catégories mutuellement exclusives,
  # résolues ici via especes_groupes_omble() (voir plus haut) plutôt que par
  # le mécanisme seul/mixte générique. Type "fn" : la fonction reçoit
  # (grp, idx_classe) et retourne list(val, note) — voir regions_actives().
  omble = list(
    ndq = list(
      nom = "Nord-du-Québec (Réserve AMW)", type = "fn",
      fn = function(grp, idx_classe) {
        # ORDRE CORRIGE (2026-09) : le dore/brochet est teste EN PREMIER.
        # Le tableau source (DGFa10, reserve AMW) attribue 0,10 kg/ha a l'Omble
        # aussi bien dans l'association 7 (dore-brochet-touladi-omble) que dans
        # l'association 9 (dore-brochet-omble) : la presence d'un piscivore
        # l'emporte sur celle du touladi. L'ancien ordre testait grp$touladi
        # d'abord et retournait donc 0,50 pour l'association 7 -- surestimation
        # d'un facteur 5.
        if (grp$piscivores)
          return(list(val = 0.10, note = "Nord-du-Québec — avec doré ou brochet"))
        # "Avec touladi" inclut aussi Moulac/Lacmou et les truites (grp$touladi
        # -- voir especes_groupes_omble(), equivalence confirmee avec le collegue).
        if (grp$touladi)
          return(list(val = 0.50, note = "Nord-du-Québec — avec touladi (ou équivalent : moulac/lacmou, truite)"))
        if (grp$catostomes && grp$cyprins)
          return(list(val = 0.50, note = "Nord-du-Québec — avec catostomidés et cyprinidés"))
        if (grp$cyprins)
          return(list(val = 1.00, note = "Nord-du-Québec — avec cyprinidés"))
        if (grp$allopatrie)
          return(list(val = NA_real_, note = "Nord-du-Québec — aucune valeur allopatrique documentée dans la grille source"))
        non_couv <- setdiff(grp$brutes, "allopatrie")
        list(val = NA_real_,
             note = paste0("Nord-du-Québec — combinaison d'espèces non couverte par la grille",
                            if (length(non_couv) > 0)
                              paste0(" (", paste(non_couv, collapse = ", "), ")") else ""))
      }
    ),
    lau = list(
      nom = "Laurentides (DGFA15)", type = "fn",
      classes   = c("0-20", "21-50", "51-100", "101+"),
      seuils_ha = c(20, 50, 100, Inf),
      fn = function(grp, idx_classe) {
        if (is.na(idx_classe))
          return(list(val = NA_real_, note = "Laurentides — superficie manquante, classe indéterminée"))
        cl <- c("0-20", "21-50", "51-100", "101+")[idx_classe]
        if (grp$piscivores) {
          v <- c(NA_real_, 0.5, 0.5, NA_real_)[idx_classe]
          return(list(val = v, note = paste0("Laurentides — avec doré/brochet, classe ", cl,
                      if (is.na(v)) " (non applicable à cette classe)" else "")))
        }
        if (grp$catostomes)
          return(list(val = c(3.0, 2.5, 2.0, 1.0)[idx_classe],
                      note = paste0("Laurentides — avec meuniers, classe ", cl)))
        if (grp$cyprins)
          return(list(val = c(4.0, 3.0, 2.5, 1.5)[idx_classe],
                      note = paste0("Laurentides — avec cyprins, classe ", cl)))
        if (grp$allopatrie)
          return(list(val = c(6.0, 4.0, 3.0, 2.0)[idx_classe],
                      note = paste0("Laurentides — allopatrique, classe ", cl)))
        non_couv <- setdiff(grp$brutes, "allopatrie")
        list(val = NA_real_,
             note = paste0("Laurentides — combinaison d'espèces non couverte par la grille",
                            if (length(non_couv) > 0)
                              paste0(" (", paste(non_couv, collapse = ", "), ")") else ""))
      }
    )
  )
)

# Ordre d'affichage des régions dans le tableau et les graphiques, par espèce
REGIONS_ORDRE <- list(
  touladi = c("ndq", "mau", "lau"),
  dore    = c("lau", "mau", "ndq"),
  omble   = c("lau", "ndq")
)

# Espèces dont la présence fait basculer la communauté de « seul » vers
# « mixte » (Touladi) / « monospécifique » vers « multispécifique » (Doré).
# Omble n'utilise pas ce mécanisme seul/mixte binaire — voir especes_groupes_omble()
# et le type "fn" des grilles régionales ci-dessus.
ESPECES_COMPETITRICES <- list(
  touladi = c("dore", "brochet", "achigan", "omble_dom"),
  dore    = c("touladi", "brochet", "achigan", "omble_dom")
)


# -----------------------------------------------------------------------------
# Pente de Sen (Theil-Sen) — médiane des pentes par paires
#   Robuste aux valeurs aberrantes; gère l'espacement irrégulier des années.
#   annee  : vecteur des années (axe temporel)
#   valeur : vecteur des valeurs, aligné sur annee
# -----------------------------------------------------------------------------
sens_slope <- function(annee, valeur) {
  ok     <- is.finite(annee) & is.finite(valeur)   # exclut NA, NaN et Inf
  annee  <- annee[ok]; valeur <- valeur[ok]
  n <- length(valeur)
  if (n < 2L) return(NA_real_)
  pr   <- combn(n, 2L)                        # toutes les paires (i < j)
  dt   <- annee[pr[2, ]]  - annee[pr[1, ]]
  dv   <- valeur[pr[2, ]] - valeur[pr[1, ]]
  keep <- dt != 0
  if (!any(keep)) return(NA_real_)
  median(dv[keep] / dt[keep])                 # variation par année
}


# -----------------------------------------------------------------------------
# Mann-Kendall — p-value bilatérale du sens de la tendance (test non-paramétrique)
#   Robuste aux non-normalités; correction pour les ex aequo.
# -----------------------------------------------------------------------------
mann_kendall_p <- function(valeur) {
  v <- valeur[is.finite(valeur)]   # exclut NA, NaN et Inf
  n <- length(v)
  if (n < 3L) return(NA_real_)
  pr    <- combn(n, 2L)
  S     <- sum(sign(v[pr[2, ]] - v[pr[1, ]]))  # somme des signes des différences
  ties  <- as.numeric(table(v))                 # correction pour les ex aequo
  var_S <- (n * (n - 1) * (2 * n + 5) -
              sum(ties * (ties - 1) * (2 * ties + 5))) / 18
  if (var_S <= 0) return(NA_real_)
  Z <- if (S > 0) (S - 1) / sqrt(var_S)         # correction de continuité
       else if (S < 0) (S + 1) / sqrt(var_S)
       else 0
  2 * (1 - pnorm(abs(Z)))                       # p-value bilatérale
}


# =============================================================================
# REGISTRE DES ESPÈCES + HELPERS (thème, barre d'espèces)
# =============================================================================
source("config_especes.R", encoding = "UTF-8", local = TRUE)


# =============================================================================
# THÈME BSLIB — Flatly personnalisé selon l'espèce active
# Flatly gère déjà : typographie, boutons, onglets, tableaux, inputs.
# On surcharge uniquement la couleur primaire et quelques réglages fins.
# =============================================================================
theme_app <- make_theme(COULEURS[[ESPECE_ACTIVE]])


# =============================================================================
# CSS MINIMAL — uniquement pour les éléments vraiment sur-mesure
# (header, barre espèces, et quelques corrections de layout)
# =============================================================================
css_minimal <- "

  /* ── Pleine largeur : retrait du padding container + full-bleed robuste ── */
  html, body { margin: 0; overflow-x: hidden; }
  .container-fluid { padding-left: 0 !important; padding-right: 0 !important; }
  .app-header, .species-bar {
    position: relative;
    width: 100vw;
    left: 50%;
    margin-left: -50vw;
  }
  /* Remettre le padding sur la rangée de contenu seulement */
  .content-row {
    padding-left:  15px;
    padding-right: 15px;
  }

  /* ── En-tête pleine largeur ── */
  .app-header {
    background: linear-gradient(135deg, #2C3E50, #3D5166);
    padding: 18px 28px;
    display: flex;
    align-items: baseline;
    gap: 14px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.25);
  }
  .app-titre {
    color: white;
    font-size: 22px;
    font-weight: 700;
    margin: 0;
  }
  .app-sous-titre {
    color: rgba(255,255,255,0.45);
    font-size: 12px;
    font-style: italic;
    margin: 0;
  }

  /* ── Barre onglets espèces pleine largeur ── */
  .species-bar {
    background: #243342;
    padding: 0 28px;
    display: flex;
  }
  .sp-tab {
    color: rgba(255,255,255,0.45);
    font-size: 13px;
    font-weight: 500;
    padding: 10px 20px 8px;
    border-bottom: 3px solid transparent;
    cursor: pointer;
    display: flex;
    align-items: center;
    gap: 7px;
    transition: color 0.15s;
    user-select: none;
  }
  .sp-tab:hover                 { color: rgba(255,255,255,0.8); }
  .sp-tab.disabled              { color: rgba(255,255,255,0.22); cursor: not-allowed; pointer-events: none; opacity: 0.45; font-style: italic; }
  .sp-tab.active-touladi        { color: white; border-bottom-color: #74B9E8; }
  .sp-tab.active-dore           { color: white; border-bottom-color: #F6C84B; }
  .sp-tab.active-omble          { color: white; border-bottom-color: #E07FAD; }
  .sp-dot                       { width: 7px; height: 7px; border-radius: 50%; }
  .dot-touladi                  { background: #74B9E8; }
  .dot-dore                     { background: #F6C84B; }
  .dot-omble                    { background: #E07FAD; }

  /* ── Panneau gauche ── */
  .left-panel {
    background: white;
    border-right: 1px solid #dee2e6;
    padding: 20px 18px;
    min-height: calc(100vh - 100px);
  }

  /* ── KPI grand (rangée du haut) ── */
  /* 2 colonnes depuis 2026-09 : la carte « Rendement maximal théorique » a été
     retirée (valeur déjà visible sur la barre et dans le tableau détail). */
  .kpi-top-row  { display: grid; grid-template-columns: minmax(0,1.35fr) minmax(0,1fr); gap: 12px; margin-bottom: 10px; }

  /* ── Ligne de dérivation du quota : « 80 % de X kg/ha × Y ha » ──
     Le champ de saisie du taux y est inséré en ligne, contre le chiffre
     qu'il pilote. */
  .quota-derivation {
    display: flex; align-items: center; flex-wrap: wrap; gap: 5px;
    font-size: 12.5px; color: #495057; margin-top: 9px;
  }
  .quota-derivation .shiny-input-container { margin: 0; }
  .quota-derivation .form-control {
    font-size: 13px; font-weight: 700; color: #2C3E50;
    text-align: center; padding: 2px 6px; height: auto;
  }
  .quota-derivation-op { color: #adb5bd; }
  .kpi-top-card {
    flex: 1; background: white; border-radius: 8px;
    padding: 28px 24px; border: 1px solid #dee2e6;
    box-shadow: 0 1px 4px rgba(0,0,0,0.05);
  }
  .kpi-top-card.main { background: #EBF2FA; border-left: 5px solid #4A6FA5; }
  .kpi-eyebrow {
    font-size: 12px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.08em; color: #6c757d; margin-bottom: 6px;
  }
  .kpi-top-card.main .kpi-eyebrow { color: #4A6FA5; }
  .kpi-unit     { font-size: 15px; color: #6c757d; margin-left: 3px; }
  .kpi-sub      { font-size: 13px; color: #6c757d; margin-top: 5px; }

  /* ── Bloc input conditionnel ── */
  .cond-block {
    border: 1px solid #D0DEF0;
    border-left: 4px solid #4A6FA5;
    border-radius: 6px;
    padding: 12px 13px;
    margin-bottom: 12px;
    background: #F7FAFD;
  }
  .cond-bloc-titre {
    font-size: 13px;
    font-weight: 600;
    color: #2C3E50;
    margin-bottom: 8px;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }
  .cond-radio .shiny-input-container { margin: 0; }
  .cond-radio label.control-label    { display: none; }
  .cond-radio .radio-inline          { margin: 0 8px 0 0; font-size: 12px; }


  /* ── Strip indicateurs onglet 2 ── */
  .kpi-strip-row  { display: grid; grid-template-columns: repeat(3, minmax(0,1fr)); gap: 10px; margin-bottom: 12px; }
  .kpi-strip-card { background: white; border-radius: 8px; padding: 14px 16px;
                    border: 1px solid #dee2e6; box-shadow: 0 1px 3px rgba(0,0,0,0.04); }
  .kpi-strip-val  { font-size: 24px; font-weight: 700; color: #2C3E50; line-height: 1; }
  .graph-card {
    background: white; border-radius: 8px; padding: 16px 18px;
    border: 1px solid #d4dae0; margin-bottom: 14px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.05);
  }
  /* ── Bloc KPI de l'onglet 2 (distinct, légèrement teinté) ── */
  .tab2-kpi-block {
    background: #f6f7f9; border: 1px solid #e3e7eb; border-radius: 10px;
    padding: 14px 16px 4px; margin-bottom: 6px;
  }
  .tab2-section-head { display: flex; justify-content: space-between;
                       align-items: center; margin-bottom: 10px; }
  .tab2-section-title { font-size: 13px; font-weight: 700; color: #2C3E50; }
  /* Séparateur de section avec libellé centré */
  .tab2-section-divider {
    display: flex; align-items: center; text-align: center;
    color: #868e96; font-size: 12px; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.05em;
    margin: 18px 0 12px;
  }
  .tab2-section-divider::before, .tab2-section-divider::after {
    content: ''; flex: 1; border-bottom: 1px solid #dee2e6;
  }
  .tab2-divider-label { padding: 0 12px; }
  /* ── Pills du sélecteur de fenêtre temporelle (kpi_obs_n) ── */
  #kpi_obs_n label.control-label       { display: none; }
  #kpi_obs_n .shiny-options-group      { display: flex; flex-wrap: wrap; gap: 4px; margin: 0; }
  #kpi_obs_n .form-check               { display: inline-flex; margin: 0; padding: 0; }
  #kpi_obs_n .form-check-input         { display: none; }
  #kpi_obs_n .form-check-label {
    font-size: 10px; padding: 2px 8px; border-radius: 99px; cursor: pointer;
    border: 0.5px solid #adb5bd; color: #6c757d; background: white; margin: 0;
  }
  #kpi_obs_n .form-check-input:checked + .form-check-label {
    background: #185FA5; border-color: #185FA5; color: white;
  }

  /* ── Infobulle : pictogramme d'information ── */
  .info-ic { font-size: 11px; color: #9AA0A6; margin-left: 4px;
             cursor: help; vertical-align: super; font-weight: 400; }
  .info-ic:hover { color: #4A6FA5; }

  /* ── Cartes KPI : bord gauche coloré selon la zone (uniformisé fourchette) ── */
  .kpi-top-card.zone-conservateur { border-left: 5px solid #8A8F94 !important; }
  .kpi-top-card.zone-recommande   { border-left: 5px solid #3B6D11 !important; }
  .kpi-top-card.zone-eleve        { border-left: 5px solid #B97A0B !important; }
  .kpi-top-card.zone-recommande .kpi-eyebrow { color: #3B6D11; }

  /* ── Rendement observé sous la bande (kg/ha) ── */
  .obs-val  { font-size: 26px; font-weight: 800; color: #2C3E50; line-height: 1; }
  .obs-unit { font-size: 13px; color: #6c757d; margin-left: 3px; font-weight: 600; }

  /* ── Valeurs en kg/an (quota estimé et quota actuel) ── */
  .quota-hero-val    { font-size: 40px; font-weight: 800; color: #2C3E50; line-height: 1; }
  .quota-hero-unit   { font-size: 16px; color: #6c757d; margin-left: 4px; font-weight: 600; }

  /* ── Tableau « Détail par modèle » : en-tête à deux niveaux ── */
  /* En-têtes centrés (les deux rangées), données inchangées */
  table.dataTable thead th { text-align: center !important; vertical-align: middle; }
  table.dataTable thead th.th-group {
    font-weight: 700; color: #2C3E50;
    border-bottom: 1px solid #dee2e6; font-size: 13px;
  }
  table.dataTable thead th.th-unit {
    font-weight: 500; color: #868e96;
    font-size: 11.5px; padding-top: 2px;
  }
  table.dataTable thead th.th-hidden { display: none; }
  /* Picto info dans la cellule Modèle (survol natif via title=) */
  .model-name .info-ic {
    color: #adb5bd; font-size: 12px; cursor: help; margin-left: 3px;
  }
  .model-name .info-ic:hover { color: #4A90D9; }

  /* ── Fourchette : barre + légende verticale à droite ── */
  .fb-wrap   { display: flex; flex-direction: column; gap: 12px; }
  .fb-bararea{ flex: 1 1 auto; min-width: 0; }
  .fb-legend { display: flex; flex-direction: row; flex-wrap: wrap;
               justify-content: center; gap: 18px; }
  .fb-leg-item { display: flex; align-items: center; gap: 7px; font-size: 12px; color: #495057; }
  .fb-leg-sw   { width: 14px; height: 14px; border-radius: 3px; flex: 0 0 auto; }

  /* ── Sous-titres internes de la sidebar ── */
  .sidebar-subtitle {
    font-size: 10.5px; font-weight: 700; text-transform: uppercase;
    letter-spacing: 0.04em; color: #ADB3BA;
    margin-bottom: 4px;
    display: flex; align-items: center;
  }

  /* ── Blocs conditionnels épurés (zone spécifique par espèce) ── */
  .cond-block.cond-narrow {
    padding: 8px 10px;
    margin-bottom: 8px;
    border-left-width: 3px;
    background: transparent;
  }
  .cond-narrow .cond-bloc-titre {
    font-size: 13px;
    margin-bottom: 6px;
    gap: 8px;
  }
  /* Radios Saisir / Calculer alignés à droite du titre */
  .cond-radio.cond-radio-right { margin-left: auto; }
  .cond-radio.cond-radio-right .radio-inline { font-size: 11px; margin: 0 0 0 8px; }
  .cond-narrow .shiny-input-container { margin-bottom: 0; }
  /* Cases à cocher espèces : compactes mais lisibles (3 blocs, un par espèce) */
  [id^='especes_presentes'] .checkbox, [id^='especes_presentes'] .form-check { margin: 2px 0; }
  [id^='especes_presentes'] label { font-size: 12.5px; }
  /* Omble : liste plus longue (13 especes) -- affichage sur 2 colonnes.
     Scope precis sur cet ID pour ne pas affecter Touladi/Dore. */
  #especes_presentes_omble { column-count: 2; column-gap: 14px; }
  #especes_presentes_omble .checkbox, #especes_presentes_omble .form-check {
    break-inside: avoid-column;
  }
  /* Barre « Upload complete » du fileInput : plus discrète */
  .left-panel .progress { height: 14px; margin-bottom: 6px; font-size: 9.5px; }

  /* ── Champs d'importation ──
     Le champ texte en lecture seule du fileInput tronque systématiquement les
     noms de fichiers longs (un <input> ne peut pas passer à la ligne). Il est
     masqué : le bouton occupe toute la largeur, et le nom complet est affiché
     en dessous par output$*_file_name, sur autant de lignes que nécessaire. */
  .left-panel .input-group > input.form-control[readonly] { display: none; }
  .left-panel .input-group > .input-group-btn,
  .left-panel .input-group > label.input-group-btn { width: 100%; }
  .left-panel .input-group .btn-file { width: 100%; }
  .file-name-line {
    font-size: 11px; color: #495057; line-height: 1.35;
    word-break: break-word; margin: -2px 0 4px 2px;
    display: flex; align-items: baseline; gap: 4px;
  }
  .file-name-line .fn-ic { color: #3B6D11; font-weight: 700; flex: 0 0 auto; }
"


# =============================================================================
# BLOCS SIDEBAR — ZONE VARIABLE PAR ESPÈCE
#
# Architecture : coquille commune (import, sélection lac, morphométrie) +
# zone variable sélectionnée par espèce active (voir output$bloc_specifique_ui
# côté serveur). Un sous-bloc partagé (thermocline, conductivité) est factorisé
# en fonction pour éviter la duplication entre Touladi et Doré.
# =============================================================================

# -- Sous-bloc partagé : température moyenne de l'air (Touladi + Doré) -------
# Deux sources exclusives, sur le même patron que la thermocline et Linf :
# saisie manuelle, ou extraction du raster TMOY au point du lac. La régression
# à partir des degrés-jours a été retirée — sans raster ni saisie, la valeur
# est simplement absente et la thermocline théorique devient indisponible.
bloc_t_air_ui <- function(texte_tip) {
  div(class = "cond-block cond-narrow",
    div(class = "cond-bloc-titre",
      info_tip("T. air moyenne (°C)", texte_tip),
      div(class = "cond-radio cond-radio-right",
        radioButtons("t_air_source", NULL,
          choices  = c("Saisir" = "manual", "Climat" = "climat"),
          selected = "manual", inline = TRUE)
      )
    ),
    conditionalPanel("input.t_air_source === 'manual'",
      tagAppendAttributes(
        numericInput("T_air", NULL, value = NA),
        placeholder = "ex. 4.5 °C"
      )
    ),
    uiOutput("t_air_climat_ui")
  )
}

# -- Sous-bloc partagé : profondeur de la thermocline (Touladi + Doré) --------
bloc_thermocline_ui <- function() {
  div(class = "cond-block cond-narrow",
    div(class = "cond-bloc-titre",
      info_tip("Profondeur de la thermocline (m)",
               paste0("Profondeur de la thermocline (= début de l'hypolimnion). ",
                      "Améliore le modèle Lester 2021. ",
                      "Calculée automatiquement depuis le fichier Habitat, ",
                      "ou saisie manuellement.")),
      div(class = "cond-radio cond-radio-right",
        radioButtons("therm_source", NULL,
          choices  = c("Saisir" = "manual", "Habitat" = "ifa"),
          selected = "manual", inline = TRUE)
      )
    ),
    conditionalPanel("input.therm_source === 'manual'",
      tagAppendAttributes(
        numericInput("therm_manual", NULL, value = NA, min = 0),
        placeholder = "ex. 12 m"
      )
    ),
    conditionalPanel("input.therm_source === 'ifa'",
      uiOutput("ifa_inv_ui")
    ),
    uiOutput("therm_result_ui")
  )
}

# -- Sous-bloc partagé : conductivité (Touladi + Doré) ------------------------
bloc_conductivite_ui <- function() {
  div(class = "cond-block cond-narrow",
    div(class = "cond-bloc-titre",
      info_tip("Conductivité (µS/cm)",
               "Conductivité de l'eau, convertie en solides dissous totaux (SDT). Nécessaire pour les modèles Shuter 1998 et IME.")
    ),
    tagAppendAttributes(
      numericInput("conductivite", NULL, value = NA, min = 0),
      placeholder = "ex. 45"
    ),
    uiOutput("tds_hint")
  )
}

# -- Sous-bloc partagé : quota actuel (Touladi, Doré, Omble) ------------------
bloc_quota_actuel_ui <- function(input_id) {
  div(class = "cond-block cond-narrow",
    div(class = "cond-bloc-titre",
      info_tip("Quota actuel (kg/an)",
               "Quota actuellement en vigueur pour ce lac et cette espèce. Sert de référence pour le comparer aux rendements théorique et observé.")
    ),
    tagAppendAttributes(
      numericInput(input_id, NULL, value = NA, min = 0),
      placeholder = "ex. 250 kg/an"
    )
  )
}

# -- Zone variable : Touladi ---------------------------------------------------
bloc_specifique_touladi <- function() {
  tagList(
    bloc_quota_actuel_ui("quota_actuel_touladi"),
    bloc_t_air_ui(paste0("Température moyenne annuelle de l'air. Requise pour le modèle ",
                         "Lester 2021 (calcul de la mortalité naturelle). En mode ",
                         "« Saisir », pré-remplie automatiquement si la colonne ",
                         "temp_air_moy est présente dans l'onglet Lacs. En mode ",
                         "« Climat », lue dans les rasters TMOY importés, au point du lac.")),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Espèces présentes",
                 paste0("Sélectionner les espèces présentes. Ajuste les rendements ",
                        "régionaux affichés à titre de référence selon la communauté ",
                        "présente. N'affecte pas les modèles Lester, Shuter et IME, ",
                        "calculés pour le Touladi."))
      ),
      checkboxGroupInput("especes_presentes_touladi", NULL,
        choices = c(
          "Doré jaune"                     = "dore",
          "Brochet"                        = "brochet",
          "Achigan"                        = "achigan",
          "Omble de fontaine (dominante)"  = "omble_dom"
        ),
        selected = NULL
      )
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Longueur asymptotique (mm)",
                 paste0("Longueur asymptotique du Touladi. Améliore le modèle Lester 2021. ",
                        "« Calculer » l'estime à partir du fichier de spécimens (tous les ",
                        "inventaires disponibles pour ce lac). « Formule Lester » l'estime ",
                        "à partir de la morphométrie du lac (théorique), même si des ",
                        "spécimens sont disponibles.")),
        div(class = "cond-radio cond-radio-right",
          radioButtons("linf_source", NULL,
            choices  = c("Saisir" = "manual", "Calculer" = "file", "Formule Lester" = "theorique"),
            selected = "manual", inline = TRUE)
        )
      ),
      conditionalPanel("input.linf_source === 'manual'",
        tagAppendAttributes(
          numericInput("linf_manual", NULL, value = NA, min = 0),
          placeholder = "ex. 520 mm"
        )
      ),
      conditionalPanel("input.linf_source === 'file' || input.linf_source === 'theorique'",
        uiOutput("linf_result_ui")
      )
    ),
    bloc_thermocline_ui(),
    bloc_conductivite_ui()
  )
}

# -- Zone variable : Doré jaune ------------------------------------------------
bloc_specifique_dore <- function() {
  tagList(
    bloc_quota_actuel_ui("quota_actuel_dore"),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Espèces présentes",
                 "Sélectionner les espèces présentes dans le lac.")
      ),
      checkboxGroupInput("especes_presentes_dore", NULL,
        choices = c(
          "Touladi"                        = "touladi",
          "Brochet"                        = "brochet",
          "Achigan"                        = "achigan",
          "Omble de fontaine (dominante)"  = "omble_dom"
        ),
        selected = NULL
      )
    ),
    bloc_conductivite_ui(),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Profondeur secchi (m)",
                 paste0("Profondeur de disparition du disque de Secchi — indicateur de ",
                        "transparence de l'eau. Pré-remplie automatiquement depuis l'onglet ",
                        "Parametre (code TR), modifiable manuellement au besoin."))
      ),
      tagAppendAttributes(
        numericInput("secchi", NULL, value = NA, min = 0),
        placeholder = "ex. 3.5 m"
      )
    ),
    bloc_t_air_ui(paste0("Température moyenne annuelle de l'air. Utilisée seulement en repli, ",
                         "pour estimer la thermocline de façon théorique (Shuter et coll. 1983) ",
                         "quand aucune profondeur de thermocline observée n'est disponible ",
                         "ci-dessous. Sans elle, un lac sans thermocline observée est simplement ",
                         "traité comme non stratifié — le modèle reste disponible. En mode ",
                         "« Climat », lue dans les rasters TMOY importés, au point du lac.")),
    bloc_thermocline_ui(),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Degrés-jours 5 °C (G)",
                 paste0("Somme annuelle des degrés-jours au-dessus de 5 °C. En mode ",
                        "« Saisir », entrer la valeur BRUTE (ex. 1825), non divisée par ",
                        "1000 : la conversion pour le modèle de Lester est faite ",
                        "automatiquement. En mode « Climat », lue dans les rasters DJC5 ",
                        "importés, au point du lac.")),
        div(class = "cond-radio cond-radio-right",
          radioButtons("g_source", NULL,
            choices  = c("Saisir" = "manual", "Climat" = "climat"),
            selected = "manual", inline = TRUE)
        )
      ),
      conditionalPanel("input.g_source === 'manual'",
        tagAppendAttributes(
          numericInput("degres_jours_g", NULL, value = NA, min = 0),
          placeholder = "ex. 1825"
        )
      ),
      uiOutput("g_climat_ui")
    )
  )
}

# -- Zone variable : Omble de fontaine (placeholder) --------------------------
bloc_specifique_omble <- function() {
  tagList(
    bloc_quota_actuel_ui("quota_actuel_omble"),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Espèces présentes",
                 paste0("Sélectionner les espèces présentes dans le lac. ",
                        "« Lac en allopatrie » est exclusif — cocher cette case ",
                        "décoche automatiquement les autres, et vice-versa."))
      ),
      checkboxGroupInput("especes_presentes_omble", NULL,
        choices = c(
          "Achigan à petite bouche" = "achigan",
          "Barbotte brune"          = "barbotte",
          "Doré jaune"              = "dore",
          "Grand brochet"           = "brochet",
          "Moulac/Lacmou"           = "moulac",
          "Meuniers"                = "meunier",
          "Mulet à cornes"          = "mulet_cornes",
          "Mulet perlé"             = "mulet_perle",
          "Ménés"                   = "menes",
          "Perchaude"               = "perchaude",
          "Touladi"                 = "touladi",
          "Truite arc-en-ciel"      = "arc_en_ciel",
          "Truite brune"            = "truite_brune",
          "Lac en allopatrie"       = "allopatrie"
        ),
        selected = NULL
      )
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Périmètre (km)", "Périmètre du lac, en kilomètres.")
      ),
      tagAppendAttributes(
        numericInput("perimetre", NULL, value = NA, min = 0),
        placeholder = "ex. 12 km"
      )
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("pH", "pH de l'eau. Utilisé par Valin/Vaillancourt — réduction de 50 % si pH < 5.")
      ),
      tagAppendAttributes(
        numericInput("ph_eau", NULL, value = NA, min = 0, max = 14, step = 0.1),
        placeholder = "ex. 6.5"
      )
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Oxygène dissous — mètres sous 5 ppm",
                 paste0("Nombre de mètres de la colonne d'eau où l'oxygène dissous est ",
                        "inférieur à 5 ppm. Pour un lac de plus de 10 m de profondeur max, ",
                        "seuls les 10 premiers mètres comptent (méthode Valin 1998). Calculé ",
                        "automatiquement depuis le profil du fichier Habitat, ou saisi ",
                        "manuellement — laissé vide, cette réduction est simplement ignorée, ",
                        "jamais fabriquée à 0.")),
        div(class = "cond-radio cond-radio-right",
          radioButtons("o2_source", NULL,
            choices  = c("Saisir" = "manual", "Habitat" = "ifa"),
            selected = "manual", inline = TRUE)
        )
      ),
      conditionalPanel("input.o2_source === 'manual'",
        tagAppendAttributes(
          numericInput("o2_metres_sous_5ppm", NULL, value = NA, min = 0),
          placeholder = "ex. 2"
        )
      ),
      uiOutput("o2_result_ui")
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Tributaire ou émissaire permanent",
                 paste0("Absence confirmée de tributaire ET d'émissaire permanent (conditions ",
                        "naturelles ou castors). Utilisé par Valin/Vaillancourt — réduction de ",
                        "25 % si absent. Laisser sur \"Inconnu\" si non vérifié."))
      ),
      radioButtons("tributaire_emissaire_omble", NULL,
        choices = c("Inconnu" = "inconnu", "Présent" = "present", "Absent" = "absent"),
        selected = "inconnu", inline = TRUE
      )
    ),
    div(class = "cond-block cond-narrow",
      div(class = "cond-bloc-titre",
        info_tip("Nombre de camps/chalets",
                 paste0("Utilisé par Valin/Vaillancourt — réduction de 1 % par chalet/10 ha. ",
                        "Laisser vide ou à 0 si aucun."))
      ),
      tagAppendAttributes(
        numericInput("nb_chalets_omble", NULL, value = NA, min = 0),
        placeholder = "ex. 3"
      )
    ),
    div(class = "text-muted fst-italic small mt-2",
        "Modèle recommandé non encore déterminé — en attente du rapport préliminaire.")
  )
}


# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  theme    = theme_app,
  tags$head(tags$style(css_minimal)),

  # --- En-tête pleine largeur --------------------------------------------------
  div(class = "app-header",
    tags$h1(class = "app-titre", "Calcul du potentiel halieutique")
  ),

  # --- Barre espèces -----------------------------------------------------------
  uiOutput("species_bar"),
  uiOutput("dynamic_theme_css"),

  # --- Corps -------------------------------------------------------------------
  fluidRow(class = "content-row",

    # =========================================================================
    # PANNEAU GAUCHE
    # =========================================================================
    column(4, class = "left-panel",

      # -- Importation des données (fichiers + sélection du lac, masquable) -------
      div(class = "d-flex justify-content-between align-items-center mb-1",
        div(class = "sidebar-subtitle mb-0", "Importation des données"),
        tags$button(
          id      = "btn_toggle_imports",
          class   = "btn btn-sm btn-link p-0 text-muted",
          style   = "text-decoration:none; font-size:11px;",
          onclick = paste0(
            "var el = document.getElementById('imports_panel');",
            "var btn = document.getElementById('btn_toggle_imports');",
            "if (el.style.display === 'none') {",
            "  el.style.display = '';",
            "  btn.textContent = '▾ Masquer';",
            "} else {",
            "  el.style.display = 'none';",
            "  btn.textContent = '▸ Afficher';",
            "}"
          ),
          "▾ Masquer"
        )
      ),
      div(id = "imports_panel",

        # Fichier Potentiel halieutique (Lacs + Profil + Parametre + Specimen)
        div(class = "cond-bloc-titre mb-1 d-flex align-items-center gap-2",
          info_tip("Potentiel halieutique",
                   paste0("Fichier à 4 onglets : Lacs, Profil, Parametre, Specimens. ",
                          "Fournit la morphométrie, le profil thermique, la conductivité ",
                          "et les spécimens (Linf) pour les 3 espèces. ",
                          "Valeurs manquantes : NULL (Lacs/Profil/Parametre) ou « - » (Specimens).")),
          uiOutput("ifa_habitat_hint", inline = TRUE)
        ),
        fileInput("pothal_file", NULL, accept = ".xlsx",
                  buttonLabel = "Importer (.xlsx)", placeholder = ""),
        uiOutput("pothal_file_name"),

        # Fichier exploitation
        div(class = "cond-bloc-titre mb-1 mt-2 d-flex align-items-center gap-2",
          info_tip("Exploitation",
                   paste0("Colonnes clés : No plan d'eau, Nom plan d'eau, ",
                          "Année, Espèce code, Nombre captures, ",
                          "Nombre pesés, Masse mesurée (kg), Effort total (j-p). ",
                          "Valeurs manquantes : laisser vide ou NULL.")),
          uiOutput("exploit_hint", inline = TRUE)
        ),
        fileInput("exploit_file", NULL, accept = ".xlsx",
                  buttonLabel = "Importer (.xlsx)", placeholder = ""),
        uiOutput("exploit_file_name"),

        # Rasters climatiques (DJC5 / TMOY)
        div(class = "cond-bloc-titre mb-1 mt-2 d-flex align-items-center gap-2",
          info_tip("Données climatiques",
                   paste0("Grilles annuelles Info-Climat (MELCCFP) au format GeoTIFF : ",
                          "DJC5 (degrés-jours au-dessus de 5 °C) et TMOY (température ",
                          "moyenne annuelle de l'air). Les fichiers du répertoire « ",
                          CLIMAT_REPERTOIRE, " » sont chargés automatiquement au ",
                          "démarrage ; l'importation ci-dessous sert à les compléter ou ",
                          "à les remplacer ponctuellement (un fichier téléversé prime ",
                          "sur celui du répertoire pour une même année). Nommage attendu : ",
                          "VARIABLE_ANNEE.tif (ex. DJC5_2024.tif) — un nom non conforme ",
                          "est ignoré plutôt que deviné. Nécessite le paquet terra et les ",
                          "coordonnées du lac (colonnes lat/lon de l'onglet Lacs).")),
          uiOutput("climat_hint", inline = TRUE)
        ),
        # Deux champs distincts : un fileInput remplace sa sélection à chaque
        # usage, donc un champ unique effaçait les TMOY dès qu'on importait
        # les DJC5. Séparés, chacun se met à jour indépendamment.
        fluidRow(
          column(6, fileInput("climat_files_tmoy", NULL, accept = ".tif",
                              multiple = TRUE, buttonLabel = "TMOY", placeholder = "")),
          column(6, fileInput("climat_files_djc5", NULL, accept = ".tif",
                              multiple = TRUE, buttonLabel = "DJC5", placeholder = ""))
        ),
        uiOutput("climat_files_name"),
        uiOutput("climat_fenetre_ui"),

        hr(class = "my-2"),

        # Sélection du lac
        uiOutput("lac_select_ui"),
        uiOutput("diagnostic_lac_ui"),

        # Saisie manuelle d'un lac — remplace l'ancien champ "No lac" caché
        tags$div(class = "mt-2",
          tags$button(
            class   = "btn btn-sm btn-outline-primary fw-semibold",
            style   = "font-size:12px;",
            onclick = paste0(
              "var el = document.getElementById('id_panel');",
              "var btn = this;",
              "if (el.style.display === 'none') {",
              "  el.style.display = '';",
              "  btn.textContent = '\u25be Masquer la saisie manuelle';",
              "} else {",
              "  el.style.display = 'none';",
              "  btn.textContent = '+ Saisir un lac manuellement';",
              "}"
            ),
            "+ Saisir un lac manuellement"
          ),
          div(id = "id_panel", style = "display:none; margin-top:8px; padding:8px; background:#f8f9fa; border-radius:5px; border:1px solid #dee2e6;",
            fluidRow(
              column(7, textInput("nom_lac", NULL, placeholder = "Nom du lac")),
              column(5, textInput("no_lac",  NULL, placeholder = "No lac"))
            )
          )
        )

      ), # fin imports_panel
      hr(class = "my-2"),

      # -- Morphométrie du lac (commune à toutes les espèces) ---------------------
      div(class = "sidebar-subtitle mb-1", "Morphométrie du lac"),
      fluidRow(
        column(4, numericInput("sup",      "Superficie (ha)", value = NA, min = 0)),
        column(4, numericInput("prof_max", "Prof. max (m)",   value = NA, min = 0)),
        column(4, numericInput("prof_moy", "Prof. moy. (m)",  value = NA, min = 0))
      ),

      hr(class = "my-2"),

      # -- Données spécifiques à l'espèce active (zone variable) ------------------
      div(class = "sidebar-subtitle mb-1", uiOutput("titre_bloc_specifique", inline = TRUE)),
      uiOutput("bloc_specifique_ui"),

      hr(class = "my-2"),

      # -- Sauvegarde ---------------------------------------------------------------
      uiOutput("save_params_ui")
    ),

    # =========================================================================
    # PANNEAU DROIT
    # =========================================================================
    column(8, style = "padding: 20px;",

      div(class = "d-flex justify-content-between align-items-start mb-1", style = "gap:10px;",
        div(style = "flex:1; min-width:0;", uiOutput("fil_ariane")),
        actionButton("btn_calc", "Calculer",
                     class = "btn btn-primary fw-bold",
                     style = "font-size:13px; padding:6px 14px; white-space:nowrap; flex-shrink:0;",
                     icon  = icon("calculator"))
      ),
      uiOutput("calc_stale_note"),

      tabsetPanel(id = "tabs_resultats",

        # --- Onglet 1 : Rendements théoriques ----------------------------------
        tabPanel("Rendements théoriques",
          br(),

          # Zone 1 — Décision (kg/an) : quota estimé et quota actuel
          uiOutput("kpi_top_ui"),

          # Zone 2 — Comparaison (kg/ha) : barre, observé, modèles
          uiOutput("gauge_rdr_ui"),

          # Tableau détaillé — collapsible
          div(class = "graph-card mb-3",
            div(class = "d-flex justify-content-between align-items-center mb-1",
              tags$strong("Détail par modèle"),
              actionButton("btn_toggle_detail", "▾ Masquer",
                           class = "btn btn-sm btn-link p-0 text-muted",
                           style = "text-decoration:none; font-size:12px;")
            ),
            uiOutput("detail_table_container")
          ),

          # Export
          div(class = "d-flex justify-content-between align-items-center mt-3",
            uiOutput("export_hint_ui"),
            div(
              downloadButton("dl_excel", "Excel",
                             icon  = icon("file-excel"),
                             class = "btn-sm me-2"),
              downloadButton("dl_pdf", "PDF — rapport complet",
                             icon  = icon("file-pdf"),
                             class = "btn-sm btn-secondary")
            )
          )
        ),

        # --- Onglet 2 : Analyse temporelle -------------------------------------
        tabPanel("Analyse des données d'exploitation",
          br(),
          uiOutput("tab2_content")
        )
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # ESPÈCE ACTIVE — sélection, registre, thème réactif
  # ---------------------------------------------------------------------------
  espece_active_rv <- reactiveVal(ESPECE_ACTIVE)
  config <- reactive(REGISTRE_ESPECES[[ espece_active_rv() ]])

  # Lac courant (independant du dropdown) + etat de validite du calcul affiche
  lac_courant_rv <- reactiveVal("")
  calc_valide_rv <- reactiveVal(FALSE)   # TRUE = resultats theoriques a jour
  a_calcule_rv   <- reactiveVal(FALSE)   # TRUE des qu'un calcul a ete lance

  # Etat precedent de la case a cocher especes Omble — pour detecter ce qui
  # vient de changer (checkboxGroupInput retourne toujours les valeurs
  # selectionnees dans l'ordre des choices, pas dans l'ordre de clic).
  especes_omble_prev_rv <- reactiveVal(character(0))

  # Clic sur un onglet espèce
  observeEvent(input$espece_click, {
    if (input$espece_click %in% names(REGISTRE_ESPECES))
      espece_active_rv(input$espece_click)
  })

  # Especes presentes dans le lac selectionne (pour griser les onglets vides)
  especes_du_lac <- reactive({
    df <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(df) || !isTruthy(input$no_lac)) return(names(REGISTRE_ESPECES))
    codes <- df %>%
      filter(normaliser_nolac(nolac) == normaliser_nolac(input$no_lac)) %>%
      distinct(espece_code) %>% pull(espece_code) %>% as.character()
    cles <- names(REGISTRE_ESPECES)[
      vapply(REGISTRE_ESPECES, function(e) e$code_ifa %in% codes, logical(1))]
    if (length(cles) == 0) names(REGISTRE_ESPECES) else cles
  })

  # La barre se redessine — la classe « active » suit l'espèce ; onglets sans
  # données pour le lac courant grisés.
  output$species_bar <- renderUI(build_species_bar(espece_active_rv(), especes_du_lac()))

  # Titre + contenu de la zone variable par espèce (coquille commune + zone spécifique)
  output$titre_bloc_specifique <- renderUI(paste0("Données spécifiques — ", config()$nom))

  output$bloc_specifique_ui <- renderUI({
    switch(espece_active_rv(),
      touladi = bloc_specifique_touladi(),
      dore    = bloc_specifique_dore(),
      omble   = bloc_specifique_omble()
    )
  })

  # Recoloration du thème à chaque changement d'espèce
  observeEvent(espece_active_rv(), {
    session$setCurrentTheme(make_theme(config()$palette))
  })

  # Changement d'espèce : conserver le lac s'il existe pour la nouvelle espèce,
  # sinon reset complet. Le calcul théorique devient périmé dans tous les cas.
  observeEvent(espece_active_rv(), {
    calc_valide_rv(FALSE)
    lac <- lac_courant_rv()
    df  <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(df) || !isTruthy(lac)) return()
    lacs_esp <- df %>%
      filter(espece_code == config()$code_ifa) %>%
      distinct(nolac) %>% pull(nolac) %>% as.character()
    if (!(lac %in% lacs_esp)) {
      # espèce absente de ce lac → reset sélection + paramètres + onglets
      lac_courant_rv("")
      updateTextInput(session, "no_lac",  value = "")
      updateTextInput(session, "nom_lac", value = "")
      updateNumericInput(session, "sup",            value = NA)
      updateNumericInput(session, "T_air",          value = NA)
      updateNumericInput(session, "prof_max",       value = NA)
      updateNumericInput(session, "prof_moy",       value = NA)
      updateNumericInput(session, "perimetre",      value = NA)
      updateNumericInput(session, "conductivite",   value = NA)
      updateNumericInput(session, "linf_manual",    value = NA)
      updateNumericInput(session, "secchi",         value = NA)
      updateNumericInput(session, "degres_jours_g", value = NA)
      updateNumericInput(session, "quota_actuel_touladi", value = NA)
      updateNumericInput(session, "quota_actuel_dore",    value = NA)
      updateNumericInput(session, "quota_actuel_omble",   value = NA)
    }
    # sinon : lac et paramètres conservés ; recalcul manuel attendu
  }, ignoreInit = TRUE)

  # CSS réactif — recolore entête, barre et accents selon l'espèce
  # (complète setCurrentTheme, qui gère déjà boutons, onglets et focus)
  output$dynamic_theme_css <- renderUI({
    p <- config()$palette
    tags$style(HTML(sprintf("
      .app-header  { background: linear-gradient(135deg, %1$s, %2$s) !important; }
      .species-bar { background: %3$s !important; }

      .kpi-top-card.main { background: %4$s !important; border-left-color: %2$s !important; }
      .kpi-top-card.main .kpi-eyebrow { color: %2$s !important; }

      .cond-block { border-left-color: %2$s !important; }

      #kpi_obs_n .form-check-input:checked + .form-check-label {
        background: %2$s !important; border-color: %2$s !important; color: white !important;
      }
    ",
    p$primaire,   # %1$s — fond gauche du dégradé
    p$accent,     # %2$s — accents (bordures, pastilles, dégradé droit)
    p$bar,        # %3$s — bande sombre des espèces
    p$tint        # %4$s — fond clair des cartes accentuées
    )))
  })

  # ── Modèles régionaux actifs selon l'espèce active et les espèces présentes ──
  #    Branché sur espece_active_rv() : chaque espèce déclare ses propres
  #    régions (MODELES_REGIONAUX[[espece]]) et son propre type de grille.
  #      - "binaire" (Touladi)        : val_seul / val_mixte
  #      - "grille"  (Doré, régions)  : classe de superficie × mono/multi
  #      - "fixe"    (Doré, NDQ)      : valeur unique, indépendante
  #    La présence d'au moins une espèce compétitrice déclarée pour l'espèce
  #    active fait basculer seul→mixte (Touladi) ou mono→multi (Doré).
  #    Renvoie une liste ordonnée : list(key, nom, val, note, communaute).
  regions_actives <- reactive({
    esp    <- espece_active_rv()
    defs   <- MODELES_REGIONAUX[[esp]]
    ordre  <- REGIONS_ORDRE[[esp]]
    if (is.null(defs) || is.null(ordre)) return(list())

    competiteurs <- ESPECES_COMPETITRICES[[esp]]
    presentes    <- input[[paste0("especes_presentes_", esp)]]
    mixte        <- length(intersect(presentes, competiteurs)) > 0
    sup_val      <- input$sup

    # Omble : grilles categorielles (type "fn"), pas de mono/multi binaire —
    # necessite le classement fin par groupe d'especes (voir especes_groupes_omble()).
    grp_omble <- if (identical(esp, "omble")) especes_groupes_omble(presentes) else NULL

    res <- lapply(ordre, function(k) {
      m <- defs[[k]]
      if (is.null(m)) return(NULL)

      if (identical(m$type, "binaire")) {
        val  <- if (mixte) m$val_mixte else m$val_seul
        note <- if (mixte) m$note_mixte else m$note_seul
      } else if (identical(m$type, "fixe")) {
        val  <- m$val
        note <- m$note
      } else if (identical(m$type, "grille")) {
        idx <- if (is.null(sup_val) || is.na(sup_val)) NA_integer_
               else which(sup_val <= m$seuils_ha)[1]
        if (is.na(idx)) {
          val  <- NA_real_
          note <- paste0(m$nom, " — superficie manquante, classe indéterminée")
        } else {
          val  <- if (mixte) m$multi[idx] else m$mono[idx]
          note <- paste0(m$nom, " — classe ", m$classes[idx], ", population ",
                         if (mixte) "multispécifique" else "monospécifique")
        }
      } else if (identical(m$type, "fn")) {
        idx <- if (is.null(m$seuils_ha)) NA_integer_
               else if (is.null(sup_val) || is.na(sup_val)) NA_integer_
               else which(sup_val <= m$seuils_ha)[1]
        r    <- m$fn(grp_omble, idx)
        val  <- r$val
        note <- r$note
      } else {
        val  <- NA_real_
        note <- ""
      }

      list(key = k, nom = m$nom, val = val, note = note,
           communaute = if (mixte) "mixte" else "seul")
    })
    Filter(Negate(is.null), res)
  })

  # ── Saisie manuelle du % → synchronise le slider ─────────────────────────
  # Debounce 600 ms : attend la fin de la saisie avant de recalculer
  pct_rv       <- reactiveVal(PCT_RECOMMANDE)
  pct_manual_d <- debounce(reactive(input$pct_manual), 600)

  observeEvent(pct_manual_d(), {
    req(!is.null(pct_manual_d()), !is.na(pct_manual_d()))
    val <- max(1L, min(200L, as.integer(round(pct_manual_d()))))
    pct_rv(val)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ---------------------------------------------------------------------------
  # RÉACTIFS — Fichier IFA habitat
  # ---------------------------------------------------------------------------

  #' Lecture + nettoyage du fichier « Potentiel halieutique » (4 onglets),
  #' indépendante du lac sélectionné — parsée une seule fois par import.
  #' Retourne : $lacs (dédupliqué IPE > OG), $lacs_brutes (avant dédup, pour
  #' diagnostic), $profil, $parametre (pivoté), $specimens, $note.
  pothal_brut <- reactive({
    req(input$pothal_file)
    tryCatch({
      chemin  <- input$pothal_file$datapath
      onglets <- readxl::excel_sheets(chemin)
      attendus  <- c("Lacs", "Profil", "Parametre", "Specimens")
      manquants <- setdiff(attendus, onglets)
      if (length(manquants) > 0) {
        showNotification(
          paste0("Onglet(s) manquant(s) dans le fichier Potentiel halieutique : ",
                 paste(manquants, collapse = ", ")),
          type = "error", duration = 8)
        return(NULL)
      }

      lacs      <- nettoyer_na_ifa(read_excel(chemin, sheet = "Lacs"))
      profil    <- nettoyer_na_ifa(read_excel(chemin, sheet = "Profil"))
      parametre <- nettoyer_na_ifa(read_excel(chemin, sheet = "Parametre"))
      specimens  <- nettoyer_na_ifa(read_excel(chemin, sheet = "Specimens"), marqueurs = "-")

      names(lacs)      <- tolower(trimws(names(lacs)))
      names(profil)    <- tolower(trimws(names(profil)))
      names(parametre) <- tolower(trimws(names(parametre)))
      names(specimens)  <- tolower(trimws(names(specimens)))

      lacs      <- appliquer_correspondances(lacs,      PATRONS_LACS)
      profil    <- appliquer_correspondances(profil,    PATRONS_PROFIL)
      parametre <- appliquer_correspondances(parametre, PATRONS_PARAMETRE)
      specimens  <- appliquer_correspondances(specimens,  PATRONS_SPECIMENS)

      # nolac : JAMAIS converti en numérique (corrigé 2026-09). Le fichier réel
      # contient des identifiants alphanumériques dans les quatre onglets
      # (04301B07, C0762, F0142, A6552...) que as.numeric() transformait
      # silencieusement en NA : ces lacs disparaissaient de la liste de sélection
      # et perdaient leur profil thermique et leur profil d'oxygène, sans aucun
      # message. normaliser_nolac() les conserve tels quels et se contente de
      # retirer les zéros non significatifs ("00001" -> "1"), ce qui rend les
      # quatre onglets comparables entre eux ET avec le fichier d'exploitation,
      # qui utilise déjà cette même normalisation.
      if ("nolac" %in% names(lacs))      lacs$nolac      <- normaliser_nolac(lacs$nolac)
      if ("nolac" %in% names(profil))    profil$nolac    <- normaliser_nolac(profil$nolac)
      if ("nolac" %in% names(parametre)) parametre$nolac <- normaliser_nolac(parametre$nolac)
      if ("nolac" %in% names(specimens)) specimens$nolac <- normaliser_nolac(specimens$nolac)

      for (col in c("lat", "lon", "sup", "prof_max", "prof_moy", "perimetre"))
        if (col %in% names(lacs)) lacs[[col]] <- suppressWarnings(as.numeric(lacs[[col]]))

      for (col in c("no_inv", "station", "prof_mes", "temp", "do", "ph", "cond"))
        if (col %in% names(profil)) profil[[col]] <- suppressWarnings(as.numeric(profil[[col]]))
      if ("date" %in% names(profil))
        profil$date <- convertir_date_excel(profil$date)

      # « Anné début inventaire » est une DATE dans le fichier réel, pas une année
      if ("annee" %in% names(specimens))  specimens$annee <- extraire_annee(specimens$annee)
      if ("long_totale" %in% names(specimens))
        specimens$long_totale <- suppressWarnings(as.numeric(specimens$long_totale))

      req_lacs <- c("nolac", "sup", "prof_max", "prof_moy")
      manq_lacs <- setdiff(req_lacs, names(lacs))
      if (length(manq_lacs) > 0) {
        showNotification(
          paste0("Colonnes manquantes dans l'onglet Lacs : ", paste(manq_lacs, collapse = ", ")),
          type = "error", duration = 8)
        return(NULL)
      }

      list(
        lacs        = dedup_lacs_par_ue(lacs),
        lacs_brutes = lacs,
        profil      = profil,
        parametre   = pivoter_parametre(parametre),
        specimens    = specimens,
        note        = "Fichier Potentiel halieutique importé avec succès."
      )
    }, error = function(e) {
      showNotification(
        paste("Erreur de lecture du fichier Potentiel halieutique :", e$message),
        type = "error", duration = 8)
      NULL
    })
  })

  #' Composition par lac — garde exactement la même forme de sortie que
  #' l'ancien import à onglet unique, pour ne rien casser en aval
  #' (therm_resolved(), remplir_morpho_depuis_inv(), etc.) :
  #'   $nomlac, $inventaires (sup, prof_max, prof_moy, cond, ...), $profils,
  #'   $inv_defaut, $note
  ifa_habitat_raw <- reactive({
    req(isTruthy(lac_courant_rv()))

    echec <- function(msg) list(nomlac = NA_character_, inventaires = NULL, profils = NULL,
                                inv_defaut = NA_character_, note = msg)

    pb <- tryCatch(pothal_brut(), error = function(e) NULL)
    if (is.null(pb)) return(echec("Fichier Potentiel halieutique non chargé ou invalide."))

    tryCatch({
      nolac_val <- cle_lac(lac_courant_rv())

      # --- Lacs : morphométrie statique (1 ligne, dédupliquée IPE > OG) ------
      lac_row <- if (!is.na(nolac_val) && "nolac" %in% names(pb$lacs)) {
        pb$lacs[!is.na(pb$lacs$nolac) & pb$lacs$nolac == nolac_val, ]
      } else pb$lacs[0, ]

      if (nrow(lac_row) == 0)
        return(echec(paste0("Aucune ligne dans l'onglet Lacs pour le lac ", lac_courant_rv(), ".")))

      nomlac_val    <- if ("nomlac" %in% names(lac_row))    lac_row$nomlac[1]    else NA_character_
      sup_val       <- if ("sup" %in% names(lac_row))       lac_row$sup[1]       else NA_real_
      prof_max_val  <- if ("prof_max" %in% names(lac_row))  lac_row$prof_max[1]  else NA_real_
      prof_moy_val  <- if ("prof_moy" %in% names(lac_row))  lac_row$prof_moy[1]  else NA_real_
      t_air_val     <- if ("t_air_moy" %in% names(lac_row)) lac_row$t_air_moy[1] else NA_real_
      # Coordonnées : déjà lues par PATRONS_LACS, désormais conservées pour
      # l'extraction climatique (elles n'étaient utilisées nulle part avant).
      lat_val       <- if ("lat" %in% names(lac_row))       lac_row$lat[1]       else NA_real_
      lon_val       <- if ("lon" %in% names(lac_row))       lac_row$lon[1]       else NA_real_
      perimetre_val <- if ("perimetre" %in% names(lac_row)) lac_row$perimetre[1] else NA_real_

      # --- Conductivité : priorité Parametre (code CD), repli sur Profil -----
      cond_parametre <- extraire_param(pb$parametre, nolac_val, "CD")

      # --- Secchi : Parametre (code TR — transparence), aucun repli Profil --
      # (contrairement à la conductivité, la transparence n'a pas d'équivalent
      # mesuré dans l'onglet Profil ; NA si code TR absent → saisie manuelle)
      secchi_parametre <- extraire_param(pb$parametre, nolac_val, "TR")

      # --- pH : Parametre (code PH), même patron que CD/TR — confirmé avec ---
      # le collègue (même emplacement que CD/TR). Aucun repli Profil pour
      # l'instant (le pH par profondeur existe dans l'onglet Profil mais n'est
      # pas encore consommé ici — cf. le point O2 ci-dessous, même limitation).
      ph_parametre <- extraire_param(pb$parametre, nolac_val, "PH")

      # --- Profil : construction des inventaires (même logique qu'avant) ----
      df_lac <- if (!is.na(nolac_val) && "nolac" %in% names(pb$profil)) {
        pb$profil[!is.na(pb$profil$nolac) & pb$profil$nolac == nolac_val, ]
      } else pb$profil[0, ]

      if (nrow(df_lac) == 0)
        return(echec(paste0("Aucune ligne dans l'onglet Profil pour le lac ", lac_courant_rv(), ".")))

      has_no_inv <- "no_inv" %in% names(df_lac) && any(!is.na(df_lac$no_inv))
      has_date   <- "date"   %in% names(df_lac) && any(!is.na(df_lac$date))

      if (has_no_inv) {
        df_lac$inv_key <- as.character(df_lac$no_inv)
      } else if (has_date) {
        df_lac$inv_key <- as.character(df_lac$date)
      } else {
        df_lac$inv_key <- "1"
      }
      df_lac$inv_key[is.na(df_lac$inv_key)] <- "NA"

      # Garantir la presence de "do" (oxygene dissous) meme si absente du
      # fichier source -- evite une erreur "object 'do' not found" dans le
      # summarise() de profils ci-dessous (appliquer_correspondances() ne
      # cree la colonne que si un motif correspondant est trouve).
      if (!("do" %in% names(df_lac))) df_lac$do <- NA_real_

      premier_grp <- function(col, df_src) {
        if (!(col %in% names(df_src))) return(NA_real_)
        v <- df_src[[col]]
        if (length(v) == 0L) return(NA_real_)
        dplyr::first(v[!is.na(v)])
      }

      invs <- do.call(rbind, lapply(unique(df_lac$inv_key), function(k) {
        sub <- df_lac[df_lac$inv_key == k, ]
        date_lbl <- if (has_date) {
          d <- dplyr::first(na.omit(sub$date))
          if (!is.na(d)) as.character(d) else k
        } else k
        n_pts     <- sum(!is.na(sub$prof_mes) & !is.na(sub$temp))
        cond_prof <- premier_grp("cond", sub)   # repli seulement — Parametre prioritaire
        data.frame(
          inv_key       = k,
          date_label    = date_lbl,
          n_pts         = n_pts,
          profil_valide = n_pts >= 5L,
          sup           = sup_val,
          prof_max      = prof_max_val,
          prof_moy      = prof_moy_val,
          t_air         = t_air_val,
          lat           = lat_val,
          lon           = lon_val,
          perimetre     = perimetre_val,
          cond          = if (!is.na(cond_parametre)) cond_parametre else cond_prof,
          cond_source   = if (!is.na(cond_parametre)) "Parametre"
                         else if (!is.na(cond_prof))  "Profil (repli)"
                         else NA_character_,
          secchi        = secchi_parametre,
          secchi_source = if (!is.na(secchi_parametre)) "Parametre" else NA_character_,
          ph            = ph_parametre,
          ph_source     = if (!is.na(ph_parametre)) "Parametre" else NA_character_,
          stringsAsFactors = FALSE
        )
      }))
      invs <- invs[order(invs$date_label, decreasing = TRUE), ]

      profils <- lapply(invs$inv_key, function(k) {
        df_lac[df_lac$inv_key == k, ] %>%
          filter(!is.na(prof_mes), !is.na(temp)) %>%
          group_by(prof_mes) %>%
          summarise(temp = mean(temp, na.rm = TRUE),
                    # do (oxygene dissous, mg/L ~ ppm) -- ajoute pour la
                    # reduction O2 automatique (Omble, Valin/Vaillancourt).
                    # NA si aucune lecture d'oxygene a cette profondeur
                    # (ne bloque pas la temperature, deja filtree separement).
                    do   = if (all(is.na(do))) NA_real_ else mean(do, na.rm = TRUE),
                    .groups = "drop") %>%
          arrange(prof_mes)
      })
      names(profils) <- invs$inv_key

      inv_valides <- invs[invs$profil_valide, ]
      inv_defaut  <- if (nrow(inv_valides) > 0) inv_valides$inv_key[1] else NA_character_

      note_lac <- if (!is.na(nomlac_val)) nomlac_val else paste0("lac ", lac_courant_rv())
      n_inv    <- nrow(invs)
      n_val    <- sum(invs$profil_valide)
      note     <- paste0(note_lac, " — ", n_inv, " inventaire(s) trouvé(s), ",
                         n_val, " avec profil thermique valide (≥ 5 pts).")

      list(nomlac      = nomlac_val,
           inventaires = invs,
           profils     = profils,
           inv_defaut  = inv_defaut,
           note        = note)

    }, error = function(e) {
      echec(paste0("Erreur de composition du lac : ", e$message))
    })
  })

  # Fonction interne : pré-remplir les champs morpho depuis une ligne d'inventaires
  remplir_morpho_depuis_inv <- function(session, inv_row) {
    # Toujours écrire la valeur — NA vide le champ, évite de laisser les données du lac précédent
    updateNumericInput(session, "sup",          value = inv_row$sup)
    updateNumericInput(session, "prof_max",     value = inv_row$prof_max)
    updateNumericInput(session, "prof_moy",     value = inv_row$prof_moy)
    updateNumericInput(session, "conductivite", value = inv_row$cond)
    updateNumericInput(session, "secchi",       value = inv_row$secchi)
    updateNumericInput(session, "perimetre",    value = inv_row$perimetre)
    updateNumericInput(session, "ph_eau",       value = inv_row$ph)
    # T. air moyenne : pré-remplie si présente dans l'onglet Lacs (colonne temp_air_moy),
    # demeure modifiable manuellement ensuite (l'utilisateur doit mettre à jour son fichier
    # lui-même pour que la valeur soit reprise automatiquement la prochaine fois).
    updateNumericInput(session, "T_air",        value = inv_row$t_air)
  }

  observeEvent(ifa_habitat_raw(), {
    ifa <- ifa_habitat_raw()

    cle_courante <- as.character(lac_courant_rv())
    if (!is.null(params_sauvegardes_rv()[[cle_courante]])) return()

    if (is.null(ifa$inventaires) || nrow(ifa$inventaires) == 0) {
      updateNumericInput(session, "sup",          value = NA)
      updateNumericInput(session, "prof_max",     value = NA)
      updateNumericInput(session, "prof_moy",     value = NA)
      updateNumericInput(session, "conductivite", value = NA)
      updateNumericInput(session, "secchi",       value = NA)
      updateNumericInput(session, "perimetre",    value = NA)
      updateNumericInput(session, "ph_eau",       value = NA)
      updateNumericInput(session, "T_air",        value = NA)
      updateRadioButtons(session, "therm_source", selected = "manual")
      updateRadioButtons(session, "o2_source",     selected = "manual")
      return()
    }

    if (!is.na(ifa$inv_defaut)) {
      updateRadioButtons(session, "therm_source", selected = "ifa")
      updateRadioButtons(session, "o2_source",     selected = "ifa")
    } else {
      updateRadioButtons(session, "therm_source", selected = "manual")
      updateRadioButtons(session, "o2_source",     selected = "manual")
    }
    inv_row <- ifa$inventaires[ifa$inventaires$inv_key == ifa$inv_defaut, ]
    if (nrow(inv_row) > 0) remplir_morpho_depuis_inv(session, inv_row[1, ])
    warnings_actifs_rv(TRUE)
  })

  # Mise à jour des champs morpho + basculement therm_source quand l'inventaire change
  observeEvent(input$ifa_inv_choisi, {
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    req(!is.null(ifa), !is.null(input$ifa_inv_choisi))
    inv_row <- ifa$inventaires[ifa$inventaires$inv_key == input$ifa_inv_choisi, ]
    if (nrow(inv_row) > 0) {
      remplir_morpho_depuis_inv(session, inv_row[1, ])
      # Basculer therm_source/o2_source selon validité du profil sélectionné
      if (isTRUE(inv_row$profil_valide[1])) {
        updateRadioButtons(session, "therm_source", selected = "ifa")
        updateRadioButtons(session, "o2_source",     selected = "ifa")
      } else {
        updateRadioButtons(session, "therm_source", selected = "manual")
        updateRadioButtons(session, "o2_source",     selected = "manual")
      }
    }
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # RÉACTIFS — Inputs conditionnels
  # ---------------------------------------------------------------------------
  linf_resolved <- reactive({
    if (input$linf_source == "manual") {
      val <- input$linf_manual
      list(value  = if (!is.na(val) && val > 0) val else NA_real_,
           source = "saisie manuelle",
           n      = NA_integer_,
           note   = "")
    } else if (input$linf_source == "theorique") {
      sup_val <- input$sup
      if (is.null(sup_val) || is.na(sup_val) || sup_val <= 0)
        return(list(value  = NA_real_,
                    source = "formule Lester : superficie du lac manquante",
                    n      = NA_integer_, note = ""))
      th <- resolve_linf(NA_real_, sup_val)
      list(value  = round(th$value, 1),
           source = "formule Lester (théorique)",
           n      = NA_integer_,
           note   = "")
    } else {
      pb <- tryCatch(pothal_brut(), error = function(e) NULL)
      if (is.null(pb) || is.null(pb$specimens))
        return(list(value = NA_real_,
                    source = "fichier Potentiel halieutique non chargé",
                    n = NA_integer_, note = ""))

      df_raw <- pb$specimens
      if (!("nolac" %in% names(df_raw)))
        return(list(value = NA_real_, source = "fichier : colonne 'No plan d'eau' introuvable",
                    n = NA_integer_, note = ""))
      if (!("long_totale" %in% names(df_raw)))
        return(list(value = NA_real_, source = "fichier : colonne 'Long. totale max' introuvable",
                    n = NA_integer_, note = ""))

      # Filtre sur nolac — toutes années confondues. Comparaison sur clé
      # normalisée : plus de double chemin numérique/texte, l'onglet Specimens
      # pouvant contenir des identifiants alphanumériques.
      nolac_val <- cle_lac(input$no_lac)
      df_f <- if (!is.na(nolac_val)) {
        df_raw[!is.na(df_raw$nolac) & df_raw$nolac == nolac_val, ]
      } else df_raw[0, ]

      # Filtre espèce active — un seul fichier Specimen couvre les 3 espèces
      if ("espece_code" %in% names(df_f))
        df_f <- df_f[!is.na(df_f$espece_code) &
                     toupper(trimws(df_f$espece_code)) == config()$code_ifa, ]

      lt_vec <- suppressWarnings(as.numeric(df_f$long_totale))
      lt_vec <- lt_vec[!is.na(lt_vec) & lt_vec > 0]
      n_spec <- length(lt_vec)

      if (n_spec < 10L)
        return(list(value  = NA_real_,
                    source = paste0("fichier : ", n_spec, " spécimen(s) — minimum 10 requis"),
                    n      = n_spec, note   = ""))

      linf_val <- calc_linf_janoscik(lt_vec)
      if (is.na(linf_val))
        return(list(value  = NA_real_,
                    source = "fichier : Linf non calculable (échantillon insuffisant après coupe)",
                    n      = n_spec, note   = ""))

      note_n <- if (n_spec < 20L)
        paste0("⚠ Échantillon faible (", n_spec, " spécimens) — Linf à interpréter avec prudence")
      else ""

      list(value  = round(linf_val, 1),
           source = paste0("fichier (Janošík, n = ", n_spec, ")"),
           n      = n_spec,
           note   = note_n)
    }
  })

  # ---------------------------------------------------------------------------
  # DONNÉES CLIMATIQUES
  # ---------------------------------------------------------------------------
  catalogue_climat <- reactive({
    catalogue_rasters(input$climat_files_tmoy, input$climat_files_djc5)
  })

  n_annees_climat <- reactive({ as.integer(input$climat_fenetre %||% "1") })

  # Coordonnées du lac sélectionné (onglet Lacs). Toutes les lignes
  # d'inventaire d'un même lac portent les mêmes coordonnées : la première suffit.
  coord_lac <- reactive({
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires) || nrow(ifa$inventaires) == 0)
      return(c(lat = NA_real_, lon = NA_real_))
    r1 <- ifa$inventaires[1, ]
    c(lat = if ("lat" %in% names(r1)) as.numeric(r1$lat) else NA_real_,
      lon = if ("lon" %in% names(r1)) as.numeric(r1$lon) else NA_real_)
  })

  # Une extraction par variable. Chaque appel relit n rasters sur disque : les
  # réactifs ne se réévaluent que si le lac, les fichiers ou la fenêtre changent.
  climat_tmoy <- reactive({
    co <- coord_lac()
    extraire_climat(co[["lat"]], co[["lon"]], catalogue_climat()$catalogue,
                    "TMOY", n_annees_climat())
  })
  climat_djc5 <- reactive({
    co <- coord_lac()
    extraire_climat(co[["lat"]], co[["lon"]], catalogue_climat()$catalogue,
                    "DJC5", n_annees_climat())
  })

  # Réactif partagé : T° air effective utilisée par les modèles et le repli
  #   thermocline. Source explicite (saisie ou raster TMOY) — plus de valeur
  #   déduite des degrés-jours : sans saisie ni raster, la valeur est absente
  #   et la thermocline théorique devient indisponible.
  t_air_effectif <- reactive({
    if (identical(input$t_air_source, "climat")) return(climat_tmoy()$valeur)
    ta <- input$T_air
    if (!is.null(ta) && !is.na(ta)) ta else NA_real_
  })

  # Degrés-jours effectifs (valeur BRUTE, non divisée par 1000).
  g_effectif <- reactive({
    if (identical(input$g_source, "climat")) return(climat_djc5()$valeur)
    g <- input$degres_jours_g
    if (!is.null(g) && !is.na(g)) g else NA_real_
  })

  therm_resolved <- reactive({
    # Profondeur théorique (repli Shuter et coll. 1983) — calculée dès que
    #   possible, affichée même quand elle ne sert que de repli informatif
    #   (cf. therm_result_ui) et non comme valeur utilisée par le modèle.
    calc_dth_theo <- function() {
      A <- input$sup; Dmn <- input$prof_moy; Dmax <- input$prof_max; ta <- t_air_effectif()
      if (is.na(A) || is.na(Dmn) || is.na(Dmax) || is.na(ta) || Dmn <= 0 || Dmax <= 0) return(NA_real_)
      val <- 3.26 * (A^0.109) * (Dmn^0.213) * exp(-0.0263 * ta)
      if (is.finite(val) && val > 0 && val < Dmax) round(val, 2) else NA_real_
    }

    if (input$therm_source == "manual") {
      val <- input$therm_manual
      list(value      = if (!is.na(val) && val > 0) val else NA_real_,
           source     = "saisie manuelle",
           date_label = NULL,
           details    = NULL,
           theorique  = calc_dth_theo(),
           note       = "")
    } else {
      # Mode IFA : utiliser le profil de l'inventaire sélectionné
      ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
      if (is.null(ifa) || is.null(ifa$inventaires))
        return(list(value = NA_real_, source = "IFA : fichier non chargé",
                    date_label = NULL, details = NULL, theorique = calc_dth_theo(), note = ""))

      # Clé d'inventaire : préférer la sélection utilisateur, sinon le défaut
      inv_key <- if (!is.null(input$ifa_inv_choisi) && nchar(input$ifa_inv_choisi) > 0) {
        input$ifa_inv_choisi
      } else {
        ifa$inv_defaut
      }

      if (is.na(inv_key) || is.null(ifa$profils[[inv_key]]))
        return(list(value   = NA_real_,
                    source  = "IFA : aucun inventaire avec profil valide",
                    date_label = NULL, details = NULL,
                    theorique  = calc_dth_theo(),
                    note    = "Repli sur la thermocline théorique (Shuter)."))

      profil_sel <- ifa$profils[[inv_key]]
      if (nrow(profil_sel) < 5L)
        return(list(value   = NA_real_,
                    source  = paste0("IFA (inventaire ", inv_key, ") : moins de 5 points"),
                    date_label = NULL, details = NULL,
                    theorique  = calc_dth_theo(),
                    note    = "Repli sur la thermocline théorique (Shuter)."))

      th <- detect_thermocline_norm(profil_sel)
      inv_info <- ifa$inventaires[ifa$inventaires$inv_key == inv_key, ]
      date_lbl <- if (nrow(inv_info) > 0) inv_info$date_label[1] else inv_key

      if (th$statut != "stratifie" || is.na(th$z_hypo))
        return(list(value   = NA_real_,
                    source  = paste0("IFA (", date_lbl, ") : ", th$raison),
                    date_label = date_lbl, details = th,
                    theorique  = calc_dth_theo(),
                    note    = "Repli sur la thermocline théorique (Shuter)."))

      list(value      = th$z_hypo,
           source     = paste0("IFA (", date_lbl, ") — z_hypo = ", th$z_hypo,
                            " m, thermo ", th$z_thermo_top, "–", th$z_thermo_bot, " m"),
           date_label = date_lbl,
           details    = th,
           theorique  = NA_real_,
           note       = "")
    }
  })

  # Résolution de la réduction O2 (Omble, Valin/Vaillancourt) — même patron
  # que therm_resolved() : source manuelle ou profil IFA (interpolé via
  # detect_o2_sous_5ppm()), avec repli explicite si le profil est absent ou
  # sans lecture d'oxygène valide. Retourne $value = m_sous_5ppm (jamais un
  # % directement — la conversion se fait dans calc_reduction_o2_valin(),
  # appelée depuis le bassin commun).
  o2_resolved <- reactive({
    if (is.null(input$o2_source) || identical(input$o2_source, "manual")) {
      # input$o2_metres_sous_5ppm vaut NULL (pas NA) tant que l'onglet Omble
      # n'a jamais ete rendu dans la session (bloc_specifique_omble() n'est
      # monte dans le DOM que lorsque cet onglet est actif -- voir
      # bloc_specifique_ui). o2_resolved() est pourtant appele sans condition
      # pour TOUTES les especes (bassin commun, results()) -- sans cette
      # normalisation, le calcul plantait pour n'importe quelle espece tant
      # qu'Omble n'avait pas ete visite au moins une fois (corrige 2026-07).
      val <- input$o2_metres_sous_5ppm
      if (is.null(val)) val <- NA_real_
      return(list(value = if (!is.na(val) && val >= 0) val else NA_real_,
                  source = "saisie manuelle", note = ""))
    }

    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires))
      return(list(value = NA_real_, source = "IFA : fichier non chargé", note = ""))

    inv_key <- if (!is.null(input$ifa_inv_choisi) && nchar(input$ifa_inv_choisi) > 0) {
      input$ifa_inv_choisi
    } else {
      ifa$inv_defaut
    }

    if (is.na(inv_key) || is.null(ifa$profils[[inv_key]]))
      return(list(value = NA_real_,
                  source = "IFA : aucun inventaire avec profil valide",
                  note = "Réduction O2 ignorée — saisir manuellement au besoin."))

    profil_sel <- ifa$profils[[inv_key]]
    od <- detect_o2_sous_5ppm(profil_sel, input$prof_max)
    inv_info <- ifa$inventaires[ifa$inventaires$inv_key == inv_key, ]
    date_lbl <- if (nrow(inv_info) > 0) inv_info$date_label[1] else inv_key

    if (od$statut != "ok" || is.na(od$m_sous_5ppm))
      return(list(value = NA_real_,
                  source = paste0("IFA (", date_lbl, ") : ", od$raison),
                  note = "Réduction O2 ignorée — saisir manuellement au besoin."))

    list(value  = od$m_sous_5ppm,
         source = paste0("IFA (", date_lbl, ") — ", od$m_sous_5ppm, " m sous 5 ppm"),
         note   = "")
  })

  # ---------------------------------------------------------------------------
  # RÉACTIFS — Données d'exploitation (pipeline IFA)
  #
  # exploit_raw      : lecture + normalisation des colonnes
  # exploit_filtered : filtre nolac + espèce SANA
  # exploit_edited_rv: reactiveVal — permet la correction manuelle de la masse
  # exploit_data     : colonnes calculées (rendement, succès, pression, masse)
  # ---------------------------------------------------------------------------

  exploit_raw <- reactive({
    req(input$exploit_file)
    tryCatch({
      df <- read_excel(input$exploit_file$datapath)
      df <- normaliser_colonnes_ifa(df)

      # Colonnes obligatoires — masse_totale_kg est calculée en aval
      cols_req <- c("annee", "nolac", "espece_code",
                    "nb_captures", "nb_peses", "masse_mesuree_kg", "effort_jp")
      manquantes <- setdiff(cols_req, names(df))
      if (length(manquantes) > 0) {
        showNotification(
          paste0("Colonnes IFA non trouvées : ",
                 paste(manquantes, collapse = ", ")),
          type = "error", duration = 8)
        return(NULL)
      }

      # Colonnes optionnelles conservées si présentes dans le fichier
      cols_opt <- c("territoire", "nom_plan_eau", "type_peche", "type_recolte")
      cols_presentes <- c(cols_req, intersect(cols_opt, names(df)))
      df <- df[, cols_presentes]

      # Forcer le type numérique — read_excel peut lire certaines colonnes
      # comme character si le fichier contient des cellules mixtes ou vides
      cols_num <- c("annee", "nb_captures", "nb_peses", "masse_mesuree_kg", "effort_jp")
      df[cols_num] <- lapply(df[cols_num], function(x) suppressWarnings(as.numeric(x)))

      # Colonnes à agréger selon leur nature :
      #   texte stable (territoire, nom du plan)  → première valeur non-NA
      #   catégorie    (type pêche, type récolte) → valeurs distinctes séparées par " / "
      cols_txt <- intersect(c("territoire", "nom_plan_eau"), names(df))
      cols_cat <- intersect(c("type_peche", "type_recolte"), names(df))

      # Agrégation par nolac × annee × espece_code (fusionne été + hiver, etc.)
      df_agg <- df %>%
        group_by(nolac, annee, espece_code) %>%
        summarise(
          across(all_of(cols_txt), ~ dplyr::first(na.omit(.x))),
          across(all_of(cols_cat), ~ paste(unique(na.omit(.x)), collapse = " / ")),
          nb_captures      = sum(nb_captures,      na.rm = TRUE),
          nb_peses         = sum(nb_peses,          na.rm = TRUE),
          masse_mesuree_kg = sum(masse_mesuree_kg,  na.rm = TRUE),
          effort_jp        = sum(effort_jp,          na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          # Masse moyenne estimée sur l'échantillon pesé, extrapolée à la capture totale
          masse_totale_kg = dplyr::if_else(
            nb_peses > 0,
            (masse_mesuree_kg / nb_peses) * nb_captures,
            NA_real_
          )
        )

      df_agg
    }, error = function(e) {
      showNotification(paste("Erreur de lecture :", e$message),
                       type = "error")
      NULL
    })
  })

  # exploit_filtered : filtre nolac + espèce active — ne lance JAMAIS
  # d'erreur/validate() ici. Retourne NULL (fichier non chargé ou aucun lac
  # sélectionné) ou un data.frame (potentiellement 0 ligne, si le lac n'a
  # aucune donnée pour l'espèce active). C'est essentiel : un validate()/req()
  # qui échoue à l'intérieur ferait taire silencieusement l'observeEvent
  # ci-dessous, qui ne remettrait alors jamais exploit_edited_rv() à jour —
  # les statistiques de l'ancien lac resteraient affichées par erreur.
  exploit_filtered <- reactive({
    if (!isTruthy(input$no_lac)) return(NULL)
    er <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(er)) return(NULL)
    er %>%
      filter(normaliser_nolac(nolac)   == normaliser_nolac(input$no_lac),
             as.character(espece_code) == config()$code_ifa)
  })

  exploit_edited_rv  <- reactiveVal(NULL)   # socle : toutes les lignes présentes

  # Fenêtre temporelle partagée (onglets 1 et 2) : "5" / "10" / "20" — défaut 5 ans
  fenetre_rv <- reactiveVal("5")

  # Helper : ajoute les colonnes calculées à un data.frame d'exploitation
  ajouter_indicateurs <- function(df, sup) {
    # Division sûre : dénominateur 0 ou NA → NA (jamais Inf/NaN, qui cassent les tests stat.)
    # NOTE : ifelse() avec un scalaire bad (length 1) ne retourne que le 1er élément du vecteur
    #        num/den — on force le recyclage via rep() pour couvrir le cas den scalaire (ex. sup).
    div_sure <- function(num, den) {
      bad <- rep(is.na(den) | den == 0, length.out = length(num))
      res <- num / den
      res[bad] <- NA_real_
      res
    }
    df %>%
      arrange(annee) %>%
      mutate(
        rendement_obs = div_sure(masse_totale_kg, sup),
        succes        = div_sure(nb_captures, effort_jp),
        pression      = div_sure(effort_jp, sup),
        masse_moy_g   = div_sure(masse_totale_kg * 1000, nb_captures)
      )
  }

  # Helper : applique la fenêtre (n dernières années calendaires) à un df trié
  # "5 dernières" = années >= (année_max - 4), pas les 5 dernières lignes présentes
  appliquer_fenetre <- function(df, fenetre) {
    if (is.null(fenetre) || fenetre == "all") return(df)
    n <- suppressWarnings(as.integer(fenetre))
    if (is.na(n) || n <= 0) return(df)
    annee_max <- max(df$annee, na.rm = TRUE)
    df[!is.na(df$annee) & df$annee >= (annee_max - n + 1L), ] %>% arrange(annee)
  }

  # exploit_data_kpi : lignes présentes + indicateurs, filtrées par la FENÊTRE
  # (ignore les cases à cocher) — alimente les KPI des onglets 1 et 2.
  exploit_data_kpi <- reactive({
    req(exploit_edited_rv())
    req(!is.na(input$sup) && input$sup > 0)
    df <- ajouter_indicateurs(exploit_edited_rv(), input$sup)
    appliquer_fenetre(df, fenetre_rv())
  })

  # graph_annees_rv : années COCHÉES = tracées dans les graphiques (en direct)
  graph_annees_rv <- reactiveVal(NULL)

  observeEvent(exploit_filtered(), {
    df <- exploit_filtered()
    if (is.null(df) || nrow(df) == 0) {
      # Aucune donnée pour ce lac/espèce — on efface explicitement l'ancien
      # état plutôt que de le laisser tel quel (sinon les stats de l'ancien
      # lac restent affichées dans l'onglet Analyse temporelle).
      exploit_edited_rv(NULL)
      graph_annees_rv(NULL)
    } else {
      exploit_edited_rv(df)
      graph_annees_rv(df$annee)        # toutes cochées par défaut
    }
    fenetre_rv("5")
    updateRadioButtons(session, "tab2_fenetre", selected = "5")
  }, ignoreNULL = FALSE)

  # Mise à jour colonnes éditables — col 0 = checkbox (ignoré), cols 1-4 = données
  observeEvent(input$exploit_table_cell_edit, {
    info   <- input$exploit_table_cell_edit
    df_cur <- exploit_edited_rv()
    req(!is.null(df_cur))
    df_display <- df_cur %>% arrange(annee)
    col_map <- c(NA_character_, "annee", "nb_captures", "effort_jp", "masse_totale_kg")
    if (info$col >= 1 && info$col <= 4) {
      col_name <- col_map[info$col + 1]
      df_display[info$row, col_name] <- suppressWarnings(as.numeric(info$value))
      exploit_edited_rv(df_display)
    }
  })

  # exploit_data_graph : lignes présentes COCHÉES + indicateurs (pour les graphiques)
  exploit_data_graph <- reactive({
    req(exploit_edited_rv())
    req(!is.na(input$sup) && input$sup > 0)
    df <- exploit_edited_rv() %>% arrange(annee)
    coches <- graph_annees_rv()
    if (!is.null(coches) && length(coches) > 0)
      df <- df[df$annee %in% coches, ]
    ajouter_indicateurs(df, input$sup)
  })

  # Alias rétrocompatible : les graphiques consomment exploit_data()
  exploit_data <- reactive({ exploit_data_graph() })

  # ---------------------------------------------------------------------------
  # RÉACTIF — Disponibilité des modèles (généralisé, piloté par le registre)
  #
  #   Un modèle est disponible ssi TOUS les intrants qu'il déclare dans
  #   REGISTRE_ESPECES$<espece>$modeles[[k]]$intrants sont valides. Ajouter un
  #   modèle = l'enregistrer dans config_especes.R avec ses intrants — rien à
  #   modifier ici. Le mapping intrant → règle de validité vit dans
  #   intrant_valide()/intrant_note_manquant(), un seul endroit pour toutes
  #   les espèces et tous les modèles.
  #
  #   Linf et Dth_obs ne sont jamais bloquants : chaque modèle qui les déclare
  #   gère lui-même son repli théorique (calc_lester_touladi, calc_lester_dore)
  #   — jamais de valeur fabriquée silencieusement, mais jamais non plus une
  #   indisponibilité artificielle pour un repli légitime.
  #
  #   NOTE (changement de comportement) : Shuter et IME (Touladi) ne
  #   requièrent plus T_air — leurs formules ne l'utilisent pas ; l'ancienne
  #   version l'exigeait par effet de bord d'un bloc "morpho_ok" partagé.
  # ---------------------------------------------------------------------------
  intrant_valide <- function(nom) {
    switch(nom,
      A       = !is.na(input$sup) && input$sup > 0,
      Dmax    = !is.na(input$prof_max) && input$prof_max > 0,
      Dmn     = !is.na(input$prof_moy) && input$prof_moy > 0 &&
                !is.na(input$prof_max) && input$prof_moy < input$prof_max,
      T_air   = !is.na(t_air_effectif()),
      TDS     = !is.na(input$conductivite) && input$conductivite > 0,
      z_sec   = !is.na(input$secchi) && input$secchi > 0,
      G       = !is.na(g_effectif()) && g_effectif() > 0,
      Linf    = TRUE,
      Dth_obs = TRUE,
      TRUE    # intrant non répertorié : ne bloque pas un modèle par défaut
    )
  }

  intrant_note_manquant <- function(nom) {
    switch(nom,
      A       = "Superficie manquante",
      Dmax    = "Profondeur maximale manquante",
      Dmn     = "Prof. moyenne \u2265 prof. max (ou manquante) \u2014 rejet\u00e9",
      T_air   = "Temp\u00e9rature de l'air moyenne manquante",
      TDS     = "Conductivit\u00e9 manquante (SDT requis)",
      z_sec   = "Profondeur de Secchi manquante",
      G       = "Degr\u00e9s-jours (G) manquants",
      paste0(nom, " manquant")
    )
  }

  model_availability <- reactive({
    cfg <- config()
    setNames(lapply(names(cfg$modeles), function(k) {
      m         <- cfg$modeles[[k]]
      manquants <- Filter(function(nom) !isTRUE(intrant_valide(nom)), m$intrants)
      ok        <- length(manquants) == 0

      note <- if (!ok) {
        paste(vapply(manquants, intrant_note_manquant, character(1)), collapse = " | ")
      } else if ("Linf" %in% m$intrants) {
        # Enrichissement : préciser la source de Linf pour les modèles qui en dépendent
        if (is.na(linf_resolved()$value)) "Linf th\u00e9orique (d\u00e9faut)"
        else paste0("Linf \u2014 ", linf_resolved()$source)
      } else "Pr\u00eat"

      list(ok = ok, note = note)
    }), names(cfg$modeles))
  })

  # ---------------------------------------------------------------------------
  # RÉACTIF — Diagnostic du lac sélectionné
  #   S'ajoute aux avertissements existants (warnings_params_ui, therm/linf
  #   source labels) sans les remplacer — résumé affiché dès la sélection
  #   du lac, avant même de cliquer sur « Calculer ».
  # ---------------------------------------------------------------------------
  diagnostic_lac <- reactive({
    req(isTruthy(input$no_lac))

    items <- list()
    ajouter <- function(niveau, texte) {
      items[[length(items) + 1]] <<- list(niveau = niveau, texte = texte)
    }

    pb        <- tryCatch(pothal_brut(), error = function(e) NULL)
    nolac_val <- cle_lac(input$no_lac)

    if (is.null(pb)) {
      ajouter("warning", "Fichier Potentiel halieutique non chargé — paramètres à saisir manuellement.")
    } else {
      # --- Lacs : UE multiples --------------------------------------------
      if (!is.na(nolac_val) && !is.null(pb$lacs_brutes) &&
          "nolac" %in% names(pb$lacs_brutes) && "ue_min" %in% names(pb$lacs_brutes)) {
        sous <- pb$lacs_brutes[!is.na(pb$lacs_brutes$nolac) & pb$lacs_brutes$nolac == nolac_val, ]
        if (nrow(sous) > 1) {
          suff   <- toupper(trimws(sub(".*-", "", sous$ue_min)))
          retenu <- if ("IPE" %in% suff) "IPE" else if ("OG" %in% suff) "OG" else suff[1]
          ajouter("info", paste0("Plusieurs UE trouvées (", paste(unique(suff), collapse = ", "),
                                 ") — ", retenu, " retenue."))
        }
      }
    }

    # --- Morphométrie du lac (mêmes critères que model_availability) --------
    sup_v  <- input$sup; pmax_v <- input$prof_max; pmoy_v <- input$prof_moy
    if (is.na(sup_v)  || sup_v  <= 0) ajouter("danger", "Superficie manquante ou invalide.")
    if (is.na(pmax_v) || pmax_v <= 0) ajouter("danger", "Prof. max manquante ou invalide.")
    if (is.na(pmoy_v) || pmoy_v <= 0) {
      ajouter("danger", "Prof. moyenne manquante ou invalide.")
    } else if (!is.na(pmax_v) && pmoy_v >= pmax_v) {
      ajouter("danger", "Prof. moyenne \u2265 prof. max \u2014 donnée rejetée, Lester et IME indisponibles.")
    }

    # --- Température de l'air (Touladi seulement — requise pour Lester) -------
    if (identical(espece_active_rv(), "touladi") && is.na(input$T_air))
      ajouter("warning", "Température de l'air moyenne manquante — requise pour le modèle recommandé (Lester).")

    # --- Doré jaune — intrants spécifiques au modèle Lester et coll. 2002 -----
    if (identical(espece_active_rv(), "dore")) {
      if (is.na(input$secchi) || input$secchi <= 0)
        ajouter("warning", "Profondeur de Secchi manquante — modèle Lester 2002 indisponible.")
      g_val <- g_effectif()
      if (is.na(g_val) || g_val <= 0) {
        ajouter("warning", "Degrés-jours (G) manquants — modèle Lester 2002 indisponible.")
      } else if (g_val < 100 || g_val > 3500) {
        # La formule Lester divise G par 1000 en interne (voir results()). Une
        # valeur déjà divisée (ex. 1,825) passe le test « > 0 » et fait
        # s'effondrer le rendement via G^1,30, sans aucun signal visible.
        # Bornes calées sur l'étendue observée de la grille Info-Climat
        # (170 à 2777 sur DJC5_2024), élargies pour ne pas signaler à tort les
        # lacs nordiques : elles servent à attraper l'erreur d'unité
        # (1,825 saisi au lieu de 1825), pas à valider la donnée elle-même.
        ajouter("warning", paste0("Degrés-jours (G) = ", fmt_nb(g_val, 3),
                                  " — hors de la plage attendue (100 à 3500). ",
                                  "Vérifier que la valeur brute a bien été saisie, ",
                                  "non divisée par 1000."))
      }
      if (is.na(t_air_effectif()))
        ajouter("info", paste0("T° air manquante — thermocline théorique indisponible en repli ; ",
                               "lac traité comme non stratifié si aucune profondeur observée. ",
                               "Saisir la valeur, ou importer les rasters TMOY et choisir ",
                               "la source « Climat »."))
      else if (identical(input$t_air_source, "climat"))
        ajouter("info", paste0("T° air extraite des rasters TMOY (",
                               fmt_nb(t_air_effectif(), 2), " °C)."))
      if (identical(input$g_source, "climat") && !is.na(g_effectif()))
        ajouter("info", paste0("Degrés-jours extraits des rasters DJC5 (",
                               fmt_nb(g_effectif(), 1), ")."))
    }

    # --- Omble de fontaine — intrants specifiques Valin/Vaillancourt (aucun ---
    # n'est bloquant : chacun raffine une reduction facultative de la cascade) --
    if (identical(espece_active_rv(), "omble")) {
      if (is.na(input$ph_eau))
        ajouter("info", "pH non saisi (ni disponible dans le fichier Habitat) — réduction « pH < 5 » (Valin/Vaillancourt) non appliquée.")
      o2 <- tryCatch(o2_resolved(), error = function(e) NULL)
      if (is.null(o2) || is.na(o2$value)) {
        ajouter("info", if (identical(input$o2_source, "ifa"))
          paste0("Profil d'oxygène IFA indisponible (", if (!is.null(o2)) o2$source else "aucun fichier",
                 ") — réduction O2 (Valin/Vaillancourt) non appliquée.")
        else
          "Profil d'oxygène non saisi — réduction O2 (Valin/Vaillancourt) non appliquée.")
      }
      if (identical(input$tributaire_emissaire_omble, "inconnu"))
        ajouter("info", "Présence de tributaire/émissaire inconnue — réduction de 25 % (Valin/Vaillancourt) non appliquée.")
      grp <- especes_groupes_omble(input$especes_presentes_omble)
      non_couv_arch <- setdiff(grp$brutes, c(ESPECES_COUVERTES_ARCHAMBAULT, "allopatrie"))
      if (length(non_couv_arch) > 0)
        ajouter("info", paste0("Espèce(s) non couverte(s) par Archambault (",
                               paste(non_couv_arch, collapse = ", "),
                               ") — modèle Archambault indisponible si aucune autre espèce couverte n'est présente."))
    }

    # --- Conductivité ---------------------------------------------------------
    if (is.na(input$conductivite) || input$conductivite <= 0) {
      noms_tds <- vapply(config()$modeles, function(m)
        if ("TDS" %in% m$intrants) m$nom else NA_character_, character(1))
      noms_tds <- noms_tds[!is.na(noms_tds)]
      if (length(noms_tds) > 0)
        ajouter("warning", paste0("Conductivité manquante — modèle(s) ",
                                  paste(noms_tds, collapse = ", "), " indisponible(s)."))
    }

    # --- Thermocline ------------------------------------------------------------
    tr <- tryCatch(therm_resolved(), error = function(e) NULL)
    if (!is.null(tr) && is.na(tr$value) && identical(input$therm_source, "ifa"))
      ajouter("warning", paste0("Thermocline non détectée depuis le profil (", tr$source,
                                ") — repli théorique (Shuter)."))

    # --- Linf / Specimen ------------------------------------------------------
    lr <- tryCatch(linf_resolved(), error = function(e) NULL)
    if (!is.null(lr) && identical(input$linf_source, "file")) {
      if (is.na(lr$value))
        ajouter("warning", paste0("Linf non calculé depuis les spécimens (", lr$source,
                                  ") — repli théorique."))
      else if (!is.null(lr$n) && !is.na(lr$n) && lr$n < 20L)
        ajouter("warning", paste0("Échantillon de spécimens faible (n = ", lr$n, ")."))
    }

    # --- Exploitation (récolte sportive) ---------------------------------------
    er <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(er)) {
      ajouter("warning", "Fichier d'exploitation non chargé.")
    } else {
      ef <- er %>% dplyr::filter(
        normaliser_nolac(nolac)   == normaliser_nolac(input$no_lac),
        as.character(espece_code) == config()$code_ifa
      )
      if (nrow(ef) == 0)
        ajouter("warning", "Aucune donnée de récolte pour ce lac/espèce — graphiques vides.")
    }

    items
  })

  output$diagnostic_lac_ui <- renderUI({
    req(isTruthy(input$no_lac))
    diag <- tryCatch(diagnostic_lac(), error = function(e) NULL)
    if (is.null(diag) || length(diag) == 0) return(NULL)

    icone <- function(niveau) switch(niveau, danger = "\u26d4", warning = "\u26a0", info = "\u2139", "\u2022")
    couleur <- function(niveau) switch(niveau, danger = "#B0453C", warning = "#B97A0B", info = "#5a6b7b", "#5a6b7b")

    div(class = "mt-2 mb-2 p-2",
        style = "background:#FBF9F6; border-radius:5px; border-left:3px solid #B97A0B;",
      tags$div(class = "fw-bold mb-1", style = "font-size:12px; color:#5a6b7b;", "Contrôle qualité des données"),
      lapply(diag, function(it) {
        tags$div(class = "d-flex align-items-start gap-1 mb-1",
          tags$span(style = paste0("color:", couleur(it$niveau), "; font-size:12px;"), icone(it$niveau)),
          tags$small(style = paste0("color:", couleur(it$niveau), ";"), it$texte)
        )
      })
    )
  })

  # Exclusivité mutuelle « Lac en allopatrie » — Omble seulement.
  # Cocher « allopatrie » décoche toute autre espèce, et vice-versa.
  # checkboxGroupInput renvoie les valeurs cochées dans l'ordre des `choices`,
  # jamais dans l'ordre de clic — on compare donc à especes_omble_prev_rv()
  # pour identifier ce qui vient d'être ajouté, plutôt que de se fier à la
  # position dans le vecteur.
  observeEvent(input$especes_presentes_omble, {
    sel  <- input$especes_presentes_omble
    if (is.null(sel)) sel <- character(0)
    prev <- especes_omble_prev_rv()
    ajoute <- setdiff(sel, prev)   # coche(s) qui vienne(nt) d'apparaître

    nouvelle_sel <- sel
    if ("allopatrie" %in% ajoute && length(sel) > 1) {
      # « allopatrie » vient d'être cochée en plus d'autres espèces → décoche le reste
      nouvelle_sel <- "allopatrie"
    } else if (length(ajoute) > 0 && "allopatrie" %in% sel && !("allopatrie" %in% ajoute)) {
      # une autre espèce vient d'être cochée alors qu'« allopatrie » l'était déjà → la retirer
      nouvelle_sel <- setdiff(sel, "allopatrie")
    }

    especes_omble_prev_rv(nouvelle_sel)
    if (!setequal(nouvelle_sel, sel))
      updateCheckboxGroupInput(session, "especes_presentes_omble", selected = nouvelle_sel)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ---------------------------------------------------------------------------
  # RÉACTIF — Résultats (déclenchés au clic)
  # ---------------------------------------------------------------------------
  # Calcul lancé : marque les résultats comme valides si un modèle a tourné
  observeEvent(input$btn_calc, {
    av    <- model_availability()
    dispo <- any(vapply(config()$cascade_reference,
                        function(k) isTRUE(av[[k]]$ok), logical(1)))
    if (dispo) {
      a_calcule_rv(TRUE)
      calc_valide_rv(TRUE)
    }
  })

  # Tout changement de paramètre rend le calcul affiché périmé
  observeEvent(
    list(input$sup, input$prof_max, input$prof_moy, input$T_air,
         input$degres_jours_g, input$g_source, input$t_air_source,
         input$climat_fenetre, input$climat_files_tmoy, input$climat_files_djc5,
         input$conductivite, input$linf_manual, input$linf_source,
         input$therm_source, input$ifa_inv_choisi,
         input$especes_presentes_omble, input$ph_eau, input$perimetre,
         input$o2_source, input$o2_metres_sous_5ppm,
         input$tributaire_emissaire_omble, input$nb_chalets_omble),
    { if (isTRUE(calc_valide_rv())) calc_valide_rv(FALSE) },
    ignoreInit = TRUE
  )

  # Libellé du bouton selon l'état
  observeEvent(calc_valide_rv(), {
    if (isTRUE(calc_valide_rv()))
      updateActionButton(session, "btn_calc", label = "Calculer",
                         icon = icon("calculator"))
    else if (isTRUE(a_calcule_rv()))
      updateActionButton(session, "btn_calc", label = "Recalculer",
                         icon = icon("calculator"))
  })

  # Note de péremption au-dessus du bouton
  output$calc_stale_note <- renderUI({
    if (isTRUE(a_calcule_rv()) && !isTRUE(calc_valide_rv()))
      div(class = "small fw-bold mt-2", style = "color:#B97A0B; line-height:1.2;",
          "\u26a0 Paramètres modifiés — recalculer")
    else NULL
  })

  results <- eventReactive(input$btn_calc, {
    warnings_actifs_rv(TRUE)
    avail <- model_availability()
    validate(need(any(vapply(avail, function(a) isTRUE(a$ok), logical(1))),
                  "Aucun modèle ne peut tourner — vérifiez les paramètres du lac."))
    sup      <- input$sup
    prof_max <- input$prof_max
    # Aucune fabrication ici : si prof_moy est invalide (NA ou >= prof_max),
    # model_availability() a déjà mis les modèles concernés à FALSE —
    # appliquer() ne les appellera donc pas. On garde la valeur brute
    # (possiblement invalide) uniquement pour affichage informatif au tableau.
    prof_moy <- input$prof_moy

    # nz() : normalise un input$ potentiellement NULL (champ jamais rendu --
    # UI specifique a une autre espece que celle active, ex. secchi/G pour le
    # Dore, ph_eau/nb_chalets pour l'Omble) en NA du bon type. Le bassin est
    # partage par toutes les especes et construit sans condition : sans cette
    # normalisation, un NULL non attrape peut faire planter le calcul de
    # N'IMPORTE QUELLE espece tant que l'onglet propriétaire du champ n'a
    # jamais ete visite dans la session (bug corrige 2026-07 -- voir
    # o2_resolved()).
    nz <- function(x, defaut = NA_real_) if (is.null(x)) defaut else x

    # Bassin commun — le registre pioche ce dont chaque modèle a besoin
    bassin <- list(
      A       = sup, Dmax = prof_max, Dmn = prof_moy, T_air = t_air_effectif(),
      Linf    = resolve_linf(linf_resolved()$value, sup)$value,
      Dth_obs = therm_resolved()$value,
      TDS     = conductivite_vers_tds(input$conductivite),
      z_sec   = nz(input$secchi),
      # G du modèle Lester = degrés-jours (base 5 °C) / 1000 — le champ
      # sidebar demande la valeur BRUTE (ex. 1825), la conversion se fait ici.
      G       = nz(g_effectif()) / 1000,

      # --- Omble de fontaine — Vézina/Archambault/Valin/Vaillancourt -------
      # grp : classification des espèces présentes en groupes (cyprins,
      # catostomes, perchaude, piscivores, etc.) — voir especes_groupes_omble().
      # Toujours calculé, même hors onglet Omble (ignoré par les autres
      # espèces, aucun de leurs modèles ne déclare "grp" dans ses intrants).
      grp                = especes_groupes_omble(input$especes_presentes_omble),
      pH                 = nz(input$ph_eau),
      o2_pct_reduction   = calc_reduction_o2_valin(prof_max, o2_resolved()$value),
      tributaire_absent  = switch(
                             if (is.null(input$tributaire_emissaire_omble)) "inconnu" else input$tributaire_emissaire_omble,
                             absent = TRUE, present = FALSE, NA),
      nb_chalets         = nz(input$nb_chalets_omble),
      # Superficie dupliquee sous un autre nom pour Valin/Vaillancourt : sert
      # uniquement au raffinement optionnel "chalets/10 ha", ne doit donc PAS
      # etre un intrant bloquant comme "A" (Archambault) -- voir intrant_valide().
      A_optionnel        = sup
    )

    # Dispatch générique : TOUS les modèles déclarés par l'espèce active sont
    # appelés et rangés sous leur propre clé (r$lester_savi, r$valin, r$ime,
    # ...). Ajouter un modèle au registre (config_especes.R) suffit — rien à
    # modifier ici.
    appliquer <- function(cle) {
      m <- config()$modeles[[cle]]
      if (is.null(m))               return(NULL)   # espèce ne déclare pas ce modèle
      if (!isTRUE(avail[[cle]]$ok)) return(NULL)   # intrants insuffisants
      args <- bassin[m$intrants]
      if (!is.null(m$partition)) args$partition <- m$partition
      tryCatch(do.call(m$fn, args), error = function(e) NULL)
    }
    res_modeles <- setNames(lapply(names(config()$modeles), appliquer),
                            names(config()$modeles))

    # Source et valeur de Linf utilisées (pour affichage dans le tableau)
    lf_info     <- resolve_linf(linf_resolved()$value, sup)
    linf_source <- lf_info$source
    linf_value  <- round(lf_info$value, 1)

    c(list(
      nom_lac     = if (nchar(trimws(input$nom_lac)) > 0) input$nom_lac else "Lac non nommé",
      sup         = sup,
      linf_source = linf_source,
      linf_value  = linf_value,
      prof_max    = prof_max,
      prof_moy    = prof_moy,
      T_air       = t_air_effectif(),
      tds         = if (!is.na(input$conductivite) && input$conductivite > 0)
                      round(conductivite_vers_tds(input$conductivite), 1) else NA_real_
    ), res_modeles, list(avail = avail))
  })

  # ---------------------------------------------------------------------------
  # OUTPUTS — Panneau gauche
  # ---------------------------------------------------------------------------
  # --- Interface des données climatiques ------------------------------------
  # Ligne de provenance affichée sous un champ en mode « Climat » : la valeur,
  # et les années réellement utilisées (jamais le nombre demandé, qui peut
  # dépasser le nombre de rasters importés).
  ligne_climat_ui <- function(res, unite, dec) {
    if (is.na(res$valeur))
      return(tags$small(class = "text-muted d-block mt-1",
                        paste0("Indisponible — ", res$motif)))
    lib <- if (length(res$annees) == 1)
      paste0("année ", res$annees)
    else
      paste0("moy. ", min(res$annees), "\u2013", max(res$annees),
             " (", length(res$annees), " ans)")
    tagList(
      tags$small(class = "d-block mt-1 fw-semibold",
                 paste0(fmt_nb(res$valeur, dec), " ", unite)),
      tags$small(class = "text-muted d-block", lib),
      if (!is.na(res$motif))
        tags$small(class = "d-block", style = "color:#B97A0B;", res$motif) else NULL
    )
  }

  output$t_air_climat_ui <- renderUI({
    if (!identical(input$t_air_source, "climat")) return(NULL)
    ligne_climat_ui(climat_tmoy(), "°C", 2)
  })
  output$g_climat_ui <- renderUI({
    if (!identical(input$g_source, "climat")) return(NULL)
    ligne_climat_ui(climat_djc5(), "degrés-jours", 1)
  })

  # Icône d'état du bloc d'importation : détail des variables et années au survol
  output$climat_hint <- renderUI({
    cat_res <- catalogue_climat()
    if (is.null(cat_res$catalogue)) return(NULL)
    cd  <- cat_res$catalogue
    det <- paste(vapply(unique(cd$variable), function(v) {
      a <- sort(cd$annee[cd$variable == v])
      paste0(v, " : ", min(a), "\u2013", max(a), " (", length(a), ")")
    }, character(1)), collapse = " | ")
    bslib::tooltip(
      tags$span(class = "text-success fw-semibold", style = "font-size:14px;", "\u2713"),
      det, placement = "right"
    )
  })

  # Récapitulatif du catalogue : une ligne par variable, avec la plage d'années
  # et l'origine (répertoire du dépôt ou téléversement).
  output$climat_files_name <- renderUI({
    cat_res <- catalogue_climat()
    cd      <- cat_res$catalogue

    lignes <- if (is.null(cd)) NULL else lapply(sort(unique(cd$variable)), function(v) {
      sub <- cd[cd$variable == v, ]
      a   <- sort(sub$annee)
      org <- if (all(sub$origine == "dépôt")) "répertoire"
             else if (all(sub$origine == "téléversé")) "téléversés"
             else "répertoire + téléversés"
      div(class = "file-name-line",
          span(class = "fn-ic", "\u2713"),
          span(paste0(v, " : ", min(a), "\u2013", max(a),
                      " (", length(a), " an", if (length(a) > 1) "s" else "", ") \u00b7 ", org)))
    })

    manquantes <- setdiff(c("DJC5", "TMOY"), if (is.null(cd)) character(0) else unique(cd$variable))

    tagList(
      lignes,
      if (length(manquantes) > 0)
        div(class = "file-name-line", style = "color:#B97A0B;",
            span(class = "fn-ic", style = "color:#B97A0B;", "\u26a0"),
            span(paste0("Aucun raster ", paste(manquantes, collapse = " ni "), ".")))
      else NULL,
      if (length(cat_res$ignores) > 0)
        div(class = "file-name-line", style = "color:#B97A0B;",
            span(class = "fn-ic", style = "color:#B97A0B;", "\u26a0"),
            span(paste0("Nom non conforme, ignoré : ",
                        paste(cat_res$ignores, collapse = ", "))))
      else NULL,
      if (!is.null(cd) && !terra_disponible())
        div(class = "file-name-line", style = "color:#A32D2D;",
            span(class = "fn-ic", style = "color:#A32D2D;", "\u26a0"),
            span("Paquet terra non installé — extraction impossible."))
      else NULL
    )
  })

  # Fenêtre de moyenne : n'apparaît qu'une fois des rasters reconnus.
  # Une année isolée porte la variabilité interannuelle ; les moyennes sur 5 ou
  # 10 ans s'approchent davantage des conditions climatiques moyennes que
  # supposent les modèles de rendement.
  output$climat_fenetre_ui <- renderUI({
    if (is.null(catalogue_climat()$catalogue)) return(NULL)
    div(class = "cond-block cond-narrow mt-1",
      div(class = "cond-bloc-titre",
        info_tip("Fenêtre climatique",
                 paste0("Nombre d'années utilisées pour la moyenne, à partir de la plus ",
                        "récente disponible. Une année isolée porte la variabilité ",
                        "interannuelle ; une moyenne sur 5 ou 10 ans s'approche davantage ",
                        "des conditions moyennes supposées par les modèles. Si moins ",
                        "d'années sont importées que demandé, les années présentes sont ",
                        "utilisées et le nombre réel est affiché sous le champ."))
      ),
      radioButtons("climat_fenetre", NULL, choices = CLIMAT_FENETRES,
                   selected = "1", inline = FALSE)
    )
  })

  # Nom complet du fichier importé, affiché sous le bouton et libre de passer
  # à la ligne — le champ natif du fileInput est masqué (voir CSS).
  nom_fichier_ui <- function(f) {
    if (is.null(f) || is.null(f$name) || !nzchar(f$name)) return(NULL)
    div(class = "file-name-line",
        span(class = "fn-ic", "\u2713"), span(f$name))
  }
  output$pothal_file_name  <- renderUI({ nom_fichier_ui(input$pothal_file) })
  output$exploit_file_name <- renderUI({ nom_fichier_ui(input$exploit_file) })

  output$ifa_habitat_hint <- renderUI({
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa)) return(NULL)
    ok <- !is.null(ifa$inventaires)
    if (ok) {
      # Icône seule + détail au survol (aucun texte permanent)
      bslib::tooltip(
        tags$span(class = "text-success fw-semibold", style = "font-size:14px;", "✓"),
        ifa$note, placement = "right"
      )
    } else {
      bslib::tooltip(
        tags$span(class = "text-muted", style = "font-size:12px;", "⚠"),
        ifa$note, placement = "right"
      )
    }
  })

  output$ifa_inv_ui <- renderUI({
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires)) return(NULL)
    invs <- ifa$inventaires
    if (nrow(invs) <= 1) return(NULL)   # un seul inventaire → pas besoin de sélecteur

    # Construire les étiquettes : date + nb points + indicateur validité
    choix <- setNames(
      invs$inv_key,
      paste0(invs$date_label,
             ifelse(invs$profil_valide,
                    paste0("  (", invs$n_pts, " pts ✓)"),
                    paste0("  (", invs$n_pts, " pt(s) — sans profil)")))
    )

    selectInput("ifa_inv_choisi", label = NULL,
                choices  = choix,
                selected = ifa$inv_defaut,
                width    = "100%")
  })

  # Linf calculée (mode « Calculer ») : valeur utilisée affichée directement,
  # sans le détail technique (méthode/n en petit, avertissement seulement si pertinent)
  output$linf_result_ui <- renderUI({
    lr <- tryCatch(linf_resolved(), error = function(e) NULL)
    if (is.null(lr)) return(NULL)
    if (!is.na(lr$value)) {
      detail <- if (!is.na(lr$n))
        tags$small(class = "text-muted d-block", paste0("n = ", lr$n, ", méthode Janošík"))
      else NULL
      tagList(
        tags$div(style = "font-size:14px; color:#2C3E50;",
                 paste0("Linf utilisée : ", lr$value, " mm")),
        detail,
        if (nchar(lr$note) > 0)
          tags$small(class = "text-warning fw-semibold d-block mt-1", lr$note)
        else NULL
      )
    } else {
      tagList(
        tags$small(class = "text-warning fst-italic d-block", lr$source),
        if (nchar(lr$note) > 0)
          tags$small(class = "text-warning fw-semibold d-block mt-1", lr$note)
        else NULL
      )
    }
  })

  # Thermocline résolue : profondeur simple + source courte (sans z_hypo/thermo)
  output$therm_result_ui <- renderUI({
    tr <- tryCatch(therm_resolved(), error = function(e) NULL)
    if (is.null(tr)) return(NULL)
    if (!is.na(tr$value)) {
      tagList(
        tags$div(style = "font-size:14px; color:#2C3E50;",
                 paste0("Profondeur thermocline : ", tr$value, " m")),
        if (nchar(tr$note) > 0)
          tags$small(class = "text-warning fw-semibold d-block mt-1", tr$note)
        else NULL
      )
    } else {
      tagList(
        tags$small(class = "text-warning fst-italic d-block", tr$source),
        if (nchar(tr$note) > 0)
          tags$small(class = "text-warning fw-semibold d-block mt-1", tr$note)
        else NULL,
        if (!is.null(tr$theorique) && !is.na(tr$theorique))
          tags$small(class = "text-muted d-block mt-1",
                     paste0("Profondeur théorique utilisée (Shuter) : ", tr$theorique, " m"))
        else NULL
      )
    }
  })

  output$o2_result_ui <- renderUI({
    req(identical(espece_active_rv(), "omble"))
    o2 <- tryCatch(o2_resolved(), error = function(e) NULL)
    if (is.null(o2)) return(NULL)
    if (identical(input$o2_source, "manual")) return(NULL)   # rien a ajouter, le champ manuel suffit
    if (!is.na(o2$value)) {
      tagList(
        tags$div(style = "font-size:14px; color:#2C3E50;",
                 paste0("Mètres sous 5 ppm : ", o2$value, " m")),
        tags$small(class = "text-muted d-block", o2$source)
      )
    } else {
      tagList(
        tags$small(class = "text-warning fst-italic d-block", o2$source),
        if (nchar(o2$note) > 0)
          tags$small(class = "text-warning fw-semibold d-block mt-1", o2$note)
        else NULL
      )
    }
  })

  # Badge RDR dynamique selon le % sélectionné


  output$tds_hint <- renderUI({
    req(!is.na(input$conductivite) && input$conductivite > 0)
    tds <- round(conductivite_vers_tds(input$conductivite), 1)
    tags$small(class = "text-muted fst-italic mt-1 d-block",
               paste0("≈ ", tds, " mg/L (SDT estimé)"))
  })

  # Sélecteur de modèle régional pour les graphiques (un seul à la fois)

  observeEvent(exploit_raw(), {
    warnings_actifs_rv(TRUE)
  }, ignoreInit = TRUE)

  output$exploit_hint <- renderUI({
    charge <- !is.null(exploit_edited_rv()) && !is.null(input$no_lac) &&
              nchar(trimws(input$no_lac)) > 0
    if (!charge) return(NULL)
    df <- exploit_edited_rv()
    n  <- nrow(df)
    an <- range(df$annee, na.rm = TRUE)
    detail <- paste0(n, " année", if (n > 1) "s" else "",
                     " (", an[1], "\u2013", an[2], ")")
    # Icône seule + détail au survol (aucun texte permanent)
    bslib::tooltip(
      tags$span(class = "text-success fw-semibold", style = "font-size:14px;", "✓"),
      detail, placement = "right"
    )
  })

  # ---------------------------------------------------------------------------
  # OUTPUTS — Onglet 1 : Rendements théoriques
  # ---------------------------------------------------------------------------

  # Rangée 1 : 3 cartes KPI — Maximal théorique -> Recommandé (80 %) -> Observé
  # Fil d'Ariane : espèce active + lac sélectionné
  output$fil_ariane <- renderUI({
    p   <- config()$palette
    esp <- config()$nom
    lac_txt <- if (isTruthy(input$no_lac)) {
      if (isTruthy(input$nom_lac)) paste0(input$no_lac, " — ", input$nom_lac)
      else input$no_lac
    } else NULL
    div(
      style = paste0("display:flex; align-items:center; gap:8px; font-size:13px; ",
                     "margin-bottom:12px; padding:6px 12px; background:#FFFFFF; ",
                     "border-radius:5px; border-left:4px solid ", p$accent, ";"),
      span(style = paste0("display:inline-block; width:9px; height:9px; ",
                          "border-radius:50%; background:", p$accent, ";")),
      tags$strong(esp),
      if (!is.null(lac_txt)) span(style = "color:#5a6b7b;", paste0("\u2022  ", lac_txt))
      else span(style = "color:#9aa7b2;", "\u2022  aucun lac sélectionné")
    )
  })

  # Quota actuel saisi — dépend de l'espèce active (champ propre à chaque bloc)
  quota_actuel_val <- reactive({
    val <- switch(espece_active_rv(),
      touladi = input$quota_actuel_touladi,
      dore    = input$quota_actuel_dore,
      omble   = input$quota_actuel_omble,
      NA_real_
    )
    if (is.null(val) || is.na(val) || val <= 0) NA_real_ else val
  })

  # ZONE 1 — Décision, en kg/an. Les deux seules valeurs de cette unité sont
  # ici, côte à côte : le quota estimé et le quota en vigueur se comparent
  # directement, sans changement d'unité entre les deux. Le taux d'exploitation
  # est intégré à la ligne de dérivation plutôt que présenté comme un réglage
  # séparé — c'est un terme de l'équation, pas une préférence d'affichage.
  output$kpi_top_ui <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))

    tip_quota_est <- paste0("Quota annuel obtenu en appliquant le taux d'exploitation au ",
                            "rendement maximal théorique du modèle de référence, multiplié ",
                            "par la superficie du lac. Le seuil recommandé de ",
                            PCT_RECOMMANDE, " % suit le concept de « pretty good yield » ",
                            "(Hilborn 2010) : pêcher à ~80 % du maximum conserve l'essentiel ",
                            "du rendement tout en réduisant nettement le risque de ",
                            "surexploitation.")
    tip_quota_act <- paste0("Quota en vigueur pour ce lac et cette espèce, saisi dans les ",
                            "données spécifiques. L'écart le compare au quota estimé.")

    ref_m <- NULL; max_ha <- NA_real_; sup_v <- NA_real_; quota_est <- NA_real_
    if (calc_fait) {
      r     <- results()
      ref_m <- modele_reference(r, config()$cascade_reference, config()$modeles)
      sup_v <- r$sup
      if (!is.null(ref_m) && !is.na(ref_m$val)) {
        max_ha <- ref_m$val
        if (!is.na(sup_v) && sup_v > 0)
          quota_est <- round(max_ha * pct_rv() / 100 * sup_v)
      }
    }

    # Couleur de la bordure : même code de zone que la barre de positionnement
    pct      <- pct_rv()
    zone_pct <- if (pct > 90) ZONES$eleve else if (pct >= PCT_RECOMMANDE) ZONES$recommande
                else ZONES$conservateur
    col_sel  <- if (calc_fait) zone_pct$trait else "#ADB3BA"

    # Ligne de dérivation : « 80 % de X kg/ha × Y ha ». Le champ de saisie y est
    # inséré tel quel, à côté du chiffre qu'il pilote.
    champ_pct <- numericInput("pct_manual", label = NULL, value = pct,
                              min = 1, max = 200, step = 1, width = "72px")
    ligne_derivation <- div(class = "quota-derivation",
      champ_pct,
      span(class = "text-muted", "%"),
      if (calc_fait && !is.na(max_ha))
        span(paste0(" de ", fmt_nb(max_ha), " kg/ha"))
      else
        span(class = "text-muted", " du maximum théorique"),
      if (calc_fait && !is.na(sup_v) && sup_v > 0)
        tagList(span(class = "quota-derivation-op", "\u00d7"),
                span(paste0(fmt_nb(sup_v, 0), " ha")))
      else NULL
    )

    ligne_modele <- if (calc_fait && !is.null(ref_m))
      tags$small(class = "kpi-sub mt-1 d-block",
                 paste0("Modèle de référence : ", ref_m$nom))
    else
      tags$small(class = "kpi-sub mt-1 d-block", "Calculer pour estimer le quota")

    note_ime_ref <- if (calc_fait && !is.null(ref_m) && identical(ref_m$cle, "ime"))
      tags$small(class = "kpi-sub d-block fst-italic",
                 "IME : rendement communautaire partitionné — plus incertain que Lester.")
    else NULL

    carte_est <- div(class = "kpi-top-card",
      style = paste0("border-left:5px solid ", col_sel, ";"),
      div(class = "kpi-eyebrow", style = paste0("color:", col_sel, ";"),
          info_tip("Quota estimé", tip_quota_est)),
      div(span(class = "quota-hero-val", if (is.na(quota_est)) "—" else fmt_int(quota_est)),
          span(class = "quota-hero-unit", "kg/an")),
      ligne_derivation,
      ligne_modele,
      note_ime_ref
    )

    # --- Quota actuel : même unité, même taille, écart avec l'estimation ------
    quota_val <- quota_actuel_val()
    carte_act <- if (is.na(quota_val)) {
      div(class = "kpi-top-card",
        div(class = "kpi-eyebrow", info_tip("Quota actuel", tip_quota_act)),
        div(span(class = "quota-hero-val", style = "color:#adb5bd;", "—"),
            span(class = "quota-hero-unit", "kg/an")),
        tags$small(class = "kpi-sub mt-2 d-block", "Non saisi"))
    } else {
      ecart <- if (!is.na(quota_est)) quota_val - quota_est else NA_real_
      note_ecart <- if (is.na(ecart)) {
        tags$small(class = "kpi-sub mt-2 d-block", "Calculer pour comparer")
      } else if (abs(ecart) < 1) {
        tags$small(class = "kpi-sub mt-2 d-block", "Équivalent à l'estimation")
      } else {
        # Au-dessus de l'estimation = signal d'attention (orange) ; en dessous =
        # neutre (gris). Mêmes couleurs que les zones de la barre, pas de vert :
        # être sous l'estimation n'est pas un « bon » résultat en soi.
        col <- if (ecart > 0) ZONES$eleve$trait else ZONES$conservateur$trait
        tags$small(class = "kpi-sub mt-2 d-block fw-bold",
                   style = paste0("color:", col, ";"),
                   paste0(fmt_int(abs(ecart)), " kg/an ",
                          if (ecart > 0) "au-dessus de" else "sous", " l'estimation"))
      }
      div(class = "kpi-top-card",
        div(class = "kpi-eyebrow", info_tip("Quota actuel", tip_quota_act)),
        div(span(class = "quota-hero-val", fmt_int(quota_val)),
            span(class = "quota-hero-unit", "kg/an")),
        note_ecart)
    }

    div(class = "kpi-top-row", carte_est, carte_act)
  })

  # Valeurs dynamiques dans la card pct (rangée 2)


  # Tableau collapsible — état (ouvert par défaut)
  detail_collapsed_rv     <- reactiveVal(FALSE)
  tab2_table_collapsed_rv <- reactiveVal(FALSE)

  observeEvent(input$btn_toggle_tab2_table, {
    tab2_table_collapsed_rv(!tab2_table_collapsed_rv())
    label <- if (tab2_table_collapsed_rv()) "▸ Afficher" else "▾ Masquer"
    updateActionButton(session, "btn_toggle_tab2_table", label = label)
  })

  observeEvent(input$btn_toggle_detail, {
    detail_collapsed_rv(!detail_collapsed_rv())
    label <- if (detail_collapsed_rv()) "▸ Afficher" else "▾ Masquer"
    updateActionButton(session, "btn_toggle_detail", label = label)
  })

  output$detail_table_container <- renderUI({
    if (detail_collapsed_rv()) return(NULL)
    tagList(
      DT::dataTableOutput("table_modeles"),
      uiOutput("table_note")
    )
  })

  output$gauge_rdr_ui <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))
    if (!calc_fait) return(NULL)

    r <- results()

    # Modèle de référence : premier modèle applicable de la cascade de l'espèce
    ref_m   <- modele_reference(r, config()$cascade_reference, config()$modeles)
    val_ref <- if (!is.null(ref_m)) ref_m$val else NA_real_
    req(!is.na(val_ref) && is.finite(val_ref) && val_ref > 0)
    sup <- r$sup

    # Nom court du repère sur la barre (l'espace y est limité) : premier mot du
    # nom du registre — "Lester et coll. 2021" → "Lester", "IME / Ryder" → "IME",
    # "Archambault 1988/2009" → "Archambault". Générique : aucune clé d'espèce
    # codée en dur, contrairement à l'ancienne table (cf. modele_reference()).
    lbl_ref_court <- if (!is.null(ref_m) && !is.na(ref_m$nom) && nzchar(ref_m$nom))
      sub("[[:space:]/].*$", "", ref_m$nom) else "Réf."

    pct      <- pct_rv()
    val_sel  <- val_ref * pct / 100
    quota_an <- round(val_sel * sup)

    zone_pct <- if (pct > 90) ZONES$eleve else if (pct >= PCT_RECOMMANDE) ZONES$recommande else ZONES$conservateur
    col_sel  <- zone_pct$trait

    # Observé (moyenne sur la fenêtre partagée — cohérent avec le KPI)
    df_exp <- tryCatch(exploit_data_kpi(), error = function(e) NULL)
    val_obs <- if (!is.null(df_exp) && nrow(df_exp) > 0)
      round(mean(df_exp$rendement_obs, na.rm = TRUE), 2) else NA_real_

    # Modèles superposés (cochés dans le tableau) — tout modèle du registre de
    # l'espèce active (ex. Shuter, IME, Valin...) ou toute région active,
    # générique : aucune clé d'espèce codée en dur.
    pal_ovl  <- c("#2C3E50", "#6A5ACD", "#A0522D", "#1F7A6B", "#9370DB", "#CD853F")
    sel_ovl  <- input$overlay_models
    regs_act <- regions_actives()
    reg_lookup  <- setNames(regs_act, vapply(regs_act, `[[`, character(1), "key"))
    modeles_cfg <- config()$modeles
    ovl_noms <- character(0); ovl_vals <- numeric(0)
    if (!is.null(sel_ovl) && length(sel_ovl) > 0) {
      for (k in sel_ovl) {
        if (k %in% names(reg_lookup) && !is.na(reg_lookup[[k]]$val)) {
          ovl_noms <- c(ovl_noms, reg_lookup[[k]]$nom)
          ovl_vals <- c(ovl_vals, reg_lookup[[k]]$val)
        } else if (!is.null(modeles_cfg[[k]]) && !is.null(r[[k]]) && !is.na(r[[k]]$rendement_ha)) {
          ovl_noms <- c(ovl_noms, modeles_cfg[[k]]$nom)
          ovl_vals <- c(ovl_vals, r[[k]]$rendement_ha)
        }
      }
    }

    # Échelle dynamique : bornée aux valeurs réellement affichées (Lester,
    # sélection, observé, modèles cochés) + marge de chaque côté, pour que les
    # repères ne soient jamais collés aux extrémités de la barre.
    vals_echelle <- c(val_ref, val_sel,
                      if (!is.na(val_obs)) val_obs else NULL,
                      if (length(ovl_vals) > 0) ovl_vals else NULL)
    v_min <- min(vals_echelle, na.rm = TRUE)
    v_max <- max(vals_echelle, na.rm = TRUE)
    etendue <- v_max - v_min
    if (!is.finite(etendue) || etendue <= 0) etendue <- v_max
    marge     <- max(etendue * 0.15, v_max * 0.05, 1e-6)
    scale_min <- max(0, v_min - marge)
    scale_max <- v_max + marge

    pos_frac <- function(v) (pmin(pmax(v, scale_min), scale_max) - scale_min) / (scale_max - scale_min)
    pos <- function(v) paste0(round(pos_frac(v) * 100, 2), "%")

    b1 <- val_ref * (PCT_RECOMMANDE / 100)          # 80 % du Lester
    b2 <- val_ref * ((PCT_RECOMMANDE + 10) / 100)   # 90 % du Lester
    w1 <- round(pos_frac(b1) * 100, 3)                    # gris  : scale_min -> 80 %
    w2 <- round(pos_frac(b2) * 100 - w1, 3)                # vert  : 80 % -> 90 %
    w3 <- round(100 - w1 - w2, 3)                          # orange: 90 % -> scale_max

    bar_h   <- 30L
    pad_top <- 30L   # espace réservé aux étiquettes de repères (Lester + modèles superposés)
    pad_bot <- 26L

    # Légende des trois zones (bornes exprimées en % du maximum théorique)
    legende <- div(class = "fb-legend",
      div(class = "fb-leg-item",
        div(class = "fb-leg-sw", style = paste0("background:", ZONES$conservateur$fill, ";")),
        info_tip("Conservateur", paste0("Moins de ", PCT_RECOMMANDE, " % du maximum théorique."))),
      div(class = "fb-leg-item",
        div(class = "fb-leg-sw", style = paste0("background:", ZONES$recommande$fill, ";")),
        info_tip("Recommandé", paste0(PCT_RECOMMANDE, " à 90 % du maximum théorique ",
                                      "(« pretty good yield », Hilborn 2010)."))),
      div(class = "fb-leg-item",
        div(class = "fb-leg-sw", style = paste0("background:", ZONES$eleve$fill, ";")),
        info_tip("Élevé", "Plus de 90 % du maximum — risque accru de surexploitation."))
    )

    # Sélecteur de fenêtre temporelle : il pilote la moyenne du rendement
    # observé, qui est une valeur en kg/ha — il appartient donc à cette zone,
    # à côté du repère qu'il déplace, et non à la zone des quotas.
    sel_fen  <- fenetre_rv()
    toggle_n <- div(style = "font-size:10px; line-height:1;",
      radioButtons("kpi_obs_n", label = NULL,
        choices  = c("5 ans" = "5", "10 ans" = "10", "20 ans" = "20"),
        selected = if (!is.null(sel_fen)) sel_fen else "5", inline = TRUE))

    # Ligne de synthèse de l'observé : la valeur, sa fenêtre et sa position
    # relative. Remplace l'ancienne carte KPI, qui répétait à distance le
    # repère « Obs. » déjà présent sur la barre.
    obs_pct  <- if (!is.na(val_obs) && val_ref > 0) val_obs / val_ref * 100 else NA_real_
    lib_fen  <- switch(if (!is.null(sel_fen)) sel_fen else "5",
                       "5" = "5 dern. ans", "10" = "10 dern. ans", "20" = "20 dern. ans")
    # Le rendement observé est la seule donnée empirique de l'écran : il est
    # traité comme une valeur à part entière (chiffre de 26 px), et non comme
    # une légende de la barre.
    ligne_obs <- if (is.na(val_obs)) {
      # Message distinct selon la cause : fichier absent / lac absent du fichier
      # -> "exploitation" ; lac présent mais aucune masse exploitable -> "rendement"
      base_exp_df <- tryCatch(exploit_edited_rv(), error = function(e) NULL)
      div(
        div(class = "kpi-eyebrow", "Rendement observé"),
        div(span(class = "obs-val", style = "color:#adb5bd;", "\u2014"),
            span(class = "obs-unit", "kg/ha")),
        tags$small(class = "kpi-sub d-block",
          if (!isTruthy(input$exploit_file) || is.null(base_exp_df) || nrow(base_exp_df) == 0)
            "Aucune donnée d'exploitation pour ce lac"
          else "Aucune donnée de rendement observé")
      )
    } else {
      df_obs  <- tryCatch(exploit_data_kpi(), error = function(e) NULL)
      periode <- if (!is.null(df_obs) && nrow(df_obs) > 0)
        paste0(" (", min(df_obs$annee), "\u2013", max(df_obs$annee), ")") else ""
      div(
        div(class = "kpi-eyebrow",
            info_tip("Rendement observé",
                     paste0("Rendement moyen réellement observé à la pêche sur la fenêtre ",
                            "choisie (kg/ha/an), à comparer au maximum théorique."))),
        div(span(class = "obs-val", fmt_nb(val_obs)), span(class = "obs-unit", "kg/ha")),
        tags$small(class = "kpi-sub d-block",
                   paste0("Moy. ", lib_fen, periode,
                          if (!is.na(obs_pct))
                            paste0(" \u00b7 ", fmt_int(obs_pct), " % du maximum") else ""))
      )
    }

    div(class = "mb-3",
      div(style = paste0("background:white; border:1px solid #dee2e6; ",
                         "padding:14px 16px; box-shadow:0 1px 4px rgba(0,0,0,0.04);"),

        div(style = "display:flex; justify-content:space-between; align-items:center; gap:12px;",
          tags$strong(style = "font-size:12.5px;",
                      "Positionnement du rendement (kg/ha)"),
          toggle_n
        ),

      div(class = "fb-wrap",
        div(class = "fb-bararea",
          div(style = paste0("position:relative; padding:", pad_top, "px 0 ", pad_bot, "px 0;"),
            div(style = paste0("display:flex; height:", bar_h, "px; border-radius:6px; overflow:hidden;"),
              div(style = paste0("width:", w1, "%; background:", ZONES$conservateur$fill, ";")),
              div(style = paste0("width:", w2, "%; background:", ZONES$recommande$fill, ";")),
              div(style = paste0("width:", w3, "%; background:", ZONES$eleve$fill, ";"))
            ),
            # Repère fixe : maximum Lester (base de la fourchette), nommé comme les autres modèles
            div(style = paste0("position:absolute; left:", pos(val_ref), "; top:", pad_top, "px; height:", bar_h, "px; width:2px; transform:translateX(-50%); background:", COL$primaire, "; opacity:.85;")),
            div(style = paste0("position:absolute; left:", pos(val_ref), "; top:", (pad_top - 16L), "px; transform:translateX(-50%); white-space:nowrap; font-size:10px; font-weight:700; color:", COL$primaire, ";"),
                paste0(lbl_ref_court, " ", fmt_nb(val_ref))),
            div(style = paste0("position:absolute; left:", pos(val_sel), "; top:", pad_top, "px; height:", bar_h, "px; width:2px; transform:translateX(-50%); background:", col_sel, ";")),
            div(style = paste0("position:absolute; left:", pos(val_sel), "; top:2px; transform:translateX(-50%); white-space:nowrap; font-size:12px; font-weight:700; color:", col_sel, ";"),
                paste0("Cible ", fmt_nb(val_sel))),
            if (!is.na(val_obs)) tagList(
              div(style = paste0("position:absolute; left:", pos(val_obs), "; top:", pad_top, "px; height:", bar_h, "px; width:2px; transform:translateX(-50%); background:#2C3E50; opacity:.55;")),
              div(style = paste0("position:absolute; left:", pos(val_obs), "; top:", (pad_top + bar_h + 4L), "px; transform:translateX(-50%); white-space:nowrap; font-size:11px; font-weight:600; color:#2C3E50;"),
                  paste0("Observé ", fmt_nb(val_obs)))
            ) else NULL,
            if (length(ovl_vals) > 0)
              lapply(seq_along(ovl_vals), function(i) {
                col <- pal_ovl[((i - 1L) %% length(pal_ovl)) + 1L]
                tagList(
                  div(style = paste0("position:absolute; left:", pos(ovl_vals[i]), "; top:", pad_top, "px; height:", bar_h, "px; width:1.5px; transform:translateX(-50%); background:", col, "; opacity:.9;")),
                  div(style = paste0("position:absolute; left:", pos(ovl_vals[i]), "; top:", (pad_top - 16L), "px; transform:translateX(-50%); white-space:nowrap; font-size:10px; font-weight:600; color:", col, ";"),
                      paste0(ovl_noms[i], " ", fmt_nb(ovl_vals[i])))
                )
              }) else NULL
          )
        )
      ),

        # Pied de carte : la valeur observée et la légende des zones, toutes
        # deux en kg/ha, sur la même ligne que le repère qu'elles décrivent.
        div(style = paste0("display:flex; justify-content:space-between; ",
                           "align-items:flex-end; flex-wrap:wrap; gap:12px; ",
                           "border-top:0.5px solid #e9ecef; padding-top:11px;"),
          ligne_obs,
          legende
        )
      )
    )
  })

  # Tableau — 5 colonnes : Comparer | Modèle | Max théorique (kg/ha) |
  # Quota recommandé (kg/an, fixé à 80 % — indépendant de l'outil de calcul) | Note
  # La disponibilité (si applicable) est intégrée directement dans la colonne Note.
  output$table_modeles <- DT::renderDT({
    avail     <- model_availability()
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))

    # Case à cocher « Comparer » (overlay sur la figure). Lester = ancre, pas de case.
    chk_cell <- function(value, available) {
      if (!available) return("")
      ck <- if (!is.null(input$overlay_models) && value %in% input$overlay_models) " checked" else ""
      paste0("<input type='checkbox' class='overlay-chk' value='", value, "'", ck, ">")
    }

    cb <- DT::JS(
      "table.on('change', 'input.overlay-chk', function() {",
      "  var vals = [];",
      "  $(table.table().container()).find('input.overlay-chk:checked').each(function(){ vals.push(this.value); });",
      "  Shiny.setInputValue('overlay_models', vals, {priority: 'event'});",
      "});"
    )

    opts_base <- list(dom = "t", paging = FALSE, ordering = FALSE,
                      language = list(emptyTable = "Aucun résultat"))

    # En-tête simple, partagé par les deux états (vide / calculé).
    # Colonnes : 0 Comparer | 1 Modèle | 2 Max (kg/ha) | 3 À N % (kg/an) | 4 Note
    # La 4e colonne est TOUJOURS calculée à PCT_RECOMMANDE, indépendamment du
    # taux saisi en haut de l'onglet : l'en-tête le dit explicitement pour
    # éviter qu'elle semble contredire le quota estimé lorsque le taux diffère.
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("Comparer"),
          th("Modèle"),
          th(class = "th-unit", "Max. théorique (kg/ha)"),
          th(class = "th-unit", title = paste0(
               "Quota correspondant à ", PCT_RECOMMANDE, " % du maximum théorique ",
               "de chaque modèle — valeur fixe, indépendante du taux d'exploitation ",
               "saisi en haut de l'onglet."),
             paste0("À ", PCT_RECOMMANDE, " % (kg/an)")),
          th("Note")
        )
      )
    ))

    # Modèles de l'espèce active (ordre = cascade ; le 1er est le recommandé)
    keys_mod <- config()$cascade_reference
    ref_key  <- keys_mod[1]
    lib_mod  <- list(
      lester        = "Lester et coll. 2021",
      shuter        = "Shuter et coll. 1998",
      ime           = "IME (Ryder 1965 + OMNR 1982)",
      touladi_valin = "Valin 1998",                     # Touladi (IME/TDS, coeff. réajustés)
      lester_savi   = "Lester et coll. 2002 (éq. 6/14)",
      valin         = "Valin / Vaillancourt 1998",       # Doré (IME/TDS)
      vezina        = "Vézina 1978",
      archambault   = "Archambault 1988/2009",
      omble_valin   = "Valin et Vaillancourt 1998"
    )
    label_mod <- function(k) {
      lab <- lib_mod[[k]]
      if (identical(k, ref_key)) paste0(lab, " (recommandé)") else lab
    }

    # Note affichée : indisponibilité en premier (si applicable), sinon détail du calcul
    note_avec_dispo <- function(k, note_calc) {
      info <- avail[[k]]
      if (!is.null(info) && !isTRUE(info$ok))
        return(paste0("Indisponible — ", info$note))
      note_calc
    }

    if (!calc_fait) {
      n_mod <- length(keys_mod)
      df_empty <- data.frame(
        "Comparer"                  = rep("", n_mod),
        "Modèle"                    = vapply(keys_mod, label_mod, character(1)),
        "Max. théorique (kg/ha)"    = rep("—", n_mod),
        "Quota recommandé (kg/an)" = rep("—", n_mod),
        "Note"                      = vapply(keys_mod, function(k) note_avec_dispo(k, ""), character(1)),
        check.names     = FALSE
      )
      return(DT::datatable(df_empty,
        container = sketch,
        options   = opts_base,
        rownames = FALSE,
        class    = "table table-sm table-striped"
      ))
    }

    r   <- results()
    sup <- r$sup

    fmt_res <- function(res) {
      if (is.null(res) || is.na(res$rendement_ha)) list(num = NA_real_)
      else list(num = res$rendement_ha)
    }
    fl <- fmt_res(r$lester); fs <- fmt_res(r$shuter); fi <- fmt_res(r$ime)

    # --- Infobulles par modèle : quoi · comment · intrants -------------------
    # title= natif (survol navigateur), fiable dans une cellule DT échappée.
    tip_mod <- function(nom, texte, gras = FALSE) {
      cls <- if (gras) "model-name fw-bold" else "model-name"
      paste0("<span class='", cls, "'>", nom,
             " <span class='info-ic' title=\"", gsub('"', "&quot;", texte),
             "\">\u24d8</span></span>")
    }

    esp_nom  <- config()$nom
    part_cfg <- config()$modeles$ime$partition

    tt_lester <- "Modèle bioénergétique de référence (Lester et coll. 2021). Estime la biomasse au rendement maximal soutenu (B_rms) puis la mortalité naturelle (M) à partir de la morphométrie, de l'habitat thermique et de la croissance du Touladi. Intrants : superficie, prof. max, prof. moyenne, T° air, Linf."
    tt_shuter <- "Modèle empirique calibré sur l'inland lake trout (Shuter et coll. 1998). Régression log-log reliant le rendement à la superficie et aux solides dissous totaux (SDT). Intrants : superficie, SDT (depuis la conductivité)."
    tt_ime    <- paste0("Indice morpho-édaphique (Ryder 1965). Estime le rendement total de la communauté à partir du ratio SDT / profondeur moyenne (MEI), puis attribue ",
                        if (!is.null(part_cfg) && !is.na(part_cfg)) paste0(round(part_cfg * 100), " % à l'espèce (", esp_nom, ")") else "une part à l'espèce",
                        " selon OMNR 1982. Intrants : SDT, profondeur moyenne.")
    tt_reg    <- "Rendement régional fixe (kg/ha) issu de la littérature grise du MRNF. Valeur de référence par région et type de communauté, indépendante des paramètres du lac. Bascule « seul » / « mixte » selon les espèces présentes cochées."
    tt_lester_savi <- paste0(
      "Modèle bioénergétique de référence pour le Doré (Lester et coll. 2002). Estime l'habitat thermo-optique (TOHA) à partir de la morphométrie, du Secchi et de la thermocline, puis le rendement (MSY) via les solides dissous totaux (SDT) et les degrés-jours (G). ",
      "TOHA (P_TOHA) : indice d'habitat thermo-optique, sans unité (0 à 1). ",
      "P_T : proportion épibenthique du lac (fraction du lac au-dessus de la thermocline), sans unité (0 à 1). ",
      "Secchi relatif (z_rel) : ratio profondeur de Secchi / profondeur effective maximale, sans unité. ",
      "G : degrés-jours cumulés (base 5 °C), saisis en valeur brute et divisés par 1000 dans la formule du rendement. ",
      "Intrants : superficie, prof. max, prof. moyenne, SDT, Secchi, degrés-jours (G) ; thermocline observée ou repli théorique (T° air)."
    )
    tt_valin  <- "Modèle empirique régional (Valin / Vaillancourt 1998, Saguenay–Lac-Saint-Jean). Indice morpho-édaphique (IME = SDT / prof. moyenne) puis part Doré (32 %). Intrants : SDT, profondeur moyenne ; repli à 0,60 kg/ha (lacs ≥ 20 ha) si donnée insuffisante."
    tt_touladi_valin <- "Modèle empirique régional (Valin 1998, Saguenay–Lac-Saint-Jean) — mêmes coefficients IME réajustés que le Valin du Doré (0,66 / 0,466), mais part Touladi (25 %) au lieu de 32 %. Intrants : SDT, profondeur moyenne. Aucun repli documenté si donnée insuffisante — modèle alors indisponible."
    tt_vezina <- "Régression puissance-exponentielle du rendement optimal en fonction de la profondeur moyenne seule (83 lacs, Vézina 1978). Base commune de Valin et Vaillancourt. Non valide sous ~2 m de profondeur moyenne. Intrants : profondeur moyenne."
    tt_archambault <- paste0(
      "Houde 1982, adaptation Archambault 1988/2009. Table exacte (<40 ha) ou formule fermée (≥40 ha) selon la superficie et le groupe d'espèces présentes (allopatrie/cyprins/catostomes/perchaude). ",
      "Association doré ou brochet : valeur fixe 0,5 kg/ha/an. N'inclut pas achigan/barbotte (non couverts par la source). Intrants : superficie, espèces présentes."
    )
    tt_valin_omble <- paste0(
      "Valin 1998 (Saguenay–Lac-Saint-Jean), dans la version modifiée par Vaillancourt (30 juillet 1998). Cascade de réductions % successives sur la base Vézina : espèces présentes, pH < 5, oxygène dissous, absence de tributaire/émissaire, camp/chalet. ",
      "Chaque réduction optionnelle (pH, O2, tributaire, chalets) n'est appliquée que si la donnée est disponible. ",
      "Apports de la version Vaillancourt : domaine de validité restreint à 2,0-25,9 m de profondeur moyenne, et palier \"ménés/catostomes seuls\" porté à 60 % (plage documentée 50-70 %, valeur par défaut non sourcée). ",
      "Intrants : profondeur moyenne, espèces présentes."
    )

    # --- Notes compactes : param = valeur | param = valeur -------------------
    note_lester <- if (!is.null(r$lester) && !is.na(r$lester$rendement_ha))
      paste0("B_rms = ", fmt_nb(r$lester$B_rms), " kg/ha",
             " | M = ", fmt_nb(r$lester$M, 3),
             " | Linf (", r$linf_source, ") = ", fmt_int(r$linf_value), " mm",
             " | T° air = ", fmt_nb(r$T_air, 1), " °C")
    else ""

    note_shuter <- if (!is.null(r$shuter) && !is.na(r$shuter$rendement_ha)) {
      base <- paste0("Superficie = ", fmt_int(sup), " ha",
                     " | SDT = ", if (!is.na(r$tds)) paste0(fmt_nb(r$tds, 1), " mg/L") else "—")
      if (nchar(r$shuter$note) > 0) paste0(base, " | \u26a0 ", r$shuter$note) else base
    } else ""

    note_ime <- if (!is.null(r$ime) && !is.na(r$ime$rendement_ha)) {
      part_p   <- r$ime$partition
      part_txt <- if (!is.null(part_p) && !is.na(part_p))
        paste0(round(part_p * 100), " % ", esp_nom) else "rendement total communauté"
      base <- paste0("MEI = ", fmt_nb(r$ime$MEI, 3),
                     " | SDT = ", if (!is.na(r$tds)) paste0(fmt_nb(r$tds, 1), " mg/L") else "—",
                     " | prof. moy. = ", fmt_nb(r$prof_moy, 1), " m",
                     " | total communauté = ", fmt_nb(r$ime$MSY_total), " kg/ha → ", part_txt)
      if (nchar(r$ime$note) > 0) paste0(base, " | \u26a0 ", r$ime$note) else base
    } else ""

    note_lester_savi <- if (!is.null(r$lester_savi) && !is.na(r$lester_savi$rendement_ha)) {
      base <- paste0("TOHA = ", fmt_nb(r$lester_savi$P_TOHA, 4),
                     " | P_T = ", fmt_nb(r$lester_savi$P_T, 3),
                     " | Secchi relatif = ", fmt_nb(r$lester_savi$z_rel, 3),
                     " | thermocline (", r$lester_savi$Dth_source, ") = ",
                     fmt_nb(r$lester_savi$Dth, 1), " m",
                     " | SDT = ", if (!is.na(r$tds)) paste0(fmt_nb(r$tds, 1), " mg/L") else "—")
      if (nchar(r$lester_savi$note) > 0) paste0(base, " | \u26a0 ", r$lester_savi$note) else base
    } else ""

    note_valin <- if (!is.null(r$valin) && !is.na(r$valin$rendement_ha)) {
      base <- if (!is.na(r$valin$IME))
        paste0("IME = ", fmt_nb(r$valin$IME, 3),
               " | SDT = ", if (!is.na(r$tds)) paste0(fmt_nb(r$tds, 1), " mg/L") else "—",
               " | prof. moy. = ", fmt_nb(r$prof_moy, 1), " m")
      else paste0("Superficie = ", fmt_int(sup), " ha")
      if (nchar(r$valin$note) > 0) paste0(base, " | \u26a0 ", r$valin$note) else base
    } else ""

    note_touladi_valin <- if (!is.null(r$touladi_valin) && !is.na(r$touladi_valin$rendement_ha)) {
      base <- paste0("IME = ", fmt_nb(r$touladi_valin$IME, 3),
                     " | SDT = ", if (!is.na(r$tds)) paste0(fmt_nb(r$tds, 1), " mg/L") else "—",
                     " | prof. moy. = ", fmt_nb(r$prof_moy, 1), " m")
      if (nchar(r$touladi_valin$note) > 0) paste0(base, " | \u26a0 ", r$touladi_valin$note) else base
    } else ""

    # --- Omble de fontaine : Vézina / Archambault / Valin / Vaillancourt -----
    note_vezina <- if (!is.null(r$vezina) && !is.na(r$vezina$rendement_ha)) {
      paste0("Prof. moy. = ", fmt_nb(r$prof_moy, 1), " m (base Vézina 1978, sans ajustement espèces)")
    } else ""

    note_archambault <- if (!is.null(r$archambault) && !is.na(r$archambault$rendement_ha)) {
      cat_txt <- if (!is.null(r$archambault$categorie)) paste0(" | catégorie = ", r$archambault$categorie) else ""
      base <- paste0("Superficie = ", fmt_int(sup), " ha", cat_txt)
      if (nchar(r$archambault$note) > 0) paste0(base, " | ", r$archambault$note) else base
    } else if (!is.null(r$archambault)) paste0("\u26a0 ", r$archambault$note) else ""

    note_valin_omble <- if (!is.null(r$omble_valin) && !is.na(r$omble_valin$rendement_ha)) {
      base <- paste0("Base Vézina = ", fmt_nb(r$omble_valin$base_vezina), " kg/ha",
                     if (!is.null(r$omble_valin$pct_especes)) paste0(" | réduction espèces = ", r$omble_valin$pct_especes, " %") else "")
      if (nchar(r$omble_valin$note) > 0) paste0(base, " | ", r$omble_valin$note) else base
    } else if (!is.null(r$omble_valin)) paste0("\u26a0 ", r$omble_valin$note) else ""

    # Modèles régionaux : les régions de l'espèce active s'affichent toujours
    # (valeur seul/mixte ou mono/multi selon les espèces présentes cochées).
    regs      <- regions_actives()
    reg_keys  <- vapply(regs, `[[`, character(1), "key")
    reg_noms  <- vapply(regs, `[[`, character(1), "nom")
    reg_vals  <- vapply(regs, `[[`, numeric(1),   "val")
    reg_notes <- vapply(regs, `[[`, character(1), "note")

    # Lignes modèles = uniquement les modèles déclarés par l'espèce active
    tip_lib  <- list(lester = tt_lester, shuter = tt_shuter, ime = tt_ime,
                     touladi_valin = tt_touladi_valin,
                     lester_savi = tt_lester_savi, valin = tt_valin,
                     vezina = tt_vezina, archambault = tt_archambault,
                     omble_valin = tt_valin_omble)
    note_lib <- list(lester = note_lester, shuter = note_shuter, ime = note_ime,
                     touladi_valin = note_touladi_valin,
                     lester_savi = note_lester_savi, valin = note_valin,
                     vezina = note_vezina, archambault = note_archambault,
                     omble_valin = note_valin_omble)
    val_mod  <- vapply(keys_mod, function(k) {
      res <- r[[k]]
      if (is.null(res) || is.na(res$rendement_ha)) NA_real_ else res$rendement_ha
    }, numeric(1))
    nom_mod   <- vapply(keys_mod, function(k)
      tip_mod(label_mod(k), tip_lib[[k]], gras = identical(k, ref_key)), character(1))

    # Lien vers la référence (optionnel — LIEN_LESTER_SAVI à remplir en haut du fichier)
    if (nchar(LIEN_LESTER_SAVI) > 0 && "lester_savi" %in% keys_mod) {
      idx_ls <- which(keys_mod == "lester_savi")
      lien_html <- paste0(" <a href=\"", LIEN_LESTER_SAVI, "\" target=\"_blank\" rel=\"noopener\" ",
                          "style=\"font-size:11px;\">(référence)</a>")
      nom_mod[idx_ls] <- paste0(nom_mod[idx_ls], lien_html)
    }

    note_mod  <- vapply(keys_mod, function(k) note_avec_dispo(k, note_lib[[k]]), character(1))
    chk_mod   <- vapply(keys_mod, function(k)
      if (identical(k, ref_key)) "" else chk_cell(k, isTRUE(avail[[k]]$ok)), character(1))

    all_vals <- c(val_mod, reg_vals)

    fmt_v  <- function(v) fmt_nb(v)
    # Quota recommandé : toujours fixé à 80 % (PCT_RECOMMANDE) du rendement maximal
    # théorique, indépendamment du % choisi dans l'outil de calcul du rendement.
    fmt_qn_reco <- function(v) fmt_int(v * PCT_RECOMMANDE / 100 * sup)

    chk_reg <- if (length(reg_keys) > 0) vapply(reg_keys, function(k) chk_cell(k, TRUE), character(1)) else character(0)
    comparer <- c(chk_mod, chk_reg)

    reg_noms_tip <- if (length(reg_noms) > 0)
      vapply(reg_noms, function(nm) tip_mod(nm, tt_reg), character(1)) else character(0)

    df <- data.frame(
      "Comparer"                  = comparer,
      "Modèle"                    = c(nom_mod, reg_noms_tip),
      "Max. théorique (kg/ha)"    = fmt_v(all_vals),
      "Quota recommandé (kg/an)" = fmt_qn_reco(all_vals),
      "Note"                      = c(note_mod,
                                      if (length(reg_noms) > 0) reg_notes else character(0)),
      check.names     = FALSE,
      stringsAsFactors = FALSE
    )

    dt_out <- DT::datatable(df,
      escape    = FALSE,
      callback  = cb,
      container = sketch,
      options   = c(opts_base, list(
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-center", width = "72px", targets = 0),
          list(className = "dt-left",   targets = 1),
          list(className = "dt-center", targets = list(2, 3)),
          list(className = "dt-left",   targets = 4)
        )
      )),
      rownames = FALSE,
      class    = "table table-sm table-striped"
    )
    dt_out
  })

  output$table_note <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))
    if (!calc_fait) return(NULL)
    tagList(
      tags$p(class = "text-muted small mt-1 mb-0 fst-italic",
             "Cocher un modèle pour l'ajouter à la barre")
    )
  })

  output$export_hint_ui <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))
    # Après calcul : aucune mention. L'ancienne ligne annonçait
    # « Touladi — Lester 2021 » quelle que soit l'espèce active (retirée 2026-09).
    if (!calc_fait)
      tags$small(class = "text-muted", "Calculer pour générer les résultats")
    else NULL
  })

  output$dl_excel <- downloadHandler(
    filename = function() paste0("quotas_",
                                 if (isTruthy(input$nom_lac)) input$nom_lac else "lac",
                                 "_", Sys.Date(), ".xlsx"),
    content = function(file) writeLines("Export Excel à implémenter", file)
  )
  output$dl_pdf <- downloadHandler(
    filename = function() paste0("rapport_quotas_",
                                 if (isTruthy(input$nom_lac)) input$nom_lac else "lac",
                                 "_", Sys.Date(), ".pdf"),
    content = function(file) writeLines("Export PDF à implémenter", file)
  )

  # ---------------------------------------------------------------------------
  # OUTPUTS — Onglet 2 : Analyse temporelle
  # ---------------------------------------------------------------------------
  output$tab2_table_container <- renderUI({
    if (tab2_table_collapsed_rv()) return(NULL)
    tagList(
      tags$style(HTML("#graph_window{margin-bottom:0}")),
      div(class = "d-flex align-items-center gap-2 mb-2",
        tags$small(class = "text-muted fw-medium", "Affichage graphiques :"),
        radioButtons("graph_window", NULL,
                     choices  = c("Toutes les années" = "all",
                                  "10 dernières" = "10",
                                  "5 dernières"  = "5"),
                     selected = "all", inline = TRUE)
      ),
      DT::DTOutput("exploit_table"),
      tags$small(class = "text-muted fst-italic d-block mt-2",
                 "« Affichage graphiques » coche rapidement les années à tracer. ",
                 "Cocher / décocher une année trace ou retire ses points des graphiques.")
    )
  })

  output$tab2_kpi_strip <- renderUI({
    df <- tryCatch(exploit_data_kpi(), error = function(e) NULL)
    sel_fen <- fenetre_rv()
    selecteur <- div(class = "d-flex align-items-center gap-2",
      tags$small(class = "text-muted fw-medium", "Années compilées :"),
      radioButtons("tab2_fenetre", NULL,
                   choices  = c("5 dernières"  = "5",
                                "10 dernières" = "10",
                                "20 dernières" = "20"),
                   selected = if (!is.null(sel_fen)) sel_fen else "5",
                   inline = TRUE)
    )
    if (is.null(df) || nrow(df) == 0) {
      return(tagList(
        tags$style(HTML("#tab2_fenetre{margin-bottom:0}")),
        div(class = "d-flex justify-content-end mb-2", selecteur)
      ))
    }
    df <- df[order(df$annee), ]
    n  <- nrow(df)

    # Tendance : pente de Sen + Mann-Kendall
    #   - Couleur, flèche orientée et % : seulement si significatif (p<0.05) ET ampleur ≥5%
    #   - Sinon : flèche grise neutre SANS pourcentage
    strip_trend <- function(annee, vals, inv = FALSE) {
      ok    <- !is.na(annee) & !is.na(vals)
      annee <- annee[ok]; vals <- vals[ok]
      neutre <- list(
        arrow  = span(style = "color:#9DA3A9; font-weight:700; font-size:13px; margin-left:4px;", "→"),
        border = "border-left:3px solid #DEE2E6;",
        montre = FALSE
      )
      if (length(vals) < 4L) return(neutre)
      moy <- mean(vals)
      if (moy == 0) return(neutre)
      pente <- sens_slope(annee, vals)
      p_val <- mann_kendall_p(vals)
      if (is.na(pente) || is.na(p_val)) return(neutre)
      # Variation relative par année (pente de Sen / moyenne) — naturellement bornée
      d      <- pente / moy * 100   # %/an
      signif <- p_val < 0.05
      ample  <- abs(d) >= 5
      montre <- signif && ample
      if (!montre) return(neutre)   # non significatif (ou trop faible) → flèche grise neutre, sans %
      col_up <- if (inv) "#8B1A1A" else "#1A6B3C"
      col_dn <- if (inv) "#1A6B3C" else "#8B1A1A"
      col    <- if (d > 0) col_up else col_dn
      symb   <- if (d > 0) "↑"    else "↓"
      signe  <- if (d >= 0) "+" else ""
      list(
        arrow  = span(style = paste0("color:", col, "; font-weight:700; font-size:13px; margin-left:4px;"),
                      paste0(symb, " ", signe, round(d), "%/an")),
        border = paste0("border-left:3px solid ", col, ";"),
        montre = TRUE
      )
    }

    # fmt0 / fmt2 : aliases locaux vers les formateurs globaux (convention québécoise)
    fmt0 <- fmt_int
    fmt2 <- fmt_nb

    # Ordre de remplissage de la grille 3×2 → paires verticales :
    #   col 1 : Captures / Succès   col 2 : Effort / Pression   col 3 : Masse / Rendement
    indicateurs <- list(
      list(nom = "Nb captures",   vals = df$nb_captures,     val = mean(df$nb_captures,    na.rm=TRUE), fmt = fmt0, unit = "capt./an", inv = FALSE),
      list(nom = "Effort",        vals = df$effort_jp,       val = mean(df$effort_jp,      na.rm=TRUE), fmt = fmt0, unit = "j-p/an",  inv = TRUE),
      list(nom = "Masse totale",  vals = df$masse_totale_kg, val = mean(df$masse_totale_kg,na.rm=TRUE), fmt = fmt0, unit = "kg/an",   inv = FALSE),
      list(nom = "Succès",        vals = df$succes,          val = mean(df$succes,         na.rm=TRUE), fmt = fmt2, unit = "n/j-p",   inv = FALSE),
      list(nom = "Pression",      vals = df$pression,        val = mean(df$pression,       na.rm=TRUE), fmt = fmt2, unit = "j-p/ha",  inv = TRUE),
      list(nom = "Rendement",     vals = df$rendement_obs,   val = mean(df$rendement_obs,  na.rm=TRUE), fmt = fmt2, unit = "kg/ha",   inv = FALSE)
    )

    cards <- lapply(indicateurs, function(ind) {
      tr <- strip_trend(df$annee, ind$vals, inv = if (!is.null(ind$inv)) ind$inv else FALSE)
      div(class = "kpi-strip-card",
          style = if (!is.null(tr)) tr$border else "border-left:3px solid #DEE2E6;",
        div(class = "kpi-eyebrow mb-1", ind$nom),
        div(
          span(class = "kpi-strip-val", ind$fmt(ind$val)),
          span(class = "kpi-unit", ind$unit),
          if (!is.null(tr)) tr$arrow else NULL
        )
      )
    })

    contexte_tip <- paste0(
      "Moyennes calculées sur ", n, " année", if (n > 1) "s" else "",
      " (", min(df$annee), "–", max(df$annee), "), selon la fenêtre choisie. ",
      "Tendance : pente de Sen, significativité par test de Mann-Kendall (p < 0,05). ",
      "Une flèche colorée avec pourcentage n'apparaît que si la tendance est significative ",
      "et d'ampleur ≥ 5 % ; sinon une flèche grise neutre indique l'absence de tendance fiable."
    )

    tagList(
      tags$style(HTML("#tab2_fenetre{margin-bottom:0}")),
      div(class = "tab2-section-head",
        div(class = "tab2-section-title",
            info_tip("Indicateurs : moyennes et tendances", contexte_tip)),
        selecteur
      ),
      do.call(div, c(list(class = "kpi-strip-row"), cards))
    )
  })

  output$tab2_content <- renderUI({
    # Dépend uniquement de exploit_edited_rv() pour le verrou —
    # PAS de exploit_data() qui dépend de rows_selected → évite la boucle :
    #   tab2_content → exploit_data → rows_selected → DT re-init → tab2_content
    charge <- !is.null(exploit_edited_rv()) &&
              isTruthy(input$no_lac) &&
              !is.na(input$sup) && input$sup > 0

    if (!charge) {
      fichier_charge <- !is.null(tryCatch(exploit_raw(), error = function(e) NULL))
      titre <- if (fichier_charge && isTruthy(input$no_lac))
        "Aucune donnée de récolte pour ce lac"
      else
        "Importez les données d'exploitation"
      sous_titre <- if (fichier_charge && isTruthy(input$no_lac))
        paste0("pour l'espèce active, dans ce fichier — analyse temporelle indisponible.")
      else
        "pour accéder à l'analyse temporelle."
      return(
        div(class = "text-center py-5 text-muted",
          tags$i(class = "fa fa-chart-line fa-2x mb-3 d-block"),
          tags$p(class = "fw-bold mb-1", titre),
          tags$p(class = "small", sous_titre)
        )
      )
    }
    tagList(
      # ── Bloc 1 : Indicateurs moyens (KPI) ──────────────────────────────────
      div(class = "tab2-kpi-block",
        uiOutput("tab2_kpi_strip")
      ),

      # ── Bloc 2 : Données et visualisations ─────────────────────────────────
      div(class = "tab2-section-divider",
          tags$span(class = "tab2-divider-label", "Données et visualisations")),

      # Graphique 1 : rendement observé vs Lester (Lester seul, pas de sélecteur)
      div(class = "graph-card",
        div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong("Rendement observé vs maximum théorique (Lester 2021)"),
          tags$button(
            class   = "btn btn-sm btn-link p-0 text-muted",
            style   = "text-decoration:none; font-size:12px;",
            onclick = "var el=document.getElementById('plot_rendement_panel'); var btn=this; el.style.display=el.style.display==='none'?'':'none'; btn.textContent=el.style.display===''?'\u25be Masquer':'\u25b8 Afficher';",
            "\u25be Masquer"
          )
        ),
        div(id = "plot_rendement_panel",
          plotly::plotlyOutput("plot_rendement", height = "300px")
        )
      ),
      # Graphique 2 : effort et succès
      div(class = "graph-card",
        div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong("Effort de pêche et succès"),
          tags$button(
            class   = "btn btn-sm btn-link p-0 text-muted",
            style   = "text-decoration:none; font-size:12px;",
            onclick = "var el=document.getElementById('plot_pression_panel'); var btn=this; el.style.display=el.style.display==='none'?'':'none'; btn.textContent=el.style.display===''?'\u25be Masquer':'\u25b8 Afficher';",
            "\u25be Masquer"
          )
        ),
        div(id = "plot_pression_panel",
          plotly::plotlyOutput("plot_pression", height = "260px")
        )
      ),
      # Graphique 3 : masse moyenne
      div(class = "graph-card",
        div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong("Masse moyenne des captures (g)"),
          tags$button(
            class   = "btn btn-sm btn-link p-0 text-muted",
            style   = "text-decoration:none; font-size:12px;",
            onclick = "var el=document.getElementById('plot_masse_panel'); var btn=this; el.style.display=el.style.display==='none'?'':'none'; btn.textContent=el.style.display===''?'\u25be Masquer':'\u25b8 Afficher';",
            "\u25be Masquer"
          )
        ),
        div(id = "plot_masse_panel",
          plotly::plotlyOutput("plot_masse", height = "260px")
        )
      ),

      # Tableau (collapsible) — déplacé en bas de l'onglet
      div(class = "graph-card",
        div(class = "d-flex justify-content-between align-items-center mb-2",
          tags$strong("Données annuelles d'exploitation"),
          actionButton("btn_toggle_tab2_table", "▾ Masquer",
                       class = "btn btn-sm btn-link p-0 text-muted",
                       style = "text-decoration:none; font-size:12px;")
        ),
        uiOutput("tab2_table_container")
      )
    )
  })

  # Helper : construit le df affiché dans le tableau (réutilisé par renderDT et proxy)
  build_exploit_df <- function(coches) {
    req(exploit_edited_rv())
    sup_val <- if (!is.na(input$sup) && input$sup > 0) input$sup else NA_real_
    if (is.null(coches)) coches <- exploit_edited_rv()$annee
    df <- exploit_edited_rv() %>%
      arrange(annee) %>%
      mutate(
        rendement_obs = if (!is.na(sup_val)) masse_totale_kg / sup_val else NA_real_,
        succes        = dplyr::if_else(is.na(effort_jp) | effort_jp == 0,
                                       NA_real_, round(nb_captures / effort_jp, 2))
      )
    df$Graph <- sprintf(
      '<input type="checkbox" %s class="row-check" data-annee="%s" style="cursor:pointer;width:15px;height:15px;">',
      ifelse(df$annee %in% coches, "checked", ""),
      df$annee
    )
    df %>% select(Graph, annee, nb_captures, effort_jp,
                  masse_totale_kg, rendement_obs, succes)
  }

  # Table DT éditable — col 0 = case « tracer » HTML, cols 1-4 éditables,
  # cols 5-6 calculées/verrouillées. Proxy pour MAJ sans
  # reconstruire le tableau (préserve la position de défilement).
  output$exploit_table <- DT::renderDT({
    req(exploit_edited_rv())
    df <- build_exploit_df(isolate(graph_annees_rv()))

    DT::datatable(
      df,
      escape    = FALSE,
      filter    = "none",
      colnames  = c("Graph.", "Année", "Nb capturés", "Effort (j-p)",
                    "Masse totale (kg)", "Rendement (kg/ha)", "Succès (n/j-p)"),
      rownames  = FALSE,
      editable  = list(target  = "cell",
                       disable = list(columns = c(0, 5, 6))),
      options   = list(
        dom        = "t",
        scrollY    = "220px",
        scrollCollapse = TRUE,
        paging     = FALSE,
        columnDefs = list(
          list(className   = "dt-center", targets = "_all"),
          list(width       = "38px",      targets = 0)
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(document).off('change.exptable').on('change.exptable', '.row-check', function() {",
          "    Shiny.setInputValue('exploit_check_toggle',",
          "      {annee: String($(this).data('annee')), checked: $(this).prop('checked')},",
          "      {priority: 'event'});",
          "  });",
          "}"
        )
      ),
      selection = "none"
    ) %>%
      DT::formatRound(columns = 5, digits = 2) %>%
      DT::formatRound(columns = 6, digits = 3) %>%
      DT::formatStyle(columns = 6, color = "#7F8C8D", fontStyle = "italic")
  })

  # Proxy : met à jour les cases à cocher sans reconstruire le tableau entier
  exploit_proxy <- DT::dataTableProxy("exploit_table")

  observeEvent(graph_annees_rv(), {
    req(exploit_edited_rv())
    df <- build_exploit_df(graph_annees_rv())
    DT::replaceData(exploit_proxy, df, resetPaging = FALSE, rownames = FALSE)
  }, ignoreInit = TRUE)

  # Case cochée/décochée → met à jour EN DIRECT les années tracées (graphiques)
  observeEvent(input$exploit_check_toggle, {
    info      <- input$exploit_check_toggle
    annee_val <- as.numeric(info$annee)
    coches    <- graph_annees_rv()
    if (is.null(coches)) coches <- numeric(0)
    graph_annees_rv(
      sort(if (isTRUE(info$checked)) union(coches, annee_val)
           else setdiff(coches, annee_val))
    )
  })

  # Raccourci « Affichage graphiques » → coche les N dernières années (cases)
  observeEvent(input$graph_window, {
    req(exploit_edited_rv())
    toutes    <- sort(unique(exploit_edited_rv()$annee))
    annee_max <- max(toutes)
    sel <- switch(input$graph_window,
      "10" = toutes[toutes >= annee_max - 9L],
      "5"  = toutes[toutes >= annee_max - 4L],
      toutes)                       # "all"
    graph_annees_rv(sel)
  }, ignoreInit = TRUE)

  # Fenêtre temporelle (partagée onglets 1 & 2) → pilote UNIQUEMENT les KPI
  observeEvent(input$tab2_fenetre, {
    if (!identical(fenetre_rv(), input$tab2_fenetre)) fenetre_rv(input$tab2_fenetre)
  }, ignoreInit = TRUE)

  observeEvent(input$kpi_obs_n, {
    if (!identical(fenetre_rv(), input$kpi_obs_n)) fenetre_rv(input$kpi_obs_n)
  }, ignoreInit = TRUE)

  # Resynchronise les deux sélecteurs quand la fenêtre change (peu importe la source)
  observeEvent(fenetre_rv(), {
    updateRadioButtons(session, "tab2_fenetre", selected = fenetre_rv())
    updateRadioButtons(session, "kpi_obs_n",    selected = fenetre_rv())
  }, ignoreInit = TRUE)

  # Sélecteur de lac depuis la base importée → met à jour Identification
  # ---------------------------------------------------------------------------
  # RÉACTIF — Liste combinée des lacs (Habitat ∪ Exploitation)
  #   Un seul sélecteur (pas deux) pour éviter d'avoir deux « lacs actifs »
  #   incohérents entre les deux fichiers. L'onglet Lacs (morphométrie, non
  #   spécifique à une espèce) est la source principale ; l'exploitation
  #   complète avec les lacs qui ont de la récolte pour l'espèce active mais
  #   pas encore de fiche habitat. Le nom du lac priorise Lacs (une seule
  #   ligne par lac, plus stable), avec repli sur le nom de l'exploitation.
  # ---------------------------------------------------------------------------
  lacs_disponibles <- reactive({
    pb <- tryCatch(pothal_brut(), error = function(e) NULL)
    er <- tryCatch(exploit_raw(), error = function(e) NULL)

    df_lacs <- if (!is.null(pb) && !is.null(pb$lacs) && "nolac" %in% names(pb$lacs)) {
      tmp <- pb$lacs[!is.na(pb$lacs$nolac), ]
      data.frame(
        nolac          = normaliser_nolac(tmp$nolac),
        nomlac_habitat = if ("nomlac" %in% names(tmp)) as.character(tmp$nomlac) else NA_character_,
        habitat        = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(nolac = character(0), nomlac_habitat = character(0),
                habitat = logical(0), stringsAsFactors = FALSE)
    }

    df_recolte <- if (!is.null(er) && "nolac" %in% names(er) && "espece_code" %in% names(er)) {
      tmp <- er[!is.na(er$nolac) & er$espece_code == config()$code_ifa, ]
      tmp <- tmp[!duplicated(normaliser_nolac(tmp$nolac)), ]
      data.frame(
        nolac          = normaliser_nolac(tmp$nolac),
        nomlac_exploit = if ("nom_plan_eau" %in% names(tmp)) as.character(tmp$nom_plan_eau) else NA_character_,
        recolte        = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(nolac = character(0), nomlac_exploit = character(0),
                recolte = logical(0), stringsAsFactors = FALSE)
    }

    if (nrow(df_lacs) == 0 && nrow(df_recolte) == 0) return(NULL)

    combine <- merge(df_lacs, df_recolte, by = "nolac", all = TRUE)
    combine$habitat <- !is.na(combine$habitat) & combine$habitat
    combine$recolte <- !is.na(combine$recolte) & combine$recolte
    # Nom priorisé : Lacs d'abord (une seule ligne par lac), exploitation en repli
    combine$nomlac <- dplyr::coalesce(combine$nomlac_habitat, combine$nomlac_exploit)

    # Ordre d'affichage : Habitat + Récolte d'abord, puis Récolte seule,
    # puis Habitat seul — et par nolac croissant à l'intérieur de chaque groupe.
    rang_aff <- ifelse(combine$habitat & combine$recolte, 1L,
                ifelse(combine$recolte,                   2L,
                ifelse(combine$habitat,                   3L, 4L)))

    combine[order(rang_aff, suppressWarnings(as.numeric(combine$nolac))),
            c("nolac", "nomlac", "habitat", "recolte")]
  })

  output$lac_select_ui <- renderUI({
    lacs <- lacs_disponibles()
    req(!is.null(lacs), nrow(lacs) > 0)

    etiquette <- function(nolac, nomlac, habitat, recolte) {
      base  <- if (!is.na(nomlac) && nchar(nomlac) > 0) paste0(nolac, " — ", nomlac) else nolac
      drapx <- c(if (habitat) "\u2713 Habitat" else NULL,
                if (recolte) "\u2713 R\u00e9colte" else NULL)
      if (length(drapx) > 0) paste0(base, "  (", paste(drapx, collapse = " \u00b7 "), ")") else base
    }

    choix_labels <- mapply(etiquette, lacs$nolac, lacs$nomlac, lacs$habitat, lacs$recolte)
    choix <- setNames(lacs$nolac, choix_labels)

    bouton_deselect <- if (isTruthy(lac_courant_rv())) {
      tags$button(
        id      = "btn_deselect_lac",
        class   = "btn btn-sm btn-outline-secondary mt-1",
        style   = "font-size:11.5px; padding:2px 8px;",
        type    = "button",
        onclick = "Shiny.setInputValue('btn_deselect_lac', Math.random());",
        "\u2715 Changer de lac"
      )
    } else NULL

    div(class = "mt-2",
      selectInput("lac_select", NULL,
                  choices  = c("— Choisir —" = "", choix),
                  selected = lac_courant_rv(),
                  width    = "100%"),
      bouton_deselect
    )
  })

  # Bouton explicite pour dé-sélectionner le lac courant (en plus de l'option
  # « — Choisir — » dans la liste, moins visible/intuitive)
  observeEvent(input$btn_deselect_lac, {
    updateSelectInput(session, "lac_select", selected = "")
  })

  observeEvent(input$lac_select, {
    # Dé-sélection (« — Choisir — ») : vider les champs pour saisie manuelle
    if (!isTruthy(input$lac_select)) {
      if (isTruthy(lac_courant_rv())) {
        lac_courant_rv("")
        updateTextInput(session, "no_lac",  value = "")
        updateTextInput(session, "nom_lac", value = "")
        updateNumericInput(session, "sup",            value = NA)
        updateNumericInput(session, "T_air",          value = NA)
        updateNumericInput(session, "prof_max",       value = NA)
        updateNumericInput(session, "prof_moy",       value = NA)
        updateNumericInput(session, "perimetre",      value = NA)
        updateNumericInput(session, "conductivite",   value = NA)
        updateNumericInput(session, "linf_manual",    value = NA)
        updateNumericInput(session, "secchi",         value = NA)
        updateNumericInput(session, "degres_jours_g", value = NA)
        updateNumericInput(session, "quota_actuel_touladi", value = NA)
        updateNumericInput(session, "quota_actuel_dore",    value = NA)
        updateNumericInput(session, "quota_actuel_omble",   value = NA)
        updateNumericInput(session, "ph_eau",               value = NA)
        updateNumericInput(session, "o2_metres_sous_5ppm",  value = NA)
        updateNumericInput(session, "nb_chalets_omble",     value = NA)
        updateRadioButtons(session, "tributaire_emissaire_omble", selected = "inconnu")
        updateCheckboxGroupInput(session, "especes_presentes_touladi", selected = character(0))
        updateCheckboxGroupInput(session, "especes_presentes_dore",    selected = character(0))
        updateCheckboxGroupInput(session, "especes_presentes_omble",   selected = character(0))
      }
      return()
    }
    # Re-rendu programmatique sur le même lac (changement d'espèce conservant
    # le lac) : ne pas recharger les paramètres
    if (identical(input$lac_select, lac_courant_rv())) return()

    lacs <- lacs_disponibles()
    row  <- if (!is.null(lacs)) lacs[lacs$nolac == input$lac_select, ] else NULL
    if (is.null(row) || nrow(row) == 0) return()
    lac_courant_rv(as.character(row$nolac[1]))
    updateTextInput(session, "no_lac",  value = as.character(row$nolac[1]))
    if (!is.na(row$nomlac[1]) && nchar(row$nomlac[1]) > 0)
      updateTextInput(session, "nom_lac", value = as.character(row$nomlac[1]))
    # Charger les paramètres sauvegardés OU reset complet si aucune sauvegarde
    cle <- as.character(row$nolac[1])
    params <- params_sauvegardes_rv()
    if (!is.null(params[[cle]])) {
      p <- params[[cle]]
      updateNumericInput(session, "sup",            value = p$sup)
      updateNumericInput(session, "T_air",          value = p$T_air)
      updateNumericInput(session, "prof_max",       value = p$prof_max)
      updateNumericInput(session, "prof_moy",       value = p$prof_moy)
      updateNumericInput(session, "perimetre",      value = p$perimetre)
      updateNumericInput(session, "conductivite",   value = p$conductivite)
      updateNumericInput(session, "linf_manual",    value = p$linf_manual)
      updateNumericInput(session, "secchi",         value = p$secchi)
      updateNumericInput(session, "degres_jours_g", value = p$degres_jours_g)
      updateNumericInput(session, "quota_actuel_touladi", value = p$quota_actuel_touladi)
      updateNumericInput(session, "quota_actuel_dore",    value = p$quota_actuel_dore)
      updateNumericInput(session, "quota_actuel_omble",   value = p$quota_actuel_omble)
      updateNumericInput(session, "ph_eau",               value = p$ph_eau)
      updateNumericInput(session, "o2_metres_sous_5ppm",  value = p$o2_metres_sous_5ppm)
      updateNumericInput(session, "nb_chalets_omble",     value = p$nb_chalets_omble)
      updateRadioButtons(session, "tributaire_emissaire_omble",
                          selected = if (is.null(p$tributaire_emissaire_omble)) "inconnu" else p$tributaire_emissaire_omble)
      updateCheckboxGroupInput(session, "especes_presentes_touladi",
                                selected = p$especes_presentes_touladi)
      updateCheckboxGroupInput(session, "especes_presentes_dore",
                                selected = p$especes_presentes_dore)
      updateCheckboxGroupInput(session, "especes_presentes_omble",
                                selected = p$especes_presentes_omble)
    } else {
      updateNumericInput(session, "perimetre",      value = NA)
      updateNumericInput(session, "linf_manual",    value = NA)
      updateNumericInput(session, "degres_jours_g", value = NA)
      updateNumericInput(session, "quota_actuel_touladi", value = NA)
      updateNumericInput(session, "quota_actuel_dore",    value = NA)
      updateNumericInput(session, "quota_actuel_omble",   value = NA)
      updateNumericInput(session, "ph_eau",               value = NA)
      updateNumericInput(session, "o2_metres_sous_5ppm",  value = NA)
      updateNumericInput(session, "nb_chalets_omble",     value = NA)
      updateRadioButtons(session, "tributaire_emissaire_omble", selected = "inconnu")
      updateCheckboxGroupInput(session, "especes_presentes_touladi", selected = character(0))
      updateCheckboxGroupInput(session, "especes_presentes_dore",    selected = character(0))
      updateCheckboxGroupInput(session, "especes_presentes_omble",   selected = character(0))
      updateRadioButtons(session, "linf_source",  selected = "manual")
    }
  }, ignoreInit = TRUE)

  # Paramètres sauvegardés par lac — session uniquement
  params_sauvegardes_rv <- reactiveVal(list())

  # Déclencheur pour les avertissements visuels (activé après import ou calcul)
  warnings_actifs_rv <- reactiveVal(FALSE)

  output$save_params_ui <- renderUI({
    req(isTruthy(input$no_lac))
    div(class = "mt-1",
      actionButton("btn_save_params",
                   label = info_tip("Sauvegarder ce lac",
                                    paste0("Conserve les paramètres saisis pour ce lac, ",
                                           "dans la session en cours seulement.")),
                   class = "btn btn-sm btn-outline-primary w-100")
    )
  })

  observeEvent(input$btn_save_params, {
    req(isTruthy(input$no_lac))
    cle <- trimws(input$no_lac)
    nouveaux <- list(
      sup                       = input$sup,
      T_air                     = input$T_air,
      prof_max                  = input$prof_max,
      prof_moy                  = input$prof_moy,
      perimetre                 = input$perimetre,
      conductivite              = input$conductivite,
      linf_manual               = input$linf_manual,
      secchi                    = input$secchi,
      degres_jours_g            = input$degres_jours_g,
      quota_actuel_touladi      = input$quota_actuel_touladi,
      quota_actuel_dore         = input$quota_actuel_dore,
      quota_actuel_omble        = input$quota_actuel_omble,
      ph_eau                    = input$ph_eau,
      o2_metres_sous_5ppm       = input$o2_metres_sous_5ppm,
      nb_chalets_omble          = input$nb_chalets_omble,
      tributaire_emissaire_omble = input$tributaire_emissaire_omble,
      especes_presentes_touladi = input$especes_presentes_touladi,
      especes_presentes_dore    = input$especes_presentes_dore,
      especes_presentes_omble   = input$especes_presentes_omble
    )
    params <- params_sauvegardes_rv()
    params[[cle]] <- nouveaux
    params_sauvegardes_rv(params)
    showNotification(
      paste0("Paramètres sauvegardés pour le lac ", cle, "."),
      type = "message", duration = 3
    )
  })

  # --- Graphique 1 : Rendement observé vs rendement théorique (interactif) ----
  # (Sélecteur de modèle retiré — le graphe rendement utilise Lester 2021 seul)

  output$plot_rendement <- plotly::renderPlotly({
    req(exploit_data())
    df <- exploit_data()
    r  <- tryCatch(results(), error = function(e) NULL)

    # Référence = modèle de référence de l'espèce (cascade du registre),
    # masquée tant que le calcul affiché est périmé.
    ref_m_g <- if (isTRUE(calc_valide_rv()))
                 modele_reference(r, config()$cascade_reference, config()$modeles) else NULL
    ref <- if (!is.null(ref_m_g))
             list(val = ref_m_g$val, nom = ref_m_g$nom)
           else NULL
    # Modèles régionaux affichés sur le graphique = ceux cochés « Comparer »
    # dans le tableau (input$overlay_models), valeur seul/mixte selon espèces.
    pal_reg_p  <- c("#6A5ACD", "#2E8B57", "#A0522D", "#9370DB", "#3CB371", "#CD853F")
    regs_act_p <- regions_actives()
    reg_sel_p  <- intersect(input$overlay_models, vapply(regs_act_p, `[[`, character(1), "key"))
    regs_p     <- Filter(function(x) x$key %in% reg_sel_p, regs_act_p)
    reg_vals_p <- if (length(regs_p) > 0) vapply(regs_p, `[[`, numeric(1),   "val") else numeric(0)
    reg_noms_p <- if (length(regs_p) > 0) vapply(regs_p, `[[`, character(1), "nom") else character(0)

    vals_y  <- c(df$rendement_obs,
                 if (!is.null(ref)) ref$val else NULL,
                 reg_vals_p)
    y_nudge <- diff(range(vals_y, na.rm = TRUE)) * 0.07
    if (!is.finite(y_nudge) || y_nudge == 0) y_nudge <- 0.1

    p <- suppressWarnings(
      ggplot(df, aes(x = annee, y = rendement_obs)) +
      geom_line(color = COL$accent, linewidth = 0.9) +
      geom_point(aes(text = paste0("Année : ", annee, "<br>",
                                   "Rendement obs. : ", round(rendement_obs, 2), " kg/ha")),
                 color = COL$primaire, size = 2.8) +
      labs(x = NULL, y = "Rendement (kg/ha)") +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.margin = margin(6, 12, 4, 4))
    )

    # Construction des couches de référence (modèle calculé + régionaux)
    has_ref <- !is.null(ref)
    has_reg <- length(regs_p) > 0

    if (has_ref || has_reg) {
      x_min <- min(df$annee) - 0.3
      x_max <- max(df$annee) + 0.3

      color_vals <- setNames(character(0), character(0))

      if (has_ref) {
        val_ref    <- ref$val
        lbl_ref    <- paste0(ref$nom, " — max. (", round(val_ref, 1), " kg/ha)")
        df_ref_line <- data.frame(x = c(x_min, x_max), y = val_ref, serie = lbl_ref)
        color_vals[lbl_ref] <- COL$primaire
        p <- p +
          geom_line(data = df_ref_line,
                    aes(x = x, y = y, color = serie),
                    linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE)
      }

      if (has_reg) {
        for (i in seq_along(regs_p)) {
          rv   <- reg_vals_p[i]
          nm   <- reg_noms_p[i]
          col  <- pal_reg_p[((i - 1L) %% length(pal_reg_p)) + 1L]
          lbl  <- paste0(nm, " (", round(rv, 2), " kg/ha)")
          df_r <- data.frame(x = c(x_min, x_max), y = rv, serie = lbl)
          color_vals[lbl] <- col
          p <- p + geom_line(data = df_r,
                             aes(x = x, y = y, color = serie),
                             linetype = "dotted", linewidth = 0.8, inherit.aes = FALSE)
        }
      }

      p <- p +
        scale_color_manual(name = NULL, values = color_vals) +
        theme(legend.position  = "bottom",
              legend.text      = element_text(size = 9),
              legend.key.width = unit(1.2, "cm"))
    }

    # Plage Y : minimum fixé à 0, maximum ajusté aux valeurs (+ marge).
    # Appliqué via coord_cartesian sur le ggplot : ggplotly respecte cette
    # échelle, alors qu'un range passé à lay() est écrasé par la conversion.
    all_y <- c(df$rendement_obs,
               if (has_ref) ref$val else NULL,
               reg_vals_p)
    y_hi  <- max(all_y, na.rm = TRUE) * 1.25
    if (!is.finite(y_hi) || y_hi <= 0) y_hi <- 5
    p <- p + coord_cartesian(ylim = c(0, y_hi))

    gp <- plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        yaxis     = list(range = c(0, y_hi), autorange = FALSE),
        legend    = list(orientation = "h", y = -0.22, x = 0.5, xanchor = "center"),
        hovermode = "x unified",
        font      = list(family = "sans-serif", size = 13)
      )
    # ggplotly, avec un mapping color + fill simultané, nomme les traces
    # « (label,1) » : parenthèses d'enrobage + indice de groupe interne.
    # On retire l'indice « ,<chiffres> » puis les parenthèses parasites.
    nettoyer_nom <- function(nm) {
      if (is.null(nm)) return(nm)
      nm <- gsub(",\\s*\\d+\\s*(?=\\)|$)", "", nm, perl = TRUE)  # ,1 avant ) ou fin
      nm <- sub("^\\((.*)\\)$", "\\1", nm)                          # déparenthèse (..)
      trimws(nm)
    }
    for (i in seq_along(gp$x$data)) {
      gp$x$data[[i]]$name         <- nettoyer_nom(gp$x$data[[i]]$name)
      if (!is.null(gp$x$data[[i]]$legendgroup))
        gp$x$data[[i]]$legendgroup <- nettoyer_nom(gp$x$data[[i]]$legendgroup)
    }
    gp
  })

  # --- Graphique 2 : Pression (barres) + Succès (ligne) — plotly natif -------
  output$plot_pression <- plotly::renderPlotly({
    req(exploit_data())
    df <- exploit_data()

    plotly::plot_ly(df, x = ~annee) %>%
      plotly::add_bars(
        y         = ~pression,
        name      = "Pression de pêche (j-p/ha)",
        marker    = list(color = COL$accent, opacity = 0.8),
        hovertemplate = "Année : %{x}<br>Pression : %{y:.2f} j-p/ha<extra></extra>"
      ) %>%
      plotly::add_trace(
        y         = ~succes,
        name      = "Succès (poissons/j-p)",
        yaxis     = "y2",
        type      = "scatter",
        mode      = "lines+markers",
        line      = list(color = "#E74C3C", width = 2.5),
        marker    = list(color = "#E74C3C", size = 8),
        hovertemplate = "Année : %{x}<br>Succès : %{y:.2f} poissons/j-p<extra></extra>"
      ) %>%
      plotly::layout(
        xaxis  = list(title = "", tickformat = "d", dtick = 1, ticks = "outside"),
        yaxis  = list(title     = "Pression de pêche (j-p/ha)",
                      titlefont = list(color = COL$accent),
                      tickfont  = list(color = COL$accent),
                      tickcolor = COL$accent,
                      linecolor = COL$accent,
                      zeroline  = FALSE,
                      range     = c(0, max(df$pression, na.rm = TRUE) * 1.30)),
        yaxis2 = list(title          = "Succès (poissons/j-p)",
                      titlefont      = list(color = "#E74C3C"),
                      overlaying     = "y", side = "right",
                      anchor         = "x",
                      color          = "#E74C3C",
                      showgrid       = FALSE,
                      zeroline       = FALSE,
                      showline       = TRUE,
                      linecolor      = "#E74C3C",
                      linewidth      = 2,
                      showticklabels = TRUE,
                      tickfont       = list(color = "#E74C3C", size = 12),
                      tickcolor      = "#E74C3C",
                      tickmode       = "auto",
                      rangemode      = "tozero",
                      range          = c(0, max(df$succes, na.rm = TRUE) * 1.30)),
        legend        = list(orientation = "h", y = -0.32, x = 0.5, xanchor = "center",
                             yanchor = "top"),
        margin        = list(r = 65, b = 80),
        hovermode     = "x unified",
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        font          = list(family = "sans-serif", size = 13)
      )
  })

  # --- Graphique 3 : Masse moyenne + repère 3 dernières années ---------------
  output$plot_masse <- plotly::renderPlotly({
    req(exploit_data())
    df <- exploit_data()

    dernieres <- tail(sort(unique(df$annee)), 3)
    moy_3ans  <- mean(df$masse_moy_g[df$annee %in% dernieres], na.rm = TRUE)
    label_moy <- paste0("Moy. ", length(dernieres), " dern. années : ",
                        round(moy_3ans), " g")

    y_nudge_m <- diff(range(df$masse_moy_g, na.rm = TRUE)) * 0.06
    if (!is.finite(y_nudge_m) || y_nudge_m == 0) y_nudge_m <- 5

    p <- suppressWarnings(
      ggplot(df, aes(x = annee, y = masse_moy_g)) +
      geom_line(color = COL$accent, linewidth = 0.9) +
      geom_point(aes(text = paste0("Année : ", annee, "<br>",
                                   "Masse moy. : ", round(masse_moy_g, 0), " g")),
                 color = COL$primaire, size = 2.8) +
      geom_line(data = data.frame(x     = c(min(df$annee) - 0.3, max(df$annee) + 0.3),
                                  y     = moy_3ans,
                                  serie = label_moy),
                aes(x = x, y = y, color = serie),
                linetype = "dashed", linewidth = 0.7, inherit.aes = FALSE) +
      scale_color_manual(name = NULL, values = setNames("#E67E22", label_moy)) +
      scale_x_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = "Masse moy. (g)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.minor = element_blank(),
            plot.margin      = margin(6, 12, 4, 4),
            legend.position  = "bottom",
            legend.text      = element_text(size = 9))
    )  # fin suppressWarnings

    all_m <- c(df$masse_moy_g, moy_3ans)
    m_lo  <- min(all_m, na.rm = TRUE) * 0.80
    m_hi  <- max(all_m, na.rm = TRUE) * 1.25
    if (!is.finite(m_lo) || !is.finite(m_hi)) { m_lo <- 0; m_hi <- 1000 }

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::layout(
        yaxis     = list(range = c(m_lo, m_hi)),
        legend    = list(orientation = "h", y = -0.18, x = 0.5, xanchor = "center"),
        hovermode = "x unified",
        font      = list(family = "sans-serif", size = 13)
      )
  })
}


# =============================================================================
shinyApp(ui, server)
