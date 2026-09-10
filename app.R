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
# (Les rasters climatiques ne sont plus téléversés depuis 2026-09 : ils sont
# lus dans le répertoire du dépôt, voir CLIMAT_REPERTOIRE.)
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
  eleve        = list(fill = "#FAC775", trait = "#B97A0B", nom = "Élevé"),
  # Au-delà du maximum théorique (> 100 %) : récolte supérieure au rendement
  # maximal soutenu estimé — seul seuil au sens biologique (révision 2026-09)
  depasse      = list(fill = "#FCEBEB", trait = "#A32D2D", nom = "Au-delà du maximum")
)

#' Zone d'un taux (% du maximum théorique) — source unique des couleurs.
#' Seuils (révision 2026-09) : la méthode recommande UNE valeur (80 %) ;
#' l'ancienne borne de 90 % (non sourcée) est abandonnée.
#'   = 80 % : recommandé (vert) ; < 80 % : conservateur (gris) ;
#'   80-100 % : au-dessus du recommandé (orange) ; > 100 % : au-delà du maximum (rouge)
zone_taux <- function(pct) {
  if (is.na(pct)) return(ZONES$conservateur)
  if (abs(pct - PCT_RECOMMANDE) < 0.5) ZONES$recommande
  else if (pct < PCT_RECOMMANDE) ZONES$conservateur
  else if (pct <= 100) ZONES$eleve
  else ZONES$depasse
}

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
#'
#' Format canonique (révision 2026-09) : les numéros purement numériques sont
#' complétés par des zéros à gauche — 5 chiffres pour un lac (1162 -> 01162),
#' 8 chiffres pour les identifiants plus longs (4301200 -> 04301200). Avant,
#' les zéros étaient RETIRÉS (01162 -> 1162), ce qui rendait l'affichage non
#' conforme à la numérotation officielle.
#' Hypothèse vérifiée sur le fichier R08 (onglets Lacs, Profil, Parametre,
#' Specimens et Stats_PS) : tous les identifiants numériques y comptent 5 ou
#' 8 chiffres. Un identifiant de 6 ou 7 chiffres serait complété à 8 — à
#' revoir si de tels numéros existent dans d'autres régions.
#' Les identifiants alphanumériques (04301B07, C0762...) sont conservés tels quels.
normaliser_nolac <- function(x) {
  x <- trimws(as.character(x))
  n <- suppressWarnings(as.numeric(x))
  entier <- !is.na(n) & grepl("^[0-9]+(\\.0+)?$", x)
  out <- x
  out[entier] <- ifelse(n[entier] < 1e5,
                        sprintf("%05.0f", n[entier]),
                        sprintf("%08.0f", n[entier]))
  out
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

  # Date la plus récente par (lac, paramètre), puis MOYENNE des mesures de
  # cette date (révision 2026-09). Avant : une seule ligne retenue par
  # slice(1), donc une station arbitraire quand plusieurs stations avaient été
  # échantillonnées le même jour. La moyenne arithmétique entre stations
  # suppose qu'elles sont également représentatives du lac (hypothèse
  # raisonnable pour la conductivité et le pH ; pour le Secchi, elle lisse
  # les différences entre bassins). Mesures sans date : utilisées seulement
  # si aucune mesure datée n'existe pour ce lac et ce paramètre.
  df_param %>%
    dplyr::filter(!is.na(nolac), !is.na(code_param), !is.na(resultat)) %>%
    dplyr::group_by(nolac, code_param) %>%
    dplyr::filter(if (all(is.na(date))) TRUE
                  else !is.na(date) & date == max(date, na.rm = TRUE)) %>%
    dplyr::summarise(resultat   = round(mean(resultat), 2),
                     n_mesures  = dplyr::n(),
                     date       = dplyr::first(date),
                     .groups    = "drop") %>%
    dplyr::select(nolac, code_param, resultat, date, n_mesures)
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

#' Date de la mesure retenue par extraire_param() (la plus récente), en texte
#' « aaaa-mm-jj » — affichée sous le champ pour situer la valeur dans le temps.
#' @return  chaîne, ou NA_character_ si absente
extraire_param_date <- function(df_param_pivot, nolac_val, code) {
  if (is.null(df_param_pivot) || length(nolac_val) != 1L || is.na(nolac_val) ||
      !("date" %in% names(df_param_pivot)))
    return(NA_character_)
  cle   <- normaliser_nolac(nolac_val)
  ligne <- df_param_pivot[normaliser_nolac(df_param_pivot$nolac) == cle &
                          df_param_pivot$code_param == code, ]
  if (nrow(ligne) == 0 || is.na(ligne$date[1])) return(NA_character_)
  n <- if ("n_mesures" %in% names(ligne)) ligne$n_mesures[1] else 1L
  paste0(format(as.Date(ligne$date[1]), "%Y-%m-%d"),
         if (!is.na(n) && n > 1) paste0(" (moy. de ", n, " mesures)") else "")
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

  /* ── Panneau gauche en 4 blocs (révision 2026-09) ──
     Tailles de texte relevées d'un cran pour la lecture ; bordure plus
     marquée, avec un liseré gauche dans la couleur de l'espèce active
     (recoloré par output$dynamic_theme_css). */
  .bloc-gauche {
    border: 1px solid #B8C4D0; border-left: 3px solid #4A6FA5; border-radius: 6px;
    padding: 12px 14px 10px; margin-bottom: 12px; background: #FFFFFF;
  }
  .bloc-titre {
    font-size: 15px; font-weight: 600; color: #2C3E50; margin-bottom: 8px;
    display: flex; align-items: baseline; gap: 8px;
  }
  .bloc-titre-note { font-size: 12px; font-weight: 400; color: #8a96a3; font-style: italic; }
  .consigne { font-size: 13px; color: #495057; line-height: 1.4; margin-bottom: 10px; }
  .grille-2 { display: grid; grid-template-columns: repeat(2, minmax(0,1fr)); gap: 12px 14px; }
  .grille-3 { display: grid; grid-template-columns: repeat(3, minmax(0,1fr)); gap: 12px 10px; }
  .grille-nom { display: grid; grid-template-columns: minmax(0,1.6fr) minmax(0,1fr); gap: 12px 10px; }
  .grille-2 > .pleine, .grille-3 > .pleine { grid-column: 1 / -1; }
  /* Case vide (ex. sélecteur d'inventaire absent) : ne réserve aucune cellule */
  .grille-2 > .shiny-html-output:empty, .grille-2 > div:empty { display: none; }
  .left-panel .champ .shiny-input-container,
  .left-panel .grille-nom .shiny-input-container { width: 100% !important; margin-bottom: 0; }
  .left-panel .champ label.control-label,
  .left-panel .grille-nom label.control-label,
  .etiquette-champ {
    font-size: 13px; font-weight: 500; color: #3d4b59; margin-bottom: 3px;
  }
  /* Champs sur fond blanc, bordure visible (le thème leur donne le gris du fond) */
  .left-panel .form-control,
  .left-panel .selectize-input {
    background: #FFFFFF !important; border: 1px solid #AEBBC8;
  }
  .left-panel .form-control:focus { border-color: #4A6FA5; }
  .left-panel .champ .form-control { padding: 5px 9px; height: auto; font-size: 14px; }
  .left-panel .bloc-gauche .shiny-input-container { margin-bottom: 4px; }
  .left-panel .bloc-gauche .radio label,
  .left-panel .bloc-gauche .form-check label,
  .left-panel .bloc-gauche .checkbox label { font-size: 13.5px; }
  /* Étiquette « Fenêtre climatique » : décollée des choix, ⓘ aligné au texte */
  .etiquette-champ.avec-choix { margin-bottom: 7px; }
  .etiquette-champ .info-ic { vertical-align: middle; font-size: 12px; margin-left: 5px; }
  /* Ligne de source sous un champ sourcé, et messages de validation */
  .src-ligne { font-size: 12px; color: #5f6b77; line-height: 1.35; margin-top: 3px; }
  .src-ligne a { color: inherit; text-decoration: underline dotted; cursor: pointer; }
  .src-ligne a:hover { color: #2C3E50; }
  .msg-var { font-size: 12px; line-height: 1.35; margin-top: 3px; }
  .msg-danger  { color: #B0453C; }
  .msg-warning { color: #A86E08; }
  .msg-info    { color: #5a6b7b; }
  .lien-discret { font-size: 12.5px; color: #5f6b77; text-decoration: none; }
  .lien-discret:hover { color: #B0453C; }
  /* Figure « Maximum théorique par modèle » (graphique à points, kg/an) */
  .fm-carte { background: white; border: 1px solid #dee2e6; padding: 14px 16px;
              box-shadow: 0 1px 4px rgba(0,0,0,0.04); }
  .fm-graph { margin-top: 8px; }
  .fm-rangee, .fm-ligne { display: grid; grid-template-columns: 190px minmax(0,1fr); }
  .fm-rangee { position: relative; height: 18px; }
  .fm-etiq { position: absolute; top: 1px; white-space: nowrap; font-size: 11.5px; font-weight: 700; }
  .fm-corps { position: relative; }
  .fm-couche { position: absolute; top: 0; bottom: 0; left: 190px; right: 0; pointer-events: none; }
  .fm-titre-axe { height: 16px; }
  .fm-titre-axe .fm-piste { text-align: center; font-size: 11.5px; color: #6c757d; }
  .fm-fenetre .shiny-input-container { margin: 0; }
  .fm-fenetre .radio-inline, .fm-fenetre .form-check-inline { font-size: 11.5px; margin-right: 6px; }
  .fm-trait { position: absolute; bottom: 0; width: 0; }
  .fm-ligne { height: 28px; align-items: center; position: relative; }
  .fm-corps .fm-ligne .fm-piste { border-bottom: 1px solid #eef0f2; }
  .fm-nom { font-size: 12.5px; color: #3d4b59; padding-right: 12px; text-align: right;
            white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
  .fm-nom.ref { font-weight: 700; color: #2C3E50; }
  .fm-nom.reg { color: #6c757d; }
  .fm-tag { font-size: 10.5px; font-weight: 600; color: #185FA5; background: #E6F1FB;
            border-radius: 8px; padding: 0 6px; margin-left: 5px; }
  .fm-piste { position: relative; height: 100%; }
  .fm-point { position: absolute; top: 50%; width: 11px; height: 11px; border-radius: 50%;
              background: #888780; transform: translate(-50%, -50%); z-index: 2; cursor: help; }
  .fm-point.ref { width: 15px; height: 15px; background: #185FA5; }
  .fm-point.reg { background: #FFFFFF; border: 2px solid #888780; }
  .fm-val { position: absolute; top: 50%; margin-top: -8px; font-size: 11.5px; color: #495057;
            white-space: nowrap; z-index: 2; }
  .fm-indispo { position: absolute; left: 6px; top: 50%; margin-top: -8px; font-size: 11.5px;
                color: #9aa7b2; font-style: italic; }
  .fm-separateur { height: 0; border-top: 1px dashed #c3c2b7; margin: 3px 0 3px 190px; }
  .fm-axe { height: 22px; }
  .fm-axe .fm-piste { border-top: 1px solid #c3c2b7; }
  .fm-tick { position: absolute; top: 3px; transform: translateX(-50%); font-size: 11px;
             color: #898781; white-space: nowrap; }
  .fm-leg-point { display: inline-block; width: 11px; height: 11px; border-radius: 50%; background: #888780; }
  .fm-leg-point.reg { background: #FFFFFF; border: 2px solid #888780; }
  /* Encadré « Données utilisées » (provenance des intrants) */
  .intrants-box {
    background: #FFFFFF; border: 1px solid #dee2e6; border-radius: 8px;
    padding: 10px 16px; margin: -2px 0 12px; font-size: 12.5px; color: #3d4b59;
  }
  .intrants-titre { display: flex; align-items: baseline; gap: 10px; flex-wrap: wrap;
                    font-weight: 600; font-size: 12.5px; margin-bottom: 4px; color: #2C3E50; }
  .intrants-statut { font-weight: 600; font-size: 11.5px; padding: 1px 8px; border-radius: 10px; }
  .intrants-statut.ok   { color: #3B6D11; background: #EAF3DE; }
  .intrants-statut.theo { color: #8a5a00; background: #FFF6E0; }
  .intrants-liste { line-height: 1.55; }
  .intrants-sep   { color: #adb5bd; }
  .intrants-theo  { color: #A86E08; font-weight: 600; }
  /* Page Information (brouillon) */
  .info-page { max-width: 900px; }
  .info-page h5 { font-size: 16px; font-weight: 600; color: #2C3E50; }
  .info-page h6 { font-size: 14px; font-weight: 600; color: #3d4b59; }
  .info-table { font-size: 12.5px; background: #FFFFFF; }
  .info-table th { font-weight: 600; color: #3d4b59; }
  /* Avertissement : lac potentiel et lac d'exploitation différents */
  .alerte-lacs {
    font-size: 12.5px; color: #8a5a00; background: #FFF6E0;
    border: 1px solid #F0D48A; border-left: 4px solid #B97A0B;
    border-radius: 5px; padding: 6px 12px; margin: -4px 0 12px;
  }
"


# =============================================================================
# PANNEAU GAUCHE — 4 BLOCS (révision 2026-09)
#
#   1. Données du lac          : import, choix du lac, morphométrie, habitat
#   2. Données climatiques     : Touladi et Doré seulement
#   3. Espèces présentes       : (+ Milieu environnant pour l'Omble)
#   4. Données d'exploitation  : facultatif — quota actuel, fichier, lac
#
# Architecture : chaque champ est défini UNE seule fois et reste dans le DOM
# en tout temps ; l'affichage par espèce passe par conditionalPanel() sur
# output.espece_active (voir server). L'ancien renderUI par espèce recréait
# les champs à chaque changement d'onglet : leurs valeurs (conductivité,
# T° air, etc.) repartaient à vide et un même identifiant (T_air) existait en
# deux exemplaires. Ici, changer d'espèce conserve les valeurs saisies.
#
# Champs « sourcés » (Linf, thermocline, O2, T° air, DJC5) : le champ affiche
# la valeur retenue et reste modifiable ; la ligne en dessous (output$src_*)
# indique la source et offre les autres sources en lien. Taper une valeur
# bascule automatiquement en saisie manuelle — voir CHAMPS_SOURCES au serveur.
# =============================================================================

# Condition JS d'affichage selon l'espèce active
si_espece <- function(...) {
  esp <- c(...)
  paste0("[", paste0("'", esp, "'", collapse = ","), "].indexOf(output.espece_active) >= 0")
}

# Champ numérique compact : étiquette courte au-dessus, texte indicatif dans
# le champ, et deux zones optionnelles en dessous (source / messages).
champ_num <- function(id, label, placeholder = NULL, src = FALSE, msg = NULL, ...) {
  champ <- numericInput(id, label, value = NA, ...)
  # Le texte indicatif doit viser l'élément <input> lui-même, pas le conteneur
  if (!is.null(placeholder))
    champ <- tagAppendAttributes(champ, placeholder = placeholder, .cssSelector = "input")
  div(class = "champ",
    champ,
    if (src) uiOutput(paste0("src_", id)) else NULL,
    if (!is.null(msg)) uiOutput(paste0("msg_", msg)) else NULL
  )
}

# Case de la grille visible pour certaines espèces seulement
case_espece <- function(especes, contenu, pleine_largeur = FALSE) {
  conditionalPanel(si_espece(especes), class = if (pleine_largeur) "pleine" else NULL, contenu)
}

# -- Bloc 1 : Données du lac ---------------------------------------------------
bloc_donnees_lac_ui <- function() {
  div(class = "bloc-gauche",
    div(class = "bloc-titre", "Données du lac"),
    conditionalPanel(si_espece("touladi", "dore"),
      div(class = "consigne",
          paste0("Saisir les données manuellement ou importer un fichier Excel ",
                 "(classeur IFA). Une fois importées, les données peuvent aussi être ",
                 "modifiées manuellement au besoin. Compléter ensuite la section sur ",
                 "les données climatiques, les espèces présentes et les données ",
                 "d'exploitation, le cas échéant."))),
    conditionalPanel(si_espece("omble"),
      div(class = "consigne",
          paste0("Saisir les données manuellement ou importer un fichier Excel ",
                 "(classeur IFA). Une fois importées, les données peuvent aussi être ",
                 "modifiées manuellement au besoin. Compléter ensuite la section sur ",
                 "les espèces présentes et le milieu environnant, ainsi que les ",
                 "données d'exploitation, le cas échéant."))),

    # Import — le bouton « Effacer » n'apparaît qu'une fois un fichier chargé
    fileInput("pothal_file", NULL, accept = c(".xlsx", ".xlsm"),
              buttonLabel = "Importer le fichier Potentiel halieutique",
              placeholder = ""),
    uiOutput("pothal_file_name"),

    # Fichier chargé : liste déroulante des lacs du fichier
    conditionalPanel("output.pothal_charge",
      uiOutput("lac_select_ui"),
      uiOutput("msg_lac"),
      actionLink("btn_effacer_import",
                 label = "\u2715 Effacer l'importation",
                 class = "lien-discret")
    ),
    # Aucun fichier : identification manuelle (nom obligatoire pour calculer)
    conditionalPanel("!output.pothal_charge",
      div(class = "grille-nom",
        textInput("nom_lac", "Nom du lac *"),
        textInput("no_lac",  "No lac (facultatif)")
      ),
      uiOutput("msg_nom")
    ),

    # Morphométrie — commune aux trois espèces
    div(class = "grille-3 mt-2",
      champ_num("sup",      "Superficie (ha)", min = 0),
      champ_num("prof_max", "Prof. max (m)",   min = 0),
      champ_num("prof_moy", "Prof. moy. (m)",  min = 0)
    ),
    uiOutput("msg_morpho"),

    # Variables propres à chaque espèce — une case de grille par champ ;
    # les cases masquées (display:none) ne consomment pas de cellule.
    div(class = "grille-2 mt-2",
      # Inventaire (profil) : n'apparaît que si le lac en compte plus d'un.
      # Enfant direct de la grille : vide, il ne réserve aucune cellule
      # (voir .grille-2 > .shiny-html-output:empty) ; présent, il occupe la
      # case de gauche, à côté de la thermocline qu'il pilote.
      uiOutput("ifa_inv_ui"),
      case_espece(c("touladi", "dore"),
        champ_num("therm_manual", "Prof. thermocline (m)", min = 0, src = TRUE)),
      case_espece(c("touladi", "dore"),
        champ_num("conductivite", "Conductivité (µS/cm)", min = 0, msg = "conductivite")),
      case_espece("touladi",
        champ_num("linf_manual", "Long. asymp. (mm)", min = 0, src = TRUE)),
      case_espece("dore",
        champ_num("secchi", "Secchi (m)", min = 0, msg = "secchi")),
      case_espece("omble",
        champ_num("perimetre", "Périmètre (km)", min = 0)),
      case_espece("omble",
        champ_num("ph_eau", "pH", min = 0, max = 14, step = 0.1, msg = "ph")),
      case_espece("omble",
        champ_num("o2_metres_sous_5ppm", "O\u2082 : m sous 5 ppm", min = 0, src = TRUE))
    )
  )
}

# -- Bloc 2 : Données climatiques (Touladi, Doré) -----------------------------
bloc_climat_ui <- function() {
  conditionalPanel(si_espece("touladi", "dore"),
    div(class = "bloc-gauche",
      div(class = "bloc-titre", "Données climatiques"),
      div(class = "grille-2",
        div(
          div(class = "etiquette-champ avec-choix",
              info_tip("Fenêtre climatique",
                       paste0("Nombre d'années utilisées pour la moyenne, à partir de la ",
                              "plus récente disponible. Une année isolée porte la ",
                              "variabilité interannuelle ; une moyenne sur 5 ou 10 ans ",
                              "s'approche davantage des conditions moyennes supposées par ",
                              "les modèles. Si moins d'années sont disponibles que demandé, ",
                              "le nombre réel est affiché sous le champ."))),
          radioButtons("climat_fenetre", NULL, choices = CLIMAT_FENETRES,
                       selected = "1", inline = FALSE)
        ),
        div(
          champ_num("T_air", "Temp. moy. de l'air (°C)", src = TRUE),
          conditionalPanel(si_espece("dore"),
            div(class = "mt-2",
              champ_num("degres_jours_g", "DJC5 (degrés-jours)",
                        placeholder = "valeur brute, ex. 1825", min = 0, src = TRUE)))
        )
      ),
      uiOutput("msg_climat")
    )
  )
}

# -- Bloc 3 : Espèces présentes (+ Milieu environnant pour l'Omble) ----------
bloc_especes_ui <- function() {
  tagList(
    div(class = "bloc-gauche",
      conditionalPanel(si_espece("touladi"),
        div(class = "bloc-titre",
            info_tip("Espèces présentes",
                     paste0("Ajuste les rendements régionaux affichés à titre de référence ",
                            "selon la communauté présente. N'affecte pas les modèles Lester, ",
                            "Shuter et IME."))),
        checkboxGroupInput("especes_presentes_touladi", NULL,
          choices = c("Doré jaune"                    = "dore",
                      "Brochet"                       = "brochet",
                      "Achigan"                       = "achigan",
                      "Omble de fontaine (dominante)" = "omble_dom"),
          selected = NULL)
      ),
      conditionalPanel(si_espece("dore"),
        div(class = "bloc-titre",
            info_tip("Espèces présentes",
                     paste0("Ajuste les grilles régionales affichées à titre de référence ",
                            "(population mono- ou multispécifique)."))),
        checkboxGroupInput("especes_presentes_dore", NULL,
          choices = c("Touladi"                       = "touladi",
                      "Brochet"                       = "brochet",
                      "Achigan"                       = "achigan",
                      "Omble de fontaine (dominante)" = "omble_dom"),
          selected = NULL)
      ),
      conditionalPanel(si_espece("omble"),
        div(class = "bloc-titre",
            info_tip("Espèces présentes",
                     paste0("« Lac en allopatrie » est exclusif : le cocher décoche les ",
                            "autres espèces, et vice-versa."))),
        checkboxGroupInput("especes_presentes_omble", NULL,
          choices = c("Achigan à petite bouche" = "achigan",
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
                      "Lac en allopatrie"       = "allopatrie"),
          selected = NULL),
        uiOutput("msg_especes_omble")
      )
    ),
    conditionalPanel(si_espece("omble"),
      div(class = "bloc-gauche",
        div(class = "bloc-titre", "Milieu environnant"),
        div(class = "etiquette-champ", "Tributaire ou émissaire permanent"),
        radioButtons("tributaire_emissaire_omble", NULL,
          choices  = c("Inconnu" = "inconnu", "Présent" = "present", "Absent" = "absent"),
          selected = "inconnu", inline = TRUE),
        uiOutput("msg_tributaire"),
        div(class = "grille-2 mt-1",
          champ_num("nb_chalets_omble", "Nombre de camps/chalets", min = 0)
        )
      )
    )
  )
}

# -- Bloc 4 : Données d'exploitation (facultatif) -----------------------------
bloc_exploitation_ui <- function() {
  div(class = "bloc-gauche",
    div(class = "bloc-titre", "Données d'exploitation",
        span(class = "bloc-titre-note", "facultatif")),
    div(class = "grille-2",
      case_espece("touladi", champ_num("quota_actuel_touladi", "Quota actuel (kg/an)", min = 0)),
      case_espece("dore",    champ_num("quota_actuel_dore",    "Quota actuel (kg/an)", min = 0)),
      case_espece("omble",   champ_num("quota_actuel_omble",   "Quota actuel (kg/an)", min = 0))
    ),
    div(class = "mt-2",
      fileInput("exploit_file", NULL, accept = c(".xlsx", ".xlsm"),
                buttonLabel = "Importer les données d'exploitation",
                placeholder = "")
    ),
    uiOutput("exploit_file_name"),
    conditionalPanel("output.exploit_charge",
      uiOutput("lac_exploit_ui")
    ),
    uiOutput("msg_exploit")
  )
}

# -- Page Information (BROUILLON, 2026-09) ------------------------------------
# Construite à partir du registre (REGISTRE_ESPECES, MODELES_REGIONAUX) pour
# rester synchronisée avec le code : ajouter un modèle au registre l'ajoute
# ici. Les références sont en forme abrégée (auteur, année) : références
# complètes et liens à ajouter si la page est conservée.
LIBELLES_INTRANTS <- c(
  A = "Superficie", Dmax = "Prof. max", Dmn = "Prof. moyenne",
  T_air = "T° moyenne annuelle de l'air",
  Linf = "Longueur asymptotique (facultative)",
  Dth_obs = "Prof. thermocline (facultative)",
  TDS = "Conductivité (convertie en SDT)", G = "Degrés-jours > 5 °C (DJC5)",
  z_sec = "Secchi", grp = "Espèces présentes", pH = "pH",
  o2_pct_reduction = "O\u2082 (m sous 5 ppm)", tributaire_absent = "Tributaire/émissaire",
  nb_chalets = "Nombre de camps/chalets", A_optionnel = "Superficie (raffinement chalets)"
)

page_information_ui <- function() {
  tableau <- function(entetes, lignes) {
    tags$table(class = "table table-sm info-table",
      tags$thead(tags$tr(lapply(entetes, tags$th))),
      tags$tbody(lapply(lignes, function(l) tags$tr(lapply(l, tags$td)))))
  }

  bloc_espece <- function(cle) {
    esp  <- REGISTRE_ESPECES[[cle]]
    casc <- esp$cascade_reference
    lignes <- lapply(casc, function(k) {
      m    <- esp$modeles[[k]]
      role <- if (k == casc[1]) "Référence" else paste0("Repli n\u00b0 ", which(casc == k) - 1)
      intr <- unname(ifelse(m$intrants %in% names(LIBELLES_INTRANTS),
                            LIBELLES_INTRANTS[m$intrants], m$intrants))
      list(m$nom, role, paste(intr, collapse = ", "))
    })
    regs <- MODELES_REGIONAUX[[cle]]
    noms_regs <- if (!is.null(regs)) unique(vapply(regs, `[[`, character(1), "nom")) else character(0)
    tagList(
      tags$h6(class = "mt-3", esp$nom),
      tableau(c("Modèle", "Rôle", "Données utilisées"), lignes),
      if (length(noms_regs) > 0)
        tags$p(class = "small text-muted",
               paste0("Grilles régionales affichées à titre de comparaison : ",
                      paste(noms_regs, collapse = ", "), "."))
    )
  }

  div(class = "info-page",
    div(class = "alerte-lacs",
        tags$strong("Brouillon. "),
        "Page en évaluation — références complètes et liens à ajouter si elle est conservée."),

    tags$h5("Modèles par espèce"),
    tags$p(class = "small",
           paste0("Le modèle de référence est le premier disponible dans l'ordre ci-dessous ; ",
                  "un modèle est indisponible si une donnée non facultative manque.")),
    lapply(ESPECES_ORDRE, bloc_espece),

    tags$h5(class = "mt-4", "Règles de calcul"),
    tags$ul(class = "small",
      tags$li(paste0("Quota (kg/an) = taux × rendement maximal théorique (kg/ha) × superficie. ",
                     "Taux recommandé : ", PCT_RECOMMANDE, " % (« pretty good yield », Hilborn 2010) ; ",
                     "à tout autre taux, le quota est dit « ajusté ».")),
      tags$li(paste0("Longueur asymptotique (Touladi) : calculée à partir des spécimens ",
                     "(méthode de Janošík, minimum 10 spécimens) ; à défaut, formule de Lester ",
                     "(éq. 1, selon la superficie).")),
      tags$li(paste0("Thermocline : détectée sur le profil de l'inventaire choisi (minimum 5 ",
                     "profondeurs) ; à défaut, repli théorique de Shuter et coll. (1983), ",
                     "qui requiert la T° de l'air.")),
      tags$li(paste0("Conductivité, Secchi, pH : mesure la plus récente du lac (onglet Parametre). ",
                     "Conductivité : repli sur le profil si Parametre n'en contient pas. ",
                     "SDT = conductivité × 0,666.")),
      tags$li(paste0("T° de l'air et DJC5 : grilles annuelles Info-Climat (MELCCFP), extraites au ",
                     "point du lac (latitude/longitude de l'onglet Lacs), moyennées sur la ",
                     "fenêtre choisie. La grille ne couvre que le Québec."))
    ),

    tags$h5(class = "mt-4", "Fichiers d'importation"),
    tags$p(class = "small", tags$strong("Potentiel halieutique"),
           " (.xlsx ou .xlsm) — quatre onglets obligatoires. Valeurs manquantes : ",
           "NULL (Lacs, Profil, Parametre) ou « - » (Specimens)."),
    tableau(c("Onglet", "Colonnes lues"), list(
      list("Lacs", "No plan d'eau, Nom plan d'eau, Superficie, Prof. max, Prof. moy (obligatoires) ; Latitude, Longitude, Périmètre, No UE (facultatives)"),
      list("Profil", "No plan d'eau, Date, No inventaire, No station, Profondeur, Température, Oxygène, pH, Conductivité"),
      list("Parametre", "No plan d'eau, Date, No station, Paramètre physico-chimique (codes CD, TR, PH), Résultat"),
      list("Specimens", "No plan d'eau, Année début inventaire, Espèce code (SANA, SAVI, SAFO), Long. totale max")
    )),
    tags$p(class = "small", tags$strong("Données d'exploitation"),
           " (.xlsx ou .xlsm) — colonnes obligatoires : No plan d'eau, Année, Espèce code, ",
           "Nombre capturés, Nombre pesés, Masse mesurée (kg), Effort total (jours-pêche). ",
           "Facultatives : Territoire, Nom plan d'eau, Type de pêche, Type de récolte."),
    tags$p(class = "small text-muted",
           "Les colonnes sont reconnues par leur nom, sans égard aux majuscules ni aux accents."),

    tags$h5(class = "mt-4", "Références (forme abrégée)"),
    tags$ul(class = "small",
      tags$li("Lester et coll. (2021) — Touladi, modèle de référence. [lien à ajouter]"),
      tags$li("Lester et coll. (2002) — Doré jaune, habitat thermo-optique. [lien à ajouter]"),
      tags$li("Shuter (1998) — Touladi. [lien à ajouter]"),
      tags$li("Shuter et coll. (1983) — profondeur théorique de la thermocline. [lien à ajouter]"),
      tags$li("Ryder (1965) et OMNR (1982) — indice morpho-édaphique et partition par espèce. [lien à ajouter]"),
      tags$li("Loranger (1986) — portée de l'IME dans les lacs à accès contrôlé du Québec. [lien à ajouter]"),
      tags$li("Valin (1998) ; Vaillancourt (1998) — Omble de fontaine, Touladi, Doré jaune. [lien à ajouter]"),
      tags$li("Archambault (1988, 2009) ; Houde (1982) — Omble de fontaine. [lien à ajouter]"),
      tags$li("Vézina (1978) — Omble de fontaine. [lien à ajouter]"),
      tags$li("Janošík — estimation de la longueur asymptotique. [référence à compléter]"),
      tags$li("Hilborn (2010) — « pretty good yield ». [lien à ajouter]")
    )
  )
}

# =============================================================================
# UI
# =============================================================================
ui <- fluidPage(
  theme    = theme_app,
  tags$head(
    # HTML() indispensable : sans lui, htmltools échappe « > » en « &gt; » et
    # toutes les règles à sélecteur enfant (a > b) sont ignorées par le
    # navigateur (champ natif du fileInput visible, grilles décalées).
    tags$style(HTML(css_minimal)),
    # Réinitialisation visuelle d'un fileInput (« Effacer l'importation ») :
    # Shiny ne sait pas vider un fileInput côté serveur. On vide l'élément
    # <input type=file> pour qu'un nouvel import du MÊME fichier redéclenche
    # bien l'événement « change », et on masque la barre de progression.
    tags$script(HTML("
      Shiny.addCustomMessageHandler('reinit_fichier', function(id) {
        var el = document.getElementById(id);
        if (el) el.value = '';
        var prog = document.getElementById(id + '_progress');
        if (prog) prog.style.visibility = 'hidden';
      });
    "))
  ),

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
      bloc_donnees_lac_ui(),
      bloc_climat_ui(),
      bloc_especes_ui(),
      bloc_exploitation_ui(),
      # Omble : rappel méthodologique (choix du modèle de référence provisoire)
      conditionalPanel(si_espece("omble"),
        div(class = "text-muted fst-italic small mb-2",
            "Modèle recommandé provisoire — en attente du rapport préliminaire.")),
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
        tabPanel("Rendement théorique",
          br(),

          # Zone 1 — Décision (kg/an) : quota recommandé (ou ajusté) et quota actuel
          uiOutput("kpi_top_ui"),
          # Provenance des intrants du modèle de référence (valeurs théoriques signalées)
          uiOutput("intrants_ui"),

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
        ),

        # --- Onglet 3 : Information (brouillon) -------------------------------
        tabPanel("Information",
          br(),
          page_information_ui()
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

  # Barre d'espèces : les trois onglets restent toujours actifs (révision
  # 2026-09). L'ancien grisage des espèces sans récolte pour le lac courant a
  # été retiré : les modèles de rendement ne dépendent pas des données de
  # récolte, il n'y a donc aucune raison d'empêcher le calcul.
  output$species_bar <- renderUI(build_species_bar(espece_active_rv()))

  # Valeurs lues par les conditionalPanel() du panneau gauche. Doivent être
  # calculées même masquées (suspendWhenHidden = FALSE), sinon la condition
  # JS ne serait jamais évaluée au premier affichage.
  output$espece_active <- reactive(espece_active_rv())
  outputOptions(output, "espece_active", suspendWhenHidden = FALSE)

  # Recoloration du thème à chaque changement d'espèce
  observeEvent(espece_active_rv(), {
    session$setCurrentTheme(make_theme(config()$palette))
  })

  # Changement d'espèce : le lac et les paramètres sont conservés (le fichier
  # Potentiel halieutique n'est pas propre à une espèce) ; seul le calcul
  # théorique devient périmé. Avant 2026-09, le lac était réinitialisé si
  # l'espèce n'avait pas de récolte dans le fichier d'exploitation — ce lien
  # entre les deux fichiers n'existe plus (sélecteurs dissociés).
  observeEvent(espece_active_rv(), {
    calc_valide_rv(FALSE)
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

      .cond-block, .bloc-gauche { border-left-color: %2$s !important; }

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
  # Fichier Potentiel halieutique « actif » : devient FALSE avec « Effacer
  # l'importation ». Nécessaire parce que Shiny ne peut pas vider un fileInput
  # côté serveur : input$pothal_file conserve l'ancien fichier jusqu'au
  # prochain import. Chaque nouvel import (même fichier) le remet à TRUE.
  pothal_actif_rv <- reactiveVal(FALSE)
  observeEvent(input$pothal_file, pothal_actif_rv(TRUE))

  pothal_brut <- reactive({
    req(input$pothal_file, isTRUE(pothal_actif_rv()))
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
      # message. normaliser_nolac() les conserve tels quels et ramène les
      # numéros au format officiel ("1" -> "00001"), ce qui rend les
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
      # Dates des mesures retenues (la plus récente du lac) — pour la ligne de
      # source sous chaque champ. Ces valeurs sont propres au LAC et non à
      # l'inventaire : choix confirmé (2026-09), la conductivité étant
      # généralement stable d'une année à l'autre et traitée comme une
      # caractéristique du lac par Shuter et l'IME.
      cond_date_param   <- extraire_param_date(pb$parametre, nolac_val, "CD")
      secchi_date_param <- extraire_param_date(pb$parametre, nolac_val, "TR")
      ph_date_param     <- extraire_param_date(pb$parametre, nolac_val, "PH")

      # --- Profil : construction des inventaires (même logique qu'avant) ----
      df_lac <- if (!is.na(nolac_val) && "nolac" %in% names(pb$profil)) {
        pb$profil[!is.na(pb$profil$nolac) & pb$profil$nolac == nolac_val, ]
      } else pb$profil[0, ]

      # Aucun profil pour ce lac : la morphométrie (onglet Lacs) et les
      # paramètres (onglet Parametre) restent disponibles. Avant 2026-09, ce
      # cas retournait echec() et AUCUN champ n'était rempli — ce qui touchait
      # la majorité des lacs (ex. fichier R08 : ~2 970 lacs dans Lacs, ~440
      # seulement dans Profil). Un pseudo-inventaire sans profil est créé :
      # profil_valide = FALSE, donc thermocline et O2 passent par leur repli.
      if (nrow(df_lac) == 0) {
        inv_seul <- data.frame(
          inv_key = "sans_profil", date_label = "Aucun profil", n_pts = 0L,
          profil_valide = FALSE,
          sup = sup_val, prof_max = prof_max_val, prof_moy = prof_moy_val,
          t_air = t_air_val, lat = lat_val, lon = lon_val, perimetre = perimetre_val,
          cond = cond_parametre,
          cond_source = if (!is.na(cond_parametre)) "Parametre" else NA_character_,
          cond_date   = cond_date_param,
          secchi = secchi_parametre,
          secchi_source = if (!is.na(secchi_parametre)) "Parametre" else NA_character_,
          secchi_date   = secchi_date_param,
          ph = ph_parametre,
          ph_source = if (!is.na(ph_parametre)) "Parametre" else NA_character_,
          ph_date   = ph_date_param,
          stringsAsFactors = FALSE
        )
        return(list(nomlac      = nomlac_val,
                    inventaires = inv_seul,
                    profils     = list(),
                    inv_defaut  = NA_character_,
                    note        = "Aucun profil dans l'onglet Profil pour ce lac."))
      }

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
          cond_date     = if (!is.na(cond_parametre)) cond_date_param
                         else if (!is.na(cond_prof))  as.character(date_lbl)
                         else NA_character_,
          secchi        = secchi_parametre,
          secchi_source = if (!is.na(secchi_parametre)) "Parametre" else NA_character_,
          secchi_date   = secchi_date_param,
          ph            = ph_parametre,
          ph_source     = if (!is.na(ph_parametre)) "Parametre" else NA_character_,
          ph_date       = ph_date_param,
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

  # Fonction interne : pré-remplir les champs simples depuis une ligne d'inventaires.
  # La T° air n'est plus écrite ici : c'est un champ « sourcé » (Info-Climat,
  # repli sur la colonne temp_air_moy de l'onglet Lacs) — voir tair_resolu().
  remplir_morpho_depuis_inv <- function(session, inv_row) {
    # Toujours écrire la valeur — NA vide le champ, évite de laisser les données du lac précédent
    updateNumericInput(session, "sup",          value = inv_row$sup)
    updateNumericInput(session, "prof_max",     value = inv_row$prof_max)
    updateNumericInput(session, "prof_moy",     value = inv_row$prof_moy)
    updateNumericInput(session, "conductivite", value = inv_row$cond)
    updateNumericInput(session, "secchi",       value = inv_row$secchi)
    updateNumericInput(session, "perimetre",    value = inv_row$perimetre)
    updateNumericInput(session, "ph_eau",       value = inv_row$ph)
  }

  # Clé d'inventaire effective : la sélection de l'utilisateur SEULEMENT si
  # elle appartient au lac courant. input$ifa_inv_choisi conserve sa dernière
  # valeur quand le sélecteur disparaît (lac à un seul inventaire) : sans ce
  # contrôle, la clé d'un lac précédent était réutilisée et le profil du
  # nouveau lac était déclaré introuvable.
  inv_effectif <- function(ifa) {
    choix <- input$ifa_inv_choisi
    if (!is.null(choix) && nzchar(choix) && !is.null(ifa$inventaires) &&
        choix %in% ifa$inventaires$inv_key) choix else ifa$inv_defaut
  }

  observeEvent(ifa_habitat_raw(), {
    ifa <- ifa_habitat_raw()

    cle_courante <- as.character(lac_courant_rv())
    if (!is.null(params_sauvegardes_rv()[[cle_courante]])) return()

    # Sources automatiques remises par défaut (profil, spécimens, climat)
    reinitialiser_modes_src()

    if (is.null(ifa$inventaires) || nrow(ifa$inventaires) == 0) {
      updateNumericInput(session, "sup",          value = NA)
      updateNumericInput(session, "prof_max",     value = NA)
      updateNumericInput(session, "prof_moy",     value = NA)
      updateNumericInput(session, "conductivite", value = NA)
      updateNumericInput(session, "secchi",       value = NA)
      updateNumericInput(session, "perimetre",    value = NA)
      updateNumericInput(session, "ph_eau",       value = NA)
      return()
    }

    # Ligne de référence : inventaire par défaut (profil valide le plus récent),
    # sinon la première ligne. La morphométrie et les paramètres étant propres
    # au lac, ils sont identiques d'une ligne à l'autre. (Avant 2026-09 : si
    # aucun profil n'était valide, inv_defaut valait NA et le filtre renvoyait
    # une ligne entièrement NA — les champs étaient vidés.)
    cle_inv <- if (!is.na(ifa$inv_defaut)) ifa$inv_defaut else ifa$inventaires$inv_key[1]
    inv_row <- ifa$inventaires[ifa$inventaires$inv_key == cle_inv, ]
    if (nrow(inv_row) > 0) remplir_morpho_depuis_inv(session, inv_row[1, ])
    warnings_actifs_rv(TRUE)
  })

  # Changement d'inventaire : la thermocline et l'O2 suivent automatiquement
  # (therm_resolved / o2_resolved lisent inv_effectif()). La morphométrie, le
  # Secchi et le pH sont propres au lac : ils ne sont PLUS réécrits (avant
  # 2026-09, changer d'inventaire effaçait les modifications manuelles de ces
  # champs). Seule exception : la conductivité, quand elle provient du repli
  # sur le profil (aucune mesure CD dans Parametre) — elle dépend alors de
  # l'inventaire.
  observeEvent(input$ifa_inv_choisi, {
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    req(!is.null(ifa), !is.null(ifa$inventaires), !is.null(input$ifa_inv_choisi))
    inv_row <- ifa$inventaires[ifa$inventaires$inv_key == input$ifa_inv_choisi, ]
    if (nrow(inv_row) > 0 && identical(inv_row$cond_source[1], "Profil (repli)"))
      updateNumericInput(session, "conductivite", value = inv_row$cond[1])
  }, ignoreInit = TRUE)

  # Ligne de référence (valeurs du fichier) pour l'inventaire effectif —
  # sert aux lignes de source sous Conductivité, Secchi et pH.
  inv_ligne_courante <- reactive({
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires) || nrow(ifa$inventaires) == 0) return(NULL)
    cle <- inv_effectif(ifa)
    if (is.na(cle)) cle <- ifa$inventaires$inv_key[1]
    ligne <- ifa$inventaires[ifa$inventaires$inv_key == cle, ]
    if (nrow(ligne) == 0) NULL else ligne[1, ]
  })

  # ---------------------------------------------------------------------------
  # RÉACTIFS — Inputs conditionnels
  # ---------------------------------------------------------------------------
  # ---------------------------------------------------------------------------
  # Linf (Touladi) — trois sources, choisies par mode_src$linf() :
  #   "file"      : spécimens du fichier (méthode Janošík) — défaut
  #   "theorique" : formule de Lester (éq. 1), à partir de la superficie
  #   "manual"    : valeur saisie dans le champ
  # En mode "file", s'il n'y a pas assez de spécimens, la valeur est NA et le
  # modèle applique la formule de Lester (resolve_linf) — comportement
  # inchangé, désormais affiché explicitement sous le champ.
  # ---------------------------------------------------------------------------
  linf_specimens <- reactive({
    pb <- tryCatch(pothal_brut(), error = function(e) NULL)
    if (is.null(pb) || is.null(pb$specimens))
      return(list(value = NA_real_, source = "fichier Potentiel halieutique non chargé",
                  n = 0L, note = ""))

    df_raw <- pb$specimens
    if (!("nolac" %in% names(df_raw)))
      return(list(value = NA_real_, source = "colonne 'No plan d'eau' introuvable (Specimens)",
                  n = 0L, note = ""))
    if (!("long_totale" %in% names(df_raw)))
      return(list(value = NA_real_, source = "colonne 'Long. totale max' introuvable (Specimens)",
                  n = 0L, note = ""))

    # Filtre sur le lac du fichier Potentiel halieutique (lac_courant_rv),
    # toutes années confondues. On lit la valeur serveur plutôt que
    # input$no_lac, qui n'est à jour qu'après l'aller-retour navigateur.
    nolac_val <- cle_lac(lac_courant_rv())
    df_f <- if (!is.na(nolac_val)) {
      df_raw[!is.na(df_raw$nolac) & df_raw$nolac == nolac_val, ]
    } else df_raw[0, ]

    # Filtre espèce active — un seul onglet Specimens couvre les 3 espèces
    if ("espece_code" %in% names(df_f))
      df_f <- df_f[!is.na(df_f$espece_code) &
                   toupper(trimws(df_f$espece_code)) == config()$code_ifa, ]

    lt_vec <- suppressWarnings(as.numeric(df_f$long_totale))
    lt_vec <- lt_vec[!is.na(lt_vec) & lt_vec > 0]
    n_spec <- length(lt_vec)

    # Seuil de 10 spécimens conservé tel quel (valeur de la version
    # précédente) : en deçà, l'estimation de Janošík est jugée trop instable.
    if (n_spec < 10L)
      return(list(value = NA_real_,
                  source = if (n_spec == 0L) "aucun spécimen"
                           else paste0("spécimens insuffisants (n = ", n_spec, ", min. 10)"),
                  n = n_spec, note = ""))

    linf_val <- calc_linf_janoscik(lt_vec)
    if (is.na(linf_val))
      return(list(value = NA_real_,
                  source = "Linf non calculable (échantillon insuffisant après coupe)",
                  n = n_spec, note = ""))

    list(value  = round(linf_val, 1),
         source = paste0("spécimens (n = ", n_spec, ", Janošík)"),
         n      = n_spec,
         note   = if (n_spec < 20L)
                    paste0("Échantillon faible (n = ", n_spec, ") — interpréter avec prudence")
                  else "")
  })

  # Formule de Lester (éq. 1) — dépend seulement de la superficie
  linf_theorique <- reactive({
    sup_val <- input$sup
    if (is.null(sup_val) || is.na(sup_val) || sup_val <= 0) return(NA_real_)
    round(resolve_linf(NA_real_, sup_val)$value, 1)
  })

  linf_resolved <- reactive({
    switch(mode_src$linf(),
      manual = {
        val <- input$linf_manual
        list(value  = if (!is.null(val) && !is.na(val) && val > 0) val else NA_real_,
             source = "saisie manuelle", n = NA_integer_, note = "")
      },
      theorique = list(value  = linf_theorique(),
                       source = "formule Lester (théorique)",
                       n = NA_integer_, note = ""),
      linf_specimens()
    )
  })


  # ---------------------------------------------------------------------------
  # DONNÉES CLIMATIQUES
  # ---------------------------------------------------------------------------
  # Rasters du dépôt seulement (donnees/climat). L'importation manuelle de
  # rasters a été retirée de l'interface (révision 2026-09) : sans raster ou
  # sans coordonnées, la valeur se saisit directement dans le champ.
  # catalogue_rasters() conserve ses arguments de téléversement, inutilisés.
  catalogue_climat <- reactive({
    catalogue_rasters(NULL, NULL)
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

  # T° air — deux sources, choisies par mode_src$tair() :
  #   "climat" : raster TMOY au point du lac (fenêtre choisie) ; à défaut, la
  #              colonne temp_air_moy de l'onglet Lacs si elle existe
  #   "manual" : valeur saisie
  # Aucune valeur déduite des degrés-jours (régression retirée) : sans raster,
  # sans colonne et sans saisie, la valeur est absente — jamais fabriquée.
  tair_resolu <- reactive({
    if (identical(mode_src$tair(), "manual")) {
      ta <- input$T_air
      return(list(value = if (!is.null(ta) && !is.na(ta)) ta else NA_real_,
                  source = "saisie manuelle", annees = integer(0), motif = NA_character_))
    }
    cl <- climat_tmoy()
    if (!is.na(cl$valeur))
      return(list(value = cl$valeur, source = "climat", annees = cl$annees, motif = cl$motif))
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    t_lacs <- if (!is.null(ifa) && !is.null(ifa$inventaires) && "t_air" %in% names(ifa$inventaires))
      suppressWarnings(as.numeric(ifa$inventaires$t_air[1])) else NA_real_
    if (!is.na(t_lacs))
      return(list(value = t_lacs, source = "lacs", annees = integer(0), motif = cl$motif))
    list(value = NA_real_, source = "aucune", annees = integer(0), motif = cl$motif)
  })

  # Degrés-jours (DJC5) — valeur BRUTE (non divisée par 1000) ; mêmes modes.
  g_resolu <- reactive({
    if (identical(mode_src$g(), "manual")) {
      g <- input$degres_jours_g
      return(list(value = if (!is.null(g) && !is.na(g)) g else NA_real_,
                  source = "saisie manuelle", annees = integer(0), motif = NA_character_))
    }
    cl <- climat_djc5()
    list(value = cl$valeur, source = if (is.na(cl$valeur)) "aucune" else "climat",
         annees = cl$annees, motif = cl$motif)
  })

  # Valeurs effectives lues par les modèles et le repli thermocline
  t_air_effectif <- reactive(tair_resolu()$value)
  g_effectif     <- reactive(g_resolu()$value)

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

    if (identical(mode_src$therm(), "manual")) {
      val <- input$therm_manual
      list(value      = if (!is.null(val) && !is.na(val) && val > 0) val else NA_real_,
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

      # Clé d'inventaire : sélection de l'utilisateur si elle appartient au
      # lac courant, sinon l'inventaire par défaut (voir inv_effectif())
      inv_key <- inv_effectif(ifa)

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
    if (identical(mode_src$o2(), "manual")) {
      # Depuis 2026-09, tous les champs restent dans le DOM (conditionalPanel) :
      # input$o2_metres_sous_5ppm n'est plus NULL en pratique. La normalisation
      # NULL -> NA est conservée par prudence (appel sans condition, pour
      # toutes les espèces, depuis results()).
      val <- input$o2_metres_sous_5ppm
      if (is.null(val)) val <- NA_real_
      return(list(value = if (!is.na(val) && val >= 0) val else NA_real_,
                  source = "saisie manuelle", note = ""))
    }

    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires))
      return(list(value = NA_real_, source = "IFA : fichier non chargé", note = ""))

    inv_key <- inv_effectif(ifa)

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
  # CHAMPS « SOURCÉS » — valeur pré-remplie, modifiable, source affichée
  #
  #   Remplace les boutons radio « Saisir / Habitat / Calculer / Formule
  #   Lester / Climat » (révision 2026-09). Pour chaque variable :
  #     - mode_src[[cle]]() : source retenue ("manual" ou une source auto) ;
  #     - hors mode manuel, la valeur de la source est POUSSÉE dans le champ ;
  #     - si l'utilisateur tape une valeur différente, le mode passe à
  #       "manual" (détection ci-dessous) ;
  #     - la ligne sous le champ (output$src_<id>) affiche la source et offre
  #       les autres sources en lien (input$src_switch).
  #
  #   Difficulté : Shiny ne distingue pas une valeur tapée d'une valeur
  #   poussée par updateNumericInput() — les deux reviennent par input$<id>.
  #   On conserve donc l'historique des dernières valeurs poussées : une valeur
  #   reçue qui y figure est un simple retour de pré-remplissage. Un historique
  #   (plutôt que la seule dernière valeur) couvre le cas où deux mises à jour
  #   se suivent avant le retour de la première (ex. changement de lac :
  #   superficie puis spécimens). L'observateur de détection a une priorité
  #   plus haute que celui qui pousse, pour comparer avant d'enregistrer la
  #   prochaine valeur.
  # ---------------------------------------------------------------------------
  CHAMPS_SOURCES <- list(
    linf  = list(id = "linf_manual",         defaut = "file",   dec = 1),
    therm = list(id = "therm_manual",        defaut = "ifa",    dec = 2),
    o2    = list(id = "o2_metres_sous_5ppm", defaut = "ifa",    dec = 1),
    tair  = list(id = "T_air",               defaut = "climat", dec = 2),
    g     = list(id = "degres_jours_g",      defaut = "climat", dec = 1)
  )
  mode_src <- lapply(CHAMPS_SOURCES, function(ch) reactiveVal(ch$defaut))
  resolu_src <- list(linf = linf_resolved, therm = therm_resolved,
                     o2 = o2_resolved, tair = tair_resolu, g = g_resolu)

  historique_src <- lapply(CHAMPS_SOURCES, function(ch) reactiveVal(numeric(0)))

  reinitialiser_modes_src <- function() {
    for (cle in names(CHAMPS_SOURCES)) mode_src[[cle]](CHAMPS_SOURCES[[cle]]$defaut)
  }

  valeur_dans <- function(v, hist) {
    if (length(hist) == 0) return(FALSE)
    if (is.null(v) || length(v) == 0 || is.na(v)) return(any(is.na(hist)))
    any(!is.na(hist) & abs(hist - v) < 1e-6)
  }

  pousser_src <- function(cle, v) {
    ch <- CHAMPS_SOURCES[[cle]]
    v  <- if (is.null(v) || length(v) == 0 || is.na(v)) NA_real_ else round(as.numeric(v), ch$dec)
    # Historique : on n'empile pas deux fois la même valeur de suite (les
    # sources se réévaluent souvent à valeur identique), pour qu'une valeur
    # encore en transit ne soit pas chassée de l'historique par des doublons.
    hist <- historique_src[[cle]]()
    if (length(hist) == 0 || !valeur_dans(v, utils::tail(hist, 1)))
      historique_src[[cle]](utils::tail(c(hist, v), 6))
    updateNumericInput(session, ch$id, value = v)
  }

  for (cle_src in names(CHAMPS_SOURCES)) local({
    cle <- cle_src
    id  <- CHAMPS_SOURCES[[cle]]$id

    # Pré-remplissage : hors mode manuel, le champ suit la source
    observe({
      if (identical(mode_src[[cle]](), "manual")) return()
      v <- tryCatch(resolu_src[[cle]]()$value, error = function(e) NA_real_)
      isolate(pousser_src(cle, v))
    })

    # Détection d'une saisie : valeur reçue absente de l'historique des
    # valeurs poussées -> bascule en saisie manuelle
    observeEvent(input[[id]], {
      if (identical(mode_src[[cle]](), "manual")) return()
      if (!valeur_dans(input[[id]], historique_src[[cle]]())) mode_src[[cle]]("manual")
    }, ignoreInit = TRUE, ignoreNULL = FALSE, priority = 10)
  })

  # Liens de la ligne de source : retour à une source automatique
  observeEvent(input$src_switch, {
    s <- input$src_switch
    if (!is.null(s$cle) && !is.null(mode_src[[s$cle]]) && !is.null(s$mode))
      mode_src[[s$cle]](s$mode)
  })

  # Éléments d'affichage de la ligne de source
  lien_src <- function(cle, mode, libelle) {
    tags$a(href = "#",
           onclick = sprintf(paste0("Shiny.setInputValue('src_switch', ",
                                    "{cle: '%s', mode: '%s', n: Math.random()}, ",
                                    "{priority: 'event'}); return false;"), cle, mode),
           libelle)
  }
  ligne_src <- function(texte, liens = list(), alerte = NULL) {
    liens <- Filter(Negate(is.null), liens)
    tagList(
      div(class = "src-ligne",
          span(texte),
          lapply(liens, function(l) tagList(span(" \u00b7 "), l))),
      if (!is.null(alerte) && nzchar(alerte))
        div(class = "msg-var msg-warning", alerte) else NULL
    )
  }
  maj1 <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  fmt_annees <- function(annees) {
    if (length(annees) == 0) return("")
    if (length(annees) == 1) paste0("année ", annees)
    else paste0("moy. ", min(annees), "\u2013", max(annees), " (", length(annees), " ans)")
  }

  # --- Linf ---------------------------------------------------------------
  output$src_linf_manual <- renderUI({
    m    <- mode_src$linf()
    spec <- linf_specimens()
    theo <- linf_theorique()
    txt_theo <- if (!is.na(theo)) paste0(" (\u2248 ", fmt_nb(theo, 0), " mm)") else ""
    lien_spec  <- if (!is.na(spec$value)) lien_src("linf", "file", "\u21ba Spécimens") else NULL
    lien_theo  <- lien_src("linf", "theorique", "Formule Lester")
    if (identical(m, "manual")) {
      v <- input$linf_manual
      if (is.null(v) || is.na(v))
        ligne_src(paste0("Champ vide — formule de Lester utilisée automatiquement", txt_theo),
                  list(lien_spec))
      else ligne_src("Saisie manuelle", list(lien_spec, lien_theo))
    } else if (identical(m, "theorique")) {
      ligne_src(if (is.na(theo)) "Formule de Lester — superficie requise"
                else "Formule de Lester (théorique)", list(lien_spec))
    } else if (!is.na(spec$value)) {
      ligne_src(paste0("Calculé à partir des ", spec$source), list(lien_theo), alerte = spec$note)
    } else {
      motif <- if (grepl("non chargé", spec$source)) "aucune donnée de spécimens" else spec$source
      ligne_src(paste0(maj1(motif), " — formule de Lester utilisée automatiquement", txt_theo))
    }
  })

  # --- Thermocline ----------------------------------------------------------
  output$src_therm_manual <- renderUI({
    m  <- mode_src$therm()
    tr <- tryCatch(therm_resolved(), error = function(e) NULL)
    req(!is.null(tr))
    # Valeur du profil (indépendante du mode) : sert à offrir le lien de retour
    profil_dispo <- {
      ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
      !is.null(ifa) && !is.na(inv_effectif(ifa))
    }
    lien_profil <- if (profil_dispo) lien_src("therm", "ifa", "\u21ba Profil") else NULL
    txt_repli <- if (!is.null(tr$theorique) && !is.na(tr$theorique))
      paste0("repli théorique (Shuter) : ", fmt_nb(tr$theorique, 1), " m")
    else "repli théorique (Shuter) indisponible sans T° air"
    if (identical(m, "manual")) {
      v <- input$therm_manual
      if (is.null(v) || is.na(v)) ligne_src(paste0("Champ vide — ", txt_repli), list(lien_profil))
      else ligne_src("Saisie manuelle", list(lien_profil))
    } else if (!is.na(tr$value)) {
      ligne_src(paste0("Profil du ", tr$date_label))
    } else {
      motif <- sub("^IFA( \\([^)]*\\))? ?: ?", "", tr$source)
      if (grepl("non chargé", motif)) motif <- "aucun profil"
      ligne_src(paste0(if (nzchar(motif)) paste0(maj1(motif), " — ") else "", txt_repli))
    }
  })

  # --- O2 (Omble) -----------------------------------------------------------
  output$src_o2_metres_sous_5ppm <- renderUI({
    m  <- mode_src$o2()
    o2 <- tryCatch(o2_resolved(), error = function(e) NULL)
    req(!is.null(o2))
    profil_dispo <- {
      ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
      !is.null(ifa) && !is.na(inv_effectif(ifa))
    }
    lien_profil <- if (profil_dispo) lien_src("o2", "ifa", "\u21ba Profil") else NULL
    if (identical(m, "manual")) {
      v <- input$o2_metres_sous_5ppm
      if (is.null(v) || is.na(v))
        ligne_src("Champ vide — réduction O\u2082 non appliquée", list(lien_profil))
      else ligne_src("Saisie manuelle", list(lien_profil))
    } else if (!is.na(o2$value)) {
      date_lbl <- sub("^IFA \\(([^)]*)\\).*$", "\\1", o2$source)
      ligne_src(paste0("Profil du ", date_lbl))
    } else {
      ligne_src("Aucun profil d'oxygène exploitable — réduction O\u2082 non appliquée")
    }
  })

  # --- Messages climatiques --------------------------------------------------
  # extraire_climat() renvoie un motif technique préfixé de l'année
  # (« 2024 : cellule sans valeur (NoData)… »). On le traduit en consigne
  # courte, orientée vers l'action. Cas fréquents dans les fichiers réels :
  #   - coordonnées absentes de l'onglet Lacs (ex. R08 : 988 lacs sur 2 969) ;
  #   - point hors de la grille Info-Climat, qui ne couvre que le Québec (ex.
  #     lac Abitibi, dont le point de référence, à -79,71°, est à l'ouest de
  #     la frontière Québec–Ontario, vers -79,52°).
  motif_climat <- function(motif) {
    if (!isTRUE(pothal_actif_rv()) || !isTruthy(lac_courant_rv())) return("")
    if (is.null(motif) || is.na(motif)) return("Aucune donnée climatique")
    if (grepl("coordonn", motif))              return("Coordonnées absentes du fichier")
    if (grepl("NoData|emprise", motif))
      return("Aucune valeur climatique à l'emplacement du lac (hors de la grille Info-Climat)")
    if (grepl("aucun raster", motif))          return("Aucun raster climatique disponible")
    if (grepl("terra", motif))                 return("Extraction impossible (paquet terra absent)")
    "Extraction impossible"
  }
  msg_saisir <- function(motif, justification) {
    debut <- if (nzchar(motif)) paste0(motif, " — saisir la valeur") else "Saisir la valeur"
    div(class = "msg-var msg-warning", paste0(debut, " (", justification, ")."))
  }
  # Valeur obtenue mais certaines années de la fenêtre sans valeur au point
  alerte_annees <- function(motif, annees) {
    if (is.null(motif) || is.na(motif) || length(annees) == 0) return(NULL)
    paste0(maj1(motif), " à cet emplacement — moyenne sur les années disponibles.")
  }

  # --- T° air ---------------------------------------------------------------
  output$src_T_air <- renderUI({
    m  <- mode_src$tair()
    tr <- tair_resolu()
    lien_clim <- lien_src("tair", "climat", "\u21ba Info-Climat")
    justif <- if (identical(espece_active_rv(), "touladi"))
      "requise pour Lester 2021" else "facultative — sert au repli de la thermocline"
    if (identical(m, "manual")) {
      if (is.na(tr$value)) ligne_src(paste0("Champ vide — ", justif), list(lien_clim))
      else ligne_src("Saisie manuelle", list(lien_clim))
    } else if (identical(tr$source, "climat")) {
      ligne_src(paste0("Info-Climat, ", fmt_annees(tr$annees)),
                alerte = alerte_annees(tr$motif, tr$annees))
    } else if (identical(tr$source, "lacs")) {
      ligne_src("Onglet Lacs (colonne temp_air_moy)")
    } else {
      msg_saisir(motif_climat(tr$motif), justif)
    }
  })

  # --- DJC5 (Doré) ------------------------------------------------------------
  output$src_degres_jours_g <- renderUI({
    m  <- mode_src$g()
    gr <- g_resolu()
    lien_clim <- lien_src("g", "climat", "\u21ba Info-Climat")
    # Bornes calées sur l'étendue de la grille Info-Climat (170 à 2777 sur
    # DJC5_2024), élargies : elles servent à attraper une valeur divisée par
    # 1000 par erreur (1,825 au lieu de 1825), pas à valider la donnée.
    alerte_plage <- if (!is.na(gr$value) && gr$value > 0 && (gr$value < 100 || gr$value > 3500))
      paste0("Hors plage attendue (100 à 3500) — vérifier que la valeur brute a été saisie, ",
             "non divisée par 1000.") else NULL
    if (identical(m, "manual")) {
      if (is.na(gr$value))
        ligne_src("Champ vide — modèle Lester 2002 indisponible", list(lien_clim))
      else ligne_src("Saisie manuelle", list(lien_clim), alerte = alerte_plage)
    } else if (!is.na(gr$value)) {
      ligne_src(paste0("Info-Climat, ", fmt_annees(gr$annees)),
                alerte = c(alerte_plage, alerte_annees(gr$motif, gr$annees))[1])
    } else {
      msg_saisir(motif_climat(gr$motif), "requise pour Lester 2002")
    }
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
  # Filtre sur le lac choisi dans le bloc « Données d'exploitation »
  # (cle_lac_exploit), indépendant du lac du potentiel halieutique depuis 2026-09.
  exploit_filtered <- reactive({
    cle <- cle_lac_exploit()
    if (is.na(cle)) return(NULL)
    er <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(er)) return(NULL)
    er %>%
      filter(normaliser_nolac(nolac)   == cle,
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
  # La superficie est celle du lac d'exploitation (sup_exploit), pas
  # forcément celle du lac du potentiel halieutique.
  exploit_data_kpi <- reactive({
    req(exploit_edited_rv())
    req(!is.na(sup_exploit()))
    df <- ajouter_indicateurs(exploit_edited_rv(), sup_exploit())
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
    req(!is.na(sup_exploit()))
    df <- exploit_edited_rv() %>% arrange(annee)
    coches <- graph_annees_rv()
    if (!is.null(coches) && length(coches) > 0)
      df <- df[df$annee %in% coches, ]
    ajouter_indicateurs(df, sup_exploit())
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
  # MESSAGES SOUS LES VARIABLES (remplace le bloc « Contrôle qualité »)
  #
  #   Chaque message s'affiche directement sous la variable concernée, dès
  #   qu'un lac est identifié (liste, nom saisi ou calcul lancé) — pas au
  #   démarrage, pour ne pas couvrir un formulaire vide d'avertissements.
  #   Les messages de source (repli Shuter, Linf de Lester, T° air/DJC5,
  #   profil O2) sont désormais portés par la ligne de source du champ
  #   (output$src_*) et ne sont pas répétés ici.
  #   Mêmes critères que model_availability() : un message « danger »
  #   correspond toujours à un modèle réellement bloqué.
  # ---------------------------------------------------------------------------
  msg_ui <- function(niveau, texte) div(class = paste0("msg-var msg-", niveau), texte)

  afficher_msgs <- reactive({
    isTruthy(lac_courant_rv()) || isTruthy(input$nom_lac) ||
      isTruthy(input$no_lac)   || isTRUE(a_calcule_rv())
  })

  # Nom du lac obligatoire : signalé seulement après une tentative de calcul
  nom_requis_rv <- reactiveVal(FALSE)
  output$msg_nom <- renderUI({
    if (isTRUE(nom_requis_rv()) && !isTruthy(trimws(input$nom_lac %||% "")))
      msg_ui("danger", "Nom du lac requis pour lancer le calcul.")
  })

  # Unités d'évaluation multiples dans l'onglet Lacs (IPE retenue avant OG)
  output$msg_lac <- renderUI({
    pb        <- tryCatch(pothal_brut(), error = function(e) NULL)
    nolac_val <- cle_lac(lac_courant_rv())
    if (is.null(pb) || is.na(nolac_val) || is.null(pb$lacs_brutes) ||
        !all(c("nolac", "ue_min") %in% names(pb$lacs_brutes))) return(NULL)
    sous <- pb$lacs_brutes[!is.na(pb$lacs_brutes$nolac) & pb$lacs_brutes$nolac == nolac_val, ]
    if (nrow(sous) <= 1) return(NULL)
    suff   <- toupper(trimws(sub(".*-", "", sous$ue_min)))
    retenu <- if ("IPE" %in% suff) "IPE" else if ("OG" %in% suff) "OG" else suff[1]
    msg_ui("info", paste0("Plusieurs UE trouvées (", paste(unique(suff), collapse = ", "),
                          ") — ", retenu, " retenue."))
  })

  output$msg_morpho <- renderUI({
    if (!afficher_msgs()) return(NULL)
    sup_v <- input$sup; pmax_v <- input$prof_max; pmoy_v <- input$prof_moy
    # Regroupés sur une ligne pour ne pas empiler trois avertissements
    manquants <- c(if (is.na(sup_v)  || sup_v  <= 0) "superficie",
                   if (is.na(pmax_v) || pmax_v <= 0) "prof. max",
                   if (is.na(pmoy_v) || pmoy_v <= 0) "prof. moy.")
    tagList(
      if (length(manquants) > 0)
        msg_ui("danger", paste0("Manquant ou invalide : ", paste(manquants, collapse = ", "), ".")),
      if (!is.na(pmoy_v) && pmoy_v > 0 && !is.na(pmax_v) && pmax_v > 0 && pmoy_v >= pmax_v)
        msg_ui("danger", "Prof. moyenne \u2265 prof. max — donnée rejetée, modèles concernés indisponibles.")
    )
  })

  # Ligne de source d'un paramètre physico-chimique (conductivité, Secchi, pH).
  # Valeur la plus récente du lac (onglet Parametre) — choix confirmé
  # 2026-09. La date est affichée pour rendre visible un éventuel décalage
  # avec l'inventaire utilisé pour la thermocline.
  #   champ = valeur actuelle du champ ; fichier/source/date = valeur du fichier
  ligne_param <- function(champ, fichier, source, date, dec, suffixe = NULL) {
    if (is.null(fichier) || length(fichier) == 0 || is.na(fichier)) {
      if (is.null(champ) || is.na(champ)) return(NULL)
      return(div(class = "src-ligne", paste0(c("Saisie manuelle", suffixe), collapse = " \u00b7 ")))
    }
    txt_src <- if (identical(source, "Profil (repli)"))
      paste0("Profil du ", date, " (aucune mesure dans Parametre)")
    else paste0("Onglet Parametre", if (!is.na(date)) paste0(", ", date) else "")
    if (!is.null(champ) && !is.na(champ) && abs(champ - fichier) > 1e-6)
      txt_src <- paste0("Modifié (fichier : ", fmt_nb(fichier, dec), " — ", txt_src, ")")
    div(class = "src-ligne", paste0(c(txt_src, suffixe), collapse = " \u00b7 "))
  }

  output$msg_conductivite <- renderUI({
    if (!afficher_msgs()) return(NULL)
    if (!is.na(input$conductivite) && input$conductivite > 0) {
      lg <- inv_ligne_courante()
      # Conversion affichée : c'est le SDT qui entre dans les modèles
      sdt <- paste0("\u2248 ", fmt_nb(conductivite_vers_tds(input$conductivite), 1), " mg/L (SDT)")
      return(ligne_param(input$conductivite, lg$cond, lg$cond_source, lg$cond_date, 1,
                         suffixe = sdt))
    }
    noms_tds <- vapply(config()$modeles, function(m)
      if ("TDS" %in% m$intrants) m$nom else NA_character_, character(1))
    noms_tds <- noms_tds[!is.na(noms_tds)]
    if (length(noms_tds) > 0)
      msg_ui("warning", paste0("Manquante — ", paste(noms_tds, collapse = ", "),
                               " indisponible(s)."))
  })

  output$msg_secchi <- renderUI({
    if (!afficher_msgs()) return(NULL)
    if (is.na(input$secchi) || input$secchi <= 0)
      return(msg_ui("warning", "Manquante — Lester 2002 indisponible."))
    lg <- inv_ligne_courante()
    ligne_param(input$secchi, lg$secchi, lg$secchi_source, lg$secchi_date, 1)
  })

  output$msg_ph <- renderUI({
    if (!afficher_msgs()) return(NULL)
    if (is.na(input$ph_eau))
      return(msg_ui("info", "Non saisi — réduction « pH < 5 » non appliquée."))
    lg <- inv_ligne_courante()
    ligne_param(input$ph_eau, lg$ph, lg$ph_source, lg$ph_date, 2)
  })

  output$msg_tributaire <- renderUI({
    if (!afficher_msgs()) return(NULL)
    if (identical(input$tributaire_emissaire_omble, "inconnu"))
      msg_ui("info", "Inconnu — réduction de 25 % (Valin/Vaillancourt) non appliquée.")
  })

  output$msg_especes_omble <- renderUI({
    grp <- especes_groupes_omble(input$especes_presentes_omble)
    non_couv <- setdiff(grp$brutes, c(ESPECES_COUVERTES_ARCHAMBAULT, "allopatrie"))
    if (length(non_couv) > 0)
      msg_ui("info", paste0("Espèce(s) non couverte(s) par Archambault (",
                            paste(non_couv, collapse = ", "),
                            ") — modèle indisponible si aucune espèce couverte n'est cochée."))
  })

  # État des rasters climatiques (répertoire du dépôt)
  output$msg_climat <- renderUI({
    cd <- catalogue_climat()$catalogue
    if (is.null(cd) || nrow(cd) == 0)
      return(msg_ui("warning", paste0("Aucun raster climatique dans « ", CLIMAT_REPERTOIRE,
                                      " » — saisir les valeurs manuellement.")))
    if (!terra_disponible())
      return(msg_ui("danger", "Paquet terra non installé — extraction climatique impossible."))
    NULL
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
    # Nom du lac obligatoire (identification des résultats et des exports)
    if (!isTruthy(trimws(input$nom_lac %||% ""))) {
      nom_requis_rv(TRUE)
      showNotification("Nom du lac requis pour lancer le calcul.", type = "warning", duration = 4)
      return()
    }
    nom_requis_rv(FALSE)
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
         input$degres_jours_g, mode_src$g(), mode_src$tair(),
         input$climat_fenetre,
         input$conductivite, input$secchi, input$linf_manual, mode_src$linf(),
         mode_src$therm(), input$therm_manual, input$ifa_inv_choisi,
         input$especes_presentes_omble, input$ph_eau, input$perimetre,
         mode_src$o2(), input$o2_metres_sous_5ppm,
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
    validate(need(isTruthy(trimws(input$nom_lac %||% "")), "Nom du lac requis."))
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

  # Nom complet du fichier importé, affiché sous le bouton et libre de passer
  # à la ligne — le champ natif du fileInput est masqué (voir CSS).
  nom_fichier_ui <- function(f) {
    if (is.null(f) || is.null(f$name) || !nzchar(f$name)) return(NULL)
    div(class = "file-name-line",
        span(class = "fn-ic", "\u2713"), span(f$name))
  }
  output$pothal_file_name <- renderUI({
    if (!isTRUE(pothal_actif_rv()) || is.null(tryCatch(pothal_brut(), error = function(e) NULL)))
      return(NULL)
    nom_fichier_ui(input$pothal_file)
  })
  output$exploit_file_name <- renderUI({ nom_fichier_ui(input$exploit_file) })

  # Indicateurs lus par les conditionalPanel() (fichiers chargés ou non)
  output$pothal_charge <- reactive({
    isTRUE(pothal_actif_rv()) && !is.null(tryCatch(pothal_brut(), error = function(e) NULL))
  })
  outputOptions(output, "pothal_charge", suspendWhenHidden = FALSE)
  output$exploit_charge <- reactive({
    !is.null(tryCatch(exploit_raw(), error = function(e) NULL))
  })
  outputOptions(output, "exploit_charge", suspendWhenHidden = FALSE)

  # Sélecteur d'inventaire (profil) : seulement si le lac en compte plus d'un.
  # Il pilote la thermocline (Touladi, Doré) et l'O2 (Omble).
  libelle_inventaire <- function(esp)
    if (identical(esp, "omble")) "Inventaire (O\u2082)" else "Inventaire (thermocline)"
  observeEvent(espece_active_rv(), {
    updateSelectInput(session, "ifa_inv_choisi", label = libelle_inventaire(espece_active_rv()))
  }, ignoreInit = TRUE)

  output$ifa_inv_ui <- renderUI({
    ifa <- tryCatch(ifa_habitat_raw(), error = function(e) NULL)
    if (is.null(ifa) || is.null(ifa$inventaires)) return(NULL)
    invs <- ifa$inventaires
    if (nrow(invs) <= 1) return(NULL)   # un seul inventaire → pas besoin de sélecteur

    # Étiquettes : date + nb points + indicateur de validité du profil
    choix <- setNames(
      invs$inv_key,
      paste0(invs$date_label,
             ifelse(invs$profil_valide,
                    paste0("  (", invs$n_pts, " pts \u2713)"),
                    paste0("  (", invs$n_pts, " pt(s) — sans profil)")))
    )
    # Libellé explicite sur ce que pilote l'inventaire : la thermocline
    # (Touladi, Doré) ou le profil d'oxygène (Omble). Conductivité, Secchi et
    # pH n'en dépendent pas (valeur la plus récente du lac — choix 2026-09).
    # isolate() : changer d'espèce ne doit pas recréer le sélecteur (le choix
    # d'inventaire serait perdu) — le libellé est mis à jour par l'observateur
    # ci-dessous.
    lib_inv <- libelle_inventaire(isolate(espece_active_rv()))
    div(class = "champ",
      selectInput("ifa_inv_choisi", label = lib_inv,
                  choices  = choix,
                  selected = ifa$inv_defaut,
                  width    = "100%"))
  })
  # Indispensable : vide, ce conteneur est masqué par le CSS (:empty, pour ne
  # pas laisser de trou dans la grille). Or Shiny suspend par défaut les
  # sorties masquées : le sélecteur n'était donc jamais rendu, et le conteneur
  # restait vide et masqué (interblocage). On force le rendu même masqué.
  outputOptions(output, "ifa_inv_ui", suspendWhenHidden = FALSE)

  observeEvent(exploit_raw(), {
    warnings_actifs_rv(TRUE)
  }, ignoreInit = TRUE)

  # ---------------------------------------------------------------------------
  # IMPORT — « Effacer l'importation »
  #   Remplace « Saisir un lac manuellement » : on revient au formulaire vide,
  #   avec les champs Nom / No lac visibles pour une saisie manuelle.
  # ---------------------------------------------------------------------------
  observeEvent(input$btn_effacer_import, {
    pothal_actif_rv(FALSE)
    session$sendCustomMessage("reinit_fichier", "pothal_file")
    reinitialiser_lac()
  })


  # ---------------------------------------------------------------------------
  # OUTPUTS — Onglet 1 : Rendements théoriques
  # ---------------------------------------------------------------------------

  # Rangée 1 : 3 cartes KPI — Maximal théorique -> Recommandé (80 %) -> Observé
  # Fil d'Ariane : espèce active + lac sélectionné, et avertissement visible
  # si le lac d'exploitation n'est pas celui du potentiel halieutique : le
  # rendement observé (kg/ha) serait alors comparé au maximum théorique d'un
  # autre lac.
  output$fil_ariane <- renderUI({
    p   <- config()$palette
    esp <- config()$nom
    nom <- trimws(input$nom_lac %||% "")
    no  <- trimws(input$no_lac  %||% "")
    lac_txt <- if (nzchar(no) && nzchar(nom)) paste0(no, " \u2014 ", nom)
               else if (nzchar(nom)) nom else if (nzchar(no)) no else NULL

    nom_exploit <- function(cle) {
      lst <- lacs_exploit()
      nm  <- if (!is.null(lst)) lst$nomlac[lst$nolac == cle][1] else NA_character_
      if (!is.na(nm) && nzchar(nm)) paste0(cle, " \u2014 ", nm) else cle
    }
    alerte <- switch(correspondance_lacs(),
      different = div(class = "alerte-lacs",
        tags$strong("\u26a0 Lacs différents. "),
        paste0("Potentiel halieutique : ", lac_txt %||% "—",
               "  \u00b7  Exploitation : ", nom_exploit(cle_lac_exploit()),
               ". Les rendements observés ne portent pas sur le même lac que le calcul théorique.")),
      non_verifiable = div(class = "alerte-lacs",
        tags$strong("\u26a0 Correspondance non vérifiable. "),
        paste0("Aucun numéro de lac saisi pour le potentiel halieutique — vérifier que le lac ",
               "d'exploitation (", nom_exploit(cle_lac_exploit()), ") est bien le même.")),
      NULL)

    tagList(
      div(
        style = paste0("display:flex; align-items:center; gap:8px; font-size:13px; ",
                       "margin-bottom:12px; padding:6px 12px; background:#FFFFFF; ",
                       "border-radius:5px; border-left:4px solid ", p$accent, ";"),
        span(style = paste0("display:inline-block; width:9px; height:9px; ",
                            "border-radius:50%; background:", p$accent, ";")),
        tags$strong(esp),
        if (!is.null(lac_txt)) span(style = "color:#5a6b7b;", paste0("\u2022  ", lac_txt))
        else span(style = "color:#9aa7b2;", "\u2022  aucun lac sélectionné")
      ),
      alerte
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

    tip_quota_est <- paste0("Quota annuel = taux d'exploitation × rendement maximal théorique ",
                            "du modèle de référence × superficie du lac. Le taux recommandé de ",
                            PCT_RECOMMANDE, " % suit le concept de « pretty good yield » ",
                            "(Hilborn 2010) : pêcher à ~80 % du maximum conserve l'essentiel ",
                            "du rendement tout en réduisant nettement le risque de ",
                            "surexploitation. À un autre taux, le quota est dit « ajusté ».")

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
    # Libellé : « recommandé » uniquement au taux recommandé (80 %). À tout
    # autre taux choisi par l'utilisateur, la valeur n'est plus celle que
    # recommande la méthode — elle est dite « ajustée », et le quota
    # recommandé est rappelé en dessous pour comparaison.
    au_taux_reco <- isTRUE(pct == PCT_RECOMMANDE)
    lib_quota    <- if (au_taux_reco) "Quota recommandé" else paste0("Quota ajusté (", pct, " %)")
    lib_compare  <- if (au_taux_reco) "quota recommandé" else "quota ajusté"
    quota_reco   <- if (calc_fait && !is.na(max_ha) && !is.na(sup_v) && sup_v > 0)
                      round(max_ha * PCT_RECOMMANDE / 100 * sup_v) else NA_real_
    ligne_rappel <- if (!au_taux_reco && !is.na(quota_reco))
      tags$small(class = "kpi-sub d-block",
                 paste0("Recommandé (", PCT_RECOMMANDE, " %) : ", fmt_int(quota_reco), " kg/an"))
    else NULL
    zone_pct <- zone_taux(pct)
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
          info_tip(lib_quota, tip_quota_est)),
      div(span(class = "quota-hero-val", if (is.na(quota_est)) "—" else fmt_int(quota_est)),
          span(class = "quota-hero-unit", "kg/an")),
      ligne_derivation,
      ligne_rappel,
      ligne_modele,
      note_ime_ref
    )

    # --- Quota actuel : même unité, même taille, écart avec l'estimation ------
    quota_val <- quota_actuel_val()
    carte_act <- if (is.na(quota_val)) {
      div(class = "kpi-top-card",
        div(class = "kpi-eyebrow", "Quota actuel"),
        div(span(class = "quota-hero-val", style = "color:#adb5bd;", "—"),
            span(class = "quota-hero-unit", "kg/an")),
        tags$small(class = "kpi-sub mt-2 d-block", "Non saisi"))
    } else {
      ecart <- if (!is.na(quota_est)) quota_val - quota_est else NA_real_
      note_ecart <- if (is.na(ecart)) {
        tags$small(class = "kpi-sub mt-2 d-block", "Calculer pour comparer")
      } else if (abs(ecart) < 1) {
        tags$small(class = "kpi-sub mt-2 d-block", paste0("Équivalent au ", lib_compare))
      } else {
        # Au-dessus de l'estimation = signal d'attention (orange) ; en dessous =
        # neutre (gris). Mêmes couleurs que les zones de la barre, pas de vert :
        # être sous l'estimation n'est pas un « bon » résultat en soi.
        # Orange au-dessus du recommandé, rouge au-delà du maximum théorique
        pct_a <- if (!is.na(max_ha) && !is.na(sup_v) && max_ha > 0 && sup_v > 0)
                   quota_val / (max_ha * sup_v) * 100 else NA_real_
        col <- if (ecart <= 0) ZONES$conservateur$trait
               else if (!is.na(pct_a) && pct_a > 100) ZONES$depasse$trait
               else ZONES$eleve$trait
        tags$small(class = "kpi-sub mt-2 d-block fw-bold",
                   style = paste0("color:", col, ";"),
                   paste0(fmt_int(abs(ecart)), " kg/an ",
                          if (ecart > 0) "au-dessus du " else "sous le ", lib_compare))
      }
      # Position du quota actuel en % du maximum théorique : même lecture que
      # la barre de positionnement (zones à 80 % et 90 %)
      pct_act <- if (calc_fait && !is.na(max_ha) && !is.na(sup_v) && max_ha > 0 && sup_v > 0)
                   quota_val / (max_ha * sup_v) * 100 else NA_real_
      div(class = "kpi-top-card",
        div(class = "kpi-eyebrow", "Quota actuel"),
        div(span(class = "quota-hero-val", fmt_int(quota_val)),
            span(class = "quota-hero-unit", "kg/an")),
        note_ecart,
        if (!is.na(pct_act))
          tags$small(class = "kpi-sub d-block",
                     style = if (pct_act > 100) paste0("color:", ZONES$depasse$trait, "; font-weight:600;") else NULL,
                     paste0(fmt_int(pct_act), " % du maximum théorique",
                            if (pct_act > 100) " — au-delà du maximum" else "")))
    }

    div(class = "kpi-top-row", carte_est, carte_act)
  })

  # ---------------------------------------------------------------------------
  # DONNÉES UTILISÉES — provenance des intrants du modèle de référence
  #
  #   Résume, sous le quota, d'où vient chaque donnée du modèle de référence
  #   et signale les valeurs théoriques (replis) : le quota n'a pas la même
  #   solidité s'il repose sur des mesures récentes ou sur des formules de
  #   repli. Affiché seulement quand le calcul est à jour : tout changement
  #   d'intrant ou de source rend le calcul périmé (voir l'observateur de
  #   péremption), donc l'état des sources lu ici est celui du calcul.
  #   Chaque élément : list(texte, theorique = TRUE/FALSE).
  # ---------------------------------------------------------------------------
  linf_libelle <- function() {
    lr <- linf_resolved()
    if (is.na(lr$value)) return(list(txt = "formule de Lester (aucune donnée de spécimens)", theo = TRUE))
    switch(mode_src$linf(),
      manual    = list(txt = "saisie manuelle", theo = FALSE),
      theorique = list(txt = "formule de Lester (choisie)", theo = TRUE),
      list(txt = paste0("spécimens, n = ", lr$n), theo = FALSE))
  }

  source_param <- function(lg, cle, champ) {
    fich <- lg[[cle]]; src <- lg[[paste0(cle, "_source")]]; dt <- lg[[paste0(cle, "_date")]]
    if (is.null(fich) || is.na(fich)) return("saisie manuelle")
    if (!is.na(champ) && abs(champ - fich) > 1e-6) return("modifiée manuellement")
    if (identical(src, "Profil (repli)")) paste0("profil ", dt)
    else paste0("Parametre", if (!is.null(dt) && !is.na(dt)) paste0(" ", dt) else "")
  }

  output$intrants_ui <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))
    if (!calc_fait) return(NULL)
    r     <- results()
    ref_m <- modele_reference(r, config()$cascade_reference, config()$modeles)
    if (is.null(ref_m)) return(NULL)
    intr  <- config()$modeles[[ref_m$cle]]$intrants
    res   <- r[[ref_m$cle]]
    lg    <- inv_ligne_courante()

    el <- function(txt, theo = FALSE) list(txt = txt, theo = isTRUE(theo))
    items <- lapply(intr, function(k) switch(k,
      A    = el(paste0("Superficie ", fmt_int(r$sup), " ha")),
      Dmax = el(paste0("Prof. max ", fmt_nb(r$prof_max, 1), " m")),
      Dmn  = el(paste0("Prof. moy. ", fmt_nb(r$prof_moy, 1), " m")),
      T_air = if (is.na(r$T_air)) el("T° air absente (pas de repli théorique possible)", TRUE) else {
        tr <- tair_resolu()
        src <- switch(tr$source, climat = paste0("Info-Climat, ", fmt_annees(tr$annees)),
                      lacs = "onglet Lacs", "saisie manuelle")
        el(paste0("T° air ", fmt_nb(r$T_air, 2), " °C (", src, ")"))
      },
      Linf = {
        ll <- linf_libelle()
        el(paste0("Linf ", fmt_int(r$linf_value), " mm (", ll$txt, ")"), ll$theo)
      },
      Dth_obs = {
        dsrc <- res$Dth_source %||% ""
        if (grepl("^observ", dsrc)) {
          tr  <- tryCatch(therm_resolved(), error = function(e) NULL)
          src <- if (identical(mode_src$therm(), "manual")) "saisie manuelle"
                 else if (!is.null(tr$date_label)) paste0("profil ", tr$date_label) else "profil"
          el(paste0("Thermocline ", fmt_nb(res$Dth, 1), " m (", src, ")"))
        } else if (grepl("^th", dsrc)) {
          el(paste0("Thermocline ", fmt_nb(res$Dth, 1), " m (repli théorique, Shuter)"), TRUE)
        } else el("Lac traité comme non stratifié (aucune thermocline)", TRUE)
      },
      TDS = el(paste0("SDT ", fmt_nb(r$tds, 1), " mg/L (conductivité ",
                      fmt_nb(input$conductivite, 1), " µS/cm, ",
                      source_param(lg, "cond", input$conductivite), ")")),
      z_sec = el(paste0("Secchi ", fmt_nb(input$secchi, 1), " m (",
                        source_param(lg, "secchi", input$secchi), ")")),
      G = {
        gr  <- g_resolu()
        src <- if (identical(gr$source, "climat")) paste0("Info-Climat, ", fmt_annees(gr$annees))
               else "saisie manuelle"
        el(paste0("DJC5 ", fmt_nb(gr$value, 0), " (", src, ")"))
      },
      grp = {
        esp <- input[[paste0("especes_presentes_", espece_active_rv())]]
        el(if (length(esp) == 0) "Aucune autre espèce cochée"
           else paste0(length(esp), " espèce(s) cochée(s)"))
      },
      pH = if (!is.na(input$ph_eau))
             el(paste0("pH ", fmt_nb(input$ph_eau, 2), " (", source_param(lg, "ph", input$ph_eau), ")"))
           else el("pH non saisi (réduction non appliquée)"),
      o2_pct_reduction = {
        o2 <- tryCatch(o2_resolved(), error = function(e) NULL)
        if (is.null(o2) || is.na(o2$value)) el("O\u2082 : aucune donnée (réduction non appliquée)")
        else el(paste0("O\u2082 : ", fmt_nb(o2$value, 1), " m sous 5 ppm"))
      },
      tributaire_absent = el(paste0("Tributaire/émissaire : ",
                                    input$tributaire_emissaire_omble %||% "inconnu")),
      nb_chalets = if (!is.na(input$nb_chalets_omble %||% NA))
                     el(paste0(fmt_int(input$nb_chalets_omble), " camp(s)/chalet(s)")) else NULL,
      NULL))
    items <- Filter(Negate(is.null), items)
    n_theo <- sum(vapply(items, `[[`, logical(1), "theo"))

    div(class = "intrants-box",
      div(class = "intrants-titre",
          # Le nom du modèle figure déjà sur la carte du quota : pas de répétition
          span("Données utilisées"),
          if (n_theo == 0) span(class = "intrants-statut ok", "aucune valeur théorique")
          else span(class = "intrants-statut theo",
                    paste0("\u26a0 ", n_theo, " valeur", if (n_theo > 1) "s" else "",
                           " théorique", if (n_theo > 1) "s" else ""))),
      div(class = "intrants-liste",
        lapply(seq_along(items), function(i) tagList(
          if (i > 1) span(class = "intrants-sep", " \u00b7 "),
          span(class = if (items[[i]]$theo) "intrants-theo" else NULL, items[[i]]$txt))))
    )
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

  # ---------------------------------------------------------------------------
  # FIGURE — Comparaison des modèles (kg/an)   [remplace la bande, 2026-09]
  #
  #   Graphique à points : un point par modèle (maximum théorique × superficie),
  #   sur un axe commun partant de ZÉRO — l'ancienne bande « zoomait » l'échelle,
  #   ce qui exagérait la zone verte et écrasait la grise. Tout est en kg/an,
  #   l'unité de la décision (mêmes chiffres que les cartes de quota).
  #   Repères verticaux :
  #     - aucune zone colorée : la bande verte 80-90 % (borne de 90 % non
  #       sourcée) puis la zone rouge « au-delà du maximum » ont été retirées ;
  #       les étiquettes Actuel / Rendement observé passent en rouge au-delà
  #       de 100 % du maximum de la référence ;
  #     - quota recommandé (80 %) et, si le taux diffère, quota ajusté (taux
  #       saisi dans la carte — l'utilisateur garde la liberté de simuler) ;
  #     - quota actuel (pointillé) ;
  #     - rendement observé moyen (kg/an), seulement si le lac d'exploitation
  #       est le même que celui du calcul (sinon elle porterait sur un autre lac).
  #   Points pleins = modèles calculés à partir des données du lac ; cercles
  #   vides = grilles régionales (valeurs fixes).
  #   Implémentation HTML (positions en %) plutôt que SVG : le texte garde une
  #   taille fixe quelle que soit la largeur de l'écran.
  # ---------------------------------------------------------------------------
  output$gauge_rdr_ui <- renderUI({
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))
    if (!calc_fait) return(NULL)

    r   <- results()
    sup <- r$sup
    req(!is.na(sup) && sup > 0)
    ref_m <- modele_reference(r, config()$cascade_reference, config()$modeles)
    req(!is.null(ref_m), !is.na(ref_m$val), ref_m$val > 0)

    max_ref <- ref_m$val * sup                       # kg/an
    pct     <- pct_rv()
    q_reco  <- max_ref * PCT_RECOMMANDE / 100
    q_aj    <- if (!isTRUE(pct == PCT_RECOMMANDE)) max_ref * pct / 100 else NA_real_
    q_act   <- quota_actuel_val()

    # Rendement observé (kg/an = masse récoltée annuelle moyenne) sur la
    # fenêtre choisie, même lac seulement
    base_exp  <- tryCatch(exploit_edited_rv(), error = function(e) NULL)
    a_recolte <- !is.null(base_exp) && nrow(base_exp) > 0
    df_obs    <- tryCatch(exploit_data_kpi(), error = function(e) NULL)
    meme_lac  <- correspondance_lacs() %in% c("identique", "non_verifiable")
    q_obs <- if (meme_lac && !is.null(df_obs) && nrow(df_obs) > 0)
               mean(df_obs$masse_totale_kg, na.rm = TRUE) else NA_real_
    if (!is.finite(q_obs)) q_obs <- NA_real_

    # Lignes : modèles de l'espèce (ordre de la cascade, référence en tête),
    # puis grilles régionales
    keys <- config()$cascade_reference
    lignes_mod <- lapply(keys, function(k) {
      res <- r[[k]]
      v   <- if (!is.null(res) && !is.na(res$rendement_ha)) res$rendement_ha * sup else NA_real_
      list(nom = config()$modeles[[k]]$nom, val = as.numeric(v),
           ha = if (is.na(v)) NA_real_ else as.numeric(res$rendement_ha),
           ref = identical(k, ref_m$cle), regional = FALSE)
    })
    lignes_reg <- lapply(regions_actives(), function(g)
      list(nom = g$nom, val = if (is.na(g$val)) NA_real_ else as.numeric(g$val) * sup,
           ha = as.numeric(g$val),
           ref = FALSE, regional = TRUE))

    # Échelle : 0 -> valeur maximale affichée + 8 %, arrondie à une graduation « propre »
    tous  <- c(vapply(c(lignes_mod, lignes_reg), `[[`, numeric(1), "val"), q_act, q_obs, q_aj, max_ref)
    haut  <- max(tous, na.rm = TRUE) * 1.08
    ticks <- pretty(c(0, haut), n = 5)
    xmax  <- max(ticks)
    xp    <- function(v) paste0(round(100 * v / xmax, 2), "%")

    # --- Étiquettes des repères (deux rangées pour éviter les chevauchements)
    #   rangée 1 : Recommandé (ancré à droite du trait) | Actuel (à gauche)
    #   rangée 2 : Ajusté (ancré à droite)              | Observé (à gauche)
    etiq <- function(v, txt, cote, couleur) if (is.na(v)) NULL else
      div(class = "fm-etiq",
          style = paste0("left:", xp(v), "; color:", couleur, ";",
                         if (cote == "g") "transform:translateX(calc(-100% - 5px));"
                         else "transform:translateX(5px);"),
          txt)
    pct_max <- function(v) paste0(" (", fmt_int(100 * v / max_ref), " %)")
    # Les étiquettes sont dans une « piste » alignée sur celle des points, pour
    # que leurs positions en % se rapportent au même axe.
    rangee <- function(...) div(class = "fm-rangee", div(), div(class = "fm-piste", ...))
    rangee1 <- rangee(
      etiq(q_reco, paste0("Recommandé ", fmt_int(q_reco)), "g", "#3B6D11"),
      etiq(q_act,  paste0("Actuel ", fmt_int(q_act), pct_max(q_act)), "d",
           if (!is.na(q_act) && q_act > max_ref) ZONES$depasse$trait else "#34495E"))
    rangee2 <- if (!is.na(q_aj) || !is.na(q_obs)) rangee(
      etiq(q_aj,  paste0("Ajusté ", pct, " % : ", fmt_int(q_aj)), "g", "#5a6b7b"),
      etiq(q_obs, paste0("Rendement observé ", fmt_int(q_obs), pct_max(q_obs)), "d",
           if (!is.na(q_obs) && q_obs > max_ref) ZONES$depasse$trait else "#8a5a00"))
    else NULL
    # Les traits verticaux remontent jusqu'aux étiquettes
    h_etiq <- 18L * (1L + as.integer(!is.null(rangee2)))

    # --- Repères verticaux (couche superposée à la zone des points) --------
    trait <- function(v, style) if (is.na(v)) NULL else
      div(class = "fm-trait", style = paste0("left:", xp(v), "; top:-", h_etiq, "px;", style))
    couche <- div(class = "fm-couche",
      trait(q_reco, "border-left:2px solid #3B6D11;"),
      trait(q_aj,   "border-left:2px solid #6c757d;"),
      trait(q_act,  "border-left:2px dashed #34495E;"),
      trait(q_obs,  "border-left:2px dotted #8a5a00;")
    )


    # --- Une rangée par modèle ------------------------------------------------
    rang <- function(l) {
      cls_nom <- paste0("fm-nom", if (l$ref) " ref" else "", if (l$regional) " reg" else "")
      point <- if (is.na(l$val)) span(class = "fm-indispo", "indisponible")
      else tagList(
        div(class = paste0("fm-point", if (l$ref) " ref" else "", if (l$regional) " reg" else ""),
            style = paste0("left:", xp(l$val), ";"),
            title = paste0(l$nom, " : maximum ", fmt_int(l$val), " kg/an (",
                           fmt_nb(l$ha), " kg/ha) — ", fmt_int(100 * l$val / max_ref),
                           " % de la référence")),
        # Valeur à droite du point, ou à gauche s'il est près du bord droit
        div(class = "fm-val",
            style = paste0("left:", xp(l$val), ";",
                           if (l$val / xmax > 0.85) "transform:translateX(calc(-100% - 10px));"
                           else "transform:translateX(10px);"),
            fmt_int(l$val)))
      div(class = "fm-ligne", div(class = cls_nom, l$nom, if (l$ref) span(class = "fm-tag", "réf.")),
          div(class = "fm-piste", point))
    }

    axe <- div(class = "fm-ligne fm-axe",
      div(class = "fm-nom"),
      div(class = "fm-piste",
        # Première graduation alignée à gauche, dernière à droite : elles ne
        # débordent plus de la piste (le « 1 000 » passait sur deux lignes)
        lapply(seq_along(ticks), function(i) div(class = "fm-tick",
          style = paste0("left:", xp(ticks[i]), ";",
                         if (i == 1) "transform:none;"
                         else if (i == length(ticks)) "transform:translateX(-100%);" else ""),
          fmt_int(ticks[i])))))
    titre_axe <- div(class = "fm-ligne fm-titre-axe", div(), div(class = "fm-piste", "kg/an"))

    # --- Pied : récolte observée (texte) + légende ------------------------------
    sel_fen <- fenetre_rv()
    toggle_n <- if (a_recolte && meme_lac) div(class = "fm-fenetre",
      radioButtons("kpi_obs_n", label = NULL,
        choices  = c("5 ans" = "5", "10 ans" = "10", "20 ans" = "20"),
        selected = if (!is.null(sel_fen)) sel_fen else "5", inline = TRUE)) else NULL
    # Le sélecteur de fenêtre est placé sur la ligne du rendement observé,
    # la seule valeur qu'il modifie
    ligne_obs <- div(class = "d-flex align-items-center gap-2 flex-wrap",
      tags$small(class = "kpi-sub",
        if (!a_recolte) "Rendement observé : aucune donnée d'exploitation pour ce lac"
        else if (!meme_lac) "Rendement observé : lac d'exploitation différent — non affiché"
        else if (is.na(q_obs)) "Rendement observé : aucune masse exploitable"
        else paste0("Rendement observé : moyenne (", min(df_obs$annee), "\u2013",
                    max(df_obs$annee), ") sur")),
      toggle_n)

    legende <- div(class = "fb-legend",
      div(class = "fb-leg-item", span(class = "fm-leg-point"), "Modèle"),
      div(class = "fb-leg-item", span(class = "fm-leg-point reg"), "Grille régionale"),
      # Pas de zone colorée (retirée à la demande, 2026-09) : la position est
      # portée par la couleur des étiquettes, expliquée dans cette bulle
      div(class = "fb-leg-item",
          info_tip("Couleurs", paste0(
            "Vert : quota recommandé (", PCT_RECOMMANDE, " % du maximum du modèle de référence, ",
            "« pretty good yield », Hilborn 2010). Rouge : au-delà de 100 % du maximum ",
            "théorique de la référence, soit une récolte supérieure au rendement maximal ",
            "soutenu estimé."))))

    div(class = "mb-3 fm-carte",
      tags$strong(style = "font-size:12.5px;", "Maximum théorique par modèle (kg/an)"),
      div(class = "fm-graph",
        rangee1, rangee2,
        div(class = "fm-corps", couche,
            lapply(lignes_mod, rang),
            if (length(lignes_reg) > 0) div(class = "fm-separateur"),
            lapply(lignes_reg, rang)),
        axe, titre_axe),
      div(style = paste0("display:flex; justify-content:space-between; align-items:flex-end; ",
                         "flex-wrap:wrap; gap:12px; border-top:0.5px solid #e9ecef; ",
                         "padding-top:9px; margin-top:6px;"),
          ligne_obs, legende)
    )
  })

  # Tableau — 4 colonnes : Modèle | Max théorique (kg/ha) |
  # Quota recommandé (kg/an, fixé à 80 % — indépendant de l'outil de calcul) | Note
  # La disponibilité (si applicable) est intégrée directement dans la colonne Note.
  output$table_modeles <- DT::renderDT({
    avail     <- model_availability()
    calc_fait <- isTRUE(calc_valide_rv()) &&
                 !is.null(tryCatch(results(), error = function(e) NULL))

    # Colonne « Comparer » retirée (2026-09) : tous les modèles et toutes les
    # grilles sont désormais visibles d'emblée sur la figure à points.

    opts_base <- list(dom = "t", paging = FALSE, ordering = FALSE,
                      language = list(emptyTable = "Aucun résultat"))

    # En-tête simple, partagé par les deux états (vide / calculé).
    # Colonnes : 0 Modèle | 1 Max (kg/ha) | 2 Quota recommandé (kg/an) | 3 Note
    # La 4e colonne est TOUJOURS calculée à PCT_RECOMMANDE, indépendamment du
    # taux saisi en haut de l'onglet : l'en-tête le dit explicitement pour
    # éviter qu'elle semble contredire le quota estimé lorsque le taux diffère.
    sketch <- htmltools::withTags(table(
      class = "display",
      thead(
        tr(
          th("Modèle"),
          th(class = "th-unit", "Max. théorique (kg/ha)"),
          th(class = "th-unit", title = paste0(
               "Quota recommandé de chaque modèle : ", PCT_RECOMMANDE, " % du maximum ",
               "théorique × superficie — toujours au taux recommandé, indépendamment ",
               "du taux saisi en haut de l'onglet."),
             "Quota recommandé (kg/an)"),
          th("Note")
        )
      )
    ))

    # Modèles de l'espèce active (ordre = cascade ; le 1er est le modèle de référence)
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
      # « référence » (et non « recommandé ») : c'est le modèle qui sert de
      # base au quota, le terme « recommandé » étant réservé au quota lui-même
      if (identical(k, ref_key)) paste0(lab, " (référence)") else lab
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

    # Note COURTE affichée (repli utilisé, mise en garde du modèle) ; le détail
    # technique (TOHA, P_T, B_rms, M...) passe dans une infobulle ⓘ, pour
    # alléger la lecture courante sans perdre la traçabilité du calcul.
    note_modele <- function(k) {
      res <- r[[k]]
      if (is.null(res) || is.na(res$rendement_ha)) return("")
      avert <- res$note %||% ""
      if (length(avert) != 1 || is.na(avert)) avert <- ""
      court <- switch(k,
        lester      = paste0("Linf : ", linf_libelle()$txt,
                             " · thermocline ", res$Dth_source %||% "—"),
        lester_savi = paste0("Thermocline ", res$Dth_source %||% "—"),
        ime         = "Rendement communautaire partitionné — peu prédictif dans les lacs à accès contrôlé du Québec (Loranger 1986)",
        vezina      = "Profondeur moyenne seule, sans ajustement pour les espèces",
        # Notes de base (parts reprises des infobulles tt_touladi_valin / tt_valin)
        shuter        = "Régression superficie × SDT",
        touladi_valin = "Indice morpho-édaphique, part Touladi 25 %",
        valin         = "Indice morpho-édaphique, part Doré 32 %",
        archambault = if (!is.null(res$categorie)) paste0("Catégorie : ", res$categorie) else "",
        omble_valin = if (!is.null(res$pct_especes)) paste0("Réduction espèces : ", res$pct_especes, " %") else "",
        "")
      paste(Filter(nzchar, c(court, if (nzchar(avert)) paste0("\u26a0 ", avert))), collapse = " · ")
    }
    note_cellule <- function(court, detail) {
      ic <- if (nzchar(detail))
        paste0(" <span class='info-ic' title=\"", gsub('"', "&quot;", detail), "\">\u24d8</span>")
      else ""
      paste0(court, ic)
    }
    note_mod <- vapply(keys_mod, function(k) {
      info <- avail[[k]]
      if (!is.null(info) && !isTRUE(info$ok)) return(paste0("Indisponible — ", info$note))
      note_cellule(note_modele(k), note_lib[[k]])
    }, character(1))

    all_vals <- c(val_mod, reg_vals)

    fmt_v  <- function(v) fmt_nb(v)
    # Quota recommandé : toujours fixé à 80 % (PCT_RECOMMANDE) du rendement maximal
    # théorique, indépendamment du % choisi dans l'outil de calcul du rendement.
    fmt_qn_reco <- function(v) fmt_int(v * PCT_RECOMMANDE / 100 * sup)


    reg_noms_tip <- if (length(reg_noms) > 0)
      vapply(reg_noms, function(nm) tip_mod(nm, tt_reg), character(1)) else character(0)

    df <- data.frame(
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
      container = sketch,
      options   = c(opts_base, list(
        ordering = FALSE,
        columnDefs = list(
          list(className = "dt-left",   targets = 0),
          list(className = "dt-center", targets = list(1, 2)),
          list(className = "dt-left",   targets = 3)
        )
      )),
      rownames = FALSE,
      class    = "table table-sm table-striped"
    )
    dt_out
  })

  output$table_note <- renderUI(NULL)   # ancienne consigne « Cocher un modèle… » (retirée)

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
    lac_ex <- !is.na(cle_lac_exploit())
    charge <- !is.null(exploit_edited_rv()) && lac_ex && !is.na(sup_exploit())

    if (!charge) {
      fichier_charge <- !is.null(tryCatch(exploit_raw(), error = function(e) NULL))
      titre <- if (!fichier_charge) "Importez les données d'exploitation"
               else if (!lac_ex)    "Choisissez un lac"
               else if (is.null(exploit_edited_rv())) "Aucune donnée de récolte pour ce lac"
               else "Superficie du lac inconnue"
      sous_titre <- if (!fichier_charge) "pour accéder à l'analyse temporelle."
               else if (!lac_ex) "dans le bloc « Données d'exploitation » du panneau de gauche."
               else if (is.null(exploit_edited_rv()))
                 "pour l'espèce active, dans ce fichier — analyse temporelle indisponible."
               else "rendement observé (kg/ha) non calculable — saisir la superficie."
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
    sup_val <- sup_exploit()
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

  # ---------------------------------------------------------------------------
  # LAC DU POTENTIEL HALIEUTIQUE — liste déroulante
  #   Uniquement les lacs de l'onglet Lacs du fichier Potentiel halieutique
  #   (révision 2026-09) : un lac présent seulement dans le fichier
  #   d'exploitation n'a aucune donnée pour les modèles. Le lac d'exploitation
  #   se choisit séparément, dans le bloc « Données d'exploitation ».
  # ---------------------------------------------------------------------------
  lacs_disponibles <- reactive({
    pb <- tryCatch(pothal_brut(), error = function(e) NULL)
    if (is.null(pb) || is.null(pb$lacs) || !("nolac" %in% names(pb$lacs))) return(NULL)
    tmp <- pb$lacs[!is.na(pb$lacs$nolac), ]
    if (nrow(tmp) == 0) return(NULL)
    df <- data.frame(
      nolac  = normaliser_nolac(tmp$nolac),
      nomlac = if ("nomlac" %in% names(tmp)) as.character(tmp$nomlac) else NA_character_,
      stringsAsFactors = FALSE
    )
    df <- df[!duplicated(df$nolac), ]
    # Tri par numéro croissant ; identifiants alphanumériques en fin de liste
    df[order(suppressWarnings(as.numeric(df$nolac)), df$nolac), ]
  })

  output$lac_select_ui <- renderUI({
    lacs <- lacs_disponibles()
    req(!is.null(lacs), nrow(lacs) > 0)
    etiquettes <- ifelse(!is.na(lacs$nomlac) & nzchar(lacs$nomlac),
                         paste0(lacs$nolac, " \u2014 ", lacs$nomlac), lacs$nolac)
    div(class = "champ",
      selectInput("lac_select", "Nom du lac",
                  choices  = c("— Choisir —" = "", setNames(lacs$nolac, etiquettes)),
                  selected = isolate(lac_courant_rv()),
                  width    = "100%"))
  })

  # Réinitialisation complète des champs (changement de lac, effacement)
  reinitialiser_lac <- function() {
    lac_courant_rv("")
    updateTextInput(session, "no_lac",  value = "")
    updateTextInput(session, "nom_lac", value = "")
    for (id in c("sup", "prof_max", "prof_moy", "perimetre", "conductivite", "secchi",
                 "ph_eau", "nb_chalets_omble",
                 "quota_actuel_touladi", "quota_actuel_dore", "quota_actuel_omble"))
      updateNumericInput(session, id, value = NA)
    updateRadioButtons(session, "tributaire_emissaire_omble", selected = "inconnu")
    updateCheckboxGroupInput(session, "especes_presentes_touladi", selected = character(0))
    updateCheckboxGroupInput(session, "especes_presentes_dore",    selected = character(0))
    updateCheckboxGroupInput(session, "especes_presentes_omble",   selected = character(0))
    # Champs sourcés : retour aux sources automatiques (qui se vident d'elles-
    # mêmes faute de lac) — ne jamais écrire directement dans ces champs.
    reinitialiser_modes_src()
    nom_requis_rv(FALSE)
    calc_valide_rv(FALSE)
  }

  observeEvent(input$lac_select, {
    # Dé-sélection (« — Choisir — ») : vider les champs
    if (!isTruthy(input$lac_select)) {
      if (isTruthy(lac_courant_rv())) reinitialiser_lac()
      return()
    }
    # Re-rendu programmatique sur le même lac : ne pas recharger
    if (identical(input$lac_select, lac_courant_rv())) return()

    lacs <- lacs_disponibles()
    row  <- if (!is.null(lacs)) lacs[lacs$nolac == input$lac_select, ] else NULL
    if (is.null(row) || nrow(row) == 0) return()
    cle <- as.character(row$nolac[1])

    # Champs propres à l'utilisateur (non fournis par le fichier) : remis à
    # vide ; la morphométrie et les paramètres sont réécrits par
    # l'observateur de ifa_habitat_raw(), les champs sourcés suivent leur source.
    for (id in c("nb_chalets_omble", "quota_actuel_touladi", "quota_actuel_dore",
                 "quota_actuel_omble"))
      updateNumericInput(session, id, value = NA)
    updateRadioButtons(session, "tributaire_emissaire_omble", selected = "inconnu")
    updateCheckboxGroupInput(session, "especes_presentes_touladi", selected = character(0))
    updateCheckboxGroupInput(session, "especes_presentes_dore",    selected = character(0))
    updateCheckboxGroupInput(session, "especes_presentes_omble",   selected = character(0))
    reinitialiser_modes_src()

    lac_courant_rv(cle)
    updateTextInput(session, "no_lac", value = cle)
    # Nom obligatoire pour calculer : repli sur « Lac <no> » si l'onglet Lacs
    # n'a pas de nom (les champs Nom/No sont masqués quand un fichier est chargé)
    updateTextInput(session, "nom_lac",
                    value = if (!is.na(row$nomlac[1]) && nzchar(row$nomlac[1]))
                              as.character(row$nomlac[1]) else paste0("Lac ", cle))
    nom_requis_rv(FALSE)

    # Paramètres sauvegardés pour ce lac (session) : ils priment sur le fichier
    p <- params_sauvegardes_rv()[[cle]]
    if (!is.null(p)) appliquer_params_sauvegardes(p)
  }, ignoreInit = TRUE)

  # Clé du lac « potentiel halieutique » : lac de la liste si un fichier est
  # chargé, sinon le numéro saisi manuellement (NA s'il est vide).
  cle_lac_potentiel <- reactive({
    if (isTRUE(pothal_actif_rv()) && isTruthy(lac_courant_rv())) cle_lac(lac_courant_rv())
    else cle_lac(input$no_lac)
  })

  # ---------------------------------------------------------------------------
  # LAC D'EXPLOITATION — sélecteur propre au bloc « Données d'exploitation »
  #   Liste : lacs du fichier d'exploitation ayant des données pour l'espèce
  #   active. Présélection : le lac du potentiel halieutique s'il figure dans
  #   la liste (à l'import du fichier, au changement de lac ou d'espèce) ;
  #   sinon, aucun lac. L'utilisateur peut choisir un autre lac — un
  #   avertissement s'affiche alors au-dessus des résultats.
  # ---------------------------------------------------------------------------
  lac_exploit_rv <- reactiveVal("")

  lacs_exploit <- reactive({
    er <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(er) || !all(c("nolac", "espece_code") %in% names(er))) return(NULL)
    tmp <- er[!is.na(er$nolac) & as.character(er$espece_code) == config()$code_ifa, ]
    df <- data.frame(
      nolac  = normaliser_nolac(tmp$nolac),
      nomlac = if ("nom_plan_eau" %in% names(tmp)) as.character(tmp$nom_plan_eau) else NA_character_,
      stringsAsFactors = FALSE
    )
    df <- df[!duplicated(df$nolac), ]
    df[order(suppressWarnings(as.numeric(df$nolac)), df$nolac), ]
  })

  # Règle de présélection, partagée par le rendu et les observateurs pour
  # rester déterministe quel que soit l'ordre d'exécution
  choisir_lac_exploit <- function(garder_courant) {
    lst <- lacs_exploit()
    if (is.null(lst) || nrow(lst) == 0) return("")
    cur <- isolate(lac_exploit_rv())
    pl  <- cle_lac_potentiel()
    if (garder_courant && isTruthy(cur) && cur %in% lst$nolac) return(cur)
    if (!is.na(pl) && pl %in% lst$nolac) return(pl)
    ""
  }

  output$lac_exploit_ui <- renderUI({
    lst <- lacs_exploit()
    if (is.null(lst) || nrow(lst) == 0) return(NULL)
    etiquettes <- ifelse(!is.na(lst$nomlac) & nzchar(lst$nomlac),
                         paste0(lst$nolac, " \u2014 ", lst$nomlac), lst$nolac)
    div(class = "champ mt-1",
      selectInput("lac_exploit_select", "Lac (données d'exploitation)",
                  choices  = c("— Choisir —" = "", setNames(lst$nolac, etiquettes)),
                  selected = isolate(choisir_lac_exploit(garder_courant = TRUE)),
                  width    = "100%"))
  })

  maj_lac_exploit <- function(cle) {
    lac_exploit_rv(cle)
    updateSelectInput(session, "lac_exploit_select", selected = cle)
  }
  # Nouveau fichier ou changement d'espèce : garder le lac s'il est toujours
  # dans la liste, sinon présélectionner le lac du potentiel halieutique
  observeEvent(lacs_exploit(), maj_lac_exploit(choisir_lac_exploit(TRUE)), ignoreNULL = FALSE)
  # Changement du lac du potentiel halieutique : le suivre si possible
  observeEvent(cle_lac_potentiel(), maj_lac_exploit(choisir_lac_exploit(FALSE)),
               ignoreNULL = FALSE, ignoreInit = TRUE)
  # Choix de l'utilisateur
  observeEvent(input$lac_exploit_select, {
    v <- input$lac_exploit_select
    lac_exploit_rv(if (isTruthy(v)) v else "")
  }, ignoreInit = TRUE)

  cle_lac_exploit <- reactive(cle_lac(lac_exploit_rv()))

  # Lacs différents entre les deux sélecteurs (ou non vérifiable)
  #   "identique" | "different" | "non_verifiable" | "aucun"
  correspondance_lacs <- reactive({
    ex <- cle_lac_exploit()
    if (is.na(ex)) return("aucun")
    pl <- cle_lac_potentiel()
    if (is.na(pl)) return("non_verifiable")
    if (identical(pl, ex)) "identique" else "different"
  })

  # Superficie utilisée pour le rendement observé (kg/ha) : celle du lac
  # D'EXPLOITATION. Si ce n'est pas le lac du potentiel halieutique, on la
  # cherche dans l'onglet Lacs ; introuvable -> NA (rendement non calculable,
  # plutôt qu'une division par la superficie d'un autre lac).
  sup_exploit <- reactive({
    corr <- correspondance_lacs()
    if (corr %in% c("identique", "non_verifiable"))
      return(if (!is.na(input$sup) && input$sup > 0) input$sup else NA_real_)
    if (corr == "aucun") return(NA_real_)
    pb <- tryCatch(pothal_brut(), error = function(e) NULL)
    if (is.null(pb) || !all(c("nolac", "sup") %in% names(pb$lacs))) return(NA_real_)
    v <- pb$lacs$sup[!is.na(pb$lacs$nolac) & pb$lacs$nolac == cle_lac_exploit()]
    v <- suppressWarnings(as.numeric(v[1]))
    if (length(v) == 1 && !is.na(v) && v > 0) v else NA_real_
  })

  # Messages du bloc d'exploitation
  output$msg_exploit <- renderUI({
    er <- tryCatch(exploit_raw(), error = function(e) NULL)
    if (is.null(er)) return(NULL)
    lst <- lacs_exploit()
    if (is.null(lst) || nrow(lst) == 0)
      return(msg_ui("warning", paste0("Aucune donnée pour l'espèce ", config()$nom,
                                      " dans ce fichier.")))
    if (is.na(cle_lac_exploit())) {
      pl <- cle_lac_potentiel()
      return(msg_ui("info", if (!is.na(pl))
        paste0("Le lac ", pl, " n'a aucune donnée de récolte pour cette espèce — ",
               "choisir un lac au besoin.")
        else "Choisir un lac pour afficher les données d'exploitation."))
    }
    df <- exploit_edited_rv()
    tagList(
      if (!is.null(df) && nrow(df) > 0) {
        an <- range(df$annee, na.rm = TRUE)
        div(class = "src-ligne", paste0(nrow(df), " année", if (nrow(df) > 1) "s" else "",
                                        " (", an[1], "\u2013", an[2], ")"))
      },
      if (identical(correspondance_lacs(), "different") && is.na(sup_exploit()))
        msg_ui("warning", paste0("Superficie du lac ", cle_lac_exploit(), " introuvable dans ",
                                 "l'onglet Lacs — rendement observé (kg/ha) non calculable."))
    )
  })

  # Paramètres sauvegardés par lac — session uniquement
  params_sauvegardes_rv <- reactiveVal(list())

  # Déclencheur pour les avertissements visuels (activé après import ou calcul)
  warnings_actifs_rv <- reactiveVal(FALSE)

  output$save_params_ui <- renderUI({
    req(isTruthy(input$no_lac))
    div(class = "mt-1",
      actionButton("btn_save_params",
                   label = "Sauvegarder ce lac (session en cours)",
                   class = "btn btn-sm btn-outline-primary w-100")
    )
  })

  # Champs simples conservés par la sauvegarde (session seulement)
  CHAMPS_SAUVES_NUM <- c("sup", "prof_max", "prof_moy", "perimetre", "conductivite",
                         "secchi", "ph_eau", "nb_chalets_omble",
                         "quota_actuel_touladi", "quota_actuel_dore", "quota_actuel_omble")

  observeEvent(input$btn_save_params, {
    req(isTruthy(input$no_lac))
    cle <- cle_lac(input$no_lac)
    nouveaux <- c(
      setNames(lapply(CHAMPS_SAUVES_NUM, function(id) input[[id]]), CHAMPS_SAUVES_NUM),
      list(
        tributaire_emissaire_omble = input$tributaire_emissaire_omble,
        especes_presentes_touladi  = input$especes_presentes_touladi,
        especes_presentes_dore     = input$especes_presentes_dore,
        especes_presentes_omble    = input$especes_presentes_omble,
        # Champs sourcés : le mode, et la valeur seulement si saisie manuelle
        # (sinon la source automatique la recalcule à l'identique)
        modes_src   = lapply(mode_src, function(m) m()),
        valeurs_src = lapply(CHAMPS_SOURCES, function(ch) input[[ch$id]])
      )
    )
    params <- params_sauvegardes_rv()
    params[[cle]] <- nouveaux
    params_sauvegardes_rv(params)
    showNotification(
      paste0("Paramètres sauvegardés pour le lac ", cle, "."),
      type = "message", duration = 3
    )
  })

  appliquer_params_sauvegardes <- function(p) {
    for (id in CHAMPS_SAUVES_NUM) updateNumericInput(session, id, value = p[[id]] %||% NA)
    updateRadioButtons(session, "tributaire_emissaire_omble",
                       selected = p$tributaire_emissaire_omble %||% "inconnu")
    updateCheckboxGroupInput(session, "especes_presentes_touladi",
                             selected = p$especes_presentes_touladi %||% character(0))
    updateCheckboxGroupInput(session, "especes_presentes_dore",
                             selected = p$especes_presentes_dore %||% character(0))
    updateCheckboxGroupInput(session, "especes_presentes_omble",
                             selected = p$especes_presentes_omble %||% character(0))
    for (cle in names(CHAMPS_SOURCES)) {
      m <- p$modes_src[[cle]] %||% CHAMPS_SOURCES[[cle]]$defaut
      mode_src[[cle]](m)
      # Mode manuel : le mode est fixé AVANT d'écrire la valeur, pour que son
      # retour ne soit pas confondu avec un pré-remplissage
      if (identical(m, "manual"))
        updateNumericInput(session, CHAMPS_SOURCES[[cle]]$id,
                           value = p$valeurs_src[[cle]] %||% NA)
    }
  }

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
    # NB (2026-09) : la colonne « Comparer » a été retirée ; input$overlay_models
    # reste NULL et ce graphique n'affiche plus que le modèle de référence.
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
