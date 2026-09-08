# =============================================================================
# config_especes.R
# Registre des espèces + helpers de thème et de barre d'espèces.
# À sourcer dans app.R APRÈS la définition de COULEURS et des fonctions
# de modèle (calc_lester_touladi, calc_shuter_1998, calc_ime_ryder,
# calc_valin_vaillancourt_omble, ...).
#
#   source("config_especes.R", encoding = "UTF-8")
# =============================================================================


# =============================================================================
# REGISTRE DES ESPÈCES
#
# Une entrée par espèce. Tout ce qui varie d'une espèce à l'autre vit ici :
#   - code_ifa  : code espèce dans les exports IFA (filtre des données)
#   - palette   : couleurs (réutilise le registre COULEURS existant)
#   - modeles   : liste des modèles APPLICABLES — diffère selon l'espèce
#       * chaque modèle pointe vers sa fonction (fn) et déclare ses intrants
#       * partition : part de l'espèce dans le rendement IME total (OMNR 1982)
#   - cascade_reference : ordre de priorité pour le modèle de référence
#
# Ajouter une espèce = ajouter une entrée ici + activer son onglet.
# Aucune modification d'interface requise.
# =============================================================================
REGISTRE_ESPECES <- list(

  # ---------------------------------------------------------------------------
  # Touladi (Salvelinus namaycush) — complet : Lester + Shuter + IME + Valin
  # ---------------------------------------------------------------------------
  touladi = list(
    nom      = "Touladi",
    code_ifa = "SANA",
    palette  = COULEURS$touladi,
    modeles  = list(
      lester = list(
        nom      = "Lester 2021",
        fn       = calc_lester_touladi,
        intrants = c("A", "Dmax", "Dmn", "T_air", "Linf", "Dth_obs")
      ),
      shuter = list(
        nom      = "Shuter 1998",
        fn       = calc_shuter_1998,
        intrants = c("A", "TDS")
      ),
      touladi_valin = list(
        nom      = "Valin 1998",
        fn       = calc_valin_touladi,
        intrants = c("TDS", "Dmn")
      ),
      ime = list(
        nom       = "IME / Ryder",
        fn        = calc_ime_ryder,
        intrants  = c("TDS", "Dmn"),
        partition = 0.25            # part Touladi — OMNR 1982 (~25 %)
      )
    ),
    cascade_reference = c("lester", "shuter", "touladi_valin", "ime")
  ),

  # ---------------------------------------------------------------------------
  # Doré jaune (Sander vitreus) — complet : Lester (éq. 6/14) + Valin + IME
  #   Modèle recommandé : Lester et coll. 2002 (habitat thermo-optique, TOHA).
  #   Valin (Saguenay) et IME (Ryder + OMNR 1982, partition 32 %) suivent en
  #   cascade de référence si Lester est indisponible.
  #   Les grilles régionales (Laurentides, Mauricie, Nord-du-Québec) sont
  #   gérées séparément (MODELES_REGIONAUX / regions_actives() dans app.R) —
  #   ce ne sont pas des fonctions du bassin d'intrants, mais des tables.
  # ---------------------------------------------------------------------------
  dore = list(
    nom      = "Doré jaune",
    code_ifa = "SAVI",
    palette  = COULEURS$dore,
    modeles  = list(
      lester_savi = list(
        nom      = "Lester et coll. 2002",
        fn       = calc_lester_dore,
        intrants = c("A", "Dmax", "Dmn", "TDS", "G", "z_sec", "Dth_obs", "T_air")
      ),
      valin = list(
        nom      = "Valin / Vaillancourt 1998",
        fn       = calc_valin_dore,
        intrants = c("TDS", "Dmn", "A")
      ),
      ime = list(
        nom       = "IME / Ryder",
        fn        = calc_ime_ryder,
        intrants  = c("TDS", "Dmn"),
        partition = 0.32        # part Doré — OMNR 1982
      )
    ),
    cascade_reference = c("lester_savi", "valin", "ime")
  ),

  # ---------------------------------------------------------------------------
  # Omble de fontaine (Salvelinus fontinalis) — 3 modèles.
  #   Modèle recommandé : Valin et Vaillancourt 1998 (Saguenay–Lac-Saint-Jean),
  #   suivi en cascade d'Archambault puis Vézina si indisponible.
  #   (Choix provisoire — rapport préliminaire toujours en attente,
  #   susceptible d'être révisé.)
  #   - omble_valin  : cascade de réductions % sur la base Vézina. Clé nommée
  #       "omble_valin" (et non "valin") pour éviter toute correspondance
  #       partielle avec la clé "valin" du Doré via l'opérateur $ (bug corrigé
  #       le 2026-07 : r$valin correspondait par préfixe à r$valin_omble et
  #       faisait planter le tableau détail par modèle).
  #   - archambault  : Houde 1982 / adaptation Archambault 1988-2009,
  #       table <40 ha + formule fermée >=40 ha, piloté par superficie + groupe
  #       d'espèces (voir especes_groupes_omble() dans app.R)
  #   - vezina       : base Vézina 1978, profondeur moyenne seule
  #   Valin et Vaillancourt fusionnés en une seule entrée (2026-09) : les deux
  #   implémentations retournaient une valeur identique pour tout lac de moins
  #   de 25,9 m dont la combinaison d'espèces ne déclenchait pas le palier
  #   ménés/catostomes. Voir calc_valin_vaillancourt_omble() dans app.R.
  #   Grilles régionales (Nord-du-Québec, Laurentides) gérées séparément —
  #   voir MODELES_REGIONAUX$omble / regions_actives() dans app.R.
  #   IME retiré (2026-07) : aucune formule IME pour l'Omble n'est sourcée
  #   dans la littérature consultée (Vézina/Archambault/Valin/Vaillancourt) —
  #   la partition 0,25 précédente n'était pas justifiée.
  # ---------------------------------------------------------------------------
  omble = list(
    nom      = "Omble de fontaine",
    code_ifa = "SAFO",
    palette  = COULEURS$omble,
    modeles  = list(
      vezina = list(
        nom      = "Vézina 1978",
        fn       = calc_vezina_omble,
        intrants = c("Dmn")
      ),
      archambault = list(
        nom      = "Archambault 1988/2009",
        fn       = calc_archambault_omble,
        intrants = c("A", "grp")
      ),
      omble_valin = list(
        nom      = "Valin et Vaillancourt 1998",
        fn       = calc_valin_vaillancourt_omble,
        intrants = c("Dmn", "grp", "pH", "o2_pct_reduction",
                     "tributaire_absent", "nb_chalets", "A_optionnel")
      )
    ),
    cascade_reference = c("omble_valin", "archambault", "vezina")
  )
)

# Ordre d'affichage des onglets dans la barre d'espèces
ESPECES_ORDRE <- c("touladi", "dore", "omble")


# =============================================================================
# THÈME — construit à partir d'une palette d'espèce
#   Reprend exactement le thème existant ; seule la couleur primaire change.
#   Utilisé à l'initialisation ET au changement d'espèce (setCurrentTheme).
# =============================================================================
make_theme <- function(palette) {
  bslib::bs_theme(
    bootswatch       = "flatly",
    primary          = palette$primaire,   # couleur espèce → boutons, onglets, focus
    "font-size-base" = "0.9rem",
    "border-radius"  = "5px",
    bg               = "#F0F3F6",
    fg               = "#2C3E50"
  )
}


# =============================================================================
# BARRE D'ESPÈCES — rendue selon l'espèce active (la classe « active » suit)
#   Chaque onglet émet input$espece_click au clic.
# =============================================================================
build_species_bar <- function(active, disponibles = names(REGISTRE_ESPECES)) {
  onglet <- function(cle) {
    sp     <- REGISTRE_ESPECES[[cle]]
    dispo  <- cle %in% disponibles
    classe <- paste0(
      "sp-tab",
      if (!dispo) " disabled" else "",
      if (identical(cle, active)) paste0(" active-", cle) else ""
    )
    div(
      class   = classe,
      title   = if (!dispo) "Aucune donnée pour le lac sélectionné" else NULL,
      onclick = sprintf("Shiny.setInputValue('espece_click', '%s', {priority: 'event'})", cle),
      span(class = paste0("sp-dot dot-", cle)),
      sp$nom
    )
  }
  div(class = "species-bar",
      lapply(ESPECES_ORDRE, onglet)
  )
}
