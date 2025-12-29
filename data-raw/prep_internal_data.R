# prep internal package data

# Vartypes -----------------------------------------------------

## v1.4 ---------------------------------------------------------

v1.4 = c(
  ent_id = "integer",
  uid = "character",
  fall_id = "integer",
  fall_id_ch = "integer",
  burnr = "integer",
  burnr_gesv = "integer",
  abc_fall = "spigesvar",
  geschlecht = "spigesvar",
  alter = "integer",
  alter_U1 = "integer",
  wohnort_medstat = "character",
  wohnkanton = "character",
  wohnland = "character",
  nationalitaet = "character",
  eintrittsdatum = "integer",
  eintritt_aufenthalt = "spigesvar",
  eintrittsart = "spigesvar",
  einw_instanz = "spigesvar",
  liegeklasse = "spigesvar",
  versicherungsklasse = "spigesvar",
  admin_urlaub = "integer",
  chlz = "integer",
  aufenthalt_ips = "integer",
  beatmung = "integer",
  schwere_score = "integer",
  art_score = "spigesvar",
  nems = "integer",
  aufenthalt_imc = "integer",
  aufwand_imc = "integer",
  hauptleistungsstelle = "character",
  grundversicherung = "spigesvar",
  splg = "character",
  tarif = "spigesvar",
  austrittsdatum = "integer",
  austrittsentscheid = "spigesvar",
  austritt_aufenthalt = "spigesvar",
  austritt_behandlung = "spigesvar",
  geburtszeit = "integer",
  vitalstatus = "spigesvar",
  mehrling = "spigesvar",
  geburtsrang = "integer",
  geburtsgewicht = "integer",
  laenge = "integer",
  missbildungen = "spigesvar",
  fall_id_mutter = "integer",
  gestationsalter1 = "integer",
  gestationsalter2 = "integer",
  vorh_schwanger = "integer",
  vorh_lebendgeburten = "integer",
  vorh_fehlgeburten = "integer",
  vorh_abbrueche = "integer",
  aufnahmegewicht = "integer",
  kopfumfang = "integer",
  psy_zivilstand = "spigesvar",
  psy_eintritt_aufenthalt = "spigesvar",
  psy_eintritt_teilzeit = "spigesvar",
  psy_eintritt_vollzeit = "spigesvar",
  psy_eintritt_arbeitslos = "spigesvar",
  psy_eintritt_hausarbeit = "spigesvar",
  psy_eintritt_ausbildung = "spigesvar",
  psy_eintritt_reha = "spigesvar",
  psy_eintritt_rente = "spigesvar",
  psy_eintritt_gesch_arbeit = "spigesvar",
  psy_eintritt_unbekannt = "spigesvar",
  psy_schulbildung = "integer",
  psy_einweisende_instanz = "spigesvar",
  psy_fu = "spigesvar",
  psy_behandlung = "spigesvar",
  psy_pp_neuroleptika = "spigesvar",
  psy_pp_depotneuroleptika = "spigesvar",
  psy_pp_antidepressiva = "spigesvar",
  psy_pp_tranquilizer = "spigesvar",
  psy_pp_hypnotika = "spigesvar",
  psy_pp_antiepileptika = "spigesvar",
  psy_pp_lithium = "spigesvar",
  psy_pp_substitution = "spigesvar",
  psy_pp_suchtaversion = "spigesvar",
  psy_pp_antiparkinson = "spigesvar",
  psy_pp_andere = "spigesvar",
  psy_pp_koerper_medi = "spigesvar",
  psy_entsch_austritt = "spigesvar",
  psy_austritt_aufenthalt = "spigesvar",
  psy_austritt_behandlung = "spigesvar",
  psy_behandlungsbereich = "spigesvar",
  diagnose_id = "integer",
  diagnose_kode = "character",
  diagnose_seitigkeit = "spigesvar",
  diagnose_poa = "spigesvar",
  diagnose_zusatz = "character",
  behandlung_id = "integer",
  behandlung_chop = "character",
  behandlung_seitigkeit = "spigesvar",
  behandlung_beginn = "integer",
  behandlung_auswaerts = "spigesvar",
  behandlung_bur = "integer",
  medi_id = "integer",
  medi_atc = "character",
  medi_zusatz = "character",
  medi_verabreichungsart = "spigesvar",
  medi_dosis = "numeric",
  medi_einheit = "character",
  rech_id = "integer",
  rech_kostentraeger = "spigesvar",
  rech_versicherer = "integer",
  rech_unfallnr = "character",
  rech_betrag = "numeric",
  rech_tariftyp = "integer",
  rech_tarifcode = "character",
  rech_ext_faktor = "numeric",
  rech_basispreis = "numeric",
  rech_einheit = "numeric",
  rech_menge = "numeric",
  ktr_typ = "integer",
  ktr_beschr = "character",
  ktr_60 = "numeric",
  ktr_61 = "numeric",
  ktr_62 = "numeric",
  ktr_65 = "numeric",
  ktr_66 = "numeric",
  ktr_68 = "numeric",
  ktr_69 = "numeric",
  ktr_697 = "numeric",
  ktr_4001 = "numeric",
  ktr_4002 = "numeric",
  ktr_4012 = "numeric",
  ktr_4011 = "numeric",
  ktr_40_rest = "numeric",
  ktr_4051 = "numeric",
  ktr_4052 = "numeric",
  ktr_3801 = "numeric",
  ktr_3802 = "numeric",
  ktr_3811 = "numeric",
  ktr_3812 = "numeric",
  ktr_480 = "numeric",
  ktr_485 = "numeric",
  ktr_486 = "numeric",
  ktr_10 = "numeric",
  ktr_20 = "numeric",
  ktr_21 = "numeric",
  ktr_23 = "numeric",
  ktr_24 = "numeric",
  ktr_25 = "numeric",
  ktr_26 = "numeric",
  ktr_27 = "numeric",
  ktr_28 = "numeric",
  ktr_29 = "numeric",
  ktr_30 = "numeric",
  ktr_31_1_5 = "numeric",
  ktr_31_6a1 = "numeric",
  ktr_31_6a2 = "numeric",
  ktr_31_6b1 = "numeric",
  ktr_31_6b2 = "numeric",
  ktr_31_6b3 = "numeric",
  ktr_31_6b4 = "numeric",
  ktr_31_6b5 = "numeric",
  ktr_32 = "numeric",
  ktr_33 = "numeric",
  ktr_34 = "numeric",
  ktr_35 = "numeric",
  ktr_36 = "numeric",
  ktr_38 = "numeric",
  ktr_39 = "numeric",
  ktr_40 = "numeric",
  ktr_41 = "numeric",
  ktr_42 = "numeric",
  ktr_43 = "numeric",
  ktr_44 = "numeric",
  ktr_45 = "numeric",
  ktr_77 = "numeric",
  ktr_nicht_pb = "numeric",
  ktr_10_ank = "numeric",
  ktr_20_ank = "numeric",
  ktr_21_ank = "numeric",
  ktr_23_ank = "numeric",
  ktr_24_ank = "numeric",
  ktr_25_ank = "numeric",
  ktr_26_ank = "numeric",
  ktr_27_ank = "numeric",
  ktr_28_ank = "numeric",
  ktr_29_ank = "numeric",
  ktr_30_ank = "numeric",
  ktr_31_1_5_ank = "numeric",
  ktr_31_6a1_ank = "numeric",
  ktr_31_6a2_ank = "numeric",
  ktr_31_6b1_ank = "numeric",
  ktr_31_6b2_ank = "numeric",
  ktr_31_6b3_ank = "numeric",
  ktr_31_6b4_ank = "numeric",
  ktr_31_6b5_ank = "numeric",
  ktr_32_ank = "numeric",
  ktr_33_ank = "numeric",
  ktr_34_ank = "numeric",
  ktr_35_ank = "numeric",
  ktr_36_ank = "numeric",
  ktr_38_ank = "numeric",
  ktr_39_ank = "numeric",
  ktr_40_ank = "numeric",
  ktr_41_ank = "numeric",
  ktr_42_ank = "numeric",
  ktr_43_ank = "numeric",
  ktr_44_ank = "numeric",
  ktr_45_ank = "numeric",
  ktr_77_ank = "numeric",
  ktr_44_vkl = "numeric",
  ktr_44_rekole = "numeric",
  ktr_kosten_65 = "numeric",
  ktr_le_ambulant = "numeric",
  ktr_methodik = "spigesvar",
  itark_spalte = "character",
  op_gln = "integer",
  op_liste = "spigesvar",
  op_rolle = "spigesvar",
  episode_id = "integer",
  episode_beginn = "integer",
  episode_ende = "integer",
  episode_art = "spigesvar",
  burnr_episode = "integer",
  wiedereintritt_aufenthalt = "spigesvar",
  grund_wiedereintritt = "spigesvar",
  ent_id = "integer",
  fall_id = "integer",
  ahv = "integer",
  geburtsdatum = "integer",
  kanton_zusatzdaten = "character"
)

var_types <- list(v1.4 = v1.4)

# Code Dictionary -----------------------------------------------------

## v1.4 ---------------------------------------------------------

v1.4 = list(
  abc_fall = list(
    de = c(A = "Austritte", B = "\U{DC}berlieger", C = "Durchlieger"),
    fr = c(A = "sorties", B = "\U{E0} cheval", C = "toute la p\U{E9}riode"),
    it = c(A = "dimissione", B = "a cavallo", C = "intero anno")
  ),
  geschlecht = list(
    de = c(`1` = "m\U{E4}nnlich", `2` = "weiblich"),
    fr = c(`1` = "masculin", `2` = "f\U{E9}minin"),
    it = c(`1` = "maschile", `2` = "femminile")
  ),
  eintritt_aufenthalt = list(
    de = c(
      `1` = "zuhause",
      `2` = "zuhause mit SPITEX Versorgung",
      `3` = "Krankenheim, Pflegeheim",
      `4` = "Altersheim, andere sozialmed. Institutionen",
      `5` = "Psychiatrische Klinik, anderer Betrieb",
      `55` = "Psychiatrische Abteilung/Klinik, gleicher Betrieb",
      `6` = "anderes Krankenhaus (Akutspital) oder Geburtshaus",
      `66` = "Akutabteilung/-klinik,  gleicher Betrieb",
      `7` = "Strafvollzugsanstalt",
      `8` = "andere",
      `83` = "Rehabilitationsklinik, anderer Betrieb",
      `84` = "Rehabilitations-abteilung/-klinik, gleicher  Betrieb",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "domicile",
      `2` = "domicile avec soins \U{E0} domicile",
      `3` = "\U{E9}tablissement de sant\U{E9} non hospitalier m\U{E9}dicalis\U{E9}",
      `4` = "\U{E9}tablissement de sant\U{E9} non hospitalier non m\U{E9}dicalis\U{E9}",
      `5` = "clinique psychiatrique, autre \U{E9}tablissement",
      `55` = "division/clinique psychiatrique, m\U{EA}me \U{E9}tablissement",
      `6` = "autre h\U{F4}pital (soins aigus) ou maison de naissance",
      `66` = "division/Clinique de soins aigus, m\U{EA}me \U{E9}tablissement",
      `7` = "institution d\U{2019}ex\U{E9}cution des peines",
      `8` = "autre",
      `83` = "clinique de r\U{E9}adaptation, autre \U{E9}tablissement",
      `84` = "division/clinique de r\U{E9}adaptation, m\U{EA}me \U{E9}tablissement",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "domicilio",
      `2` = "domicilio con assistenza SPITEX",
      `3` = "casa di cura (comprese case per anziani medicalizzate)",
      `4` = "altro istituto sanitario non ospedaliero, non medicalizzato",
      `5` = "clinica psichiatrica, altro istituto",
      `55` = "reparto psichiatrico / clinica psichiatrica, stesso istituto",
      `6` = "altro istituto ospedaliero (ospedale acuto) o casa per partorienti",
      `66` = "reparto acuto / ospedale acuto, stesso istituto",
      `7` = "penitenziario",
      `8` = "altro",
      `83` = "clinica di riabilitazione, altro istituto",
      `84` = "reparto di riabilitazione / clinica di riabilitazione, stesso istituto",
      `9` = "sconosciuto"
    )
  ),
  eintrittsart = list(
    de = c(
      `1` = "Notfall (Behandlung innerhalb von 12 Std. unabdingbar)",
      `2` = "angemeldet, geplant",
      `3` = "Geburt (Kind in diesem station\U{E4}ren Aufenthalt und dieser Klinik geboren)",
      `4` = "interner \U{DC}bertritt",
      `5` = "Verlegung innerhalb 24 Std.",
      `6` = "R\U{FC}ckverlegung",
      `7` = "Repatriierung",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "urgence (n\U{E9}cessit\U{E9} d\U{2019}un traitement dans les 12 heures)",
      `2` = "annonc\U{E9}, planifi\U{E9}",
      `3` = "accouchement (enfant n\U{E9} lors de ce s\U{E9}jour stationnaire et dans cette clinique)",
      `4` = "transfert interne",
      `5` = "transfert dans les 24 heures",
      `6` = "r\U{E9}hospitalisation",
      `7` = "rapatriement",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "urgenza (necessit\U{E0} di trattamento entro 12 ore)",
      `2` = "annunciata, programmata",
      `3` = "nascita (neonato nato nello stabilimento)",
      `4` = "trasferimento interno",
      `5` = "trasferimento entro 24 ore",
      `6` = "paziente ritrasferito",
      `7` = "rimpatrio",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  einw_instanz = list(
    de = c(
      `1` = "selbst, Angeh\U{F6}rige",
      `2` = "Rettungsdienst (Ambulanz, Polizei)",
      `3` = "\U{C4}rztin / Arzt",
      `4` = "nichtmedizinischer Therapeut",
      `5` = "Sozialmedizinischer Dienst",
      `6` = "Justizbeh\U{F6}rden",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "initiative propre, proches",
      `2` = "services de sauvetage (ambulance, police)",
      `3` = "m\U{E9}decin",
      `4` = "th\U{E9}rapeute non-m\U{E9}decin",
      `5` = "services socio-m\U{E9}dicaux",
      `6` = "autorit\U{E9}s judiciaires",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "paziente stesso, familiare",
      `2` = "servizio di soccorso (ambulanza, polizia)",
      `3` = "medico",
      `4` = "terapista non medico",
      `5` = "servizio sociosanitario",
      `6` = "autorit\U{E0} giudiziarie",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  liegeklasse = list(
    de = c(`1` = "allgemein", `2` = "halbprivat", `3` = "privat", `9` = "unbekannt"),
    fr = c(
      `1` = "chambre commune",
      `2` = "semi-priv\U{E9}",
      `3` = "priv\U{E9}",
      `9` = "inconnu"
    ),
    it = c(`1` = "comune", `2` = "semiprivata", `3` = "privata", `9` = "sconosciuto")
  ),
  versicherungsklasse = list(
    de = c(
      `1` = "allgemein",
      `2` = "halbprivat",
      `3` = "privat",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "chambre commune",
      `2` = "semi-priv\U{E9}",
      `3` = "priv\U{E9}",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "comune",
      `2` = "semiprivata",
      `3` = "privata",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  art_score = list(
    de = c(S = "SAPS", P = "PIM2", P3 = "PIM3", C = "CRIB"),
    fr = c(S = "SAPS", P = "PIM2", P3 = "PIM3", C = "CRIB"),
    it = c(S = "SAPS", P = "PIM2", P3 = "PIM3", C = "CRIB")
  ),
  grundversicherung = list(
    de = c(
      `1` = "Krankenversicherung (obligat.)",
      `2` = "Invalidenversicherung",
      `3` = "Milit\U{E4}rversicherung",
      `4` = "Unfallversicherung",
      `5` = "Selbstzahler (z.B. Ausl\U{E4}nder ohne Grundversicherung)",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "assurance-maladie (obligatoire)",
      `2` = "assurance-invalidit\U{E9}",
      `3` = "assurance-militaire",
      `4` = "assurance-accident",
      `5` = "autopayeur (par ex. \U{E9}trangers sans assurance de base)",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "assicurazione malattie (obbligatoria)",
      `2` = "assicurazione invalidit\U{E0}",
      `3` = "assicurazione militare",
      `4` = "assicurazione infortuni",
      `5` = "paziente stesso (p.es. straniero senza assicurazione di base)",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  tarif = list(
    de = c(
      `0` = "unbekannt",
      `1` = "SwissDRG",
      `2` = "Andere Tarife Akutsomatik",
      `3` = "ST Reha",
      `4` = "Andere Tarife station\U{E4}re Reha",
      `5` = "TARPSY",
      `6` = "Andere Tarife station\U{E4}re Psychiatrie",
      `7` = "Pflegetaxe"
    ),
    fr = c(
      `0` = "inconnu",
      `1` = "SwissDRG",
      `2` = "autres tarifs soins aigus",
      `3` = "ST Reha",
      `4` = "autres tarifs Reha stationnaire",
      `5` = "TARPSY",
      `6` = "autres tarifs psychiatrie stationnaire",
      `7` = "taxes de soin"
    ),
    it = c(
      `0` = "sconosciuto",
      `1` = "SwissDRG",
      `2` = "altre tariffe per le cure acute",
      `3` = "ST Reha",
      `4` = "altre tariffe Reha",
      `5` = "TARPSY",
      `6` = "altre tariffe per la psichiatria stazionaria",
      `7` = "tasse per le cure"
    )
  ),
  austrittsentscheid = list(
    de = c(
      `1` = "auf Initiative der behandelnden Person",
      `2` = "auf Initiative des Patienten (gegen Ansicht der behandelnden Person)",
      `3` = "auf Initiative einer Drittperson",
      `4` = "interner \U{DC}bertritt",
      `5` = "gestorben",
      `8` = "anderes",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "\U{E0} l\U{2019}initiative de la personne qui g\U{E8}re le traitement",
      `2` = "\U{E0} l\U{2019}initiative du patient (contre l\U{2019}avis de la personne qui g\U{E8}re le traitement)",
      `3` = "\U{E0} l\U{2019}initiative d\U{2019}une tierce personne",
      `4` = "transfert interne",
      `5` = "d\U{E9}c\U{E9}d\U{E9}(e)",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "su iniziativa del curante",
      `2` = "su iniziativa del paziente (contro il parere del curante)",
      `3` = "su iniziativa di terzi",
      `4` = "trasferimento interno",
      `5` = "decesso",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  austritt_aufenthalt = list(
    de = c(
      `1` = "Zuhause",
      `2` = "Krankenheim, Pflegeheim",
      `3` = "Altersheim, andere sozialmed. Institution",
      `4` = "Psychiatrische Klinik, anderer Betrieb",
      `44` = "Psychiatrische Abteilung/Klinik, gleicher Betrieb",
      `5` = "Rehabilitationsklinik,",
      `anderer Betrieb` = NA,
      `55` = "Rehabilitations-",
      `abteilung/-klinik,` = NA,
      `gleicher Betrieb` = NA,
      `6` = "anderes Krankenhaus (Akutspital) oder Geburtshaus",
      `66` = "Akutabteilung/-klinik,  gleicher Betrieb",
      `7` = "Strafvollzugsanstalt",
      `8` = "andere",
      `9` = "unbekannt",
      `0` = "Todesfall"
    ),
    fr = c(
      `1` = "domicile",
      `2` = "\U{E9}tabl. de sant\U{E9} non hospit. m\U{E9}dicalis\U{E9}",
      `3` = "id. non m\U{E9}dicalis\U{E9}",
      `4` = "institution psychiatrique, autre \U{E9}tablissement",
      `44` = "division/clinique psychiatrique, m\U{EA}me \U{E9}tablissement",
      `5` = "institution de r\U{E9}adaptation, autre \U{E9}tablissement",
      `55` = "Division/clinique de r\U{E9}adaptation, m\U{EA}me \U{E9}tablissement",
      `6` = "Autre h\U{F4}pital (soins aigus) ou maison de naissance",
      `66` = "division/clinique de soins aigus, m\U{EA}me \U{E9}tablissement",
      `7` = "institution d\U{2019}ex\U{E9}cution des peines",
      `8` = "autre",
      `9` = "inconnu",
      `0` = "d\U{E9}c\U{E9}d\U{E9}"
    ),
    it = c(
      `1` = "domicilio",
      `2` = "casa di cura (comprese case per anziani medicalizzate)",
      `3` = "altro istituto sanitario non ospedaliero, non medicalizzato",
      `4` = "clinica psichiatrica, altro istituto",
      `44` = "reparto psichiatrico / clinica psichiatrica, stesso istituto",
      `5` = "clinica di riabilitazione, altro istituto",
      `55` = "reparto di riabilitazione / clinica di riabilitazione, stesso istituto",
      `6` = "altro stabilimento ospedaliero (ospedale acuto) o casa per partorienti",
      `66` = "reparto acuto / ospedale acuto, stesso istituto",
      `7` = "penitenziario",
      `8` = "altro",
      `9` = "sconosciuto",
      `0` = "decesso"
    )
  ),
  austritt_behandlung = list(
    de = c(
      `1` = "kein Behandlungsbedarf",
      `2` = "ambulante Behandlung",
      `3` = "ambulante Pflege (z.B. SPITEX)",
      `4` = "station\U{E4}re Behandlung oder Pflege",
      `5` = "Rehabilitation (amb. oder stat.)",
      `8` = "anderes",
      `9` = "unbekannt",
      `0` = "Todesfall"
    ),
    fr = c(
      `1` = "aucun besoin de traitement",
      `2` = "Soins ou traitement ambulatoires (cabinet m\U{E9}dical ou en \U{E9}tablissement)",
      `3` = "Soins \U{E0} domicile",
      `4` = "Soins ou traitement stationnaires",
      `5` = "R\U{E9}adaptation ambulatoire ou stationnaire",
      `8` = "Autre",
      `9` = "Inconnue",
      `0` = "D\U{E9}c\U{E9}d\U{E9}"
    ),
    it = c(
      `1` = "nessun trattamento necessario",
      `2` = "trattamento ambulatoriale",
      `3` = "cure ambulatoriali (p.es. SPITEX)",
      `4` = "trattamento o cure stazionari",
      `5` = "riabilitazione (ambulatoriale o stazionaria)",
      `8` = "altro",
      `9` = "sconosciuto",
      `0` = "decesso"
    )
  ),
  vitalstatus = list(
    de = c(`0` = "Lebendgeburt", `1` = "Totgeburt"),
    fr = c(`0` = "naissance vivante", `1` = "mortinaissance"),
    it = c(`0` = "nato vivo", `1` = "nato morto")
  ),
  mehrling = list(
    de = c(
      `1` = "Einfachgeburt",
      `2` = "Zwillinge",
      `3` = "Drillinge",
      `4` = "Vierlinge",
      `5` = "F\U{FC}nflinge",
      `6` = "Sechslingsgeburt oder mehr"
    ),
    fr = c(
      `1` = "naissance simple",
      `2` = "jumeaux",
      `3` = "tripl\U{E9}s",
      `4` = "quadrupl\U{E9}s",
      `5` = "quintupl\U{E9}s",
      `6` = "naissance de sextupl\U{E9}s et plus"
    ),
    it = c(
      `1` = "singolo",
      `2` = "bigemino",
      `3` = "trigemino",
      `4` = "quadrigemino",
      `5` = "parto di 5 gemelli",
      `6` = "parto di 6 o pi\U{F9} gemelli"
    )
  ),
  missbildungen = list(
    de = c(`0` = "nein", `1` = "ja", `9` = "unbekannt"),
    fr = c(`0` = "non", `1` = "oui", `9` = "inconnu"),
    it = c(`0` = "no", `1` = "s\U{EC}", `9` = "sconosciuto")
  ),
  psy_zivilstand = list(
    de = c(
      `1` = "ledig",
      `2` = "verheiratet",
      `3` = "verwitwet",
      `4` = "geschieden",
      `5` = "unverheiratet",
      `6` = "in eingetragener Partnerschaft",
      `7` = "aufgel\U{F6}ste Partnerschaft",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "c\U{E9}libataire",
      `2` = "mari\U{E9}",
      `3` = "veuf",
      `4` = "divorc\U{E9}",
      `5` = "non mari\U{E9}",
      `6` = "li\U{E9} par un partenariat enregistr\U{E9}",
      `7` = "partenariat dissous",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "celibe/nubile",
      `2` = "coniugato",
      `3` = "vedovo",
      `4` = "divorziato",
      `5` = "non coniugato",
      `6` = "in unione domestica registrata",
      `7` = "unione domestica sciolta",
      `9` = "sconosciuto"
    )
  ),
  psy_eintritt_aufenthalt = list(
    de = c(
      `11` = "zuhause, alleine",
      `12` = "zuhause, mit anderen",
      `21` = "zuhause, alleine, mit Spitex",
      `22` = "zuhause, mit anderen, mit Spitex",
      `30` = "Krankenheim, Pflegeheim",
      `40` = "Altersheim, andere sozialmed. Instit.",
      `41` = "Wohnheim",
      `50` = "Psychiatrische Klinik, anderer Betrieb",
      `55` = "Psychiatrische Abteilung/Klinik, gleicher Betrieb",
      `60` = "anderes Krankenhaus       (Akutspital) oder        Geburtshaus",
      `66` = "Akutabteilung/-klinik, gleicher Betrieb",
      `70` = "Strafvollzugsanstalt",
      `81` = "andere",
      `82` = "ohne festen Wohnsitz",
      `83` = "Rehabilitationsklinik, anderer Betrieb",
      `84` = "Rehabilitations-abteilung/ -klinik, gleicher Betrieb",
      `90` = "unbekannt"
    ),
    fr = c(
      `11` = "\U{E0} la maison, seul(e)",
      `12` = "\U{E0} la maison, avec d'autres personnes",
      `21` = "\U{E0} la maison, seul(e), soins \U{E0} domicile",
      `22` = "\U{E0} la maison,avec d'autres,soins \U{E0} dom.",
      `30` = "home m\U{E9}dicalis\U{E9}",
      `40` = "foyer pour pers. \U{E2}g\U{E9}es, institution m\U{E9}dico-sociale",
      `41` = "logement dans un foyer",
      `50` = "clinique psychiatrique, autre \U{E9}tablissement",
      `55` = "division/clinique psychiatrique, m\U{EA}me \U{E9}tablissement",
      `60` = "autre h\U{F4}pital (soins aigus) ou maison de naissance",
      `66` = "division/clinique de soins aigus, m\U{EA}me \U{E9}tablissement",
      `70` = "\U{E9}tablissement d'ex\U{E9}cution des peines",
      `81` = "autre",
      `82` = "sans domicile fixe",
      `83` = "Clinique de r\U{E9}adaptation, autre \U{E9}tablissement",
      `84` = "Division/clinique de r\U{E9}adaptation, m\U{EA}me \U{E9}tablissement",
      `90` = "inconnu"
    ),
    it = c(
      `11` = "domicilio, da solo/a",
      `12` = "domicilio, con altri",
      `21` = "domicilio, da solo/a, con Spitex",
      `22` = "domicilio, con altri, con Spitex",
      `30` = "casa di cura (comprese case per anziani medicalizzate)",
      `40` = "altro istituto sanitario non ospedaliero, non medicalizzato",
      `41` = "istituto d\U{2019}accoglienza (foyer non medicalizzato)",
      `50` = "clinica psichiatrica, altro istituto",
      `55` = "reparto psichiatrico / clinica psichiatrica, stesso istituto",
      `60` = "altro istituto ospedaliero (ospedale acuto) o casa per partorienti",
      `66` = "reparto acuto / ospedale acuto, stesso istituto",
      `70` = "penitenziario",
      `81` = "altro",
      `82` = "senza dimora fissa",
      `83` = "clinica di riabilitazione, altro istituto",
      `84` = "reparto di riabilitazione / clinica di riabilitazione, stesso istituto",
      `90` = "sconosciuto"
    )
  ),
  psy_eintritt_teilzeit = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_vollzeit = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_arbeitslos = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_hausarbeit = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_ausbildung = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_reha = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_rente = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_gesch_arbeit = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_eintritt_unbekannt = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_einweisende_instanz = list(
    de = c(
      `11` = "Patient kommt auf eigene Initiative",
      `12` = "Angeh\U{F6}rige, Bezugsperson, freiw. Mitarb., Laien",
      `31` = "praktizierender Arzt",
      `32` = "praktizierender Psychiater/innen",
      `40` = "nicht \U{E4}rztliche (Psycho-, u.a.) Therapeut/innen",
      `33` = "Allgemeinspital, somatisches Spital",
      `34` = "Psychiatrie eigene Instit. ambulant",
      `36` = "Psychiatrie eigene Instit. station\U{E4}r",
      `37` = "Psychiatrie andere Instit. ambulant",
      `39` = "Psychiatrie andere Instit. station\U{E4}r",
      `20` = "Rettungsdienst (Ambulanz o.\U{E4}., nicht \U{E4}rztlich)",
      `50` = "Beratungsstelle, soziale Institution",
      `81` = "Beh\U{F6}rden: Vormundschaft, F\U{FC}rsorge, zivilrechtlich",
      `60` = "strafrechtlich, Justizbeh\U{F6}rden",
      `82` = "Milit\U{E4}r",
      `83` = "Versicherungen, IV, SUVA",
      `84` = "andere",
      `90` = "unbekannt"
    ),
    fr = c(
      `11` = "le patient vient de sa propre initiative",
      `12` = "proche, personne de confiance, volontaire, personne quelconque",
      `31` = "m\U{E9}decin praticien",
      `32` = "psychiatre praticien",
      `40` = "(psycho)th\U{E9}rapeute non-m\U{E9}decin",
      `33` = "h\U{F4}pital g\U{E9}n\U{E9}raliste ou sp\U{E9}cialis\U{E9} dans le somatique",
      `34` = "clin. psych. de l\U{2019}\U{E9}tabl., ambulatoire",
      `36` = "clin. psych. de l\U{2019}\U{E9}tabl., stationnaire",
      `37` = "clinique psych. externe, ambulatoire",
      `39` = "clin. psych. externe, stationnaire",
      `20` = "service de sauvetage (ambulance ou autre, non m\U{E9}dical)",
      `50` = "centre de consultation, service social",
      `81` = "autorit\U{E9}: de tutelle, civil, assistance publique",
      `60` = "institution p\U{E9}nale, autorit\U{E9}s judiciaires",
      `82` = "autorit\U{E9}s militaires",
      `83` = "assurances, AI, SUVA",
      `84` = "autre",
      `90` = "inconnu"
    ),
    it = c(
      `11` = "il paziente si presenta di sua iniziativa",
      `12` = "familiare, persona di riferimento, volontario, persona che non opera nel settore sanitario",
      `31` = "medico curante",
      `32` = "psichiatra curante",
      `40` = "terapista non medico (psicoterapeuta o altro)",
      `33` = "ospedale generale, ospedale somatico",
      `34` = "psichiatria stesso stabilimento, ambulatoriale",
      `36` = "psichiatria stesso stabilimento, stazionario",
      `37` = "psichiatria altro stabilimento, ambulatoriale",
      `39` = "psichiatria altro stabilimento, stazionario",
      `20` = "servizio di soccorso (ambulanza o altro, non medico)",
      `50` = "consultorio, servizio sociale",
      `81` = "autorit\U{E0}: tutore, assistenza, autorit\U{E0} civile",
      `60` = "autorit\U{E0} penale, autorit\U{E0} giudiziaria",
      `82` = "esercito",
      `83` = "assicurazioni, AI, SUVA",
      `84` = "altro",
      `90` = "sconosciuto"
    )
  ),
  psy_fu = list(
    de = c(`1` = "ohne FU", `2` = "mit FU"),
    fr = c(`1` = "sans PAFA", `2` = "avec PAFA"),
    it = c(`1` = "senza RSA", `2` = "con RSA")
  ),
  psy_behandlung = list(
    de = c(
      `1` = "Krisenintervention",
      `2` = "Beratung",
      `3` = "Psychotherapie (einzel)",
      `4` = "Psychotherapie (Paar oder Familie)",
      `5` = "Psychotherapie (Gruppe)",
      `6` = "integrierte psychiatrische Behandlung",
      `7` = "Sozialpsychiatrische Behandlung/Betreuung",
      `8` = "Konsilium, Liaisondienst",
      `9` = "Sachhilfe",
      `10` = "Abkl\U{E4}rung (nur)",
      `11` = "Begutachtung",
      `12` = "andere"
    ),
    fr = c(
      `1` = "intervention d\U{2019}urgence",
      `2` = "consultation",
      `3` = "psychoth\U{E9}rapie individuelle",
      `4` = "psychoth\U{E9}rapie de couple ou en famille",
      `5` = "psychoth\U{E9}rapie de groupe",
      `6` = "traitement psychiatrique int\U{E9}gr\U{E9}",
      `7` = "traitement/assistance sociopsychiatrique",
      `8` = "consilium",
      `9` = "assistance technique",
      `10` = "diagnostic (uniquement)",
      `11` = "expertise",
      `12` = "autre"
    ),
    it = c(
      `1` = "intervento di crisi",
      `2` = "consulenza",
      `3` = "psicoterapia individuale",
      `4` = "psicoterapia di coppia o famiglia",
      `5` = "psicoterapia di gruppo",
      `6` = "trattamento psichiatrico integrato",
      `7` = "trattamento/assistenza sociopsichiatrici",
      `8` = "consulto medico, servizio di liaison",
      `9` = "aiuto materiale",
      `10` = "accertamento (solo)",
      `11` = "perizia",
      `12` = "altro"
    )
  ),
  psy_pp_neuroleptika = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_depotneuroleptika = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_antidepressiva = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_tranquilizer = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_hypnotika = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_antiepileptika = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_lithium = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_substitution = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_suchtaversion = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_antiparkinson = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_andere = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_pp_koerper_medi = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "no", `1` = "s\U{EC}")
  ),
  psy_entsch_austritt = list(
    de = c(
      `11` = "in gegenseitigem Einvernehmen",
      `12` = "auf Initiative der Behandelnden",
      `20` = "auf Initiative des/r Patienten/in (nur gegen Ansicht des Behandelnden)",
      `30` = "auf Initiative Dritter",
      `40` = "\U{DC}bertritt von Akutstation",
      `51` = "Patient hat sich suizidiert",
      `52` = "Patient ist gestorben (ausser Suizid)",
      `80` = "anderes",
      `90` = "unbekannt"
    ),
    fr = c(
      `11` = "d\U{2019}un commun accord",
      `12` = "\U{E0} la demande du m\U{E9}decin traitant",
      `20` = "\U{E0} la demande du patient (seulement contre l\U{2019}avis du m\U{E9}decin traitant)",
      `30` = "\U{E0} la demande d\U{2019}une tierce personne",
      `40` = "transfert du service des soins aigus",
      `51` = "le patient s\U{2019}est suicid\U{E9}",
      `52` = "le patient est d\U{E9}c\U{E9}d\U{E9} (suicide except\U{E9})",
      `80` = "autre",
      `90` = "inconnu"
    ),
    it = c(
      `11` = "di comune intesa",
      `12` = "su iniziativa della persona curante",
      `20` = "su iniziativa del paziente (contro il parere della persona curante)",
      `30` = "su iniziativa di terzi",
      `40` = "trasferimento dal reparto cure acute",
      `51` = "suicidio del paziente",
      `52` = "decesso del paziente (esclusi i suicidi)",
      `80` = "altro",
      `90` = "sconosciuto"
    )
  ),
  psy_austritt_aufenthalt = list(
    de = c(
      `11` = "zuhause, alleine",
      `12` = "zuhause, mit anderen",
      `20` = "Krankenheim, Pflegeheim",
      `31` = "Wohnheim",
      `32` = "Altersheim, andere sozialmed. Instit.",
      `40` = "Psychiatrische Klinik, anderer Betrieb",
      `44` = "Psychiatrische Abteilung/Klinik, gleicher Betrieb",
      `50` = "Rehabilitationsklinik, anderer Betrieb",
      `55` = "Rehabilitationsabteilung/-klinik, gleicher Betrieb",
      `60` = "anderes Krankenhaus   (Akutspital) oder    Geburtshaus",
      `66` = "Akutabteilung/-klinik,  gleicher Betrieb",
      `70` = "Strafvollzugsanstalt",
      `81` = "ohne festen Wohnsitz",
      `82` = "andere",
      `90` = "unbekannt",
      `00` = "gestorben"
    ),
    fr = c(
      `11` = "\U{E0} la maison, seul(e)",
      `12` = "\U{E0} la maison, avec d'autres personnes",
      `20` = "foyer, home m\U{E9}dicalis\U{E9}",
      `31` = "logement dans un foyer",
      `32` = "foyer pour pers. \U{E2}g\U{E9}es, autre institution m\U{E9}dico-sociale",
      `40` = "clinique psychiatrique, autre \U{E9}tablissement",
      `44` = "division/clinique psychiatrique, m\U{EA}me \U{E9}tablissement",
      `50` = "institution de r\U{E9}adaptation, autre \U{E9}tablissement",
      `55` = "division/clinique de r\U{E9}adaptation, m\U{EA}me \U{E9}tablissement",
      `60` = "autre h\U{F4}pital (soins aigus) ou maison de naissance",
      `66` = "division/clinique de soins aigus, m\U{EA}me \U{E9}tablissement",
      `70` = "\U{E9}tabl. d'ex\U{E9}cution des peines",
      `81` = "sans domicile fixe",
      `82` = "autre",
      `90` = "inconnu",
      `00` = "d\U{E9}c\U{E9}d\U{E9}(e)"
    ),
    it = c(
      `11` = "domicilio, da solo",
      `12` = "domicilio, con altri",
      `20` = "casa di cura (comprese case per anziani medicalizzate)",
      `32` = "altro istituto sanitario non ospedaliero, non medicalizzato",
      `31` = "istituto d\U{2019}accoglienza (foyer non medicalizzato)",
      `40` = "clinica psichiatrica, altro istituto",
      `44` = "reparto psichiatrico / clinica psichiatrica, stesso istituto",
      `50` = "clinica di riabilitazione, altro istituto",
      `55` = "reparto di riabilitazione / clinica di riabilitazione, stesso istituto",
      `60` = "altro istituto ospedaliero (ospedale acuto) o casa per partorienti",
      `66` = "reparto acuto / ospedale acuto, stesso istituto",
      `70` = "penitenziario",
      `81` = "senza dimora fissa",
      `82` = "altro",
      `90` = "sconosciuto",
      `00` = "decesso"
    )
  ),
  psy_austritt_behandlung = list(
    de = c(
      `10` = "geheilt / kein Behandlungsbedarf",
      `21` = "Bezugsperson, freiw. Mitarb., Laien",
      `22` = "Beratungsstelle, soziale Institution, Sozialdienst, etc.",
      `23` = "praktizierender Arzt/in",
      `24` = "praktizierender Psychiater/in",
      `25` = "nicht \U{E4}rztlicher Psychotherapeut/in",
      `26` = "Psychiatrie, eigene Instit., ambulant",
      `27` = "Psychiatrie, andere Instit., ambulant",
      `28` = "keine, w\U{E4}re aber notwendig gewesen",
      `30` = "Pflegepersonal, Spitex, etc.",
      `41` = "Allgemeinspital, somatisches Spital",
      `42` = "Psychiatrie, eigene Instit., station\U{E4}r",
      `43` = "Psychiatrie, andere Instit., station\U{E4}r",
      `51` = "Rehabilitation (ambulant oder station\U{E4}r)",
      `81` = "Beh\U{F6}rden",
      `82` = "andere",
      `90` = "unbekannt",
      `0` = "gestorben"
    ),
    fr = c(
      `10` = "gu\U{E9}ri, aucun besoin de traitement",
      `21` = "personne de confiance, volontaire, personne quelconque",
      `22` = "centre de consultation, services sociaux, etc.",
      `23` = "m\U{E9}decin praticien",
      `24` = "psychiatre praticien",
      `25` = "psychoth\U{E9}rapeute non-m\U{E9}decin",
      `26` = "clin. psych. de l\U{2019}\U{E9}tabl., trait. ambulatoire",
      `27` = "clin. psych. ext., trait. ambulatoire",
      `28` = "aucune, mais serait n\U{E9}cessaire",
      `30` = "personnel soignant, soins \U{E0} domicile, etc.",
      `41` = "h\U{F4}pital g\U{E9}n\U{E9}ral ou sp\U{E9}cialis\U{E9} dans le somatique",
      `42` = "clinique psych. de l\U{2019}\U{E9}tabl., stationnaire",
      `43` = "clinique psych. ext., trait. stationnaire",
      `51` = "r\U{E9}adaptation (ambulatoire ou stationnaire)",
      `81` = "autorit\U{E9}s",
      `82` = "autre",
      `90` = "inconnu",
      `0` = "d\U{E9}c\U{E9}d\U{E9}(e)"
    ),
    it = c(
      `10` = "guarito/nessun trattamento necessario",
      `21` = "persona di riferimento, volontario/a, persona che non opera nel settore sanitario",
      `22` = "consultorio, istituzione sociale, servizio sociale,\U{2026} (SMP, SPS)",
      `23` = "medico curante",
      `24` = "psichiatra curante",
      `25` = "psicoterapeuta non medico",
      `26` = "psichiatria stesso stabilimento, ambulatoriale",
      `27` = "psichiatria altro stabilimento, ambulatoriale",
      `28` = "nessuno, ma sarebbe stato necessario",
      `30` = "personale curante, Spitex, ecc.",
      `41` = "ospedale generale, ospedale somatico",
      `42` = "psichiatria stesso stabilimento, stazionario",
      `43` = "psichiatria altro stabilimento, stazionario",
      `51` = "riabilitazione (ambulatoriale o stazionaria)",
      `81` = "autorit\U{E0}",
      `82` = "altro",
      `90` = "sconosciuto",
      `0` = "decesso"
    )
  ),
  psy_behandlungsbereich = list(
    de = c(
      `1` = "Allgemein Psychiatrie",
      `2` = "Kinder und Jugend-psychiatrie",
      `3` = "Alterspsychiatrie",
      `4` = "Abh\U{E4}ngigkeitserkrankungen",
      `5` = "Forensik"
    ),
    fr = c(
      `1` = "psychiatrie g\U{E9}n\U{E9}rale",
      `2` = "p\U{E9}dopsychiatrie",
      `3` = "psychiatrie g\U{E9}riatrique",
      `4` = "maladies de la d\U{E9}pendance",
      `5` = "science forensique"
    ),
    it = c(
      `1` = "psichiatria generale",
      `2` = "psichiatria infantile",
      `3` = "psichiatria geriatrica",
      `4` = "malattie legate alla dipendenza",
      `5` = "scienze forensi"
    )
  ),
  diagnose_seitigkeit = list(
    de = c(
      `0` = "beidseitig",
      `1` = "einseitig rechts",
      `2` = "einseitig links",
      `3` = "einseitig unbekannt",
      `9` = "unbekannt"
    ),
    fr = c(
      `0` = "bilat\U{E9}ral",
      `1` = "unilat\U{E9}ral droit",
      `2` = "unilat\U{E9}ral gauche",
      `3` = "unilat\U{E9}ral c\U{F4}t\U{E9} inconnu",
      `9` = "inconnu"
    ),
    it = c(
      `0` = "bilaterale",
      `1` = "unilaterale, destra",
      `2` = "unilaterale, sinistra",
      `3` = "unilaterale, lato sconosciuto",
      `9` = "sconosciuto"
    )
  ),
  diagnose_poa = list(
    de = c(
      `1` = "ja (Diagnose bestand bei Krankenhauseintritt)",
      `2` = "nein (Diagnose bestand bei Krankenhauseintritt nicht)",
      `3` = "klinisch unbestimmt (poa klinisch nicht feststellbar)",
      `9` = "unbekannt (fehlende Dokumentation)",
      leer = "keine Angabe notwendig"
    ),
    fr = c(
      `1` = "oui (le diagnostic existait lors de l\U{2019}admission \U{E0} l\U{2019}h\U{F4}pital)",
      `2` = "non (le diagnostic n\U{2019}existait pas lors de l\U{2019}admission \U{E0} l\U{2019}h\U{F4}pital)",
      `3` = "cliniquement non d\U{E9}fini (poa pas de constat clinique)",
      `9` = "inconnu (documentation manquante)",
      vide = "pas d\U{2019}indication n\U{E9}cessaire"
    ),
    it = c(
      `1` = "s\U{EC} (la diagnosi sussisteva al momento dell'ammissione)",
      `2` = "no (la diagnosi non sussisteva al momento dell'ammissione)",
      `3` = "clinicamente indeterminabile (POA non accertabile clinicamente)",
      `9` = "sconosciuto (documentazione mancante)",
      Vuoto = "informazione non necessaria"
    )
  ),
  behandlung_seitigkeit = list(
    de = c(
      `0` = "beidseitig",
      `1` = "einseitig rechts",
      `2` = "einseitig links",
      `3` = "einseitig unbekannt",
      `9` = "unbekannt"
    ),
    fr = c(
      `0` = "bilat\U{E9}ral",
      `1` = "unilat\U{E9}ral droit",
      `2` = "unilat\U{E9}ral gauche",
      `3` = "unilat\U{E9}ral c\U{F4}t\U{E9} inconnu",
      `9` = "inconnu"
    ),
    it = c(
      `0` = "bilaterale",
      `1` = "unilaterale, destra",
      `2` = "unilaterale, sinistra",
      `3` = "unilaterale, lato sconosciuto",
      `9` = "sconosciuto"
    )
  ),
  behandlung_auswaerts = list(
    de = c(
      `1` = "eigenes Krankenhausareal, anderer Betrieb",
      `2` = "ausserhalb eigenem Krankenhausareal, gleicher Betrieb",
      `3` = "ausserhalb eigenem Krankenhausareal, anderer Betrieb",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "m\U{EA}me aire hospitali\U{E8}re, autre \U{E9}tablissement.",
      `2` = "M\U{EA}me \U{E9}tablissement, autre aire hospitali\U{E8}re.",
      `3` = "Autre \U{E9}tablissement, autre aire hospitali\U{E8}re.",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "propria area, altro stabilimento",
      `2` = "altra area, stesso stabilimento",
      `3` = "altra area, altro stabilimento",
      `9` = "sconosciuto"
    )
  ),
  medi_verabreichungsart = list(
    de = c(
      ET = "endotracheopulmonal",
      IL = "intral\U{E4}sional",
      IM = "intramuskul\U{E4}r",
      IMPL = "Implantat",
      Inhal = "Inhalation",
      IT = "intrathekal",
      IV = "intraven\U{F6}s",
      IVITR = "intravitreal",
      N = "nasal",
      O = "oral",
      P = "parenteral",
      R = "rektal",
      SL = "sublingual/bukkal",
      SC = "subkutan",
      T = "topisch",
      TD = "transdermal",
      VAG = "vaginal",
      U = "urethral"
    ),
    fr = c(
      ET = "endotrach\U{E9}obronchique",
      IL = "intral\U{E9}sionnelle",
      IM = "intramusculaire",
      IMPL = "implant",
      Inhal = "inhalation",
      IT = "intrath\U{E9}cale",
      IV = "intraveineuse",
      IVITR = "intra-vitr\U{E9}enne",
      N = "nasale",
      O = "orale",
      P = "parent\U{E9}rale",
      R = "rectale",
      SL = "sublinguale/buccale",
      SC = "sous-cutan\U{E9}e",
      T = "topique",
      TD = "transdermique",
      VAG = "vaginale",
      U = "ur\U{E9}trale"
    ),
    it = c(
      ET = "endotracheopolmonare",
      IL = "intralesionale",
      IM = "intramuscolare",
      IMPL = "impianto",
      Inhal = "inalazione",
      IT = "intratecale",
      IV = "endovenoso",
      IVITR = "intravitreale",
      N = "nasale",
      O = "orale",
      P = "parenterale",
      R = "rettale",
      SL = "sublinguale/buccale",
      SC = "sottocutaneo",
      T = "topico",
      TD = "transdermale",
      VAG = "vaginale",
      U = "uretrale"
    )
  ),
  rech_kostentraeger = list(
    de = c(
      `0` = "Krankenversicherung (OKP)",
      `1` = "Invalidenversicherung",
      `2` = "Milit\U{E4}rversicherung",
      `3` = "Unfallversicherung",
      `4` = "Selbstzahler",
      `5` = "Zusatzversicherung",
      `6` = "Kanton",
      `7` = "Gemeinde",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `0` = "assurance-maladie (AOS)",
      `1` = "assurance invalidit\U{E9}",
      `2` = "assurance militaire",
      `3` = "assurance accident",
      `4` = "personne payant elle-m\U{EA}me",
      `5` = "assurance compl\U{E9}mentaire",
      `6` = "canton",
      `7` = "commune",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `0` = "assicurazione malattie (AOMS)",
      `1` = "assicurazione invalidit\U{E0}",
      `2` = "assicurazione militare",
      `3` = "assicurazione infortuni",
      `4` = "paziente autopagante",
      `5` = "assicurazione complementare",
      `6` = "cantone",
      `7` = "comune",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  ktr_methodik = list(
    de = c(
      `1` = "T\U{E4}tigkeit (oder gem\U{E4}ss REKOLE)",
      `2` = "Sch\U{E4}tzung",
      `3` = "Ertrag",
      `4` = "Erfahrungswerte (anderer Jahre)",
      `5` = "Kontierung",
      `6` = "Umlage",
      `9` = "andere"
    ),
    fr = c(
      `1` = "activit\U{E9} (ou selon REKOLE\U{AE})",
      `2` = "estimation",
      `3` = "produit",
      `4` = "valeurs d\U{2019}exp\U{E9}rience (des autres ann\U{E9}es)",
      `5` = "comptage",
      `6` = "r\U{E9}partition",
      `9` = "autre"
    ),
    it = c(
      `1` = "Attivit\U{E0} (o secondo REKOLE)",
      `2` = "Stima",
      `3` = "Ricavo",
      `4` = "Valori empirici (anni precedenti)",
      `5` = "Codifica contabile",
      `6` = "Ripartizione",
      `9` = "Altro"
    )
  ),
  op_liste = list(
    de = c(`0` = "nein", `1` = "ja"),
    fr = c(`0` = "non", `1` = "oui"),
    it = c(`0` = "No", `1` = "S\U{EC}")
  ),
  op_rolle = list(
    de = c(
      `1` = "Erstoperateur",
      `2` = "Zweitoperateur (Supervisor oder zweiter Hauptoperateur)",
      `3` = "andere Rolle"
    ),
    fr = c(
      `1` = "personne principale qui op\U{E8}re",
      `2` = "personne secondaire qui op\U{E8}re (supervision ou deuxi\U{E8}me personne principale qui op\U{E8}re)",
      `3` = "autre r\U{F4}le"
    ),
    it = c(
      `1` = "primo operatore",
      `2` = "secondo operatore (supervisore o secondo operatore principale)",
      `3` = "altro ruolo"
    )
  ),
  episode_art = list(
    de = c(
      `1` = "Behandlung vor Ort",
      `2` = "Zwischenaustritt/Wiedereintritt",
      `3` = "Urlaub (Ferien)",
      `4` = "Belastungserprobung",
      `5` = "ambulante Behandlung ausw\U{E4}rts"
    ),
    fr = c(
      `1` = "traitement sur place",
      `2` = "sortie interm\U{E9}diaire/r\U{E9}admission",
      `3` = "vacances (cong\U{E9}s)",
      `4` = "sortie d\U{2019}essai",
      `5` = "traitement ambulatoire extra-muros"
    ),
    it = c(
      `1` = "trattamento sul posto",
      `2` = "dimissione intermedia / riammissione",
      `3` = "congedo (vacanze)",
      `4` = "uscita di prova",
      `5` = "trattamento ambulatoriale esterno"
    )
  ),
  wiedereintritt_aufenthalt = list(
    de = c(
      `1` = "zuhause",
      `2` = "Krankenheim, Pflegeheim",
      `3` = "Altersheim, andere sozialmed. Institution",
      `4` = "Psychiatrische Klinik, anderer Betrieb",
      `44` = "Psychiatrische Abteilung/Klinik, gleicher Betrieb",
      `5` = "Rehabilitationsklinik, anderer Betrieb",
      `55` = "Rehabilitationsabteilung/-klinik, gleicher Betrieb",
      `6` = "anderes Krankenhaus (Akutspital) oder Geburtshaus",
      `66` = "Akutabteilung/-klinik,  gleicher Betrieb",
      `7` = "Strafvollzugsanstalt",
      `8` = "andere",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "domicile",
      `2` = "\U{E9}tabl. de sant\U{E9} non hospit. m\U{E9}dicalis\U{E9}",
      `3` = "id. non m\U{E9}dicalis\U{E9}",
      `4` = "institution psychiatrique, autre \U{E9}tablissement",
      `44` = "Division/clinique psychiatrique, m\U{EA}me \U{E9}tablissement",
      `5` = "Institution de r\U{E9}adaptation, autre \U{E9}tablissement",
      `55` = "Division/clinique de r\U{E9}adaptation, m\U{EA}me \U{E9}tablissement",
      `6` = "Autre h\U{F4}pital (soins aigus) ou maison de naissance",
      `66` = "Division/Clinique de soins aigus, m\U{EA}me \U{E9}tablissement",
      `7` = "institution d\U{2019}ex\U{E9}cution des peines",
      `8` = "autre",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "domicilio",
      `2` = "casa di cura (comprese case per anziani medicalizzate)",
      `3` = "altro istituto sanitario non ospedaliero, non medicalizzato",
      `4` = "clinica psichiatrica, altro istituto",
      `44` = "reparto psichiatrico / clinica psichiatrica, stesso istituto",
      `5` = "clinica di riabilitazione, altro istituto",
      `55` = "reparto di riabilitazione / clinica di riabilitazione, stesso istituto",
      `6` = "altro stabilimento ospedaliero (ospedale acuto) o casa per partorienti",
      `66` = "reparto acuto / ospedale acuto, stesso istituto",
      `7` = "penitenziario",
      `8` = "altro",
      `9` = "sconosciuto"
    )
  ),
  grund_wiedereintritt = list(
    de = c(
      `1` = "Wiederaufnahme mit gleicher MDC",
      `2` = "Wiederaufnahme mit gleicher MDC durch Komplikationen",
      `3` = "R\U{FC}ckverlegung",
      `9` = "unbekannt"
    ),
    fr = c(
      `1` = "r\U{E9}admission avec MDC identique",
      `2` = "r\U{E9}admission avec MDC identique, en raison de complications",
      `3` = "r\U{E9}hospitalisation",
      `9` = "inconnu"
    ),
    it = c(
      `1` = "riammissione con la stessa MDC",
      `2` = "riammissione con la stessa MDC a seguito di complicanze",
      `3` = "paziente ritrasferito",
      `9` = "sconosciuto"
    )
  )
)

code_dict <- list(v1.4 = v1.4)

# use data ----------------------------------------------------------

usethis::use_data(code_dict, var_types, internal = TRUE, overwrite = TRUE)
