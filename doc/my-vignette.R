## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 5,
  out.width = "70%",
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(baseph)
library(gtsummary)
library(tidyverse)
library(kableExtra)

## ----nenquete-----------------------------------------------------------------
nbo <- nb.obs.ph(px = 0.5, ex = 0.1, np = 1e5)

## ----listerand, eval = FALSE--------------------------------------------------
# listrandph(nbcent = 1, # Nombre de centre
#            nbtrait = 2, # nombre de traitements/classes (habituellement 2)
#            nbcas = 100 # Nombre de cas total prévu
# )

## ----petittab-----------------------------------------------------------------
patients |>
  dplyr::select(sexe, age, escarre, lieudevie1) |>
  tbl_summary(by = escarre) |>
  tabph(nomv = "Escarre", # Titre des colonnes
        normx = TRUE # Tests paramétriques
        ) |>  
  pexptabph(exp = FALSE, # pas d'export .xls
            lg = FALSE) # pas de longtable

## ----barsimple----------------------------------------------------------------
barsimpleph(dfx = patients, # tableau de données
            varx = admission, # Variable à étudier
            titre = "Mode d'admission",
            stitre = "ICU", # sous-titre
            capt = "Admission", # légende
            lab = "admin", # label pour un lein éventuel
            angle = 60) # angle d'affichage des niveaux sur l'axe x

## ----barconf------------------------------------------------------------------
barconfph(dfx = patients, # tableau de données
          varnum = igs2, # variable numérique
          vartri = admission, # variable factorielle
          titre = "IGS II vs provenance", #
          stitre = "en ICU", tx = "Mode d'admission", # sous titre
          ty = "IGS II", # titre des y
          cap = "Texte écrit petit", # légende
          angle = 30 # angle d'affichage des niveaux sur l'axe x
)

## ----bardeux------------------------------------------------------------------
bardeuxph(patients,
          lieudevie1, # Variable en %
          admission, # variable de tri
          titre = "Escarre & mode d'admission", # titre
          stitre = "%", # sous titre
          xtitre = "Mode d'admission", # titre des x
          ltitre = "Escarre", # titre de lé légende
          angle = 20, # angle d'affichage des niveaux sur l'axe x
          lab = "aa" # label
)

## ----baroui-------------------------------------------------------------------
barouiph(dfx = patients, varx =escarre, testx =sexe, valx = "oui", titre = "Sexe")


## ----histm--------------------------------------------------------------------
histmultiph(dfx = patients,
            varx= admission, # bariables de tri, factorielle
            varn = age, # variable numérique
            tit = "Mode d'admission selon l'âge", # titre
            stit = 0.002, # p-value
            titx = "Âge", # titre de l'axe x
            bin = 1 # largeur des barres
)

## ----lollip-------------------------------------------------------------------
lollipph(dfx = patients, #
         nom = lieudevie1, # variable à afficher
         tri = c("EHPAD","Maison de retraite"), # modalités de la variable à mettre en évidence
         titre = "Lieu de vie", # titre
         capt = "Lieu de vie avant l'hospitalisation" #  légende
         )

## ----viobox-------------------------------------------------------------------
vioboxph(dfx = patients, # 
         varx = admission, # Variable de tri, factorielle
         varnum = age, # variable numérique
         titre = "grands", # titre
         stit = "et grandes", # sous titre
         titx = "Mode d'admission" # titre de l'axe x
)

## ----raincloudph--------------------------------------------------------------
raincloudph(df = patients, vcat = admission, vnum = igs2, titre = "IGS2 vs adm", titcat = "Adm", titnum = "IGS 2", adj = 1)

