
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(tidyr)
library(readODS)
library(stringr)
library(lubridate)
library(ggplot2)
library(readr)

mois_ref <- "2024-06"
# le resultat doit etre de la forme mois_ref <- "20xx-xx"

######################################################################
######################################################################

Result <- read.csv2("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo/Result.csv")
Result_agreg <- read.csv2("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo/Result_agreg.csv")

# Result_horsQoE <- read.csv2("E:/Analyse_GEPEX/ProgR_QoE_RapportDispo/Result_horsQoE.csv")
# Result_horsQoE_agreg <- read.csv2("E:/Analyse_GEPEX/ProgR_QoE_RapportDispo/Result_horsQoE_agreg.csv")

######################################################################
######################################################################

chemin_racine <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo"
setwd(dir = chemin_racine)

# Import du fichier Calc contenant la liste des Exclusions et des incidents
FichierCALC <- read_ods(paste0(chemin_racine,"/incidents_et_Exclusions_",str_sub(mois_ref,1,4),".ods"),
                        sheet=paste0("ListeMois_",str_sub(mois_ref,6,7))) %>% 
  mutate(moment_DEB=str_to_lower(moment_DEB),
         moment_FIN=str_to_lower(moment_FIN)) %>% select(-MoisRef)

Excl_DEB <- FichierCALC %>% filter(Type=="Exclusion") %>% select(Service,moment_DEB,commentaire) %>% rename(date=moment_DEB) %>% mutate(type="DEB",numligne=row_number())
Excl_FIN <- FichierCALC %>% filter(Type=="Exclusion") %>% select(Service,moment_FIN,commentaire) %>% rename(date=moment_FIN) %>% mutate(type="FIN",numligne=row_number())
Uniquement_Excl <- rbind(Excl_DEB,Excl_FIN) %>% arrange(numligne,type) %>% select(-numligne,-type)
rm(Excl_DEB,Excl_FIN)

Uniquement_Inc <- FichierCALC %>% filter(Type=="Inc")

######################################################################
######################################################################


SousRepertoire <- "APIM"
nom_QoE <- "QoE-APIM-Connected"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-Unconnected"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-Connected-BDM"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "APIM_DL"
nom_QoE <- "QoE-APIM-Connected-DL"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-Requete-DL"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "APIM_Rmes"
nom_QoE <- "QoE-APIM-Connected-Rmes-Meta"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-Requetes-RMES-Meta"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "APIM_Sirene"
nom_QoE <- "QoE-APIM-Connected-Sirene"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-Requetes-Sirene"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-APIM-RequetesDirect-Sirene"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SYRACUSE"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ATHENES"
nom_QoE <- "QoE-ATHENES-Crea_Mod"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "BRPP"
nom_QoE <- "QoE-BRPP_Meta"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-BRPP-France_Connect"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-BRPP-Backup-France_Connect"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "CITRUS"
nom_QoE <- "QoE-CITRUS"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "COLLECTE_PRIX"
nom_QoE <- "QoE-PRISME"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "COLTRANE"
nom_QoE <- "QoE-COLTRANE-Compte"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-COLTRANE-Enquete"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-COLTRANE-Enquete2"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "CONJONCTURE"
nom_QoE <- "QoE-CONJONCTURE"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ECMOSS"
nom_QoE <- "QoE-ECMOSS"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ELIRE"
nom_QoE <- "QoE-ELIRE-Interne"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ELIRE-Liste_Electeurs_Beta"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ELIRE-Liste_Electeurs_Prod"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ELIRE-XWiki"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ENQUETE_EMPLOI"
nom_QoE <- "QoE-EEC-Portail"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-EEC-Sicore"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-EEC-Sirene"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ESANE"
nom_QoE <- "QoE-ESANE-Diffusion"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ESANE-Diffusion_mineure"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-ESANE-Diffusion_mineure2"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-ESANE-Metropole"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ESANE-Metropole_mineure"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-ESANE-Metropole_mineure2"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 


SousRepertoire <- "FIGARO"
nom_QoE <- "QoE-FIGARO-Catalogue"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-FIGARO-IHM"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "HARMONICA"
nom_QoE <- "QoE-HARMONICA-Consultation"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-HARMONICA-Expertise"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "LEI"
nom_QoE <- "QoE-LEI-France"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-LEI-Poste_De_Gestion"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "MESSAGERIE"
nom_QoE <- "QoE-OWA"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "OCAPI"
nom_QoE <- "QoE-OCAPI-Series"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "ORIGAMI"
nom_QoE <- "QoE-ORIGAMI-Echantillon"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ORIGAMI-Evenements"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-ORIGAMI-Questionnaire_UL"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "PLATINE"
nom_QoE <- "QoE-PLATINE-Authentification"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-PLATINE-EEC"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-PLATINE-Questionnaire"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "RECENSEMENT"
nom_QoE <- "QoE-RP-Questionnaire-Lien"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-RP-RPetMoi"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
# nom_QoE <- "QoE-RP-Questionnaire"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
# nom_QoE <- "QoE-RP-Omer"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "RORCAL"
nom_QoE <- "QoE-RORCAL-Google"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "SABIANE"
nom_QoE <- "QoE-SABIANE-Poste_De_Gestion"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "SERV_TRANSVERSES"
nom_QoE <- "QoE-KEYCLOAK-Beta"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-KEYCLOAK-DMZ"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-KEYCLOAK-Prod"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "SIAMOI"
nom_QoE <- "QoE-SIAMOI"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "SICORE"
nom_QoE <- "QoE-SICORE-Externe"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SICORE-Interne"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SICORE-Interne2"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")

SousRepertoire <- "SIRENE"
nom_QoE <- "QoE-SIRENE-AviSitu"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENE-Courrier"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENE-Diffcom"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
# nom_QoE <- "QoE-SIRENE-IHM-Habilitateur"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
# nom_QoE <- "QoE-SIRENEG-Atene"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENEG-Atis_Consultation"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENEG-Rgs"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENEG-Tdb"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
# nom_QoE <- "QoE-SIRENEG-Web_Externe"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
# nom_QoE <- "QoE-SIRENE-IdentXml_Ext"
# source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
# source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENE-IdentXml_Int"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRENE-IdentXmlCons_Int"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WS-Atis_BRPP"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WS-Atis_Sicore"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 


SousRepertoire <- "SIRENE4"
nom_QoE <- "QoE-SIRENE4-API_Declaration"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENE4-Ident_PM"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENE4-Ident_PP"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENE4-Siren"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENE4-RE_Avec-echo"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R") 
nom_QoE <- "QoE-SIRENE4-RE_Sans-echo"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "SIRUS"
nom_QoE <- "QoE-SIRUS-Externe-Recherche_par_Denom"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-SIRUS-Externe-Recherche_par_Ident"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


SousRepertoire <- "WEB4G"
nom_QoE <- "QoE-WEB4G"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WEB4G-BDM"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WEB4G-Contact"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WEB4G-DDL"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")
nom_QoE <- "QoE-WEB4G-Previ"
source(file = "ProgR_QoE_RapDispo_03_CreaGraph.R")
source(file = "ProgR_QoE_RapDispo_04_CreaData.R")


#SousRepertoire <- "TESTRIE"
#nom_QoE <- "QoE-TESTRIE-AlpafFinances"
#nom_QoE <- "QoE-TESTRIE-GalileoFinances"
#nom_QoE <- "QoE-TESTRIE-VideoFinances"

