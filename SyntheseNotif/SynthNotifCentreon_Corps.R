
## Chargement des packages utilises dans le programme ----
library(lubridate)
library(RMariaDB)
library(dplyr)
library(tidyr)
library(stringr)

##########################################################

date_jour <- Sys.time()
#date_jour_simple <- Sys.Date()
#tz <- Sys.timezone()

## Operation de decalage de l'heure
## mettre 1 lorsqu'on est en heure d'hiver et
## mettre 2 lorsqu'on est en heure d'ete
decalage_heure <- 2

#date retardee de 24h
date_jour_24hmoins <- date_jour - ddays(1) - dhours(decalage_heure)
#date retardee de 48h
date_jour_48hmoins <- date_jour - ddays(2) - dhours(decalage_heure)


####
partie_h <- ifelse(hour(date_jour)<10,
                   paste0("0",hour(date_jour)),
                   paste0(hour(date_jour)))
partie_m <- ifelse(minute(date_jour)<10,
                   paste0("0",minute(date_jour)),
                   paste0(minute(date_jour)))
heure_lancement <- paste0(partie_h,":",partie_m)
heure_lancement_demiheure <- paste0(partie_h,":",ifelse(as.numeric(partie_m)<30,"00","30"))

rm(partie_h,partie_m)
####

## APPEL DU PROGRAMME LANCANT LES REQUETES VERS LA BDD CENTREON
## ET DEFINISSANT TOUTES LES FONCTIONS PERSONNALISEES ----
setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/code/")
source(file = "SynthNotifCentreon_Requetes_et_Fonctions.R")


## ETAPE 1 : filtrage sur les seules observations AVEC notifications ----
##   Filtre sur uniquement les notifications faite aux contacts MAIS RETRAIT
##   msg_type = 10 correspondent aux acquittements-Service du Gepex !!
##   msg_type = 11 correspondent aux acquittements-Hote du Gepex !!
##   msg_type = 12 correspondent a je ne sais pas !!
##   on garde tout de meme les notifs par SMS qui font reference au meme evenement pour info

Req_00j_notif <- Req_00j %>% filter(notification_contact!="" & !(msg_type %in% c(10,11,12)))
Req_01j_notif <- Req_01j %>% filter(notification_contact!="" & !(msg_type %in% c(10,11,12)))

###############################################################


## ETAPE 2 : ajout variables Hosts ----
Req_00j_notif <- fct_jointure_avec_Hosts(Req_00j_notif)
Req_01j_notif <- fct_jointure_avec_Hosts(Req_01j_notif)
###############################################################


## ETAPE 3 : correction heure pour prise en compte heure ete VERSUS heure hiver ----
Req_00j_notif <- Req_00j_notif %>%
    mutate(chaine_date_corr=ymd_hms(chaine_date)+dhours(decalage_heure)) %>% select(-chaine_date)
           
Req_01j_notif <- Req_01j_notif %>% 
    mutate(chaine_date_corr=ymd_hms(chaine_date)+dhours(decalage_heure)) %>% select(-chaine_date)
###############################################################


## ETAPE 4 : transformation variables de temps ----
Req_00j_notif <- fct_transfo_var_temps(Req_00j_notif)
Req_01j_notif <- fct_transfo_var_temps(Req_01j_notif)
###############################################################


## ETAPE 5 : agregation et simplification ----
Req_00j_notif_mel_serv <- fct_agreg_melserv(Req_00j_notif)[[3]]
Req_01j_notif_mel_serv <- fct_agreg_melserv(Req_01j_notif)[[3]]

Req_00j_notif_sms_serv <- fct_agreg_smsserv(Req_00j_notif)[[2]]
Req_01j_notif_sms_serv <- fct_agreg_smsserv(Req_01j_notif)[[2]]

Req_00j_notif_mel_hote <- fct_agreg_melhote(Req_00j_notif)[[2]]
Req_01j_notif_mel_hote <- fct_agreg_melhote(Req_01j_notif)[[2]]


###############################################################


######################################################################
######################################################################
######################################################################
######################################################################

# Creation dun repertoire avec la date du jour
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
    cat("The folder already exists")
} else {
    dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}


#  Selection des fichiers a mettre en PJ du mel

# Creation dun fichier Calc (utilisation du package ReadODS) et
# export des resultats issus de 2 tables sur un seul fichier Calc
library(readODS)
write_ods(Req_00j_notif_mel_serv,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Mel_Serv_J00")
write_ods(Req_01j_notif_mel_serv,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Mel_Serv_J01",append=TRUE)

write_ods(Req_00j_notif_sms_serv,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Sms_Serv_J00",append=TRUE)
write_ods(Req_01j_notif_sms_serv,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Sms_Serv_J01",append=TRUE)

write_ods(Req_00j_notif_mel_hote,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Mel_Hote_J00",append=TRUE)
write_ods(Req_01j_notif_mel_hote,paste0(folder,"/ResumNotif_24dernheures.ods"),sheet="Mel_Hote_J01",append=TRUE)


######################################################################
######################################################################
######################################################################
######################################################################


## APPEL DU PROGRAMME DEFINISSANT LA PAGE HTML PUIS ENVOYANT LE MEL

setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/code/")
source(file = "SynthNotifCentreon_CreaHTML_et_EnvMel.R")





