
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
setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon_New/code/")
source(file = "SynthAlertCentreon_Requetes_et_Fonctions.R")


## ETAPE 1 : filtrage sur les seules observations HORS notifications ----
##   Filtre sur uniquement les notifications faite aux contacts MAIS
##   exclusion des msg_type = 10 qui correspondent aux acquittements-Service du Gepex !!
##   exclusion des msg_type = 11 qui correspondent aux acquittements-Hote du Gepex !!
##   exclusion des msg_type = 12 qui correspondent a je ne sais pas !!
##   ainsi que les notifs par SMS qui font reference au meme evenement et qui brouille des donnees de duree

Req_00j_notif <- Req_00j %>% filter(notification_contact=="" &
                                    !(msg_type %in% c("10","11","12")) &
                                    notification_cmd!="service-notify-by-sms" &
                                    type==1 &
                                    status %in% c("1","2","3") #&
                                    # status!=0
                                    )
Req_01j_notif <- Req_01j %>% filter(notification_contact=="" &
                                    !(msg_type %in% c("10","11","12")) &
                                    notification_cmd!="service-notify-by-sms" &
                                    type==1 &
                                    status %in% c("1","2","3") #&  
                                    # status!=0
                                    )

###############################################################


## ETAPE 2 : ajout variables Hosts ----
Req_00j_notif <- fct_jointure_avec_Hosts(Req_00j_notif)
Req_01j_notif <- fct_jointure_avec_Hosts(Req_01j_notif)
###############################################################


## ETAPE 3 : correction heure pour prise en compte heure ete VERSUS heure hiver ----
Req_00j_notif <- Req_00j_notif %>%
    mutate(chaine_date_corr=ymd_hms(chaine_date)+dhours(decalage_heure))
Req_01j_notif <- Req_01j_notif %>% 
    mutate(chaine_date_corr=ymd_hms(chaine_date)+dhours(decalage_heure))
###############################################################


## ETAPE 4 : ajout variables de temps ----
Req_00j_notif <- fct_ajout_var_temps(Req_00j_notif)
Req_01j_notif <- fct_ajout_var_temps(Req_01j_notif)
###############################################################


## ETAPE 5 : agregation initiale ----
Req_00j_agreg <- fct_agreg_notif_initiale(Req_00j_notif)
Req_01j_agreg <- fct_agreg_notif_initiale(Req_01j_notif)

Req_00j_agreg_Osny_Inco <- Req_00j_agreg %>% filter(datacenter=="OSNY" & status==3)
Req_00j_agreg_Osny_Warn <- Req_00j_agreg %>% filter(datacenter=="OSNY" & status==1)
Req_00j_agreg_Osny_Crit <- Req_00j_agreg %>% filter(datacenter=="OSNY" & status==2)

Req_01j_agreg_Osny_Inco <- Req_01j_agreg %>% filter(datacenter=="OSNY" & status==3)
Req_01j_agreg_Osny_Warn <- Req_01j_agreg %>% filter(datacenter=="OSNY" & status==1)
Req_01j_agreg_Osny_Crit <- Req_01j_agreg %>% filter(datacenter=="OSNY" & status==2)


Req_00j_agreg_Auzeville_Inco <- Req_00j_agreg %>% filter(datacenter=="AUZEVILLE" & status==3)
Req_00j_agreg_Auzeville_Warn <- Req_00j_agreg %>% filter(datacenter=="AUZEVILLE" & status==1)
Req_00j_agreg_Auzeville_Crit <- Req_00j_agreg %>% filter(datacenter=="AUZEVILLE" & status==2)

Req_01j_agreg_Auzeville_Inco <- Req_01j_agreg %>% filter(datacenter=="AUZEVILLE" & status==3)
Req_01j_agreg_Auzeville_Warn <- Req_01j_agreg %>% filter(datacenter=="AUZEVILLE" & status==1)
Req_01j_agreg_Auzeville_Crit <- Req_01j_agreg %>% filter(datacenter=="AUZEVILLE" & status==2)

###############################################################


Essai <- Req_00j_agreg %>% group_by(zone,host_name,service,status) %>%
  summarize(nb_notif=sum(nb_notif)) %>% ungroup() %>% as.data.frame() %>% 
  arrange(desc(nb_notif))


## ETAPE 6 : agregation par demiheure ----
Req_00j_demiheure <- fct_agreg_1var(Req_00j_agreg,demiheure,"nb_notif_00j",1)
Req_01j_demiheure <- fct_agreg_1var(Req_01j_agreg,demiheure,"nb_notif_01j",1)

Req_00j_demiheure_Osny_Inco <- fct_agreg_1var(Req_00j_agreg_Osny_Inco,demiheure,"nb_notif_00j",1)
Req_00j_demiheure_Osny_Warn <- fct_agreg_1var(Req_00j_agreg_Osny_Warn,demiheure,"nb_notif_00j",1)
Req_00j_demiheure_Osny_Crit <- fct_agreg_1var(Req_00j_agreg_Osny_Crit,demiheure,"nb_notif_00j",1)

Req_01j_demiheure_Osny_Inco <- fct_agreg_1var(Req_01j_agreg_Osny_Inco,demiheure,"nb_notif_01j",1)
Req_01j_demiheure_Osny_Warn <- fct_agreg_1var(Req_01j_agreg_Osny_Warn,demiheure,"nb_notif_01j",1)
Req_01j_demiheure_Osny_Crit <- fct_agreg_1var(Req_01j_agreg_Osny_Crit,demiheure,"nb_notif_01j",1)


Req_00j_demiheure_Auzeville_Inco <- fct_agreg_1var(Req_00j_agreg_Auzeville_Inco,demiheure,"nb_notif_00j",1)
Req_00j_demiheure_Auzeville_Warn <- fct_agreg_1var(Req_00j_agreg_Auzeville_Warn,demiheure,"nb_notif_00j",1)
Req_00j_demiheure_Auzeville_Crit <- fct_agreg_1var(Req_00j_agreg_Auzeville_Crit,demiheure,"nb_notif_00j",1)

Req_01j_demiheure_Auzeville_Inco <- fct_agreg_1var(Req_01j_agreg_Auzeville_Inco,demiheure,"nb_notif_01j",1)
Req_01j_demiheure_Auzeville_Warn <- fct_agreg_1var(Req_01j_agreg_Auzeville_Warn,demiheure,"nb_notif_01j",1)
Req_01j_demiheure_Auzeville_Crit <- fct_agreg_1var(Req_01j_agreg_Auzeville_Crit,demiheure,"nb_notif_01j",1)

###############################################################


## ETAPE 7 : Traitements pour obtention de la 1ere table resultat Req_par_demiheure 
## comparant le nombre de notifications envoyees a chaque demiheure
## entre J et J-1 ----

# Osny Inconnu
Req_par_demiheure_Osny_Inco <- fct_ope_demiheure(Req_00j_demiheure_Osny_Inco,
                                                 Req_01j_demiheure_Osny_Inco)
Req_par_demiheure_Osny_Inco <- Req_par_demiheure_Osny_Inco %>% 
  rename(nb_notif_00j_Osny_Inco=nb_notif_00j,
         nb_notif_01j_Osny_Inco=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Osny_Inco=as.character(nb_notif_00j_Osny_Inco),
         nb_notif_01j_Osny_Inco=as.character(nb_notif_01j_Osny_Inco))

# Osny Warning
Req_par_demiheure_Osny_Warn <- fct_ope_demiheure(Req_00j_demiheure_Osny_Warn,
                                                 Req_01j_demiheure_Osny_Warn)
Req_par_demiheure_Osny_Warn <- Req_par_demiheure_Osny_Warn %>% 
  rename(nb_notif_00j_Osny_Warn=nb_notif_00j,
         nb_notif_01j_Osny_Warn=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Osny_Warn=as.character(nb_notif_00j_Osny_Warn),
         nb_notif_01j_Osny_Warn=as.character(nb_notif_01j_Osny_Warn)) %>% 
  select(-calendrier)

# Osny Critical
Req_par_demiheure_Osny_Crit <- fct_ope_demiheure(Req_00j_demiheure_Osny_Crit,
                                                 Req_01j_demiheure_Osny_Crit)
Req_par_demiheure_Osny_Crit <- Req_par_demiheure_Osny_Crit %>% 
  rename(nb_notif_00j_Osny_Crit=nb_notif_00j,
         nb_notif_01j_Osny_Crit=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Osny_Crit=as.character(nb_notif_00j_Osny_Crit),
         nb_notif_01j_Osny_Crit=as.character(nb_notif_01j_Osny_Crit)) %>% 
  select(-calendrier)

##############################################################################################
##############################################################################################

# Auzeville Inconnu
Req_par_demiheure_Auzeville_Inco <- fct_ope_demiheure(Req_00j_demiheure_Auzeville_Inco,
                                                      Req_01j_demiheure_Auzeville_Inco)
Req_par_demiheure_Auzeville_Inco <- Req_par_demiheure_Auzeville_Inco %>% 
  rename(nb_notif_00j_Auzeville_Inco=nb_notif_00j,
         nb_notif_01j_Auzeville_Inco=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Auzeville_Inco=as.character(nb_notif_00j_Auzeville_Inco),
         nb_notif_01j_Auzeville_Inco=as.character(nb_notif_01j_Auzeville_Inco)) %>% 
  select(-calendrier)


# Auzeville Warning
Req_par_demiheure_Auzeville_Warn <- fct_ope_demiheure(Req_00j_demiheure_Auzeville_Warn,
                                                      Req_01j_demiheure_Auzeville_Warn)
Req_par_demiheure_Auzeville_Warn <- Req_par_demiheure_Auzeville_Warn %>% 
  rename(nb_notif_00j_Auzeville_Warn=nb_notif_00j,
         nb_notif_01j_Auzeville_Warn=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Auzeville_Warn=as.character(nb_notif_00j_Auzeville_Warn),
         nb_notif_01j_Auzeville_Warn=as.character(nb_notif_01j_Auzeville_Warn)) %>% 
  select(-calendrier)


# Auzeville Critical
Req_par_demiheure_Auzeville_Crit <- fct_ope_demiheure(Req_00j_demiheure_Auzeville_Crit,
                                                      Req_01j_demiheure_Auzeville_Crit)
Req_par_demiheure_Auzeville_Crit <- Req_par_demiheure_Auzeville_Crit %>% 
  rename(nb_notif_00j_Auzeville_Crit=nb_notif_00j,
         nb_notif_01j_Auzeville_Crit=nb_notif_01j) %>% 
  mutate(nb_notif_00j_Auzeville_Crit=as.character(nb_notif_00j_Auzeville_Crit),
         nb_notif_01j_Auzeville_Crit=as.character(nb_notif_01j_Auzeville_Crit)) %>% 
  select(-calendrier)



Req_par_demiheure <- Req_par_demiheure_Osny_Inco %>% 
    full_join(Req_par_demiheure_Osny_Warn,by=c("demiheure")) %>% 
    full_join(Req_par_demiheure_Osny_Crit,by=c("demiheure")) %>%
    full_join(Req_par_demiheure_Auzeville_Inco,by=c("demiheure")) %>% 
    full_join(Req_par_demiheure_Auzeville_Warn,by=c("demiheure")) %>%
    full_join(Req_par_demiheure_Auzeville_Crit,by=c("demiheure")) %>%
    mutate(nb_notif_00j_Osny_Inco=ifelse(nb_notif_00j_Osny_Inco=="0","",nb_notif_00j_Osny_Inco),
           nb_notif_00j_Osny_Warn=ifelse(nb_notif_00j_Osny_Warn=="0","",nb_notif_00j_Osny_Warn),
           nb_notif_00j_Osny_Crit=ifelse(nb_notif_00j_Osny_Crit=="0","",nb_notif_00j_Osny_Crit),
           nb_notif_01j_Osny_Inco=ifelse(nb_notif_01j_Osny_Inco=="0","",nb_notif_01j_Osny_Inco),
           nb_notif_01j_Osny_Warn=ifelse(nb_notif_01j_Osny_Warn=="0","",nb_notif_01j_Osny_Warn),
           nb_notif_01j_Osny_Crit=ifelse(nb_notif_01j_Osny_Crit=="0","",nb_notif_01j_Osny_Crit),
           nb_notif_00j_Auzeville_Inco=ifelse(nb_notif_00j_Auzeville_Inco=="0","",nb_notif_00j_Auzeville_Inco),
           nb_notif_00j_Auzeville_Warn=ifelse(nb_notif_00j_Auzeville_Warn=="0","",nb_notif_00j_Auzeville_Warn),
           nb_notif_00j_Auzeville_Crit=ifelse(nb_notif_00j_Auzeville_Crit=="0","",nb_notif_00j_Auzeville_Crit),
           nb_notif_01j_Auzeville_Inco=ifelse(nb_notif_01j_Auzeville_Inco=="0","",nb_notif_01j_Auzeville_Inco),
           nb_notif_01j_Auzeville_Warn=ifelse(nb_notif_01j_Auzeville_Warn=="0","",nb_notif_01j_Auzeville_Warn),
           nb_notif_01j_Auzeville_Crit=ifelse(nb_notif_01j_Auzeville_Crit=="0","",nb_notif_01j_Auzeville_Crit))

rm(
   Req_00j_demiheure_Osny_Inco, Req_00j_demiheure_Osny_Warn, Req_00j_demiheure_Osny_Crit,    
   Req_01j_demiheure_Osny_Inco, Req_01j_demiheure_Osny_Warn, Req_01j_demiheure_Osny_Crit,       
   
   Req_00j_demiheure_Auzeville_Inco, Req_00j_demiheure_Auzeville_Warn, Req_00j_demiheure_Auzeville_Crit,
   Req_01j_demiheure_Auzeville_Inco, Req_01j_demiheure_Auzeville_Warn, Req_01j_demiheure_Auzeville_Crit,
   
   Req_par_demiheure_Osny_Inco, Req_par_demiheure_Osny_Warn, Req_par_demiheure_Osny_Crit,
   Req_par_demiheure_Auzeville_Inco, Req_par_demiheure_Auzeville_Warn, Req_par_demiheure_Auzeville_Crit
   )

###############################################################


## ETAPE 8 : agregation par serveur, service et datacenter
## puis operations successives de transformation ---- 
Req_00j_aliasdc <- fct_agreg_6var(Req_00j_agreg,heure, min, sec,
                                  host_name,service,datacenter,"nb_notif_00j",1)
Req_01j_aliasdc <- fct_agreg_6var(Req_01j_agreg,heure, min, sec,
                                  host_name,service,datacenter,"nb_notif_01j",1)

# ope 1
Req_00j_aliasdc_final <- transfo_table_aliasdc(Req_00j_aliasdc)
Req_01j_aliasdc_final <- transfo_table_aliasdc(Req_01j_aliasdc)

Req_00j_aliasdc_final <- Req_00j_aliasdc_final %>% select(-nb_notif_00j)
Req_01j_aliasdc_final <- Req_01j_aliasdc_final %>% select(-nb_notif_01j)

# ope 2A
Req_00j_duree  <- transfo_duree_A(Req_00j_agreg)
Req_01j_duree  <- transfo_duree_A(Req_01j_agreg)

# ope 2B
Req_00j_duree2 <- transfo_duree_B(Req_00j_aliasdc_final,Req_00j_duree)
Req_01j_duree2 <- transfo_duree_B(Req_01j_aliasdc_final,Req_01j_duree)

# ope 2C
Req_00j_duree_OSNY <- transfo_duree_C(Req_00j_duree2)[[1]]
Req_01j_duree_OSNY <- transfo_duree_C(Req_01j_duree2)[[1]]

Req_00j_duree_AUZ <- transfo_duree_C(Req_00j_duree2)[[2]]
Req_01j_duree_AUZ <- transfo_duree_C(Req_01j_duree2)[[2]]
###############################################################


## ETAPE 9 : filtrage et selection de lignes des notifications
## envoyees vers la balf de la supervision ----
vue00j <- Req_00j_duree_OSNY %>% filter((nb_notif!=2 & dureemin>=900) | (nb_notif==1))
vue01j <- Req_01j_duree_OSNY %>% filter((nb_notif!=2 & dureemin>=900) | (nb_notif==1))


jointure <- Req_00j_duree_OSNY %>% full_join(Req_01j_duree_OSNY, by=c("host_name","service")) %>% 
    select(-calendrier.x,-nb_notif.x,-nb_duree.x,-dureemin.x,-dureemax.x,-dureesec.x,
           -calendrier.y,-nb_notif.y,-nb_duree.y,-dureemin.y,-dureemax.y,-dureesec.y) %>% 
    rename(OSNY_00j=OSNY.x, duree_OSNY_00j=duree_OSNY.x,
           OSNY_01j=OSNY.y, duree_OSNY_01j=duree_OSNY.y) %>% 
    relocate(OSNY_01j,.after=OSNY_00j)

Extr_presJ_absJ1 <- Req_00j_duree_OSNY %>% 
    anti_join(Req_01j_duree_OSNY,by=c("host_name","service")) %>% 
    select(host_name,service) %>% 
    cbind(qualif=c("presence en J et absence en J-1"))

Extr_absJ_presJ1 <- Req_01j_duree_OSNY %>% 
    anti_join(Req_00j_duree_OSNY,by=c("host_name","service")) %>% 
    select(host_name,service) %>% 
    cbind(qualif=c("presence en J-1 et absence en J"))

Diff_Hote <- rbind(Extr_absJ_presJ1,Extr_presJ_absJ1)

rm(Extr_absJ_presJ1,Extr_presJ_absJ1)

jointure <- jointure %>% full_join(Diff_Hote,by=c("host_name","service"))

jointure <- jointure %>%
    mutate(OSNY_00j=ifelse(is.na(OSNY_00j),"",OSNY_00j)) %>%
    mutate(OSNY_01j=ifelse(is.na(OSNY_01j),"",OSNY_01j)) %>%
    mutate(duree_OSNY_00j=ifelse(is.na(duree_OSNY_00j),"",duree_OSNY_00j)) %>%
    mutate(duree_OSNY_01j=ifelse(is.na(duree_OSNY_01j),"",duree_OSNY_01j)) %>% 
    mutate(qualif=ifelse(is.na(qualif),"",qualif))




# Creation dun repertoire avec la date du jour
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon_New/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
    cat("The folder already exists")
} else {
    dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}


#  Selection des fichiers a mettre en PJ du mel

# Creation dun fichier Calc (utilisation du package ReadODS) et
# export des resultats issus de 3 tables sur un seul fichier Calc
#library(readODS)
write.csv2(Req_00j_duree_OSNY,paste0(folder,"/ResumAlert_24dernheures_Osny.csv"))
write.csv2(Req_00j_duree_AUZ ,paste0(folder,"/ResumAlert_24dernheures_Auzeville.csv"))
# write_ods(jointure          ,paste0(folder,"/ResumAlert_24dernheures.ods"),sheet="Visu_J0J1",append=TRUE)

write.csv2(Req_00j_agreg,paste0(folder,"/ListeAlert_24dernheures.csv"))

######################################################################
######################################################################
######################################################################
######################################################################
## Sortie des resultats en HTML via R markdown ----

# library(rmarkdown)
# library(knitr)
# 
# dossierprog <- "E:/Analyse_GEPEX/ProgR_SyntheseAlertCentreon/code/"
# sortie <- folder
# 
# render(input=paste0(dossierprog,"SynthAlertCentreon_RMarkdown.Rmd"), 
#        output_file=paste("Synthese_Alerts_Sup_",format(Sys.time(), '%Y%m%d'),".html",sep=""), 
#        output_dir = sortie, encoding="UTF-8")


######################################################################
######################################################################
######################################################################
######################################################################


## APPEL DU PROGRAMME DEFINISSANT LA PAGE HTML PUIS ENVOYANT LE MEL

setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon_New/code/")
source(file = "SynthAlertCentreon_CreaHTML_et_EnvMel.R")








