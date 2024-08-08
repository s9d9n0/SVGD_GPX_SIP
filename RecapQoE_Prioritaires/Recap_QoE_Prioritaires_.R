

## Chargement des packages utilises dans le programme ----
library(lubridate)
library(RMariaDB)
library(dplyr)
library(tidyr)
library(stringr)


# extraction donnees du mois de :
AnneeMois<-202210    #202211 correspond au 11eme mois de 2022 donc novembre 2022


# Calcul des variable moissuiv , moisprec (respectivement le mois suivant et precedent suivant le mois indiquee dans la variable AnneeMois
if(AnneeMois-round(AnneeMois,-2)==12){moissuiv<-AnneeMois+100-11}else{moissuiv<-AnneeMois+1}
# if(AnneeMois-round(AnneeMois,-2)==01){moisprec<-AnneeMois-100+11}else{moisprec<-AnneeMois-1}

# cration des variables annee et mois
annee<-str_sub(AnneeMois, 1, 4) 
mois<-str_sub(AnneeMois, 5, 6) 
str_sub(AnneeMois, 5, 4) <- "-"
str_sub(moissuiv, 5, 4) <- "-"

## Operation de decalage de l'heure
## mettre 1 lorsqu'on est en heure d'hiver et
## mettre 2 lorsqu'on est en heure d'ete
decalage_heure <- 2

# le caractere commencant a la 5e position et se terminant a la 4e est "", mais est en 5e position. on insere donc "-" en 5e position
Sys.setenv(TZ="UTC") # les time stamp doivent etre geres en utc, avec les resultats en utc+1 ( correction en utc + 2 pour l heure d ete avant l ecriture en csv)


datedebutmois <- as.POSIXct(paste0(annee,"-",mois,"-01 00:00:00")) - dhours(decalage_heure)




##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
##
baseMdb <- dbConnect(MariaDB(), user="lecture",
                     password="reporting",
                     dbname="centreon_storage", 
                     host="xx.xx.xx.xx",
                     port="3306")
##################################################################

Req <- dbGetQuery(baseMdb,
                  paste("SELECT h.name as hote, s.description as service,
                                ",annee," as annee,
                                ",mois," as mois,
                                FROM_UNIXTIME(sse.start_time) as debut, FROM_UNIXTIME(sse.end_time) as fin, sse.state,
                                sse.end_time - sse.start_time as duree_indispo
                         FROM servicestateevents sse,
                              services s,
                              hosts h
                         WHERE sse.service_id=s.service_id
                               AND (
                               (s.description like 'QoE-APIM-RD-SIRENE' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-APIM-Connected-Sirene' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-APIM-Requetes-Sirene' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-BRPP-Backup-France_Connect_1min' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-BRPP-France_Connect_1min' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-COLTRANE-Enquete' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-COLTRANE-Compte' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-ELIRE-Interne' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-ELIRE-Liste_Electeurs_Prod' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-LEI-France' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-LEI_Poste_de_Gestion' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-OWA-mail_externe' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-OWA-mail_interne' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-OWA-Mail_Externe' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-OWA-Mail_Pro' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-RP-Questionnaire-Lien' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-RP-Questionnaire-Lien_DA' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-RP-RpetMoi' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-RP-RpetMoi_DA' and h.name like '%pdselenium_st%') 
                                OR (s.description like 'QoE-RP-Omer' and h.name like '%pdselenium_st%'))
                               AND s.host_id = h.host_id
                               AND sse.end_time > UNIX_TIMESTAMP('",datedebutmois,"')
                               AND sse.state IN ('2', '3')
                         GROUP BY 1, 2, 5
                         ORDER BY 3, 4, 1, 2, 5
                        "))

##################################################################


##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################


Req_bis <- Req %>% arrange(debut) %>% 
  mutate(POSIXct_debut=ymd_hms(debut)) %>% mutate(POSIXct_debut=POSIXct_debut+dhours(decalage_heure)) %>% 
  mutate(POSIXct_fin=ymd_hms(fin)) %>% mutate(POSIXct_fin=POSIXct_fin+dhours(decalage_heure)) %>% 
  relocate(c("POSIXct_debut","POSIXct_fin"),.before="state") %>% 
  select(-debut,-fin)






