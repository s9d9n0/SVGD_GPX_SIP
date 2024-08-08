
## Chargement des packages utilises dans le programme ----
library(lubridate)
library(RMariaDB)
library(dplyr)
library(tidyr)
library(stringr)

###############################################################################
###############################################################################

dateRef <- "20240618"

chemin <- "C:/Users/SIAR_YCG8L6/Docs/ProgrammesR/_AnalyseRundeck_DevOps/"

fichier <- paste0(chemin,"Listing_Rdk_DevOps_",dateRef,".csv")

DataRef <- read.csv2(fichier,sep=",") %>% 
    select(id,date_lancement,result,auteur,job_type,param,ident_job,unite) %>% 
    mutate(date_lancement=str_sub(date_lancement,1,19),
           auteur=str_sub(auteur,4,max(str_length(auteur))),
           result=str_sub(result,3,max(str_length(result)))) %>% 
    mutate(result=ifelse(result=="failed","KO",result))


###############################################################################
###############################################################################


# traitement de la date de lancement
DataRef_2 <- DataRef %>% 
  mutate(date_courte=str_locate(date_lancement,":")[,"start"]) %>% 
  relocate(date_courte,.after="date_lancement") %>% 
  mutate(date_lancement_2=ifelse(date_courte==13,
                                 paste0(str_sub(date_lancement,1,11),"0",str_sub(date_lancement,12,19)),
                                 date_lancement)) %>% 
  relocate(date_lancement_2,.after="date_courte") %>% 
  mutate(an_mois=paste0(str_sub(date_lancement_2,7,10),"-",str_sub(date_lancement_2,1,2)),
         jour=str_sub(date_lancement_2,4,5),
         heure=str_sub(date_lancement_2,12,13),
         minute=str_sub(date_lancement_2,15,16),
         ampm=str_sub(date_lancement_2,18,19)) %>% 
  relocate(an_mois,jour,heure,minute,ampm,.after="date_lancement_2") %>% 
  mutate(heure=case_when(
          ampm=="PM" & heure=="01" ~ "13", ampm=="PM" & heure=="02" ~ "14",
          ampm=="PM" & heure=="03" ~ "15", ampm=="PM" & heure=="04" ~ "16",
          ampm=="PM" & heure=="05" ~ "17", ampm=="PM" & heure=="06" ~ "18",
          ampm=="PM" & heure=="05" ~ "17", ampm=="PM" & heure=="06" ~ "18",
          ampm=="PM" & heure=="07" ~ "19", ampm=="PM" & heure=="08" ~ "20",
          ampm=="PM" & heure=="09" ~ "21", ampm=="PM" & heure=="10" ~ "22",
          ampm=="PM" & heure=="11" ~ "23", TRUE ~ heure)) %>% 
  mutate(hm=paste0(heure,":",minute)) %>% relocate(hm,.after="ampm") %>% 
  select(-date_lancement,-date_courte,-date_lancement_2,-heure,-minute,-ampm)


###############################################################################
###############################################################################


# traitement auteur
DataRef_3 <- DataRef_2 %>% 
  mutate(auteur2=ifelse(str_sub(auteur,1,2) %in% c("R_","r_"),
                       str_sub(auteur,3,max(str_length(auteur))),
                       auteur)) %>% 
  mutate(auteur2=str_to_lower(auteur2)) %>% 
  relocate(auteur2,.after="auteur") %>%
  select(-auteur) %>% rename(idep=auteur2)

# export liste des auteurs pour recuperation des noms, prenoms et sites
liste_auteur <- DataRef_3 %>% group_by(idep) %>% 
  summarise(eff=sum(unite)) %>% ungroup() %>% as.data.frame()

write.csv2(liste_auteur,file=paste0(chemin,"liste_auteur.csv"),row.names = FALSE)

# apres remise a jour de la Base_Idep.csv import pour fusion avec table DataRef...
fichierAuteur <- paste0(chemin,"Base_Idep.csv")
BaseAuteur <- read.csv2(fichierAuteur,sep=",")
DataRef_3 <- DataRef_3 %>% left_join(BaseAuteur,by=c("idep"))

DataRef_3 <- DataRef_3 %>% relocate(nom_idep,site_idep,.after="idep")


###############################################################################


# traitement job pour libelle court
DataRef_4 <- DataRef_3 %>% 
  mutate(job_type_agreg=case_when(
    str_sub(job_type,1,15)=="sudo#!/bin/bash" ~ "script bash",
    str_sub(job_type,1,16)=="sudo rainettectl"| str_sub(job_type,1,11)=="rainettectl" ~ "sudo rainettectl",
    str_sub(job_type,1,11)=="sudo puppet" | str_sub(job_type,1,11)=="puppet node" ~ "sudo puppet",
    str_sub(job_type,1,7)=="sudo ls" ~ "sudo ls",    
    str_sub(job_type,1,4)=="job:" ~ "job",    
    TRUE ~ job_type)) %>%
  relocate(job_type_agreg,.after="job_type")


# export de la table 
write.csv2(DataRef_4,file=paste0(chemin,"RESULTAT.csv"),row.names = FALSE)






















