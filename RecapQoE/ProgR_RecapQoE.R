
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
#date_jour <- as.POSIXct("2024-06-30 23:59:59")
#def_date_quelconque <- as.POSIXct("2022-06-30 07:34:51")

## Operation de decalage de l'heure
## mettre 1 lorsqu'on est en heure d'hiver et
## mettre 2 lorsqu'on est en heure d'ete
decalage_heure <- 2

# date retardee
# date_jour_hmoins <- date_jour - dhours(decalage_heure) - ddays(31)


annee_date_jour <- paste0(year(date_jour))
mois_date_jour <- ifelse(str_length(month(date_jour))==1,
                         paste0("0",month(date_jour)),
                         paste0(month(date_jour)))

datedebutmois <- as.POSIXct(paste0(annee_date_jour,"-",mois_date_jour,"-01 00:00:00")) - dhours(decalage_heure)

rm(annee_date_jour)


# nombre de secondes entre la date du jour au moment du lancement et le debut du mois
delta <- floor(int_length(interval(datedebutmois,date_jour)))

# delta <- floor(int_length(interval(datedebutmois,Sys.time())))


####


##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
##
# baseMdb <- dbConnect(MariaDB(), user="lecture",
#                      password="reporting",
#                      dbname="centreon_storage", 
#                      host="xx.xx.xx.xx",
#                      port="3306")

# Connexion a la nouvelle base 50
# maitre 10.203.0.11 ; esclave 10.203.0.12
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture",
                     password="Reporting",
                     dbname="centreon_storage", 
                     host="xx.xx.xx.xx",
                     port="3306")

##################################################################

# Essai requetes
# type = 1 correspond au HARD
#  status = 0 correspond au OK
#         = 1 correspond au Warning
#         = 2 corrrespond au Critical
#         = 3 correspond au Unknown

Req <- dbGetQuery(baseMdb,
                  paste("SELECT *,
                                FROM_UNIXTIME(ctime,'%Y-%m-%d %H:%i:%S') AS chaine_date
                         FROM logs
                         WHERE ctime > UNIX_TIMESTAMP('",datedebutmois,"') AND
                               (host_name LIKE ('%pdselenium%') OR host_name LIKE ('%PDSPVSELEN%')) AND
                               service_description LIKE ('%QoE%') AND
                               type=1 AND
                               status IN (0,1,2,3)
                         ORDER BY ctime
                        "))

##################################################################☺


##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################


####


##################################################
##                                              ##
## 1/ PARTIE DEFINITION DES FONCTIONS           ##
##                                              ##
################################################## ----


####################################################################################
####################################################################################

fct_1ers_traitements <- function(df){
    df_out <- df %>% mutate(POSIXct_date=ymd_hms(chaine_date)) %>% 
                mutate(POSIXct_date=POSIXct_date+dhours(decalage_heure)) %>% 
                select(-log_id,-ctime,-issue_id,-msg_type,
                       -notification_cmd,-notification_contact,
                       -retry,-type,-chaine_date,
                       -host_id,-service_id) %>%
                mutate(zone = case_when(
                        instance_name %in% c("Pollerdc200","Poller1dc200")  ~ "01_DMZ_dc2",
                        instance_name %in% c("Pollerdc100","Poller1dc100")  ~ "02_DMZ_dc1",
                        instance_name %in% c("Pollerdc225")                 ~ "03_Partenaire_dc2",
                        instance_name %in% c("Pollerdc125")                 ~ "04_Partenaire_dc1",
                        instance_name %in% c("Pollerdc250","Poller1dc250",
                                             "Poller2dc250","Poller3dc250") ~ "05_ZoneInterne_dc2",
                        instance_name %in% c("Pollerdc150","Poller1dc150",
                                             "Poller2dc150","Rsdc150")      ~ "06_ZoneInterne_dc1",
                        TRUE ~ "07_Autre")) %>%
                # mutate(zone = case_when(
                #             instance_name %in% c("Collecteur_Central","Central_Web") ~ "00_Central",
                #             instance_name %in% c("Collecteur_Admin") ~ "01_Admin",
                #             instance_name %in% c("Collecteur_DirectAccess") ~ "02_Direct-Access",
                #             instance_name %in% c("Collecteur_Prod","Collecteur_Prod1","Collecteur_Prod2",
                #                                  "PollerOsny50") ~ "03_Prod",
                #             instance_name %in% c("Collecteur_Hors_Prod","Collecteur_Hors_Prod1") ~ "04_Hors-Prod",
                #             instance_name %in% c("Collecteur_Dmz","Collecteur_Dmz1") ~ "05_DMZ",
                #             instance_name %in% c("Collecteur_Partenaire") ~ "06_Partenaire",
                #             instance_name %in% c("Collecteur_HorsCEI") ~ "07_Hors-CEI",
                #             TRUE ~ "08_Autre")
                #       ) %>% 
                relocate("zone") %>% select(-instance_name) %>%
                mutate(host_name=str_replace(host_name,".ad.insee.intra","")) %>% 
                # retrait du code ci-dessous le 06 sept. 2023 (inutile avec nouvelle ligne ajoutee au-dessus)
                # mutate(host_name=ifelse(str_detect(host_name,"selenium"),
                #                         str_sub(host_name,
                #                                 str_locate(host_name,"pdselenium")[1,1],
                #                                 str_length(host_name)),
                #                         host_name)) %>% 
                relocate("service_description",.after="host_name") %>% 
                distinct(zone,host_name,service_description,output,status,POSIXct_date) %>% 
                mutate(numligne=row_number()) %>% 
                arrange(service_description,POSIXct_date)
    return(df_out)
}

Req_out <- fct_1ers_traitements(Req)
# Req_B_out <- fct_1ers_traitements(Req_B)

fct_2nds_traitements <- function(df){
    df_out <- df %>% mutate(numligne=row_number()) %>% 
        # retrait des obs ou un statut INC est entoure avant et apres par un CRIT ou bien par un WARN
        filter(numligne==1 |
               (!(lag(service_description)==service_description & lead(service_description)==service_description &
                  lag(status)==2 & status==3 & lead(status)==2) &
                !(lag(service_description)==service_description & lead(service_description)==service_description &
                    lag(status)==1 & status==3 & lead(status)==1))) %>%
      
        # puis retrait du 2nd CRIT ou WARN successif survenant du coup a la suite du 1er retrait ci-dessus  
        filter(numligne==1 |
               (!(lag(service_description)==service_description & lag(status)==2 & status==2) &
                !(lag(service_description)==service_description & lag(status)==1 & status==1))) %>%
      
        # retrait des obs ou un statut WARN est entoure avant et apres par un CRIT
        filter(numligne==1 |
               (!(lag(service_description)==service_description & lead(service_description)==service_description &
                  lag(status)==2 & status==1 & lead(status)==2))) %>%
      
        # si on a CRIT suivi dun WARN ou encore un WARN suivi dun CRIT, on garde le CRIT a chaque fois  
        filter(numligne==1 |
               (!(lag(service_description)==service_description & lag(status)==2 & status==1) &
                !(lag(service_description)==service_description & lead(status)==2 & status==1))) %>%

        # si on a 2 CRIT ou 2 WARN successifs, on garde le 1er CRIT ou 1er WARN a chaque fois  
        filter(numligne==1 |
               (!(lag(service_description)==service_description & lag(status)==2 & status==2) &
                !(lag(service_description)==service_description & lead(status)==1 & status==1))) %>%
      
        mutate(duree=ifelse(lag(service_description)==service_description & lag(status)==3 & status==0,
                            int_length(interval(lag(POSIXct_date),POSIXct_date)),
                            0)) %>%
        mutate(duree=ifelse(lag(service_description)==service_description & lag(status)==2 & status==0,
                            int_length(interval(lag(POSIXct_date),POSIXct_date)),
                            duree)) %>%
        mutate(duree=ifelse(lag(service_description)==service_description & lag(status)==1 & status==0,
                            int_length(interval(lag(POSIXct_date),POSIXct_date)),
                            duree)) %>% 
    
        mutate(output=ifelse(status==3,"INC:",output)) %>%
        mutate(output=str_replace(output,"CRITICAL","CRIT")) %>% 
        mutate(output=str_replace(output,"WARNING","WARN")) %>% 
        
        
        # ajout pour les QoE sur les nouvelles machines selenium de linfo status dans loutput
        mutate(output=ifelse(str_sub(host_name,1,10)=="PDSPVSELEN" & str_sub(output,1,3)!="OK:" & status==0,
                             paste0("OK: ",output),output)) %>% 
        mutate(output=ifelse(str_sub(host_name,1,10)=="PDSPVSELEN" & str_sub(output,1,8)!="WARNING:" & status==1,
                             paste0("WARN: ",output),output)) %>%
        mutate(output=ifelse(str_sub(host_name,1,10)=="PDSPVSELEN" & str_sub(output,1,9)!="CRITICAL:" & status==2,
                             paste0("CRIT: ",output),output)) %>%
        mutate(output=ifelse(str_sub(host_name,1,10)=="PDSPVSELEN" & str_sub(output,1,4)!="INC:" & status==3,
                             paste0("INC: ",output),output)) %>% 
        
        
        
        # retrait des infos de la variable output apres le mot-clef steps
        mutate(output=ifelse(str_sub(service_description,1,3)=="QoE" & (status==0 | status==1 | status==2),
                            str_sub(output,1,str_locate(output,"steps")[row_number(),1]+5),
                            output)) %>% 
        mutate(output=ifelse(str_sub(service_description,1,3)!="QoE" & status==0,
                            str_sub(output,1,3),
                            ifelse(str_sub(service_description,1,3)!="QoE" & (status==1 | status==2),
                                   str_sub(output,1,5),
                                   output))) %>%
      
        # mise en forme pour preparation suppression ulterieure de quelques lignes
        mutate(output_final=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                                   paste0(lag(output)," -> ",output),
                                   output)) %>% 
    
        mutate(moment_crit=ifelse(lag(service_description)==service_description &
                                  lag(status) %in% c(1,2,3) & status==0,
                                  paste0(wday(lag(POSIXct_date))," ",day(lag(POSIXct_date))," ",
                                         hour(lag(POSIXct_date)),":",minute(lag(POSIXct_date))),
                                  "")) %>% 
        mutate(moment_ok=ifelse(status==0,
                                paste0(wday(POSIXct_date)," ",day(POSIXct_date)," ",
                                       hour(POSIXct_date),":",minute(POSIXct_date)),
                                "")) %>% 
    
        mutate(lib_jour_crit=case_when(str_sub(moment_crit,1,1)==1 ~ "dim.", str_sub(moment_crit,1,1)==2 ~ "lun.",
                                       str_sub(moment_crit,1,1)==3 ~ "mar.", str_sub(moment_crit,1,1)==4 ~ "mer.",
                                       str_sub(moment_crit,1,1)==5 ~ "jeu.", str_sub(moment_crit,1,1)==6 ~ "ven.",
                                       str_sub(moment_crit,1,1)==7 ~ "sam.", TRUE ~ "")) %>% 
        mutate(moment_crit=paste0(lib_jour_crit,"_",str_sub(moment_crit,3,str_length(moment_crit)))) %>% 
        select(-lib_jour_crit) %>% 
    
        mutate(lib_jour_ok=case_when(str_sub(moment_ok,1,1)==1 ~ "dim.", str_sub(moment_ok,1,1)==2 ~ "lun.",
                                     str_sub(moment_ok,1,1)==3 ~ "mar.", str_sub(moment_ok,1,1)==4 ~ "mer.",
                                     str_sub(moment_ok,1,1)==5 ~ "jeu.", str_sub(moment_ok,1,1)==6 ~ "ven.",
                                     str_sub(moment_ok,1,1)==7 ~ "sam.", TRUE ~ "")) %>% 
        mutate(moment_ok=paste0(lib_jour_ok,"_",str_sub(moment_ok,3,str_length(moment_ok)))) %>% 
        select(-lib_jour_ok) %>% 
    
        mutate(jour_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                                day(lag(POSIXct_date)),day(POSIXct_date)),
              mois_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                               month(lag(POSIXct_date)),month(POSIXct_date)),
              annee_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                                year(lag(POSIXct_date)),year(POSIXct_date)),
              sem_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                              week(lag(POSIXct_date)),week(POSIXct_date)),              
              heure_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                                hour(lag(POSIXct_date)),hour(POSIXct_date)),
              min_crit=ifelse(lag(service_description)==service_description & lag(status) %in% c(1,2,3) & status==0,
                              minute(lag(POSIXct_date)),minute(POSIXct_date)),
              sec_crit=ifelse(lag(service_description)==service_description & lag(status)==2 & status==0,
                              second(lag(POSIXct_date)),second(POSIXct_date)),
              jour_ok=ifelse(status==0,day(POSIXct_date),0),
              mois_ok=ifelse(status==0,month(POSIXct_date),0),
              annee_ok=ifelse(status==0,year(POSIXct_date),0),
              sem_ok=ifelse(status==0,week(POSIXct_date),0),              
              heure_ok=ifelse(status==0,hour(POSIXct_date),0),
              min_ok=ifelse(status==0,minute(POSIXct_date),0),
              sec_ok=ifelse(status==0,second(POSIXct_date),0)) %>%
    
        # filtre sur certaines lignes
        filter(numligne==1 |
               ((str_sub(output_final,1,4)=="CRIT" | str_sub(output_final,1,4)=="WARN" |
                 str_sub(output_final,1,2)=="OK" | str_sub(output_final,1,3)=="INC") &
                 str_detect(output_final,"->")) |
               lead(service_description)!=service_description) %>%
      
        # filtre sur les lignes ne rendant compte que d'un OK finalement
        filter(!(str_sub(output_final,1,2)=="OK" & str_length(output_final)<20)) %>% 
    
        mutate(toujours_en_cours=ifelse(lead(service_description)!=service_description & status!=0,
                                        "oui","non")) %>% 
    
        # mutate(lignesuppr=ifelse(toujours_en_cours=="non" & 
        #                          jour_ok==0 & mois_ok==0 & annee_ok==0 & heure_ok==0 & min_ok==0 & sec_ok==0,"oui","non")) %>% 
        # filter(lignesuppr=="non") %>%
    
        mutate(duree=ifelse(toujours_en_cours!="oui",round((duree+sec_crit-sec_ok)/60,0),0)) %>% 
        select(-output,-status,-POSIXct_date,-sec_crit,-sec_ok) %>% 
        filter(numligne!=1)
  return(df_out)
}

Req_out2 <- fct_2nds_traitements(Req_out)
# Req_B_out2 <- fct_2nds_traitements(Req_B_out)


fct_recode_date <- function(df,newvar,vardate){
    df %>% mutate("{{newvar}}":=paste0(ifelse(str_length({{vardate}})==1,  paste0("0",{{vardate}}), {{vardate}})))
}

Req_out2 <- fct_recode_date(Req_out2,jour_crit,jour_crit)
Req_out2 <- fct_recode_date(Req_out2,mois_crit,mois_crit)
Req_out2 <- fct_recode_date(Req_out2,heure_crit,heure_crit)
Req_out2 <- fct_recode_date(Req_out2,min_crit,min_crit)
Req_out2 <- fct_recode_date(Req_out2,jour_ok,jour_ok)
Req_out2 <- fct_recode_date(Req_out2,mois_ok,mois_ok)
Req_out2 <- fct_recode_date(Req_out2,heure_ok,heure_ok)
Req_out2 <- fct_recode_date(Req_out2,min_ok,min_ok)

# Req_B_out2 <- fct_recode_date(Req_B_out2,jour_crit,jour_crit)
# Req_B_out2 <- fct_recode_date(Req_B_out2,mois_crit,mois_crit)
# Req_B_out2 <- fct_recode_date(Req_B_out2,heure_crit,heure_crit)
# Req_B_out2 <- fct_recode_date(Req_B_out2,min_crit,min_crit)
# Req_B_out2 <- fct_recode_date(Req_B_out2,jour_ok,jour_ok)
# Req_B_out2 <- fct_recode_date(Req_B_out2,mois_ok,mois_ok)
# Req_B_out2 <- fct_recode_date(Req_B_out2,heure_ok,heure_ok)
# Req_B_out2 <- fct_recode_date(Req_B_out2,min_ok,min_ok)


fct_recode_moisdate <- function(df,newvar,vardate){
    df %>% mutate("{{newvar}}":=case_when({{vardate}}=="01" ~ "janv",  {{vardate}}=="02" ~ "fevr",
                                          {{vardate}}=="03" ~ "mars",  {{vardate}}=="04" ~ "avril",
                                          {{vardate}}=="05" ~ "mai",   {{vardate}}=="06" ~ "juin",
                                          {{vardate}}=="07" ~ "juil.", {{vardate}}=="08" ~ "aout",
                                          {{vardate}}=="09" ~ "sept",  {{vardate}}=="10" ~ "oct.",
                                          {{vardate}}=="11" ~ "nov.",  {{vardate}}=="12" ~ "dec.",
                                          TRUE ~ ""
                                          )
                  ) %>% relocate({{newvar}},.after={{vardate}})
      
}

Req_out2 <- fct_recode_moisdate(Req_out2,mois_crit_lib,mois_crit)
Req_out2 <- fct_recode_moisdate(Req_out2,mois_ok_lib,mois_ok)

# Req_B_out2 <- fct_recode_moisdate(Req_B_out2,mois_crit_lib,mois_crit)
# Req_B_out2 <- fct_recode_moisdate(Req_B_out2,mois_ok_lib,mois_ok)


fct_3eme_traitements <- function(df){
    df_out <- df %>%
        mutate(periode=ifelse(toujours_en_cours=="oui",
                              paste0("le ",jour_crit," ",mois_crit_lib," ",annee_crit,", &agrave; ",heure_crit,"h",min_crit,", toujours en cours"),
                              "")) %>%
        mutate(periode=ifelse(jour_crit==jour_ok & mois_crit==mois_ok & annee_crit==annee_ok &
                              heure_crit==heure_ok & min_crit==min_ok,
                              paste0("le ",jour_ok," ",mois_ok_lib," ",annee_ok,", &agrave; ",heure_ok,"h",min_ok),
                              periode)) %>% 
        mutate(periode=ifelse(jour_crit==jour_ok & mois_crit==mois_ok & annee_crit==annee_ok &
                              (heure_crit!=heure_ok | min_crit!=min_ok),
                              paste0("le ",jour_ok," ",mois_ok_lib,", entre ",heure_crit,"h",min_crit," et ",heure_ok,"h",min_ok),
                              periode)) %>% 
        mutate(periode=ifelse(toujours_en_cours=="non" & (jour_crit!=jour_ok | mois_crit!=mois_ok),
                              paste0("du ",jour_crit," ",mois_crit_lib," ",heure_crit,"h",min_crit," jusqu'au ",
                                     jour_ok,  " ",mois_ok_lib,  " ",heure_ok,  "h",min_ok),
                              periode)) %>% 
        mutate(periode=ifelse(toujours_en_cours=="non" & mois_crit_lib=="",
                              paste0("le ",jour_ok," ",mois_ok_lib," ",annee_ok,", &agrave; ",heure_ok,"h",min_ok),
                              periode)) %>% 
    
        mutate(jour=duree%/%1440,     jour_reste=duree%%1440,
               heure=jour_reste%/%60, minute=jour_reste%%60,
               duree_tot_min=duree) %>% 
        select(-duree,-jour_reste) %>% 
        mutate(duree_tot=paste0(jour,"j ",heure,"h ",minute,"m")) %>% 
        select(-jour,-heure,-minute) %>% 
        arrange(annee_crit,mois_crit,jour_crit,heure_crit,min_crit,
                annee_ok,  mois_ok,  jour_ok,  heure_ok,  min_ok) %>% 
        mutate(duree_tot=ifelse(duree_tot_min==0,"",duree_tot)) %>% 
        mutate(numligne=row_number())
  return(df_out)
}

Req_out3 <- fct_3eme_traitements(Req_out2)
# Req_B_out3 <- fct_3eme_traitements(Req_B_out2)

# reunion par concatenation des 2 tables Req_out_3 et Req_B_out3
# Req_out3_Global <- rbind(Req_out3,Req_B_out3) %>%
Req_out3_Global <- rbind(Req_out3) %>%
    arrange(annee_crit,mois_crit,jour_crit,heure_crit,min_crit,
            annee_ok,  mois_ok,  jour_ok,  heure_ok,  min_ok) %>%
    mutate(numligne=row_number())


# liste des Temps darret recurrents mis en place dans Centreon
maint01 <- data.frame(service_description="QoE-APIM-Requetes-DL",                  jour_deb=0, jour_fin=0, ta_deb=2.30,  ta_fin=3.30)
maint02 <- data.frame(service_description="QoE-COLTRANE-Compte",                   jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=5)
maint03 <- data.frame(service_description="QoE-COLTRANE-Enquete",                  jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=5)
maint04 <- data.frame(service_description="QoE-COLTRANE-Enquete2",                 jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=5)
maint05 <- data.frame(service_description="QoE-EPURE-Msa",                         jour_deb=0, jour_fin=0, ta_deb=19,    ta_fin=20)
maint06 <- data.frame(service_description="QoE-EPURE-Urssaf",                      jour_deb=0, jour_fin=0, ta_deb=19,    ta_fin=20)
maint07 <- data.frame(service_description="QoE-HARMONICA-Consultation",            jour_deb=1, jour_fin=6, ta_deb=20,    ta_fin=5)
maint08 <- data.frame(service_description="QoE-HARMONICA-Expertise",               jour_deb=1, jour_fin=6, ta_deb=20,    ta_fin=5)
maint09 <- data.frame(service_description="QoE-PILOT-Agents",                      jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=7.30)
maint10 <- data.frame(service_description="QoE-REPONSE_Authentification",          jour_deb=0, jour_fin=0, ta_deb=20.30, ta_fin=21.30)
maint11 <- data.frame(service_description="QoE-RORCAL-Google",                     jour_deb=0, jour_fin=0, ta_deb=5,     ta_fin=6)
maint12 <- data.frame(service_description="QoE-SIRUS-Externe-Recherche_par_Denom", jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=7)
maint13 <- data.frame(service_description="QoE-SIRUS-Externe-Recherche_par_Ident", jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=7)
maint14 <- data.frame(service_description="QoE-WS-Atis_BRPP",                      jour_deb=0, jour_fin=0, ta_deb=1.30,  ta_fin=3.30)
maint15 <- data.frame(service_description="QoE-BRPP-Etat_BDD",                     jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=3.30)
maint16 <- data.frame(service_description="QoE-ESANE-Diffusion",                   jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)
maint17 <- data.frame(service_description="QoE-ESANE-Diffusion_mineure",           jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)
maint18 <- data.frame(service_description="QoE-ESANE-Diffusion_mineure2",          jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)
maint19 <- data.frame(service_description="QoE-ESANE-Metropole",                   jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)
maint20 <- data.frame(service_description="QoE-ESANE-Metropole_mineure",           jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)
maint21 <- data.frame(service_description="QoE-ESANE-Metropole_mineure2",          jour_deb=0, jour_fin=0, ta_deb=0,     ta_fin=23.30)

maintenance <- rbind(maint01,maint02,maint03,maint04,maint05,
                     maint06,maint07,maint08,maint09,maint10,
                     maint11,maint12,maint13,maint14,maint15,
                     maint16,maint17,maint18,maint19,maint20,maint21)

rm(maint01,maint02,maint03,maint04,maint05,
   maint06,maint07,maint08,maint09,maint10,
   maint11,maint12,maint13,maint14,maint15,
   maint16,maint17,maint18,maint19,maint20,maint21)

Req_out4 <- Req_out3_Global %>% left_join(maintenance, by=c("service_description")) %>% 
        mutate(hmincritnum=as.integer(heure_crit)+as.integer(min_crit)/100,
               hminoknum=as.integer(heure_ok)+as.integer(min_ok)/100)

# detection des periodes de maintenances et temps darrets recurrents
Req_out4 <- Req_out4 %>% 
        mutate(TA_rec_00=ifelse(service_description %in% c("QoE-APIM-Requetes-DL") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_01=ifelse(service_description %in% c("QoE-COLTRANE-Compte","QoE-COLTRANE-Enquete","QoE-COLTRANE-Enquete2") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_02=ifelse(service_description %in% c("QoE-EPURE-Msa","QoE-EPURE-Urssaf") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_03=ifelse(service_description %in% c("QoE-HARMONICA-Consultation","QoE-HARMONICA-Expertise") &
                                as.integer(jour_crit)>=jour_deb & as.integer(jour_ok)<=jour_fin,"oui",""),
               TA_rec_04=ifelse(service_description %in% c("QoE-HARMONICA-Consultation","QoE-HARMONICA-Expertise") &
                                (
                                (as.integer(jour_crit)==as.integer(jour_ok)   &
                                 as.integer(heure_crit)>=ta_deb & as.integer(heure_ok)<=24) |
                                (as.integer(jour_crit)==as.integer(jour_ok)   &
                                 as.integer(heure_crit)>=0 & as.integer(heure_ok)<=ta_fin) |                                
                                (as.integer(jour_crit)==as.integer(jour_ok)-1 &
                                 as.integer(heure_crit)>=ta_deb & as.integer(heure_ok)<=ta_fin)
                                )
                                ,"oui",""),
               TA_rec_05=ifelse(service_description %in% c("QoE-PILOT-Agents") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_06=ifelse(service_description %in% c("QoE-REPONSE_Authentification") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_07=ifelse(service_description %in% c("QoE-RORCAL-Google") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_08=ifelse(service_description %in% c("QoE-SIRUS-Externe-Recherche_par_Denom") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_09=ifelse(service_description %in% c("QoE-SIRUS-Externe-Recherche_par_Ident") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_10=ifelse(service_description %in% c("QoE-WS-Atis_BRPP") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_11=ifelse(service_description %in% c("QoE-BRPP-Etat_BDD") &
                                hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_12=ifelse(str_sub(service_description,1,16) %in% c("QoE-ESANE-Diffus","QoE-ESANE-Metrop","QoE-ESANE-Reunio","QoE-ESANE-AntGuy") &
                                  str_sub(moment_crit,1,3) %in% c("sam","dim") &
                                  hmincritnum>=ta_deb & hminoknum<=ta_fin,"oui",""),
               TA_rec_13=ifelse(str_sub(service_description,1,16) %in% c("QoE-ESANE-Diffus","QoE-ESANE-Metrop","QoE-ESANE-Reunio","QoE-ESANE-AntGuy") &
                                  str_sub(moment_crit,1,3) %in% c("ven") &
                                  hmincritnum>=20 & hminoknum<=23.59,"oui",""),
               TA_rec_14=ifelse(str_sub(service_description,1,16) %in% c("QoE-ESANE-Diffus","QoE-ESANE-Metrop","QoE-ESANE-Reunio","QoE-ESANE-AntGuy") &
                                  str_sub(moment_crit,1,3) %in% c("lun") &
                                  hmincritnum>=0 & hminoknum<=7,"oui","")  
              ) %>% 
        unite(TA_rec_00,TA_rec_01,TA_rec_02,TA_rec_03,TA_rec_04,
              TA_rec_05,TA_rec_06,TA_rec_07,TA_rec_08,TA_rec_09,
              TA_rec_10,TA_rec_11,TA_rec_12,TA_rec_13,TA_rec_14,
              col="TA_rec",remove=TRUE) %>%
        mutate(TA_rec=ifelse(str_detect(TA_rec,"oui"),
                             str_sub(TA_rec,str_locate(TA_rec,"oui")[row_number(),1],str_locate(TA_rec,"oui")[row_number(),1]+2),
                             ""))

# detection des depassements dans un meme jour au niveau
# des periodes de maintenances et temps darrets recurrents mis en place
# et correction du temps dindisponibilite
Req_out4 <- Req_out4 %>% 
        mutate(depass_avant=ifelse(service_description %in% c("QoE-APIM-Requetes-DL",
                                                              "QoE-COLTRANE-Compte","QoE-COLTRANE-Enquete","QoE-COLTRANE-Enquete2",
                                                              "QoE-EPURE-Msa","QoE-EPURE-Urssaf",
                                                              "QoE-PILOT-Agents","QoE-REPONSE_Authentification",
                                                              "QoE-RORCAL-Google",
                                                              "QoE-SIRUS-Externe-Recherche_par_Denom",
                                                              "QoE-SIRUS-Externe-Recherche_par_Ident",
                                                              "QoE-WS-Atis_BRPP",
                                                              "QoE-BRPP-Etat_BDD",
                                                              "QoE-ESANE-Diffusion","QoE-ESANE-Diffusion_mineure","QoE-ESANE-Diffusion_mineure2",
                                                              "QoE-ESANE-Metropole","QoE-ESANE-Metropole_mineure","QoE-ESANE-Metropole_mineure2",
                                                              "QoE-ESANE-Reunion","QoE-ESANE-Reunion_mineure","QoE-ESANE-Reunion_mineure2",
                                                              "QoE-ESANE-AntGuyane","QoE-ESANE-AntGuyane_mineure","QoE-ESANE-AntGuyane_mineure2") &
                                   jour_crit==jour_ok &
                                      # commence trop tot...
                                   (hmincritnum<ta_deb & hminoknum>=ta_deb & hminoknum<=ta_fin),"oui","")) %>% 
    # plus le cas particulier des QoE-HARMONICA...
        mutate(depass_avant_H=ifelse(service_description %in% c("QoE-HARMONICA-Consultation","QoE-HARMONICA-Expertise") &
                                     (jour_crit==jour_ok & as.integer(jour_crit)>6 &
                                        # commence trop tot avec une fin avant minuit...
                                     (hmincritnum<ta_deb & hminoknum>=ta_deb & hminoknum<=24)) |
                                     (as.integer(jour_crit)==as.integer(jour_ok)-1 & as.integer(jour_crit)>6 &
                                        # commence trop tot avec une fin avant 4h du matin...
                                     (hmincritnum<ta_deb & hminoknum>=ta_deb & hminoknum<=4)),"oui","")) %>% 
        mutate(tps_depass_avant=ifelse(depass_avant=="oui" | depass_avant_H=="oui",
                                       (ta_deb-hmincritnum)*100 - 40*(floor(ta_deb)-as.integer(heure_crit)),
                                       0)) %>%

        mutate(depass_apres=ifelse(service_description %in% c("QoE-APIM-Requetes-DL",
                                                              "QoE-COLTRANE-Compte","QoE-COLTRANE-Enquete","QoE-COLTRANE-Enquete2",
                                                              "QoE-EPURE-Msa","QoE-EPURE-Urssaf",
                                                              "QoE-PILOT-Agents","QoE-REPONSE_Authentification",
                                                              "QoE-RORCAL-Google",
                                                              "QoE-SIRUS-Externe-Recherche_par_Denom",
                                                              "QoE-SIRUS-Externe-Recherche_par_Ident",
                                                              "QoE-WS-Atis_BRPP",
                                                              "QoE-BRPP-Etat_BDD",
                                                              "QoE-ESANE-Diffusion","QoE-ESANE-Diffusion_mineure","QoE-ESANE-Diffusion_mineure2",
                                                              "QoE-ESANE-Metropole","QoE-ESANE-Metropole_mineure","QoE-ESANE-Metropole_mineure2",
                                                              "QoE-ESANE-Reunion","QoE-ESANE-Reunion_mineure","QoE-ESANE-Reunion_mineure2",
                                                              "QoE-ESANE-AntGuyane","QoE-ESANE-AntGuyane_mineure","QoE-ESANE-AntGuyane_mineure2") &
                                   jour_crit==jour_ok &
                                      # ...ou fini trop tard
                                   (hmincritnum>=ta_deb & hmincritnum<=ta_fin & hminoknum>ta_fin),"oui","")) %>% 
    # plus le cas particulier des QoE-HARMONICA...
        mutate(depass_apres_H=ifelse(service_description %in% c("QoE-HARMONICA-Consultation","QoE-HARMONICA-Expertise") &
                                     (jour_crit==jour_ok & as.integer(jour_crit)>6 &
                                        # ...ou fini trop tard avec un debut de prob apres minuit...
                                     (hmincritnum>=0 & hmincritnum<=ta_fin & hminoknum>ta_fin)) |
                                     (as.integer(jour_crit)==as.integer(jour_ok)-1 & as.integer(jour_crit)>6 &
                                        # ...ou fini trop tard avec un debut de prob apres 21h du soir...
                                     (hmincritnum>=21 & hmincritnum<=ta_fin & hminoknum>ta_fin)),"oui","")) %>%  
        mutate(tps_depass_apres=ifelse(depass_apres=="oui" | depass_apres_H=="oui",
                                       (hminoknum-ta_fin)*100 - 40*(as.integer(heure_ok)-floor(ta_fin)),
                                       0)) %>%
  
        mutate(depass=ifelse(depass_avant=="oui" | depass_apres=="oui","oui",""),
               tps_depass=tps_depass_avant+tps_depass_apres) %>% 
        select(-depass_avant,-depass_avant_H,-tps_depass_avant,-depass_apres,-depass_apres_H,-tps_depass_apres) %>%

        mutate(jour=tps_depass%/%1440, jour_reste=tps_depass%%1440,
               heure=jour_reste%/%60, minute=round(jour_reste%%60,0),
               tps_depass_min=round(tps_depass,0)) %>% 
        select(-tps_depass,-jour_reste) %>%
  
        mutate(tps_depass_tot=paste0(jour,"j ",heure,"h ",minute,"m")) %>% 
        select(-jour,-heure,-minute) %>% 
  
        # remplacement de la duree calcule initialement pour prise en compte depassement
        mutate(duree_tot_min=ifelse(depass=="oui",tps_depass_min,duree_tot_min),
               duree_tot=ifelse(depass=="oui",tps_depass_tot,duree_tot)) %>% 
        select(-tps_depass_min,-tps_depass_tot)

  
# detection des week-end et des periodes a cheval dans un meme jour au niveau
# du calcul de la periode dindisponibilite en mode jours-ouvres 5j/7 et tranche 7h-19h
Req_out4 <- Req_out4 %>%
        mutate(coup_WE=ifelse((jour_crit==jour_ok   & str_sub(moment_crit,1,4) %in% c("sam.","dim.")) |
                              (as.integer(jour_crit)==as.integer(jour_ok)+1 & str_sub(moment_crit,1,4)=="sam." & str_sub(moment_ok,1,4)=="dim.") |
                              (as.integer(jour_crit)<=as.integer(jour_ok)+2 & str_sub(moment_crit,1,4)=="ven."               & as.integer(heure_crit)>=19 & str_sub(moment_ok,1,4) %in% c("sam.","dim.")) |
                              (as.integer(jour_crit)<=as.integer(jour_ok)+2 & str_sub(moment_crit,1,4) %in% c("sam.","dim.") & as.integer(heure_ok)<=7    & str_sub(moment_ok,1,4)=="lun.") |
                              (as.integer(jour_crit)==as.integer(jour_ok)+3 & str_sub(moment_crit,1,4)=="ven." & as.integer(heure_crit)>=19 & str_sub(moment_ok,1,4)=="lun." & as.integer(heure_ok)<=7),
                              "oui",""),
               tps_coup_WE=ifelse(coup_WE=="oui",
                                  0,
                                  NA)) %>%
                 
        mutate(coup_nuit=ifelse(coup_WE!="oui" & 
                                ((jour_crit==jour_ok & as.integer(heure_crit)>=19 & as.integer(heure_ok)<=24) |
                                 (jour_crit==jour_ok & as.integer(heure_crit)>=0  & as.integer(heure_ok)<=7) |
                                 (as.integer(jour_crit)==as.integer(jour_ok)-1 & as.integer(heure_crit)>=19  & as.integer(heure_ok)<=7)),
                                "oui",""),
                tps_coup_nuit=ifelse(coup_nuit=="oui",
                                     0,
                                     NA)) %>% 
            
        mutate(coup_matin=ifelse(coup_WE!="oui" & jour_crit==jour_ok &
                                 (as.integer(heure_crit)<7 & as.integer(heure_ok)>=7 & as.integer(heure_ok)<=19),
                                 "oui",""),
               tps_coup_matin=ifelse(coup_matin=="oui",
                                     (hminoknum-7)*100 - 40*(floor(as.integer(heure_ok))-7),
                                     NA)) %>%        
        # prise en compte maintenance de la QoE-PILOT-Agents seule QoE allant jusqua 7h30,
        # soit 30min de plus par rapport a la tranche 7h-19h quil faut donc retirer en plus
        mutate(tps_coup_matin=ifelse(service_description=="QoE-PILOT-Agents" & coup_matin=="oui",
                                     tps_coup_matin-30,tps_coup_matin)) %>%
  
        mutate(coup_soir=ifelse(coup_WE!="oui" & jour_crit==jour_ok &
                                (as.integer(heure_crit)>=7 & as.integer(heure_crit)<=19 & as.integer(heure_ok)>19),
                                "oui",""),
               tps_coup_soir=ifelse(coup_soir=="oui",
                                    (19-hmincritnum)*100 - 40*(19-floor(as.integer(heure_crit))),
                                    NA)) %>% 

        mutate(coup_jour=ifelse(coup_WE!="oui" & jour_crit==jour_ok &
                                (as.integer(heure_crit)>=7 & as.integer(heure_ok)<=19),
                                "oui",""),
                tps_coup_jour=ifelse(coup_jour=="oui",
                                     duree_tot_min,
                                     NA)) %>% 
  
        # coupure de plusieurs jours ouvres dans une meme semaine 
        mutate(coup_plusjours_memesem=ifelse(coup_WE!="oui" & jour_crit!=jour_ok & sem_crit==sem_ok & 
                                             str_sub(moment_crit,1,4) %in% c("lun.","mar.","mer.","jeu.","ven.") &
                                             str_sub(moment_ok,1,4) %in% c("lun.","mar.","mer.","jeu.","ven.") &
                                             as.integer(heure_crit)>=7 & as.integer(heure_crit)<=19 &
                                             as.integer(heure_ok)>=7 & as.integer(heure_ok)<=19,
                                             "oui",""),
               tps_coup_plusjours_memesem=ifelse(coup_plusjours_memesem=="oui",
                                                 (19-hmincritnum)*100 - 40*(19-floor(as.integer(heure_crit)))+
                                                 (hminoknum-7)*100 - 40*(floor(as.integer(heure_ok))-7)+
                                                 12*60*(as.integer(jour_ok)-as.integer(jour_crit)-1),
                                                 NA))



Req_out4 <- Req_out4 %>% 
        mutate(duree_5j7=ifelse(coup_WE=="oui" | coup_nuit=="oui",0,
                                ifelse(coup_matin=="oui",tps_coup_matin,
                                       ifelse(coup_soir=="oui",tps_coup_soir,
                                              ifelse(coup_jour=="oui",tps_coup_jour,
                                                     ifelse(coup_plusjours_memesem=="oui",tps_coup_plusjours_memesem,NA))))))


               
vue <- Req_out4 %>% 
    select(service_description,periode,duree_tot_min,duree_tot,TA_rec,duree_5j7) %>%
    filter(is.na(duree_5j7) | (duree_tot_min<duree_5j7 & TA_rec!="oui"))

###

Req_out4 <- Req_out4 %>% mutate(jour_crit=ifelse(jour_crit=="NA",jour_ok,jour_crit),
                                mois_crit=ifelse(mois_crit=="NA",mois_ok,mois_crit),
                                heure_crit=ifelse(heure_crit=="NA",heure_ok,heure_crit),
                                min_crit=ifelse(min_crit=="NA",min_ok,min_crit)) %>%
                         arrange(jour_crit,mois_crit,heure_crit,min_crit)


Req_out5 <- Req_out4 %>% select(-jour_crit:-min_crit,-jour_ok:-min_ok,
                                -toujours_en_cours,
                                -jour_deb,-jour_fin,-ta_deb,-ta_fin,
                                -hmincritnum,-hminoknum,
                                -coup_WE:-tps_coup_plusjours_memesem) %>% 
                         filter(TA_rec!="oui") %>%
                         select(-numligne,-TA_rec) %>%
  
                         rename(duree_tot_min_5j7=duree_5j7) %>% 
                         mutate(jour=duree_tot_min_5j7%/%1440, jour_reste=duree_tot_min_5j7%%1440,
                                heure=jour_reste%/%60, minute=round(jour_reste%%60,0)) %>% 
                         select(-jour_reste) %>%
                         mutate(duree_tot_5j7=paste0(jour,"j ",heure,"h ",minute,"m")) %>% 
                         select(-jour,-heure,-minute) %>%
                         mutate(duree_tot_5j7=ifelse(duree_tot_5j7=="NAj NAh NAm","?? ??",duree_tot_5j7)) %>% 
  
                         mutate(numligne=row_number()) %>% relocate(numligne) %>% 
                         mutate(duree_tot_min=ifelse(is.na(duree_tot_min),0,duree_tot_min),
                                duree_tot=ifelse(is.na(duree_tot),"",duree_tot)) %>% 
                        relocate("moment_crit","moment_ok",.after=last_col())


######################################################################
######################################################################
######################################################################
######################################################################


# encodage des 3 images du mel
# library(base64enc)
# doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE/"
# 
# Schtroumpf_a_lunettes_xs_encode <- base64encode(paste0(doss,"Schtroumpf_a_lunettes_xs.png"))
# write.csv2(Schtroumpf_a_lunettes_xs_encode,
#              paste0(doss, "Img_encode64_Schtroumpf_a_lunettes_xs.csv"),
#              row.names = FALSE)
# 
# QoE_image_encode <- base64encode(paste0(doss,"QoE_image.png"))
# write.csv2(QoE_image_encode,
#            paste0(doss, "Img_encode64_QoE_image.csv"),
#            row.names = FALSE)
# 
# Schtroumpf_village_perdu_avecMessageCEI_compress_encode <- base64encode(paste0(doss,"Schtroumpf_village_perdu_avecMessageCEI_compress.JPG"))
# write.csv2(Schtroumpf_village_perdu_avecMessageCEI_compress_encode,
#            paste0(doss, "Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"),
#            row.names = FALSE)

##

# recuperation des images encodees dans des variables
doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE/"

img64_a_lunettes <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_a_lunettes_xs.csv"))
img64_a_lunettes <- img64_a_lunettes$x[1]

img64_QoE_image <- read.csv2(paste0(doss,"Img_encode64_QoE_image.csv"))
img64_QoE_image <- img64_QoE_image$x[1]

img64_Schtroumpf_village_perdu <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"))
img64_Schtroumpf_village_perdu <- img64_Schtroumpf_village_perdu$x[1]


######################################################################
######################################################################
######################################################################
######################################################################

# Preparation partie HTML

Creat_DF_HTML <-function(df){
    df_out <- df %>% mutate(numligne=row_number()) %>%
                mutate(pos1_h=str_locate(periode,"h")[numligne,1]) %>% 
                mutate(pos2_h=pos1_h+str_locate(str_sub(periode,pos1_h+1,str_length(periode)),"h")[numligne,1])
    df_out <- df_out %>% 
        mutate(periode=ifelse(is.na(pos2_h),
                              paste0(str_sub(periode,1,pos1_h-1),"<sup><i>h",
                                     str_sub(periode,pos1_h+1,pos1_h+2),"</i></sup>",
                                     str_sub(periode,pos1_h+3,str_length(periode))),
                              paste0(str_sub(periode,1,pos1_h-1),"<sup><i>h",
                                     str_sub(periode,pos1_h+1,pos1_h+2),"</i></sup>",
                                     str_sub(periode,pos1_h+3,pos2_h-1),"<sup><i>h",
                                     str_sub(periode,pos2_h+1,pos2_h+2),"</i></sup>")))
    df_out <- df_out %>% select(-pos1_h,-pos2_h)
    return(df_out)
}
    
Req_out5_html  <- Creat_DF_HTML(Req_out5)

Req_out5_html <- Req_out5_html %>% 
                  filter(str_sub(periode,4,4) %in% c("0","1","2","3") &
                         str_sub(periode,5,5) %in% c("0","1","2","3","4","5","6","7","8","9")) %>% 
                  mutate(jour_indic=as.integer(str_sub(periode,4,6))%%2) %>% 
                  relocate("moment_crit","moment_ok",.after=last_col())



Req_out6 <- Req_out5 %>% group_by(service_description) %>%
            summarize(duree_tot_min=sum(duree_tot_min)) %>%
            ungroup() %>% as.data.frame() %>% arrange(desc(duree_tot_min)) %>% 
            filter(duree_tot_min>0) %>% 
            mutate(jour=duree_tot_min%/%1440, jour_reste=duree_tot_min%%1440,
                   heure=jour_reste%/%60, minute=round(jour_reste%%60,0)) %>% 
            select(-jour_reste) %>% 
            mutate(duree_lib=paste0(jour,"j ",heure,"h ",minute,"m")) %>% 
            select(-jour,-heure,-minute)

Req_out6_sup1h <- Req_out5 %>% group_by(service_description) %>%
            summarize(duree_tot_min=sum(duree_tot_min)) %>%
            ungroup() %>% as.data.frame() %>% arrange(desc(duree_tot_min)) %>% 
            filter(duree_tot_min>=60) %>% 
            mutate(jour=duree_tot_min%/%1440, jour_reste=duree_tot_min%%1440,
                   heure=jour_reste%/%60, minute=round(jour_reste%%60,0)) %>% 
            select(-jour_reste) %>% 
            mutate(duree_lib=paste0(jour,"j ",heure,"h ",minute,"m")) %>% 
            select(-jour,-heure,-minute)


######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mél Outlook ----

head <- paste0("<head>",
                    "<meta charset='utf-8'/>",
                    "<title>ENVOI INFOS ALERTES QoE</title>",
                    "<style>",
                        ".tabletitre {border:         none;
                                      text-align:     left;
                                      vertical-align: top;}",
               
                        "tr, td {border:          1px solid black;
                                 padding:         5px;
                                 border-collapse: collapse;
                                 text-align:      left;}",
               
                        ".piedtable {border:      none;
                                     padding-top: 3px;
                                     text-align:  left;}",
               
                        "sup {font-size: 15px;}",
               
                        ".caseRed {color:            rgb(200,0,0);
                                   font-weight:      bold;
                                   background-color: rgb(255,228,196);}",
               
                        ".caseGray {background-color: Gainsboro;}",
               
                        ".ligneTot {font-weight:      bold;
                                    background-color: rgb(239,228,176);}",
               
                    "</style>",
               "</head>")


# titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
#                   <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
#                   du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL (version DEV sur C:/user)</strong>
#                   </p>")
# titre2 <- paste0("<p style='font-size:20px'>-- R&eacute;capitulatifs des alertes QoE issues de Centreon --</p>")

titre1 <- paste0("<p style='font-size:20px, line-height:30%'>
                     <strong>QoE Checking</strong><br>
                  </p>")
titre2 <- paste0("<p style='font-size:20px'>-- R&eacute;capitulatifs des alertes --</p>")


# avec img encodees en base64
imageSchtroumpf <- paste0("<p><img src='data:image/png;base64,",img64_a_lunettes,"
                               alt='Photo de Schtroumpf'/></p>")

imageQoE <- paste0("<p><img src='data:image/png;base64,",img64_QoE_image,"
                             alt='Photo de logo QoE'/></p>")

imageVillage <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf_village_perdu,"
                            alt='Photo de Village'/></p>")


# imageSchtroumpf <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_RecapQoE\\Schtroumpf_a_lunettes_xs.png'
#                               alt='Photo de Schtroumpf'/></p>")
# 
# imageQoE <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_RecapQoE\\QoE_image.png'
#                     alt='Photo de losanges QoE'/></p>")
# 
# imageVillage <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_RecapQoE\\Schtroumpf_village_perdu_avecMessageCEI_compress.jpg'
#                            alt='Photo de Village'/></p>")


entete <- paste0("<thead>",
                    "<tr>",
                        "<td class='ligneTot'>libell&eacute; QoE</td>",
                        "<td class='ligneTot'>&Eacute;v&egrave;nements</td>",
                        "<td class='ligneTot'>p&eacute;riode</td>",
                        "<td class='ligneTot' colspan=2>dur&eacute;e<br>
                                                        (7j/7 et 24h/24)</td>",
                        "<td class='ligneTot'>indic. de lien<br>
                                              avec p&eacuteriode<br>
                                              de maintenance</td>",
                        "<td class='ligneTot' colspan=2>dur&eacute;e<br>
                                                        (5j/7 et 7h-19h)</td>",
                    "</tr>",
                 "</thead>")


entete_2 <- paste0("<thead>",
                      "<tr>",
                        "<td class='ligneTot'>libell&eacute; QoE</td>",
                        "<td class='ligneTot' colspan=2>dur&eacute;e</td>",
                   "</tr>",
                   "</thead>")


pied <- paste0 ("<tfoot>",
                    "<tr>
                        <td class='piedtable' colspan=4>",imageVillage,"</td>
                    </tr>",
                "</tfoot>")


corps_tab <- ""

if (nrow(Req_out5_html)>0){
    for (i in 1:nrow(Req_out5_html)){
        for (j in 4:11){
            if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
            if (j==8 & (Req_out5_html[i,7]>=60 | str_detect(Req_out5_html[i,6],"toujours en cours"))){
                corps_tab <- paste0(corps_tab,"<td class='caseRed'>",Req_out5_html[i,j],"</td>")
            } else {
                if (Req_out5_html[i,12]==0){
                    corps_tab <- paste0(corps_tab,"<td class='caseGray'>",Req_out5_html[i,j],"</td>")
                } else {
                    corps_tab <- paste0(corps_tab,"<td>",Req_out5_html[i,j],"</td>")
                }
            }
            if (j==11){corps_tab <- paste0(corps_tab,"</tr>")}
        }
    }
} else {
    corps_tab <- paste0(corps_tab,"<tr><td colspan=4>absence d'alertes QoE</td></tr>")
}


##

corps_tab_2 <- ""

if (nrow(Req_out6_sup1h)>0){
    for (i in 1:nrow(Req_out6_sup1h)){
        for (j in 1:3){
            if (j==1){corps_tab_2 <- paste0(corps_tab_2,"<tr>")}
              corps_tab_2 <- paste0(corps_tab_2,"<td>",Req_out6_sup1h[i,j],"</td>")
            if (j==8){corps_tab <- paste0(corps_tab,"</tr>")}
        }
    }
} else {
    corps_tab_2 <- paste0(corps_tab_2,"<tr><td colspan=3>absence d'alertes QoE</td></tr>")
}


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                    head,
                    "<body>",
              
                        # titre1,titre2,imageGargamel,
              
                        "<table class='tabletitre'>",
                            "<tr>",
                                "<td class='tabletitre'>",titre1,titre2,imageSchtroumpf,"</td>",
                                "<td class='tabletitre'>",imageQoE,"</td>",
                            "</tr>",
                        "</table>",
              
                        "<h2 style='margin-top: 10; margin-bottom: 5; color: DarkGoldenrod;'>
                              I-/ Alertes et r&eacute;tablissements QoE dans le mois
                         </h2>",
              
                        "<table>",
                            entete,corps_tab,
                        "</table>",
                        
                        "<h2 style='margin-top: 10; margin-bottom: 0; color: DarkGoldenrod;'>
                              II-/ R&eacute;partition par QoE selon une dur&eacute;e d&eacute;croissante des indisponibilit&eacute;s
                         </h2>",
                        "<p style='margin: 0; margin-bottom: 5; color: DarkGoldenrod;'>
                              &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                              <i>(...en cumul dans le mois, avec limitation aux dur&eacute;es sup&eacute;rieures &agrave; <u>60 min</u>)</i>
                         </p>",

                        "<table>",
                            entete_2,corps_tab_2, #pied,
                        "</table>",
                    "</body>",
              "</html>")


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# SANS LES IMAGES
tab_ss_img <- paste0("<!DOCTYPE html>",
                "<html>",
                    head,
                    "<body>",
              
                    # titre1,titre2,
              
                    "<table class='tabletitre'>",
                        "<tr>",
                            "<td class='tabletitre'>",titre1,titre2,"</td>",
                        "</tr>",
                    "</table>",
              
                    "<h2 style='margin-top: 10; margin-bottom: 5; color: DarkGoldenrod;'>
                              I-/ Alertes et r&eacute;tablissements QoE dans le mois
                     </h2>",
              
                    "<table>",
                        entete,corps_tab,
                    "</table>",
              
                    "<h2 style='margin-top: 10; margin-bottom: 0; color: DarkGoldenrod;'>
                              II-/ R&eacute;partition par QoE selon une dur&eacute;e d&eacute;croissante des indisponibilit&eacute;s
                     </h2>",
                    "<p style='margin: 0; margin-bottom: 5; color: DarkGoldenrod;'>
                              &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                              <i>(...en cumul dans le mois, avec limitation aux dur&eacute;es sup&eacute;rieures &agrave; <u>60 min</u>)</i>
                     </p>",
              
                    "<table>",
                        entete_2,corps_tab_2,
                    "</table>",
                    "</body>",
              "</html>")


######################################################################
######################################################################
######################################################################
######################################################################


# Creation dun repertoire avec la date du jour
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
    cat("The folder already exists")
} else {
    dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}


# Creation dun fichier HTML dans le repertoire nouvellement cree par injection du contenu de la variable tab
setwd(folder)

# if (file.exists(file)) {
#     cat("The file already exists")
# } else {
#     file.create(file, showWarnings = TRUE)
# }
file <- "CorpsMel_Notifications.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="CorpsMel_Notifications.html",append=TRUE)


# export des principaux resultats
Req_out4_csv <- Req_out4 %>% select(service_description,jour_crit:depass)
write.csv2(Req_out4_csv,paste0(folder,"/ResumCoupuresQoE_Dates.csv"))
write.csv2(Req_out5,paste0(folder,"/ResumCoupuresQoE_Durees.csv"))
write.csv2(Req_out6,paste0(folder,"/ResumCoupuresQoE_DureesCumulees.csv"))

folder2 <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo"
write.csv2(Req_out5,paste0(folder2,"/ResumCoupuresQoE_Durees_",mois_date_jour,".csv"))

# folder3 <- "E:/Analyse_GEPEX/ProgR_QoE_RapportDispo"
# write.csv2(Req_out5,paste0(folder3,"/ResumCoupuresQoE_Durees_",mois_date_jour,".csv"))



###

#second endroit generalise du fichier (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE/General"
if (file.exists(fileGen)) {
    cat("The folder already exists")
} else {
    dir.create(fileGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}
setwd(fileGen)
file.create("CorpsMel_Notifications.html", showWarnings = TRUE)
cat(tab,file="CorpsMel_Notifications.html",append=TRUE)

file.create("CorpsMel_QoE.html", showWarnings = TRUE)
cat(tab_ss_img,file="CorpsMel_QoE.html",append=TRUE)

# insertion dans un .zip
zip("QoE_Listing.zip",paste0("CorpsMel_Notifications.html"))


######################################################################
######################################################################
######################################################################
######################################################################


# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_RecapQoE\\EnvoiMail_ViaPowershell.ps1"')


# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM
