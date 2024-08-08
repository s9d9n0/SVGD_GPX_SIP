
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
date_jour_1JRmoins <- date_jour - ddays(1) - dhours(decalage_heure)

####

##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
##
# Connexion a la nouvelle base 50
# maitre xx.xx.xx.xx ; esclave xx.xx.xx.xx
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture", 
                     password="Reporting", dbname="centreon_storage", 
                     host="xx.xx.xx.xx", port="3306")

##################################################################

## Au niveau de la table DOWNTIMES ----
Req_01_ANCIEN <- dbGetQuery(baseMdb, 
                            paste("SELECT *,
                                    FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                                    FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date,
                                    FROM_UNIXTIME(start_time,'%Y-%m-%d %H:%i:%S')    AS ch_deb_date,
                                    FROM_UNIXTIME(end_time,'%Y-%m-%d %H:%i:%S')      AS ch_fin_date
                           FROM downtimes
                           WHERE entry_time <= UNIX_TIMESTAMP('",date_jour_1JRmoins,"') 
                           ORDER BY entry_time DESC
                          "))

Req_02_PASSEE <- dbGetQuery(baseMdb, 
                            paste("SELECT *,
                                    FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                                    FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date,
                                    FROM_UNIXTIME(start_time,'%Y-%m-%d %H:%i:%S')    AS ch_deb_date,
                                    FROM_UNIXTIME(end_time,'%Y-%m-%d %H:%i:%S')      AS ch_fin_date
                           FROM downtimes
                           WHERE entry_time > UNIX_TIMESTAMP('",date_jour_1JRmoins,"') AND
                                 end_time <= UNIX_TIMESTAMP('",date_jour,"')
                           ORDER BY entry_time DESC
                          "))

Req_03_ENCOURS <- dbGetQuery(baseMdb, 
                             paste("SELECT *,
                                    FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                                    FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date,
                                    FROM_UNIXTIME(start_time,'%Y-%m-%d %H:%i:%S')    AS ch_deb_date,
                                    FROM_UNIXTIME(end_time,'%Y-%m-%d %H:%i:%S')      AS ch_fin_date
                            FROM downtimes
                            WHERE entry_time > UNIX_TIMESTAMP('",date_jour_1JRmoins,"') AND
                                  deletion_time IS NULL AND
                                  start_time <= UNIX_TIMESTAMP('",date_jour,"') AND
                                  end_time > UNIX_TIMESTAMP('",date_jour,"')
                            ORDER BY entry_time DESC
                           "))

Req_04_FUTUR <- dbGetQuery(baseMdb, 
                           paste("SELECT *,
                                    FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                                    FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date,
                                    FROM_UNIXTIME(start_time,'%Y-%m-%d %H:%i:%S')    AS ch_deb_date,
                                    FROM_UNIXTIME(end_time,'%Y-%m-%d %H:%i:%S')      AS ch_fin_date
                            FROM downtimes
                            WHERE start_time > UNIX_TIMESTAMP('",date_jour,"') 
                            ORDER BY start_time ASC
                           "))

##################################################################

## Au niveau de la table HOSTS et afin de recuperer notamment les noms ----
Hosts <- dbGetQuery(baseMdb, 
                    paste("SELECT host_id, alias FROM hosts", sep=""))

##################################################################

## Au niveau de la table SERVICES et afin de recuperer notamment les noms ----
Services <- dbGetQuery(baseMdb, 
                       paste("SELECT service_id, description FROM services", sep=""))

##################################################################


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
  df_out <- df %>% mutate(POSIXct_entree=ymd_hms(ch_entree_date),POSIXct_efface=ymd_hms(ch_efface_date),
                          POSIXct_deb=ymd_hms(ch_deb_date),      POSIXct_fin=ymd_hms(ch_fin_date)) %>% 
    select(-triggered_by,-fixed,-instance_id,-internal_id) %>%
    select(-entry_time,-deletion_time,-start_time,-end_time) %>%
    select(-ch_entree_date,-ch_efface_date,-ch_deb_date,-ch_fin_date) %>%
    mutate(POSIXct_entree=POSIXct_entree+dhours(decalage_heure), POSIXct_efface=POSIXct_efface+dhours(decalage_heure),
           POSIXct_deb=POSIXct_deb+dhours(decalage_heure),       POSIXct_fin=POSIXct_fin+dhours(decalage_heure)) %>%
    # filtre sur type=1 et sur les idep du gepex
    arrange(downtime_id) %>% filter(type==1) %>% mutate(nb_obs=1) %>% 
    filter(!(author %in% c("centreon-engine","clapi","dg57-cei-service-u","Downtime cycle"))) %>% 
    # jointures avec les tables Hosts et Services
    left_join(Hosts, by=c("host_id")) %>% left_join(Services, by=c("service_id")) %>% 
    # recodage du commentaire
    mutate(commentaire=ifelse(str_sub(comment_data,1,23)=="Temps d arrêt fixé par ",
                              str_sub(comment_data,24,str_length(comment_data)),
                              comment_data)) %>% 
    mutate(commentaire=ifelse(str_sub(commentaire,1,str_length(author))==author,
                              str_sub(commentaire,str_length(author)+2,str_length(commentaire)),
                              commentaire))
  return(df_out)
}

fct_recode_auteur <- function(df,newvar,varref){
  df %>% mutate("{{newvar}}":=case_when(str_to_upper({{varref}}) %in% c("EY9CCT","SIAR_EY9CCT") ~ "EY9CCT - xxx",
                                        str_to_upper({{varref}}) %in% c("YCG8L6","SIAR_YCG8L6") ~ "YCG8L6 - Antonio",
                                        str_to_upper({{varref}}) %in% c("OK8MZB","SIAR_OK8MZB","ADMIN") ~ "OK8MZB - Romuald",
                                        str_to_upper({{varref}}) %in% c("DZIJQL","SIAR_DZIJQL") ~ "DZIJQL - Fred. P",
                                        str_to_upper({{varref}}) %in% c("TMH7F8","SIAR_TMH7F8") ~ "TMH7F8 - Jean-Bapt.",
                                        str_to_upper({{varref}}) %in% c("MKD0W7","SIAR_MKD0W7") ~ "MKD0W7 - Fred. V",
                                        str_to_upper({{varref}}) %in% c("H13YP3","SIAR_H13YP3") ~ "H13YP3 - Jean-Fran.",
                                        TRUE ~ str_to_upper({{varref}})
  )) %>% select(-{{varref}}) %>% relocate({{newvar}})
}

fct_recode_date <- function(df,newvar,vardate){
  df %>% mutate("{{newvar}}":=paste0(ifelse(str_length(day({{vardate}}))==1,  paste0("0",day({{vardate}})),  day({{vardate}}))," ",
                                     ifelse(str_length(month({{vardate}}))==1,paste0("0",month({{vardate}})),month({{vardate}}))," ",
                                     year({{vardate}})," - ",
                                     ifelse(str_length(hour({{vardate}}))==1,  paste0("0",hour({{vardate}})),  hour({{vardate}})),"h",
                                     ifelse(str_length(minute({{vardate}}))==1,paste0("0",minute({{vardate}})),minute({{vardate}}))
  ))
}

fct_recode_moisdate <- function(df,newvar,vardate){
  df %>% mutate("{{newvar}}":=case_when(str_sub({{vardate}},4,5)=="01" ~ "janv",  str_sub({{vardate}},4,5)=="02" ~ "fevr",
                                        str_sub({{vardate}},4,5)=="03" ~ "mars",  str_sub({{vardate}},4,5)=="04" ~ "avril",
                                        str_sub({{vardate}},4,5)=="05" ~ "mai",   str_sub({{vardate}},4,5)=="06" ~ "juin",
                                        str_sub({{vardate}},4,5)=="07" ~ "juil.", str_sub({{vardate}},4,5)=="08" ~ "aout",
                                        str_sub({{vardate}},4,5)=="09" ~ "sept",  str_sub({{vardate}},4,5)=="10" ~ "oct.",
                                        str_sub({{vardate}},4,5)=="11" ~ "nov.",  str_sub({{vardate}},4,5)=="12" ~ "dec.",
                                        TRUE ~ ""
  ))
}


fct_Agreg_et_Recodif <- function(df){
  # partie 1 : AGREGATION
  df_out <- df %>% 
    group_by(author, commentaire, alias, cancelled, started, POSIXct_entree, POSIXct_deb, POSIXct_fin, POSIXct_efface) %>%
    summarize(nb_obs=sum(nb_obs)) %>%
    ungroup() %>% as.data.frame()
  
  Req_1service <- df_out %>% filter(nb_obs==1) %>% left_join(df) %>%
    select(author, commentaire, alias, cancelled, started, POSIXct_entree, POSIXct_deb, POSIXct_fin, POSIXct_efface, description)
  
  df_out <- df_out %>% left_join(Req_1service) %>% 
    mutate(service=ifelse(is.na(description),paste0(" >> Selection de ",nb_obs," services"),description)) %>% 
    select(-cancelled,-started,-nb_obs,-description) %>% relocate("service",.after="alias") %>% rename(hote=alias)
  
  rm(Req_1service)
  
  # partie 2 : RECODIFICATION avec utilisation des fonctions precedemment definies :
  # fct_recode_auteur,fct_recode_date,fct_recode_moisdate
  # recodification des auteurs
  df_out <- fct_recode_auteur(df_out,auteur,author)
  
  # recodification et reformatage des differentes variables temporelles
  df_out <- fct_recode_date(df_out,mise_en_place,POSIXct_entree)
  df_out <- fct_recode_date(df_out,heure_deb,POSIXct_deb)
  df_out <- fct_recode_date(df_out,heure_fin,POSIXct_fin)
  df_out <- fct_recode_date(df_out,interruption,POSIXct_efface)
  
  df_out <- df_out %>% select(-POSIXct_deb:-POSIXct_efface) %>% 
    mutate(interruption=ifelse(str_sub(interruption,1,2)=="NA" | interruption==heure_fin,"",interruption))
  
  # recodification du mois des differentes variables temporelles
  df_out <- fct_recode_moisdate(df_out,mois_mise_en_place,mise_en_place)
  df_out <- fct_recode_moisdate(df_out,mois_deb,heure_deb)
  df_out <- fct_recode_moisdate(df_out,mois_fin,heure_fin)
  df_out <- fct_recode_moisdate(df_out,mois_interruption,interruption)
  
  # recodification de la variable indiquant le moment de mise en place du temps arret
  df_out <- df_out %>% 
    mutate(mise_en_place=paste0(str_sub(mise_en_place,1,2)," ",mois_mise_en_place," ",str_sub(mise_en_place,7,18))) %>% 
    select(-mois_mise_en_place)
  
  # recodification de la variable indiquant la periode du temps arret
  df_out <- df_out %>% 
    mutate(periode=case_when(str_sub(heure_deb,1,2)==str_sub(heure_fin,1,2) & 
                               mois_deb==mois_fin &
                               str_sub(heure_deb,7,10)==str_sub(heure_fin,7,10) ~ paste0("le ",str_sub(heure_deb,1,2)," ",mois_deb," ",str_sub(heure_deb,7,10)," - entre ",str_sub(heure_deb,14,18)," et ",str_sub(heure_fin,14,18)," "),
                             str_sub(heure_deb,1,2)!=str_sub(heure_fin,1,2) &
                               str_sub(heure_deb,7,10)==str_sub(heure_fin,7,10) ~ paste0("du ",str_sub(heure_deb,1,2)," ",mois_deb," ",str_sub(heure_deb,14,18)," jusqu'au ",str_sub(heure_fin,1,2)," ",mois_fin," ",str_sub(heure_fin,14,18)," (",str_sub(heure_deb,7,10),")"),
                             str_sub(heure_deb,1,2)!=str_sub(heure_fin,1,2)   ~ paste0("du ",str_sub(heure_deb,1,2)," ",mois_deb," ",str_sub(heure_deb,7,10)," ",str_sub(heure_deb,14,18)," jusqu'au ",str_sub(heure_fin,1,2)," ",mois_fin," ",str_sub(heure_fin,7,10)," ",str_sub(heure_fin,14,18)," "),
                             TRUE ~ ""
    )
    )
  # recodification de la variable interruption indiquant si le temps arret est arrete avant la fin prevue
  df_out <- df_out %>% select(-heure_deb,-heure_fin,-mois_deb,-mois_fin) %>% 
    mutate(arret_avt_fin=paste0(str_sub(interruption,1,2)," ",mois_interruption," ",str_sub(interruption,7,18))) %>% 
    arrange(desc(POSIXct_entree)) %>% select(-POSIXct_entree,-interruption,-mois_interruption)
  
  return(df_out)
}

####################################################################################
####################################################################################

# Vue <- Vue_ANCIEN %>% full_join(Vue_PASSEE) %>% full_join(Vue_ENCOURS) %>% full_join(Vue_FUTUR) %>% 
#   mutate(auteur=str_to_upper(auteur)) %>% 
#   mutate(auteur=ifelse(str_sub(auteur,1,2)=="R_",str_sub(auteur,3,str_length(auteur)),auteur)) %>% 
#   group_by(auteur) %>% 
#   summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
#   ungroup() %>% as.data.frame()


######################################################################
######################################################################
######################################################################
######################################################################

####################################
# DEBUT TRAITEMENT DES TEMPS ARRET #
####################################

# Gepex_ANCIEN   <- fct_1ers_traitements(Req_01_ANCIEN)
# Gepex_ANCIEN_2 <- fct_Agreg_et_Recodif(Gepex_ANCIEN)

Gepex_PASSEE   <- fct_1ers_traitements(Req_02_PASSEE)
Gepex_PASSEE_2 <- fct_Agreg_et_Recodif(Gepex_PASSEE)

Gepex_ENCOURS   <- fct_1ers_traitements(Req_03_ENCOURS)
Gepex_ENCOURS_2 <- fct_Agreg_et_Recodif(Gepex_ENCOURS)

Gepex_FUTUR   <- fct_1ers_traitements(Req_04_FUTUR)
Gepex_FUTUR_2 <- fct_Agreg_et_Recodif(Gepex_FUTUR)


Creat_DF_HTML_partie1 <-function(df){
  df %>% mutate(numligne=row_number()) %>%
    # modifications sur variable mise_en_place
    mutate(place_h_mise_en_place=str_locate(mise_en_place,"h")[numligne,1]) %>% 
    mutate(mise_en_place=paste0(str_sub(mise_en_place,1,place_h_mise_en_place-1),
                                "<sup><i>h",
                                str_sub(mise_en_place,place_h_mise_en_place+1,str_length(mise_en_place)),
                                "</i></sup>")) %>%
    select(-place_h_mise_en_place) %>% 
    # modifications sur variable periode
    mutate(place1_h_periode=str_locate(periode,"h")[numligne,1]) %>%
    mutate(place2_h_periode=str_locate(str_sub(periode,place1_h_periode+1,str_length(periode)),"h")[numligne,1]+place1_h_periode) %>%
    mutate(periode=paste0(str_sub(periode,1,place1_h_periode-1),
                          "<sup><i>h",
                          str_sub(periode,place1_h_periode+1,place1_h_periode+2),
                          "</i></sup>",
                          str_sub(periode,place1_h_periode+3,place2_h_periode-1),
                          "<sup><i>h",
                          str_sub(periode,place2_h_periode+1,place2_h_periode+2),
                          "</i></sup>",
                          str_sub(periode,place2_h_periode+3,str_length(periode)))) %>%
    select(-place1_h_periode,-place2_h_periode) %>%
    # modifications sur variable arret_avt_fin
    mutate(place_h_arret_avt_fin=ifelse(!is.na(str_locate(arret_avt_fin,"h")[numligne,1]),
                                        str_locate(arret_avt_fin,"h")[numligne,1],
                                        0)) %>%
    mutate(arret_avt_fin=ifelse(place_h_arret_avt_fin!=0,
                                paste0(str_sub(arret_avt_fin,1,place_h_arret_avt_fin-1),
                                       "<sup><i>h",
                                       str_sub(arret_avt_fin,place_h_arret_avt_fin+1,str_length(arret_avt_fin)),
                                       "</i></sup>"),
                                arret_avt_fin)) %>%
    select(-numligne,-place_h_arret_avt_fin)
}

Creat_DF_HTML_partie2 <-function(df){
  df_out <- df %>% mutate(numligne=row_number()) %>% 
    mutate(compar_lag =ifelse(auteur==lag(auteur) & commentaire==lag(commentaire) &
                                mise_en_place==lag(mise_en_place) & periode==lag(periode) &
                                arret_avt_fin==lag(arret_avt_fin),1,0)) %>% 
    mutate(compar_lead=ifelse(auteur==lead(auteur) & commentaire==lead(commentaire) &
                                mise_en_place==lead(mise_en_place) & periode==lead(periode) &
                                arret_avt_fin==lead(arret_avt_fin),1,0)) %>% 
    mutate(compar_lag =ifelse(is.na(compar_lag),0,compar_lag),
           compar_lead=ifelse(is.na(compar_lead),0,compar_lead)) %>% 
    mutate(numlg=ifelse(compar_lag==0 & compar_lead==1,
                        numligne-1,
                        ifelse(compar_lag==1 & compar_lead==0,
                               numligne,
                               ifelse(compar_lag==0 & compar_lead==0,-1,0)))) %>% 
    # traitement particulier si le 1er cas peut etre agrege
    mutate(numlg=ifelse(numligne==1 & numlg==0,1,numlg))
  
  extract <- df_out %>% select(numligne,compar_lag,compar_lead,numlg) %>% filter(numlg!=0) %>% 
    mutate(nb_rowspan=ifelse(numlg!=-1,lead(numlg)-numlg,1)) %>% select(-numlg) %>% 
    filter(nb_rowspan>0 & !is.na(nb_rowspan)) %>% 
    # traitement particulier si le 1er cas peut etre agrege
    mutate(nb_rowspan=ifelse(numligne==1 & compar_lag==0 & compar_lead==1,nb_rowspan+1,nb_rowspan)) %>% 
    select(-compar_lag,-compar_lead)
  
  df_out <- df_out %>% left_join(extract, by=c("numligne")) %>% select(-numligne:-numlg) %>% 
    mutate(nb_rowspan=ifelse(is.na(nb_rowspan),0,nb_rowspan))
  return(df_out)
}

Gepex_PASSEE_html  <- Creat_DF_HTML_partie1(Gepex_PASSEE_2)
Gepex_PASSEE_html  <- Creat_DF_HTML_partie2(Gepex_PASSEE_html)

Gepex_ENCOURS_html <- Creat_DF_HTML_partie1(Gepex_ENCOURS_2)
Gepex_ENCOURS_html <- Creat_DF_HTML_partie2(Gepex_ENCOURS_html)

Gepex_FUTUR_html   <- Creat_DF_HTML_partie1(Gepex_FUTUR_2)
Gepex_FUTUR_html   <- Creat_DF_HTML_partie2(Gepex_FUTUR_html)


######################################################################
######################################################################
######################################################################
######################################################################
# PARTIE GENERATION DES IMAGES EN ENCODAGE 64 : 3 images
# 
# setwd("C:/Users/SIAR_YCG8L6/Docs/ProgrammesR/Centreon_ListeTempsArret")
# library(base64enc)
# 
# chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeTempsArret/"
# 
# imgencode <- base64encode("Downtime_BoogieNights_xs.png")
# write.csv2(imgencode,paste0(chemin, "Img_encode64_Downtime_BoogieNights_xs.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("Schtroumpfs_qui_dansent_xs.png")
# write.csv2(imgencode,paste0(chemin, "Img_encode64_Schtroumpfs_qui_dansent.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("Schtroumpf_village_perdu_avecMessageCEI_compress.jpg")
# write.csv2(imgencode,paste0(chemin, "Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"), row.names = FALSE)


# recuperation des images encodees dans des variables

chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeTempsArret/"

img64_Downtime_BoogieNights <- read.csv2(paste0(chemin,"Img_encode64_Downtime_BoogieNights_xs.csv"))
img64_Downtime_BoogieNights <- img64_Downtime_BoogieNights$x[1]

img64_Schtroumpfs_qui_dansent <- read.csv2(paste0(chemin,"Img_encode64_Schtroumpfs_qui_dansent.csv"))
img64_Schtroumpfs_qui_dansent <- img64_Schtroumpfs_qui_dansent$x[1]

img64_Schtroumpf_village <- read.csv2(paste0(chemin,"Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"))
img64_Schtroumpf_village <- img64_Schtroumpf_village$x[1]



######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

head <- paste0("<head>",
               "<meta charset='utf-8'/>",
               "<title>ENVOI INFOS TEMPS D'ARRETS</title>",
               "<style>",
               ".tabletitre {border:         none;
                             text-align:     left;
                             vertical-align: top;}",
               
               "tr, td {border:          1px solid black;
                        padding:         5px;
                        border-collapse: collapse;
                        text-align:      center;}",
               
               ".piedtable {border:      none;
                            padding-top: 3px;
                            text-align:  left;}",
               
               "sup {font-size: 15px;}",
               
               ".caseRed {color:            rgb(200,0,0);
                          font-weight:      bold;
                          background-color: rgb(255,228,196);}",
               
               ".ligneTot {font-weight:      bold;
                           background-color: rgb(239,228,176);}",
               
               "</style>",
               "</head>")


titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")
titre2 <- paste0("<p style='font-size:20px'>-- R&eacute;capitulatifs des temps d'arr&ecirc;t positionn&eacute;s --</p>")


# avec img encodees en base64
imageSchtroumpf <- paste0("<p><img class='titre' src='data:image/png;base64,",img64_Schtroumpfs_qui_dansent,"'
                               alt='Photo de Schtroumpf'/></p>")

imageDowntimeBoogieNights <- paste0("<p><img class='titre' src='data:image/png;base64,",img64_Downtime_BoogieNights,"'
                               alt='Photo de Downtime-BoogieNights'/></p>")

imageVillage <- paste0("<p><img class='titre' src='data:image/png;base64,",img64_Schtroumpf_village,"'
                               alt='Photo de Village'/></p>")


# imageSchtroumpf <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeTempsArret\\Schtroumpfs_qui_dansent_xs.png'
#                               alt='Photo de Schtroumpf'/></p>")
# 
# imageDowntimeBoogieNights <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeTempsArret\\Downtime_BoogieNights_xs.png'
#                              alt='Photo de Downtime-BoogieNights'/></p>")
# 
# imageVillage <- paste0("<p><img src='C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeTempsArret\\Schtroumpf_village_perdu_avecMessageCEI_compress.jpg'
#                            alt='Photo de Village'/></p>")

entete <- paste0("<thead>",
                    "<tr>",
                        "<td class='ligneTot'>Auteur</td>",
                        "<td class='ligneTot'>Commentaire</td>",
                        "<td class='ligneTot'>Hote</td>",
                        "<td class='ligneTot'>Service</td>",
                        "<td class='ligneTot'>Date de mise en place</td>",
                        "<td class='ligneTot'>P&eacute;riode</td>",
                        "<td class='ligneTot'>Annulation</td>",
                    "</tr>",
                 "</thead>")

pied <- paste0 ("<tfoot>",
                    "<tr>
                        <td class='piedtable' colspan=7>",imageVillage,"</td>
                    </tr>",
                "</tfoot>")

corps_tab <- ""


corps_tab <- paste0(corps_tab,"<tr><td colspan=7 class='caseRed'>LISTE DES TEMPS D'ARR&Ecirc;T FUTURS</td></tr>")
if (nrow(Gepex_FUTUR_html)>0){
  for (i in 1:nrow(Gepex_FUTUR_html)){
    for (j in 1:7){
      if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
      if ( (Gepex_FUTUR_html[i,8]>1) & j %in% c(1,2,5,6,7,8) ){
        corps_tab <- paste0(corps_tab,"<td rowspan=",Gepex_FUTUR_html[i,8],">",Gepex_FUTUR_html[i,j],"</td>")
      } else {
        if ( (Gepex_FUTUR_html[i,8]==0 | Gepex_FUTUR_html[i,8]>1) & j %in% c(3,4) ){
          corps_tab <- paste0(corps_tab,"<td>",Gepex_FUTUR_html[i,j],"</td>")
        } else {
          if (Gepex_FUTUR_html[i,8]==1){
            corps_tab <- paste0(corps_tab,"<td>",Gepex_FUTUR_html[i,j],"</td>")
          }
        }
      }
      if (j==ncol(Gepex_FUTUR_html)-1){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=7>absence de temps d'arr&ecirc;ts</td></tr>")
}



corps_tab <- paste0(corps_tab,"<tr><td colspan=7 class='caseRed'>LISTE DES TEMPS D'ARR&Ecirc;T EN COURS</td></tr>")
if (nrow(Gepex_ENCOURS_html)>0){
  for (i in 1:nrow(Gepex_ENCOURS_html)){
    for (j in 1:7){
      if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
      if ( (Gepex_ENCOURS_html[i,8]>1) & j %in% c(1,2,5,6,7,8) ){
        corps_tab <- paste0(corps_tab,"<td rowspan=",Gepex_ENCOURS_html[i,8],">",Gepex_ENCOURS_html[i,j],"</td>")
      } else {
        if ( (Gepex_ENCOURS_html[i,8]==0 | Gepex_ENCOURS_html[i,8]>1) & j %in% c(3,4) ){
          corps_tab <- paste0(corps_tab,"<td>",Gepex_ENCOURS_html[i,j],"</td>")
        } else {
          if (Gepex_ENCOURS_html[i,8]==1){
            corps_tab <- paste0(corps_tab,"<td>",Gepex_ENCOURS_html[i,j],"</td>")
          }
        }
      }
      if (j==ncol(Gepex_ENCOURS_html)-1){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=7>absence de temps d'arr&ecirc;ts</td></tr>")
}   



corps_tab <- paste0(corps_tab,"<tr><td colspan=7 class='caseRed'>LISTE DES TEMPS D'ARR&Ecirc;T TERMIN&Eacute;ES SUR LES 7 DERNIERS JOURS</td></tr>")
if (nrow(Gepex_PASSEE_html)>0){
  for (i in 1:nrow(Gepex_PASSEE_html)){
    for (j in 1:7){
      if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
      if ( (Gepex_PASSEE_html[i,8]>1) & j %in% c(1,2,5,6,7,8) ){
        corps_tab <- paste0(corps_tab,"<td rowspan=",Gepex_PASSEE_html[i,8],">",Gepex_PASSEE_html[i,j],"</td>")
      } else {
        if ( (Gepex_PASSEE_html[i,8]==0 | Gepex_PASSEE_html[i,8]>1) & j %in% c(3,4) ){
          corps_tab <- paste0(corps_tab,"<td>",Gepex_PASSEE_html[i,j],"</td>")
        } else {
          if (Gepex_PASSEE_html[i,8]==1){
            corps_tab <- paste0(corps_tab,"<td>",Gepex_PASSEE_html[i,j],"</td>")
          }
        }
      }
      if (j==ncol(Gepex_PASSEE_html)-1){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=7>absence de temps d'arr&ecirc;ts</td></tr>")
}   

# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                  head,
                  "<body>",
              
                  # titre1,titre2,imageGargamel,
              
                  "<table class='tabletitre'>",
                      "<tr>",
                        "<td class='tabletitre'>", # titre1,
                                                  titre2,imageSchtroumpf,"</td>",
                        "<td class='tabletitre'>",imageDowntimeBoogieNights,"</td>",
                      "</tr>",
                  "</table>",
              
                  "<table>",
                      entete,corps_tab, #pied,
                  "</table>",
              
                  "</body>",
              "</html>")

# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# SANS LES IMAGES
tab_ss_img <- paste0("<!DOCTYPE html>",
              "<html>",
                  head,
                  "<body>",
              
                  # titre1,titre2,imageGargamel,
              
                    "<table class='tabletitre'>",
                        "<tr>",
                          "<td class='tabletitre'>", #titre1,
                                                    titre2,"</td>",
                        "</tr>",
                    "</table>",
              
                    "<table>",
                        entete,corps_tab,
                    "</table>",
              
                  "</body>",
              "</html>")

######################################################################
######################################################################
######################################################################
######################################################################


# Creation dun repertoire avec la date du jour
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeTempsArret/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# Creation dun fichier HTML dans le repertoire nouvellement cree par injection du contenu de la variable tab
setwd(folder)

file <- "Liste_TempsArret.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="Liste_TempsArret.html",append=TRUE)

###

#second endroit generalise du fichier (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeTempsArret/General"
if (file.exists(fileGen)) {
    cat("The folder already exists")
} else {
    dir.create(fileGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

setwd(fileGen)
file.create("Liste_TempsArret.html", showWarnings = TRUE)
cat(tab,file="Liste_TempsArret.html",append=TRUE)

file.create("Liste_TempsArret_ss_img.html", showWarnings = TRUE)
cat(tab_ss_img,file="Liste_TempsArret_ss_img.html",append=TRUE)

# insertion dans un .zip
zip("TempsArret_Listing.zip",paste0("Liste_TempsArret.html"))
zip("TempsArret_Listing.zip",paste0("Liste_TempsArret_ss_img.html"))

######################################################################
######################################################################
######################################################################
######################################################################

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeTempsArret\\EnvoiMail_ViaPowershell.ps1"')

# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM

# library(mailR) ne fonctionne pas sans lib.loc
#library(mailR, lib.loc="C:/Users/SIAR_ycg8l6/ProgrammesR_Gepex/X_PackagesMail")

#locmdp <- "E:/X_PackagesMailR/"
#mdp_cpt <- read.table(paste0(locmdp,"ident_tls.txt")) %>% as.character()

#  Envoi du mel final en SMTP authentifie
# send.mail(from = "Centreon-SIP@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("Resume sur les temps d'arret dans Centreon au",format(Sys.time(),'%A %d %B %Y')),
#           body = paste0(folder,"/CorpsMel_Notifications.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 587, 
#                       user.name = "PD0-SUPERVISION-SVC@ad.insee.intra", 
#                       passwd = mdp_cpt ),
#           authenticate = TRUE,
#           send = TRUE
# )


#  Envoi du mel final
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("R?sum? sur les temps d'arr?t dans Centreon au",format(Sys.time(),'%A %d %B %Y')),
#           body = paste0(folder,"/CorpsMel_Notifications.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE
# )

# 10.143.1.51
#.libPaths()
