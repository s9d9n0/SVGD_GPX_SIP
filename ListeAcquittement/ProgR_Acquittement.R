
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

#date retardee de 8jrs
date_jour_8JRmoins <- date_jour - ddays(8) - dhours(decalage_heure)

####

##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
# Connexion a la nouvelle base 50
# maitre xx.xx.xx.xx ; esclave xx.xx.xx.xx
##
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture", password="Reporting",
                     dbname="centreon_storage", host="xx.xx.xx.xx", port="3306")

##################################################################

## Au niveau de la table ACKNOWLEGMENT ----
Req_01_ANCactif <- dbGetQuery(baseMdb, 
            paste("SELECT acknowledgement_id, instance_id, host_id, service_id, author, comment_data,
                          state, sticky, type,
                          FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                          FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date
                   FROM acknowledgements
                   WHERE entry_time <= UNIX_TIMESTAMP('",date_jour_8JRmoins,"') AND type=1
                   ORDER BY entry_time DESC"))

Req_02_LAST7jrs <- dbGetQuery(baseMdb, 
            paste("SELECT acknowledgement_id, instance_id, host_id, service_id, author, comment_data,
                          state, sticky, type,
                          FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                          FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date
                   FROM acknowledgements
                   WHERE entry_time <= UNIX_TIMESTAMP('",date_jour_1JRmoins,"') AND
                         entry_time > UNIX_TIMESTAMP('",date_jour_8JRmoins,"')
                   ORDER BY entry_time DESC"))

Req_03_LAST24h <- dbGetQuery(baseMdb, 
            paste("SELECT acknowledgement_id, instance_id, host_id, service_id, author, comment_data,
                          state, sticky, type,
                          FROM_UNIXTIME(entry_time,'%Y-%m-%d %H:%i:%S')    AS ch_entree_date,
                          FROM_UNIXTIME(deletion_time,'%Y-%m-%d %H:%i:%S') AS ch_efface_date
                   FROM acknowledgements
                   WHERE entry_time > UNIX_TIMESTAMP('",date_jour_1JRmoins,"')
                   ORDER BY entry_time DESC"))

##################################################################

## Au niveau de la table HOSTS et afin de recuperer notamment les noms ----
Instances <- dbGetQuery(baseMdb, paste("SELECT instance_id, name FROM instances", sep=""))
Instances <- unique(Instances)

##################################################################

## Au niveau de la table HOSTS et afin de recuperer notamment les noms ----
Hosts <- dbGetQuery(baseMdb, paste("SELECT host_id, alias FROM hosts", sep=""))
Hosts <- unique(Hosts)

##################################################################

## Au niveau de la table SERVICES et afin de recuperer notamment les noms ----
Services <- dbGetQuery(baseMdb, paste("SELECT service_id, description FROM services", sep=""))
Services <- unique(Services)

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

Vue <- Req_01_ANCactif %>% group_by(author) %>% 
    summarise(nb_eff=n()) %>% ungroup() %>% as.data.frame()
Vue <- Req_01_ANCactif %>% group_by(comment_data) %>% 
    summarise(nb_eff=n()) %>% ungroup() %>% as.data.frame()    
    
    
fct_1ers_traitements <- function(df){
  df_out <- df %>% 
    mutate(POSIXct_entree=ymd_hms(ch_entree_date),
           POSIXct_efface=ymd_hms(ch_efface_date)) %>% 
    mutate(POSIXct_entree=POSIXct_entree+dhours(decalage_heure),
           POSIXct_efface=POSIXct_efface+dhours(decalage_heure)) %>%
    select(-ch_entree_date,-ch_efface_date) %>%
    
    # filtre sur type=1 et sur les quelques idep du gepex
    arrange(acknowledgement_id) %>% filter(type==1) %>% mutate(nb_obs=1) %>% 
    filter(!(author %in% c("centreon-engine","clapi","dg57-cei-service-u","Downtime cycle"))) %>% 
    
    # jointures avec les tables Instances, Hosts et Services et simplification
    left_join(Instances, by=c("instance_id")) %>% relocate(name,.after=instance_id) %>% 
    left_join(Hosts, by=c("host_id")) %>% relocate(alias,.after=host_id) %>%
    left_join(Services, by=c("service_id")) %>% relocate(description,.after=service_id) %>%
    mutate(zone = case_when(
          name %in% c("Pollerdc200","Poller1dc200")  ~ "01_DMZ_dc2",
          name %in% c("Pollerdc100","Poller1dc100")  ~ "02_DMZ_dc1",
          name %in% c("Pollerdc225")                 ~ "03_Partenaire_dc2",
          name %in% c("Pollerdc125")                 ~ "04_Partenaire_dc1",
          name %in% c("Pollerdc250","Poller1dc250",
                      "Poller2dc250","Poller3dc250") ~ "05_ZoneInterne_dc2",
          name %in% c("Pollerdc150","Poller1dc150",
                      "Poller2dc150","RsOdc150")     ~ "06_ZoneInterne_dc1",
          TRUE ~ "07_Autre")) %>% 
    relocate(zone,.after=name) %>% select(-name) %>% 
    mutate(alias=str_replace(alias,".part.insee$|.insee.intra$|.ad.insee.intra$|.insee.fr$","")) %>% 
    rename(host=alias) %>% select(-instance_id) %>% 
  
    # recodage de state
    mutate(etat = case_when(state==1  ~ "warn.",
                            state==2  ~ "crit.",
                            state==3  ~ "inc.",
                            TRUE ~ "")) %>% 
    relocate(etat,.after=state) %>% select(-state) %>%
    
    # recodages successifs du commentaire
    mutate(commentaire=ifelse(str_sub(comment_data,1,13)=="Acquitt? par ",
                              str_sub(comment_data,14,str_length(comment_data)), comment_data)) %>%
    mutate(commentaire=ifelse(str_sub(commentaire,1,14)=="Acquitté par ",
                              str_sub(commentaire,15,str_length(commentaire)), commentaire)) %>%
    mutate(commentaire=ifelse(str_sub(commentaire,1,16)=="Acknowledged by ",
                              str_sub(commentaire,17,str_length(commentaire)), commentaire)) %>%
    mutate(commentaire=ifelse(str_sub(commentaire,1,str_length(author))==author,
                              str_sub(commentaire,str_length(author)+2,str_length(commentaire)), commentaire)) %>% 
    relocate(commentaire,.after=comment_data) %>% select(-comment_data)
  
  return(df_out)
}

fct_recode_auteur <- function(df,newvar,varref){
  df %>% mutate("{{newvar}}":=case_when(
            str_to_upper({{varref}}) %in% c("EY9CCT","SIAR_EY9CCT") ~ "EY9CCT - xxx",
            str_to_upper({{varref}}) %in% c("YCG8L6","SIAR_YCG8L6") ~ "YCG8L6 - xxx",
            str_to_upper({{varref}}) %in% c("OK8MZB","SIAR_OK8MZB","ADMIN") ~ "OK8MZB - xxx",
            str_to_upper({{varref}}) %in% c("DZIJQL","SIAR_DZIJQL") ~ "DZIJQL - xxx",
            str_to_upper({{varref}}) %in% c("TMH7F8","SIAR_TMH7F8") ~ "TMH7F8 - xxx",
            str_to_upper({{varref}}) %in% c("MKD0W7","SIAR_MKD0W7") ~ "MKD0W7 - xxx",
            str_to_upper({{varref}}) %in% c("H13YP3","SIAR_H13YP3") ~ "H13YP3 - xxx",
            TRUE ~ str_to_upper({{varref}})
  )) %>% relocate({{newvar}},.after={{varref}}) %>% select(-{{varref}})
}

fct_recode_date_ordonne <- function(df,newvar,vardate){
  df %>% mutate("{{newvar}}":=paste0(year({{vardate}})," ",
                                     ifelse(str_length(month({{vardate}}))==1,paste0("0",month({{vardate}})),month({{vardate}}))," ",
                                     ifelse(str_length(day({{vardate}}))==1,  paste0("0",day({{vardate}})),  day({{vardate}}))," - ",
                                     ifelse(str_length(hour({{vardate}}))==1,  paste0("0",hour({{vardate}})),  hour({{vardate}})),"h",
                                     ifelse(str_length(minute({{vardate}}))==1,paste0("0",minute({{vardate}})),minute({{vardate}}))
  ))
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

fct_recodif <- function(df){
  # RECODIFICATION avec utilisation des fonctions precedemment definies :
  #               fct_recode_auteur,fct_recode_date,fct_recode_moisdate
  
  # recodification des auteurs
  df_out <- fct_recode_auteur(df,auteur,author)
  
  # recodification et reformatage des differentes variables temporelles
  df_out <- fct_recode_date_ordonne(df_out,date_ordre,POSIXct_entree)
  
  df_out <- fct_recode_date(df_out,mise_en_place,POSIXct_entree)
  df_out <- fct_recode_date(df_out,interruption,POSIXct_efface)
  df_out <- df_out %>% select(-POSIXct_entree,-POSIXct_efface) %>% 
    mutate(interruption=ifelse(str_sub(interruption,1,2)=="NA","",interruption))
  
  # recodification du mois des differentes variables temporelles
  df_out <- fct_recode_moisdate(df_out,mois_mise_en_place,mise_en_place)
  df_out <- fct_recode_moisdate(df_out,mois_interruption,interruption)
  
  # recodification de la variable indiquant le moment de mise en place du temps arret
  df_out <- df_out %>% 
    mutate(mise_en_place=paste0(str_sub(mise_en_place,1,2)," ",mois_mise_en_place," ",str_sub(mise_en_place,7,18))) %>% 
    select(-mois_mise_en_place) %>% 
    mutate(interruption=paste0(str_sub(interruption,1,2)," ",mois_interruption," ",str_sub(interruption,7,18))) %>% 
    select(-mois_interruption)
  
  return(df_out)
}

fct_agreg <- function(df){
  df_Acquit_Host <- df %>% filter(is.na(description)) %>% 
    mutate(acknowledgement_id=NA,service_id=NA) %>%
    group_by_all() %>% summarize(nb_obs=sum(nb_obs)) %>% ungroup() %>% as.data.frame()
  
  df_Acquit_Serv <- df %>% filter(!is.na(description))
  
  df_out <- rbind(df_Acquit_Host,df_Acquit_Serv) %>% 
    mutate(description=ifelse(nb_obs!=1,paste0(" >> Acquittement de ",nb_obs," services"),description)) %>% 
    select(-nb_obs) %>% arrange(desc(date_ordre),host,description)
  
  rm(df_Acquit_Host,df_Acquit_Serv)
  
  return(df_out)
}

fct_agreg_hote <- function(df){
  df_mult <- df %>% group_by(zone,host,auteur,commentaire,etat,sticky,type,
                             date_ordre,mise_en_place,interruption) %>%
    summarize(nb_obs=n()) %>% ungroup() %>% as.data.frame() %>% filter(nb_obs!=1) %>% 
    mutate(description=ifelse(nb_obs!=1,paste0(" >> Acquittement de ",nb_obs," services"),"")) %>% 
    select(-nb_obs) %>% relocate(description,.after=host) %>% 
    arrange(desc(date_ordre),host,description)
    
  df_uniq <- df %>% group_by(zone,host,auteur,commentaire,etat,sticky,type,
                             date_ordre,mise_en_place,interruption) %>%
    summarize(nb_obs=n()) %>% ungroup() %>% as.data.frame() %>% filter(nb_obs==1) %>% 
    select(-nb_obs)
  
  df_uniq <- df %>% select(-acknowledgement_id, -host_id, -service_id) %>% 
    inner_join(df_uniq,by=c("zone","host","auteur","commentaire","etat","sticky","type",
                            "date_ordre","mise_en_place","interruption")) 
    
  df_out <- rbind(df_mult,df_uniq) %>% arrange(desc(date_ordre),host,description)
  
  return(df_out)
}

######################################################################
######################################################################
######################################################################
######################################################################

######################################
# DEBUT TRAITEMENT DES ACQUITTEMENTS #
######################################

Acq_ANC <- fct_1ers_traitements(Req_01_ANCactif)
Acq_ANC <- fct_recodif(Acq_ANC)
Acq_ANC <- fct_agreg(Acq_ANC)
Acq_ANC_Hote <- fct_agreg_hote(Acq_ANC)

Acq_7jrs <- fct_1ers_traitements(Req_02_LAST7jrs)
Acq_7jrs <- fct_recodif(Acq_7jrs)
Acq_7jrs <- fct_agreg(Acq_7jrs)
Acq_7jrs_Hote <- fct_agreg_hote(Acq_7jrs)

Acq_24h <- fct_1ers_traitements(Req_03_LAST24h)
Acq_24h <- fct_recodif(Acq_24h)
Acq_24h <- fct_agreg(Acq_24h)
Acq_24h_Hote <- fct_agreg_hote(Acq_24h)


# fonctions de preparation pour affichage en HTML
Creat_DF_HTML_partie1 <-function(df){
  df %>% mutate(numligne=row_number()) %>%
    
    # modifications sur variable mise_en_place
    mutate(place_h_mise_en_place=str_locate(mise_en_place,"h")[numligne,1]) %>% 
    mutate(mise_en_place=paste0(str_sub(mise_en_place,1,place_h_mise_en_place-1),
                                "<sup><i>h",
                                str_sub(mise_en_place,place_h_mise_en_place+1,str_length(mise_en_place)),
                                "</i></sup>")) %>%
    select(-place_h_mise_en_place) %>%
    
    # modifications sur variable interruption
    mutate(place_h_interruption=str_locate(interruption,"h")[numligne,1]) %>% 
    mutate(interruption=ifelse(!is.na(place_h_interruption),
                               paste0(str_sub(interruption,1,place_h_interruption-1),
                                      "<sup><i>h",
                                      str_sub(interruption,place_h_interruption+1,str_length(interruption)),
                                      "</i></sup>"),
                               "")) %>%
    select(-place_h_interruption)
}

Creat_DF_HTML_partie2 <-function(df){
  df_out <- df %>% mutate(numligne=row_number()) %>% 
    mutate(compar_lag =ifelse(auteur==lag(auteur) & commentaire==lag(commentaire) & 
                              mise_en_place==lag(mise_en_place) & interruption==lag(interruption),1,0)) %>% 
    mutate(compar_lead=ifelse(auteur==lead(auteur) & commentaire==lead(commentaire) &
                              mise_en_place==lead(mise_en_place) & interruption==lead(interruption),1,0)) %>% 
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

AcqHTML_24h_Hote <- Creat_DF_HTML_partie1(Acq_24h_Hote)
AcqHTML_24h_Hote <- Creat_DF_HTML_partie2(AcqHTML_24h_Hote)

AcqHTML_7jrs_Hote <- Creat_DF_HTML_partie1(Acq_7jrs_Hote)
AcqHTML_7jrs_Hote <- Creat_DF_HTML_partie2(AcqHTML_7jrs_Hote)


# encodage des 3 images du mel
# library(base64enc)
# doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeAcquittement/"
# 
# Schtroumpf_avec_livre_xs_encode <- base64encode(paste0(doss,"Schtroumpf_avec_livre_xs.png"))
# write.csv2(Schtroumpf_avec_livre_xs_encode,
#              paste0(doss, "Img_encode64_Schtroumpf_avec_livre_xs.csv"),
#              row.names = FALSE)
# 
# Schtroumpf_informaticien_fond_blanc_xs_encode <- base64encode(paste0(doss,"Schtroumpf_informaticien_fond_blanc_xs.png"))
# write.csv2(Schtroumpf_informaticien_fond_blanc_xs_encode,
#            paste0(doss, "Img_encode64_Schtroumpf_informaticien_fond_blanc_xs.csv"),
#            row.names = FALSE)
# 
# Schtroumpf_village_perdu_avecMessageCEI_compress_encode <- base64encode(paste0(doss,"Schtroumpf_village_perdu_avecMessageCEI_compress.JPG"))
# write.csv2(Schtroumpf_village_perdu_avecMessageCEI_compress_encode,
#            paste0(doss, "Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"),
#            row.names = FALSE)

##

# recuperation des images encodees dans des variables
doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeAcquittement/"

img64_Schtroumpf_avec_livre <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_avec_livre_xs.csv"))
img64_Schtroumpf_avec_livre <- img64_Schtroumpf_avec_livre$x[1]

img64_Schtroumpf_info <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_informaticien_fond_blanc_xs.csv"))
img64_Schtroumpf_info <- img64_Schtroumpf_info$x[1]

img64_Schtroumpf_village_perdu <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"))
img64_Schtroumpf_village_perdu <- img64_Schtroumpf_village_perdu$x[1]


######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

head <- paste0("<head>",
                  "<meta charset='utf-8'/>",
                  "<title>ENVOI INFOS ACQUITTEMENTS</title>",
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

titre2 <- paste0("<p style='font-size:20px'>-- R&eacute;capitulatifs des acquittements positionn&eacute;s (hors SIA) --</p>")

cheminImage <- "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeAcquittement\\"


# avec img encodees en base64
imageSchtroumpf <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf_avec_livre,"
                               alt='Photo de Schtroumpf'/></p>")

imageProbInfo <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf_info,"
                             alt='Photo de probleme informatique'/></p>")

imageVillage <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf_village_perdu,"
                            alt='Photo de Village'/></p>")

# imageSchtroumpf <- paste0("<p><img src='",cheminImage,"Schtroumpf_avec_livre_xs.png'
#                                alt='Photo de Schtroumpf'/></p>")
#
# imageProbInfo <- paste0("<p><img src='",cheminImage,"Schtroumpf_informaticien_fond_blanc_xs.png'
#                              alt='Photo de probleme informatique'/></p>")
#
# imageVillage <- paste0("<p><img src='",cheminImage,"Schtroumpf_village_perdu_avecMessageCEI_compress.jpg'
#                            alt='Photo de Village'/></p>")


entete <- paste0("<thead>",
                    "<tr>",
                        "<td class='ligneTot'>Auteur</td>",
                        "<td class='ligneTot'>Commentaire</td>",
                        "<td class='ligneTot'>Hote</td>",
                        "<td class='ligneTot'>Service</td>",
                        "<td class='ligneTot'>Etat</td>",
                        "<td class='ligneTot'>Date de mise en place</td>",
                        "<td class='ligneTot'>Annulation</td>",
                    "</tr>",
                 "</thead>")

pied <- paste0 ("<tfoot>",
                    "<tr>
                        <td class='piedtable' colspan=7>",imageVillage,"</td>
                    </tr>",
                "</tfoot>")

corps_tab <- ""

DFref <- AcqHTML_24h_Hote %>% select(auteur,commentaire,host,description,etat,mise_en_place,interruption,nb_rowspan)

corps_tab <- paste0(corps_tab,"<tr><td colspan=7 class='caseRed'>LISTE DES ACQUITTEMENTS DES DERN. 24H</td></tr>")
if (nrow(DFref)>0){
  for (i in 1:nrow(DFref)){
    for (j in 1:7){
      if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
      if ( (DFref[i,8]>1) & j %in% c(1,2,6,7) ){
        corps_tab <- paste0(corps_tab,"<td rowspan=",DFref[i,8],">",DFref[i,j],"</td>")
      } else {
        if ( (DFref[i,8]==0 | DFref[i,8]>1) & j %in% c(3,4,5) ){
          corps_tab <- paste0(corps_tab,"<td>",DFref[i,j],"</td>")
        } else {
          if (DFref[i,8]==1 ){
            corps_tab <- paste0(corps_tab,"<td>",DFref[i,j],"</td>")
          }
        }
      }
      if (j==ncol(DFref)-1){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=7>absence d'acquittements</td></tr>")
}


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                  head,
                  "<body>",
              
                      # titre1,titre2,imageGargamel,
              
                      "<table class='tabletitre'>",
                          "<tr>",
                              "<td class='tabletitre'>", #titre1,
                                                        titre2,imageSchtroumpf,"</td>",
                              # "<td class='tabletitre'>",imageProbInfo,"</td>",
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
              
                        # titre1,titre2
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
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeAcquittement/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# Creation dun fichier HTML dans le repertoire nouvellement cree par injection du contenu de la variable tab
setwd(folder)

file <- "Liste_Acquittements.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="Liste_Acquittements.html",append=TRUE)

###

#second endroit generalise du fichier (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_ListeAcquittement/General"
if (file.exists(fileGen)) {
    cat("The folder already exists")
} else {
    dir.create(fileGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}
setwd(fileGen)
file.create("Liste_Acquittements.html", showWarnings = TRUE)
cat(tab,file="Liste_Acquittements.html",append=TRUE)

file.create("CorpsMel_Acquittements.html", showWarnings = TRUE)
cat(tab_ss_img,file="CorpsMel_Acquittements.html",append=TRUE)

# insertion dans un .zip
zip("Acquittements_Listing.zip",paste0("Liste_Acquittements.html"))
zip("Acquittements_Listing.zip",paste0("CorpsMel_Acquittements.html"))

######################################################################
######################################################################
######################################################################
######################################################################

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_ListeAcquittement\\EnvoiMail_ViaPowershell.ps1"')

# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM


