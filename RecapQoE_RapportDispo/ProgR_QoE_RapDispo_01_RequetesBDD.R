
# a changer chaque mois
mois_ref <- "2024-05"


## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(readr)
library(RMariaDB)
library(lubridate)

date_jour <- Sys.time()
nom_an <- as.character(year(date_jour))
nom_mois <- as.character(month(date_jour))
nom_mois <- ifelse(str_length(nom_mois)==1,paste0("0",nom_mois),nom_mois)
nom_jour <- as.character(day(date_jour))
nom_jour <- ifelse(str_length(nom_jour)==1,paste0("0",nom_jour),nom_jour)
nom_heure <- as.character(hour(date_jour))
nom_heure <- ifelse(str_length(nom_heure)==1,paste0("0",nom_heure),nom_heure)
nom_date <- paste0("_",nom_an,nom_mois,nom_jour,"_",nom_heure,"h")

rm(date_jour,nom_an,nom_mois,nom_jour,nom_heure)


##########################################################

next_mois <- case_when(str_sub(mois_ref,6,7)=="01" ~ "02", str_sub(mois_ref,6,7)=="02" ~ "03",
                       str_sub(mois_ref,6,7)=="03" ~ "04", str_sub(mois_ref,6,7)=="04" ~ "05",
                       str_sub(mois_ref,6,7)=="05" ~ "06", str_sub(mois_ref,6,7)=="06" ~ "07",
                       str_sub(mois_ref,6,7)=="07" ~ "08", str_sub(mois_ref,6,7)=="08" ~ "09",
                       str_sub(mois_ref,6,7)=="09" ~ "10", str_sub(mois_ref,6,7)=="10" ~ "11",
                       str_sub(mois_ref,6,7)=="11" ~ "12", str_sub(mois_ref,6,7)=="12" ~ "01")
next_annee <- if (next_mois=="01"){as.character(as.integer(str_sub(mois_ref,1,4))+1)
} else {str_sub(mois_ref,1,4)}

prev_mois <- case_when(str_sub(mois_ref,6,7)=="01" ~ "12", str_sub(mois_ref,6,7)=="02" ~ "01",
                       str_sub(mois_ref,6,7)=="03" ~ "02", str_sub(mois_ref,6,7)=="04" ~ "03",
                       str_sub(mois_ref,6,7)=="05" ~ "04", str_sub(mois_ref,6,7)=="06" ~ "05",
                       str_sub(mois_ref,6,7)=="07" ~ "06", str_sub(mois_ref,6,7)=="08" ~ "07",
                       str_sub(mois_ref,6,7)=="09" ~ "08", str_sub(mois_ref,6,7)=="10" ~ "09",
                       str_sub(mois_ref,6,7)=="11" ~ "10", str_sub(mois_ref,6,7)=="12" ~ "11")
prev_annee <- if (prev_mois=="12"){as.character(as.integer(str_sub(mois_ref,1,4))-1)
} else {str_sub(mois_ref,1,4)}

mois <- as.integer(str_sub(mois_ref,6,7))
date_deb <- paste0(prev_annee,"-",prev_mois,"-27")
date_fin <- paste0(next_annee,"-",next_mois,"-01")

rm(prev_annee,prev_mois,next_annee,next_mois)


# Nom du fichier .ods en entree a renseigner fournissant la liste des hotes contenant les QoE et leurs donnees
fichier_liste_machine <- "Liste_Machine"

dataHost <- read_csv2(paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo/",fichier_liste_machine,".csv")) %>% 
            arrange(nom_machine)

listHost <- paste(paste0("'",dataHost$nom_machine,"'"),collapse=",")
listHost

####


##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la nouvelle base 50
## maitre xx.xx.xx.xx ; esclave xx.xx.xx.xx
##
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture", password="Reporting", 
                     dbname="centreon_storage", host="xx.xx.xx.xx", port="3306")

##################################################################

## REQUETE niveau HOTE ##
Req_Host <- dbGetQuery(baseMdb, paste("SELECT host_id, name AS host_name, alias, address, enabled,
                                              last_check, last_hard_state, last_hard_state_change,
                                              last_time_up, last_time_down,
                                              last_update
                                       FROM hosts
                                       WHERE alias IN (",listHost,") AND enabled=1
                                      "))
# 
## REQUETE niveau SERVICE ##

# recup prealable des identifiants des hotes cibles
listHostId <- paste(paste0("'",Req_Host$host_id,"'"),collapse=",")

# selection des services QoE faisant partie des hotes cibles
Req_Serv <- dbGetQuery(baseMdb, paste("SELECT host_id, service_id, description AS service_description, enabled,
                                              check_interval, check_period,
                                              last_check, last_hard_state, last_hard_state_change,
                                              last_time_ok, last_time_warning, last_time_critical, last_time_unknown,
                                              last_update
                                       FROM services
                                       WHERE host_id IN (",listHostId,") AND 
                                             description LIKE ('%QoE%') AND enabled=1 
                                      "))
# 
# Req_Serv <- Req_Serv %>% unique(service_id)

# 

## REQUETE niveau HOTE_ID X SERVICE_ID ##

# recup prealable des identifiants des services cibles
listServiceId <- paste(paste0("'",Req_Serv$service_id,"'"),collapse=",")

# selection des id = couple host_id et service_id
Req_Index <- dbGetQuery(baseMdb, paste("SELECT id, host_id, host_name, service_id, service_description
                                        FROM index_data
                                       WHERE host_id IN (",listHostId,") AND 
                                             service_id IN (",listServiceId,") AND
                                             service_description != 'QoE-BRPP-France_Connect_Meta_1min'
                                      "))


# Req_Index <- dbGetQuery(baseMdb, paste("SELECT id, host_id, host_name, service_id, service_description
#                                         FROM index_data
#                                         WHERE id=5109956
#                                       "))

## REQUETE niveau METRIQUE ##

# recup prealable des identifiants des metriques ciblees
listId <- paste(paste0("'",Req_Index$id,"'"),collapse=",")

# selection des metriques et mesures des id
Req_Metr <- dbGetQuery(baseMdb, paste("SELECT metric_id AS val_id, index_id AS id, metric_name, unit_name,
                                              current_value, warn, crit
                                       FROM metrics
                                       WHERE index_id IN (",listId,")
                                      "))

## REQUETE niveau VALEUR ##

# recup prealable des identifiants des metriques ciblees
listValId <- paste(paste0("'",Req_Metr$val_id,"'"),collapse=",")

# selection des metriques et mesures des id
Req_Val <- dbGetQuery(baseMdb, paste("SELECT id_metric AS val_id, ctime, value
                                       FROM data_bin
                                       WHERE id_metric IN (",listValId,") AND
                                             ctime >= UNIX_TIMESTAMP('",date_deb,"') AND
                                             ctime <= UNIX_TIMESTAMP('",date_fin,"')
                                      "))


### DEBUT Requete particuliere pour les services Meta de BRPP
Req_Meta_BRPP <- dbGetQuery(baseMdb,
                            paste("SELECT v.id_metric AS v_val_id, v.ctime, v.value,
                               m.metric_id AS m_val_id, m.index_id AS m_id, m.metric_name,
                               m.unit_name, m.warn, m.crit,
                               i.id AS i_id, i.host_id, i.host_name, i.service_id, i.service_description,
                               s.display_name
                        FROM data_bin v
                             inner join metrics m on v.id_metric = m.metric_id
                             inner join index_data i on m.index_id = i.id
                             inner join services s on s.service_id = i.service_id
                        # WHERE ( s.display_name LIKE ('%QoE-BRPP_Meta_Dispo%') OR
                        #         s.display_name LIKE ('%QoE-BRPP_Meta_Tps%') OR
                        #         s.display_name LIKE ('%QoE-BRPP_Meta_Steps%') ) AND
                        WHERE ( s.display_name LIKE ('%QoE-BRPP_Availability_VirtualCurve%') OR
                                s.display_name LIKE ('%QoE-BRPP_Meta_Tps%') OR
                                s.display_name LIKE ('%QoE-BRPP_Meta_Steps%') ) AND
                              ctime >= UNIX_TIMESTAMP('",date_deb,"') AND
                              ctime <= UNIX_TIMESTAMP('",date_fin,"')
                       "))

### FIN Requete particuliere pour les services Meta de BRPP


###

# Req_essai <- dbGetQuery(baseMdb,
#                 paste("SELECT v.id_metric AS v_val_id, v.ctime, v.value,
#                               m.metric_id AS m_val_id, m.index_id AS m_id, m.metric_name,
#                               m.unit_name, m.warn, m.crit,
#                               i.id AS i_id, i.host_id, i.host_name, i.service_id, i.service_description
#                        FROM data_bin v
#                             inner join metrics m on v.id_metric = m.metric_id
#                             inner join index_data i on m.index_id = i.id
#                        WHERE i.service_description LIKE ('%QoE%') AND
#                              ctime >= UNIX_TIMESTAMP('",date_deb,"') AND
#                              ctime <= UNIX_TIMESTAMP('",date_fin,"')
#                       "))


##################################################################

##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################

#suppression des variables et tables intermediaires
rm(fichier_liste_machine,dataHost,baseMdb,
   listHost,listHostId,listServiceId,listId,listValId)


##################################################################
##################################################################
##################################################################
##################################################################


##################
##################
# PARTIE QoE
##################
##################

Req_Host <- Req_Host %>% mutate(last_check = as.POSIXct(last_check,origin="1970-01-01"),
                                last_hard_state_change = as.POSIXct(last_hard_state_change,origin="1970-01-01"),
                                last_time_up = as.POSIXct(last_time_up,origin="1970-01-01"),
                                last_time_down = as.POSIXct(last_time_down,origin="1970-01-01"),
                                last_update = as.POSIXct(last_update,origin="1970-01-01"))


Req_Val <- Req_Val %>% mutate(moment = as.POSIXct(ctime,origin="1970-01-01")) %>%
  relocate(moment, .after="ctime") %>% 
  arrange(val_id,ctime) %>% filter(month(moment)==mois) %>% 
  mutate(value=round(value,2))

# rejointures entre les data.frame

Result <- Req_Metr %>% inner_join(Req_Val, by=c("val_id")) %>% select(-ctime,-current_value) %>% 
  inner_join(Req_Index, by=c("id")) %>% select(-host_id,-host_name,-service_description) %>% 
  inner_join(Req_Serv, by=c("service_id")) %>% select(-enabled,-last_check:-last_update) %>%
  inner_join(Req_Host, by=c("host_id")) %>% select(-address,-enabled,-last_check:-last_update) %>% 
  select(host_id,service_id,id,val_id,
         host_name,alias,service_description,check_period,check_interval,
         metric_name,unit_name,warn,crit,moment,value) %>% 
  mutate(warn=round(warn,2), crit=round(crit,2))

Result_2 <- Result %>% 
  mutate(metric=case_when(metric_name=="availability" ~ "disponibilite",
                          metric_name=="steps" ~ "etape",
                          metric_name=="time" ~ "temps",
                          TRUE ~ "")) %>% relocate(metric,.before="metric_name") %>% 
  select(-metric_name,-unit_name)

metric_dispo <- Result_2 %>% select(host_id,service_id,id,moment,metric,value) %>%
  filter(metric=="disponibilite") %>% rename(disponibilite=value) %>% select(-metric)  
metric_etape <- Result_2 %>% select(host_id,service_id,id,moment,metric,value) %>%
  filter(metric=="etape") %>% rename(etape=value) %>% select(-metric)
metric_temps <- Result_2 %>% select(host_id,service_id,id,moment,metric,warn,crit,value) %>%
  filter(metric=="temps") %>% rename(temps=value) %>% select(-metric)                                            

Result_2_bis <- metric_temps %>% 
  full_join(metric_etape,by=c("host_id","service_id","id","moment")) %>%
  full_join(metric_dispo,by=c("host_id","service_id","id","moment")) %>% 
  distinct()

rm(metric_temps,metric_etape,metric_dispo)

Result_2_ter <- Result_2 %>% select(host_id,service_id,id,
                                    host_name,alias,service_description,
                                    check_period,check_interval,moment) %>% distinct()

Result <- Result_2_ter %>% right_join(Result_2_bis,by=c("host_id","service_id","id","moment"))

rm(Result_2,Result_2_bis,Result_2_ter)

Result_agreg <- Result %>% 
  mutate(annee=year(moment), mois=month(moment), 
         libjour=wday(moment),jour=day(moment), 
         heure=hour(moment), min=minute(moment)) %>%
  mutate(mois_bis=ifelse(mois<10,paste0("0",mois),mois),
         jour_bis=ifelse(jour<10,paste0("0",jour),jour),
         heure_bis=ifelse(heure<10,paste0("0",heure),heure),
         min_bis=case_when(min>=0 & min<30 ~ "00",
                           min>=30 & min<=59 ~ "30",
                           TRUE ~ "")) %>% 
  group_by(host_id,service_id,id,
           host_name,alias,service_description,
           check_period,check_interval,warn,crit,
           annee,mois_bis,libjour,jour_bis,heure_bis,min_bis) %>%
  summarize(temps=round(mean(temps),1),
            etape=round(mean(etape),0),
            disponibilite=round(mean(disponibilite),0)) %>% 
  ungroup() %>% as.data.frame() %>%
  mutate(lib_jour=case_when(libjour==1 ~ "dim.", libjour==2 ~ "lun.",
                            libjour==3 ~ "mar.", libjour==4 ~ "mer.",
                            libjour==5 ~ "jeu.", libjour==6 ~ "ven.",
                            libjour==7 ~ "sam.", TRUE ~ "")) %>% 
  mutate(date=paste0(lib_jour," ",jour_bis," ",heure_bis,":",min_bis),
         heure=paste0(heure_bis,":",min_bis)) %>% 
  relocate(date,heure,.after="min_bis") %>% 
  relocate(lib_jour,.before="libjour") %>% select(-libjour) %>% 
  arrange(alias,service_description,annee,mois_bis,jour_bis,heure_bis,min_bis)






######################################################################
######################################################################
######################################################################
######################################################################

# PARTIE TRAITEMENT ET AJOUT DE LA QoE-BRPP_Meta
Req_Meta_BRPP_2 <- Req_Meta_BRPP %>%
  select(-service_description,-v_val_id,-m_val_id,-metric_name) %>% rename(service_description=display_name) %>%
  mutate(moment = as.POSIXct(ctime,origin="1970-01-01")) %>%
  relocate(moment, .after="ctime") %>% 
  arrange(ctime) %>% filter(month(moment)==mois) %>% 
  mutate(value=round(value,2))

metric_dispo <- Req_Meta_BRPP_2 %>% select(host_id,moment,service_description,value) %>%
  filter(service_description=="QoE-BRPP_Availability_VirtualCurve") %>%
  rename(disponibilite=value) %>% mutate(service_description="QoE-BRPP_Meta")  
# metric_etape <- Req_Meta_BRPP_2 %>% select(host_id,moment,service_description,value) %>%
#      filter(service_description=="QoE-BRPP_Meta_Steps") %>%
#      rename(etape=value) %>% mutate(service_description="QoE-BRPP_Meta") 
# metric_temps <- Req_Meta_BRPP_2 %>% select(host_id,moment,service_description,value) %>%
#      filter(service_description=="QoE-BRPP_Meta_Tps") %>%
#      rename(temps=value) %>% mutate(service_description="QoE-BRPP_Meta")    

fct_agreg_moment<- function(df,var){
  df_out <- df %>%
    mutate(annee=year(moment), mois=month(moment), 
           libjour=wday(moment),jour=day(moment), 
           heure=hour(moment), min=minute(moment)) %>%
    mutate(mois_bis=ifelse(mois<10,paste0("0",mois),as.character(mois)),
           jour_bis=ifelse(jour<10,paste0("0",jour),as.character(jour)),
           heure_bis=ifelse(heure<10,paste0("0",heure),as.character(heure)),
           min_bis=case_when(min>=0 & min<30 ~ "00",
                             min>=30 & min<=59 ~ "30",
                             TRUE ~ "")) %>% 
    group_by(host_id,service_description,
             annee,mois_bis,libjour,jour_bis,heure_bis,min_bis) %>%
    summarize({{var}}:=round(mean({{var}}),1)) %>% 
    ungroup() %>% as.data.frame() %>%
    mutate(lib_jour=case_when(libjour==1 ~ "dim.", libjour==2 ~ "lun.",
                              libjour==3 ~ "mar.", libjour==4 ~ "mer.",
                              libjour==5 ~ "jeu.", libjour==6 ~ "ven.",
                              libjour==7 ~ "sam.", TRUE ~ "")) %>% 
    mutate(date=paste0(lib_jour," ",jour_bis," ",heure_bis,":",min_bis),
           heure=paste0(heure_bis,":",min_bis)) %>% 
    relocate(date,heure,.after="min_bis") %>% 
    relocate(lib_jour,.before="libjour") %>% select(-libjour) %>% 
    arrange(host_id,service_description,annee,mois_bis,jour_bis,heure_bis,min_bis)
  return(df_out)
}

metric_dispo_2 <- fct_agreg_moment(metric_dispo,disponibilite)
# metric_etape_2 <- fct_agreg_moment(metric_etape,etape)
# metric_temps_2 <- fct_agreg_moment(metric_temps,temps)

Req_Meta_BRPP_3 <- metric_dispo_2 %>% 
  distinct() %>% arrange(annee,mois_bis,jour_bis,heure_bis,min_bis)

# Req_Meta_BRPP_3 <- metric_temps_2 %>% 
#      full_join(metric_etape_2,by=c("host_id","service_description",
#                                    "annee","mois_bis","lib_jour","jour_bis",
#                                    "heure_bis","min_bis","date","heure")) %>%
#      full_join(metric_dispo_2,by=c("host_id","service_description",
#                                    "annee","mois_bis","lib_jour","jour_bis",
#                                    "heure_bis","min_bis","date","heure")) %>% 
#      distinct() %>% arrange(annee,mois_bis,jour_bis,heure_bis,min_bis)

# rm(metric_temps_2,metric_etape_2,metric_dispo_2)

Req_Meta_BRPP_agreg <- Req_Meta_BRPP_3 %>%
  mutate(service_id="", id="", host_name="", alias="", 
         check_period="", check_interval="", warn="", crit="") %>% 
  relocate(service_id,.after="host_id") %>% relocate(id,.after="service_id") %>% 
  relocate(host_name,.after="id") %>% relocate(alias,.after="host_name") %>% 
  relocate(check_period,.after="service_description") %>% relocate(check_interval,.after="check_period") %>% 
  relocate(warn,.after="check_interval") %>% relocate(crit,.after="warn")

Req_Meta_BRPP_agreg <- Req_Meta_BRPP_agreg %>%
  mutate(temps=0 , etape=0) %>% 
  relocate(temps,.before="disponibilite") %>% 
  relocate(etape,.before="disponibilite")     


metric_dispo <- metric_dispo %>%
  mutate(service_id="", id="", host_name="", alias="", 
         check_period="", check_interval="", warn="", crit="", temps="", etape="") %>%
  relocate(moment,.after="service_description") %>% 
  relocate(service_id,.after="host_id") %>% relocate(id,.after="service_id") %>% 
  relocate(host_name,.after="id") %>% relocate(alias,.after="host_name") %>% 
  relocate(check_period,.after="service_description") %>% relocate(check_interval,.after="check_period") %>% 
  relocate(warn,.after="moment") %>% relocate(crit,.after="warn") %>% 
  relocate(temps,.after="crit") %>% relocate(etape,.after="temps")   

######################################################################
######################################################################
######################################################################
######################################################################

# Concatenation des QoE normales avec la QoE Meta de BRPP
Result_agreg <- rbind(Result_agreg,Req_Meta_BRPP_agreg)
Result <- rbind(Result,metric_dispo)

# sauvegarde dans des fichiers Result.csv et Result_agreg.csv

repfichier <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_RecapQoE_RapportDispo/"

write.csv2(Result,paste0(repfichier,"Result",nom_date,".csv"),row.names = FALSE)
write.csv2(Result_agreg,paste0(repfichier,"Result_agreg",nom_date,".csv"),row.names = FALSE)

#suppression des tables intermediaires
rm(Req_Host,Req_Serv,Req_Index,Req_Metr,Req_Val)

rm(Result,Result_agreg)


##################
##################
# PARTIE hors QoE
##################
##################

# Req_horsQoE <- rbind(Req_horsQoE_1,Req_horsQoE_2,Req_horsQoE_3,Req_horsQoE_4) %>% 
#     select(-m_id,-m_val_id) %>% rename(val_id=v_val_id,id=i_id)
# 
# Req_horsQoE <- Req_horsQoE %>% mutate(moment = as.POSIXct(ctime,origin="1970-01-01")) %>%
#     relocate(moment, .after="ctime") %>% 
#     arrange(val_id,ctime) %>% filter(month(moment)==mois) %>% 
#     select(host_id,service_id,id,val_id,host_name,service_description,
#            metric_name,unit_name,warn,crit,moment,value) %>% 
#     mutate(warn=round(warn,1), crit=round(crit,1))

# creation de la variable disponibilite en fonction du temps mesure

# Req_horsQoE <- Req_horsQoE %>% rename(temps=value) %>% mutate(etape=1) %>% 
#     mutate(disponibilite=ifelse(service_description %in% c("SPOC_Mail_VIP_DMZ","SPOC_Mail_VIP_PROD") & temps>crit,
#                                 0,100)) %>% 
#     mutate(disponibilite=ifelse(service_description == "TEST_IGESA_GroupeApplicatif" & temps>30,
#                                 0,100)) %>% 
#     mutate(disponibilite=ifelse(service_description == "TEST_API_Gestion_Contacts" & temps!=1,
#                                 0,100)) %>% 
#     mutate(disponibilite=ifelse(str_sub(service_description,1,10) == "LDAP_Login" & temps>crit,
#                                 0,100))    
# 
# Req_horsQoE_agreg <- Req_horsQoE %>% 
#     mutate(annee=year(moment), mois=month(moment), 
#            libjour=wday(moment),jour=day(moment), 
#            heure=hour(moment), min=minute(moment)) %>%
#     mutate(mois_bis=ifelse(mois<10,paste0("0",mois),mois),
#            jour_bis=ifelse(jour<10,paste0("0",jour),jour),
#            heure_bis=ifelse(heure<10,paste0("0",heure),heure),
#            min_bis=case_when(min>=0 & min<30 ~ "00",
#                              min>=30 & min<=59 ~ "30",
#                              TRUE ~ "")) %>% 
#     group_by(host_id,service_id,id,
#              host_name,service_description,
#              metric_name,unit_name,warn,crit,
#              annee,mois_bis,libjour,jour_bis,heure_bis,min_bis) %>%
#     summarize(temps=round(mean(temps),1),
#               etape=round(mean(etape),0),
#               disponibilite=round(mean(disponibilite),0)) %>%
#     ungroup() %>% as.data.frame() %>%
#     mutate(lib_jour=case_when(libjour==1 ~ "dim.", libjour==2 ~ "lun.",
#                               libjour==3 ~ "mar.", libjour==4 ~ "mer.",
#                               libjour==5 ~ "jeu.", libjour==6 ~ "ven.",
#                               libjour==7 ~ "sam.", TRUE ~ "")) %>% 
#     mutate(date=paste0(lib_jour," ",jour_bis," ",heure_bis,":",min_bis),
#            heure=paste0(heure_bis,":",min_bis)) %>% 
#     relocate(date,heure,.after="min_bis") %>% 
#     relocate(lib_jour,.before="libjour") %>% select(-libjour) %>% 
#     arrange(host_name,service_description,annee,mois_bis,jour_bis,heure_bis,min_bis)

# gestion ensuite du cas particulier du service TEST_API_Gestion_Contacts qui na pas de seuils dalerte !!
# est donc toujours ? 100% !!
# on filtre de maniere ? avoir une seule ligne au lieu des 2 metriques Ajout et Sup et on ne garde que 
# en definitive que la metrique Ajout

# Req_horsQoE_agreg <- Req_horsQoE_agreg %>% 
#     filter(!(service_description=="TEST_API_Gestion_Contacts" & metric_name=="Sup"))

# sauvegarde dans des fichiers Result_horsQoE.csv et Result_horsQoE_agreg.csv

# write.csv2(Req_horsQoE,"E:/Analyse_GEPEX/ProgR_QoE_RapportDispo/Result_horsQoE.csv")
# write.csv2(Req_horsQoE_agreg,"E:/Analyse_GEPEX/ProgR_QoE_RapportDispo/Result_horsQoE_agreg.csv")

#suppression des tables intermediaires

# rm(Req_horsQoE_1,Req_horsQoE_2,Req_horsQoE_3,Req_horsQoE_4)
# 
# rm(Req_horsQoE,Req_horsQoE_agreg)

# fin programme de recuperation des donnees Centreon

######################################################################
######################################################################
######################################################################
######################################################################



