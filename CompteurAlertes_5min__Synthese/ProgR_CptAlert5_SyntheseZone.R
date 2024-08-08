
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)
library(ggplot2)

date_ref <- as.character(Sys.Date()-ddays(1))
date_jour <- str_replace_all(as.character(Sys.Date()-ddays(1)),"-","")

# pour tester...
# date_ref <- "2024-05-03"
# date_jour <- "20240503"

# Creation dun repertoire avec la date du jour
rep_gen <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/Fichiers_date_",date_jour,"/")
dir.create(rep_gen, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# Creation du repertoire des datas
rep_data <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/")

#second endroit generalise pour envoi du ficher (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/"

###

# liste des fichiers csv presents dans le repertoire data et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_data) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  filter(str_detect(liste_fich,"df_datajreq_") & 
           str_detect(liste_fich,date_ref) & str_detect(liste_fich,"_01h30")) 

# transfert du fichier dans le repertoire synthese
file.copy(paste0(rep_data,listing$liste_fich[1]),
          paste0(rep_gen,listing$liste_fich[1]))

######################################################################
######################################################################

# fonction de renommage des Gr.Hotes...
renommageGrHote <- function(df) {
  df_out <- df %>% 
    mutate(Gr1_libcour = ifelse(str_sub(Gr1,1,8)=="-Vlan_DC",str_sub(Gr1,14,str_length(Gr1)),Gr1)) %>% 
    mutate(Gr1_libcour = ifelse(Gr1_libcour=="_PD_STI","PD_STI",Gr1_libcour)) %>% 
    mutate(Gr1_libcour = ifelse((str_sub(Gr1_libcour,1,2)=="PD" & str_detect(zone,"Osny")) |
                                (str_sub(Gr1_libcour,1,2)=="HP" & str_detect(zone,"Auze")),
                                str_sub(Gr1_libcour,4,str_length(Gr1_libcour)),
                                Gr1_libcour)) %>% 
    mutate(Gr1_libcour = ifelse(str_sub(Gr1_libcour,1,2)=="PD" & str_detect(zone,"Auze"),
                                paste0(str_sub(Gr1_libcour,4,str_length(Gr1_libcour))," Prod"),
                                Gr1_libcour)) %>%
    mutate(Gr1_libcour = ifelse(str_sub(Gr1_libcour,1,2)=="PP" & str_detect(zone,"Auze"),
                                paste0(str_sub(Gr1_libcour,4,str_length(Gr1_libcour))," Preprod"),
                                Gr1_libcour)) %>%
    mutate(Gr1_libcour = ifelse(str_sub(Gr1,1,6)=="-Etab_" & str_sub(Gr1,str_length(Gr1)-2,str_length(Gr1)-2)=="_",
                                str_sub(Gr1,1,str_length(Gr1)-3),
                                Gr1_libcour)) %>%
    mutate(Gr1_libcour = ifelse(str_sub(Gr1,1,9)=="-Selenium","-QoE",Gr1_libcour)) %>% 
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"ClermontFerrand","ClFerrand")) %>% 
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"-RepartiteursCharge_","-Repart_F5_")) %>%
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"-Controleurs_Domaine","-Ctrl_Domaine")) %>%
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"-Messagerie_","-Msg_")) %>%
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"Plateforme","Pltf")) %>%
    mutate(Gr1_libcour = str_replace(Gr1_libcour,"-INSEE|_Osny|_Auzeville","")) %>%
    relocate(Gr1_libcour,.after="Gr1")
  # plus traitement particulier pour mieux identifier la zone Partenaire...
  df_out <- df_out %>% 
    mutate(Gr1_libcour = ifelse(zone %in% c("03_Part_Auze","04_Part_Osny"),
                                paste0(Gr1_libcour," (zone Part.)"),Gr1_libcour))
  return(df_out)
}


df <- read.csv2(paste0(rep_gen,listing$liste_fich[1]))

AgregGrHotes <- read.csv2(paste0(fileGen,"Agreg_REFERENCE_Zone_GrHote_Hote.csv"),sep=",") %>% 
  select(-zone,-nb_obs) %>% separate("Gr_Hotes", into=c("Gr1","Gr2"), sep = " // ")


#### !!! ####
#### !!! ####

df_SYNTHETIQUE <- df %>% group_by(zone,Gr_Hotes,name_host,description,moment_30m) %>% 
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame() %>% 
  pivot_wider(names_from=moment_30m, values_from = sum_poids) %>% as.data.frame()
# rearrangement des colonnes de df_SYNTHETIQUE...
listCol_SYNTH <- colnames(df_SYNTHETIQUE) %>% as.data.frame() %>%
  rename(nomcol=".") %>% 
  filter(!(nomcol %in% c("zone","Gr_Hotes","name_host","description"))) %>%
  arrange(nomcol) %>% as.vector() %>% unlist()
df_SYNTHETIQUE <- df_SYNTHETIQUE %>%
  relocate(all_of(listCol_SYNTH),.after="description")

df_SYNTHETIQUE <- df_SYNTHETIQUE %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

df_SYNTHETIQUE <- renommageGrHote(df_SYNTHETIQUE)

df_SYNTHETIQUE <- df_SYNTHETIQUE %>% rowwise() %>% 
  mutate(totscore      = sum(c_across("00h00":"23h30"),na.rm = TRUE),
         totscore_jour = sum(c_across("07h00":"18h30"),na.rm = TRUE)) %>% 
  mutate(rapport=round((totscore_jour*2)/totscore,1)) %>% as.data.frame()

df_SYNTHETIQUE_jour <- df_SYNTHETIQUE %>% filter(rapport>1 & totscore_jour>100) %>% 
  arrange(desc(rapport),desc(totscore_jour)) %>% 
  arrange(zone,Gr1_libcour,name_host,description) %>% 
  filter(!str_detect(Gr1_libcour,"Centreon"))

df_SYNTHETIQUE_jour2 <- df_SYNTHETIQUE_jour %>% 
  mutate(Gr2_libcour = ifelse(str_sub(Gr2,1,3) %in% c("HP_","PD_"), 
                              str_sub(Gr2,4,str_length(Gr2)),Gr2)) %>% 
  
  mutate(host_libcour = ifelse(str_detect(Gr1_libcour,"Preprod") & str_sub(name_host,1,2)=="pp", 
                               str_sub(name_host,3,str_length(name_host)),name_host)) %>% 
  mutate(host_libcour = ifelse(str_sub(Gr2,1,3)=="HP_" & str_sub(name_host,1,2)=="dv", 
                               str_sub(name_host,3,str_length(name_host)),host_libcour)) %>% 
  mutate(host_libcour = ifelse(str_sub(Gr2,1,3)=="PD_" & str_sub(host_libcour,1,2)=="pd",
                               str_sub(name_host,3,str_length(name_host)),host_libcour)) %>%
  
  mutate(host_libcour = ifelse(str_sub(Gr2_libcour,1,str_length(Gr2_libcour))== str_sub(host_libcour,1,str_length(Gr2_libcour)),
                               str_sub(host_libcour,str_length(Gr2_libcour)+1,str_length(host_libcour)),
                               host_libcour)) %>%
  
  mutate(host_libcour = ifelse(Gr2_libcour=="-Srv_GPDTIL",
                               str_sub(host_libcour,3,str_length(host_libcour)),host_libcour)) %>% 

  mutate(host_libcour = ifelse(str_sub(Gr1_libcour,1,5)=="-Etab" & Gr2_libcour=="-Switchs-Coeurs",
                               "",host_libcour)) %>% 
  
  mutate(host_libcour = ifelse(Gr1_libcour=="-Ctrl_Domaine" & str_to_lower(str_sub(name_host,1,6))=="pdaddc",
                               str_sub(name_host,7,str_length(name_host)),host_libcour)) %>% 
  mutate(host_libcour = ifelse(Gr1_libcour=="-Ctrl_Domaine" & str_to_lower(str_sub(name_host,1,6))=="pdaddc",
                               str_replace(host_libcour,"wd1|WD1|wd2|WD2"," num "),host_libcour)) %>% 
  mutate(host_libcour = ifelse(Gr1_libcour=="-Ctrl_Domaine" & str_to_lower(str_sub(name_host,1,5))=="pdadm",
                               str_sub(name_host,3,str_length(name_host)),host_libcour)) %>% 
  mutate(host_libcour = ifelse(Gr1_libcour=="-Ctrl_Domaine" & str_to_lower(str_sub(name_host,1,5))=="pdadm",
                               str_replace(host_libcour,"wd1|WD1|wd2|WD2"," num "),host_libcour)) %>% 
 
  mutate(host_libcour = ifelse(Gr1_libcour=="-Repart_F5" & str_to_lower(str_sub(name_host,1,9))=="pd-hlb01-",
                               str_sub(name_host,10,14),host_libcour)) %>% 
  mutate(description = ifelse( str_sub(description,1,13)=="Pool-/Common/" & str_to_lower(str_sub(name_host,1,9))=="pd-hlb01-",
                               str_sub(description,14,str_length(description)),description)) %>%
  
  mutate(host_libcour = ifelse(str_detect(host_libcour,"las"),
                               str_replace(host_libcour,"las"," Apache"),host_libcour)) %>%  
  mutate(host_libcour = ifelse(str_detect(name_host,"las"),
                               paste0(host_libcour," en las"),host_libcour)) %>% 
  mutate(host_libcour = ifelse(str_detect(host_libcour,"lht"),
                               str_replace(host_libcour,"lht"," Tomcat"),host_libcour)) %>%  
  mutate(host_libcour = ifelse(str_detect(name_host,"lht"),
                               paste0(host_libcour," en lht"),host_libcour)) %>% 
  mutate(host_libcour = ifelse(str_detect(host_libcour,"ldb"),
                               str_replace(host_libcour,"ldb"," Bdd"),host_libcour)) %>%  
  mutate(host_libcour = ifelse(str_detect(name_host,"ldb"),
                               paste0(host_libcour," en ldb"),host_libcour)) %>% 
  
  mutate(host_libcour = ifelse(str_detect(host_libcour,"lm0"),
                               str_replace(host_libcour,"lm0"," Apache"),host_libcour)) %>%  
  mutate(host_libcour = ifelse(str_detect(host_libcour,"ln0"),
                               str_replace(host_libcour,"ln0"," Tomcat"),host_libcour)) %>%  
  mutate(host_libcour = ifelse(str_detect(host_libcour,"lg0"),
                               str_replace(host_libcour,"lg0"," Bdd"),host_libcour)) %>%  
  
  mutate(host_libcour = ifelse(is.na(host_libcour),
                               str_sub(name_host,3,str_length(name_host)),host_libcour)) %>% 
  
  mutate(Gr2_libcour = ifelse(is.na(Gr2_libcour),"",Gr2_libcour)) %>% 

  relocate(Gr2_libcour,.after="Gr2") %>% 
  relocate(host_libcour,.after="name_host")  


#### !!! ####
#### !!! ####

########
####
# nombre de services apparus en alerte...
df_nbserv <- df %>% group_by(zone,Gr_Hotes,name_host,description) %>% 
  summarise(nbserv=n()) %>% ungroup() %>% as.data.frame() %>% 
  group_by(name_host) %>% summarise(nbserv=n()) %>% ungroup() %>% as.data.frame()

df_nbserv_jour <- df %>% 
  filter(moment_1h %in% c("07h","08h","09h","10h","11h","12h",
                          "13h","14h","15h","16h","17h","18h")) %>%
  group_by(zone,Gr_Hotes,name_host,description) %>% 
  summarise(nbservjour=n()) %>% ungroup() %>% as.data.frame() %>% 
  group_by(name_host) %>% summarise(nbserv_jour=n()) %>% ungroup() %>% as.data.frame()

########
####
# nombre d'hotes apparus en alerte...
df_nbhote <- df %>% group_by(zone,Gr_Hotes,name_host) %>% 
  summarise(nbhoteserv=n()) %>% ungroup() %>% as.data.frame() %>% 
  mutate(nbhote=1) %>% select(-zone,-Gr_Hotes,-nbhoteserv)
  # %>% 
  # group_by(zone,Gr_Hotes) %>% summarise(nbhote=sum(nbhote)) %>% ungroup() %>% as.data.frame()

df_nbhote_jour <- df %>% 
  filter(moment_1h %in% c("07h","08h","09h","10h","11h","12h",
                          "13h","14h","15h","16h","17h","18h")) %>%
  group_by(zone,Gr_Hotes,name_host) %>% 
  summarise(nbhoteservjour=n()) %>% ungroup() %>% as.data.frame() %>% 
  mutate(nbhote_jour=1) %>% select(-zone,-Gr_Hotes,-nbhoteservjour)
  # %>% 
  # group_by(zone,Gr_Hotes) %>% summarise(nbhote_jour=n()) %>% ungroup() %>% as.data.frame()

############
########
####
# CALCUL de repartition statut alerte crit,warn,inc selon chaque zone...
df_repstatus <- df %>% select(zone,Gr_Hotes,name_host,last_hard_state,poids) %>% 
  group_by(zone,Gr_Hotes,name_host,last_hard_state,poids) %>%
  mutate(last_hard_state=case_when(last_hard_state==2 ~ "st1_Crit",  
                                   last_hard_state==1 ~ "st2_Warn",
                                   last_hard_state==3 ~ "st3_Inco", 
                                   TRUE ~ "X")) %>% arrange(zone,Gr_Hotes) %>%
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame() %>% select(-poids) %>% 
  pivot_wider(names_from=last_hard_state, values_from = sum_poids) %>% as.data.frame() %>% 
  relocate("st1_Crit","st2_Warn","st3_Inco",.after="name_host")
df_repstatus[is.na(df_repstatus)] <- 0 
df_repstatus <- df_repstatus %>% rowwise() %>% 
  mutate(totscore = sum(c_across("st1_Crit":"st3_Inco"))) %>% as.data.frame()

df_repstatus <- df_repstatus %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

# repartition status niveau appli
df_repstatus_appli <- df_repstatus %>% group_by(zone,Gr1,Gr2) %>% 
  summarise(across(c("st1_Crit","st2_Warn","st3_Inco","totscore"),
                   ~ sum(.x, na.rm = TRUE))) %>% as.data.frame()

# repartition status niveau GrHote
df_repstatus_GrHote <- df_repstatus %>% group_by(zone,Gr1) %>% 
  summarise(across(c("st1_Crit","st2_Warn","st3_Inco","totscore"),
                   ~ sum(.x, na.rm = TRUE))) %>% as.data.frame()

df_repstatus_zone <- df_repstatus_GrHote %>% group_by(zone) %>%
  summarise(max_totscore=max(totscore)) %>% ungroup() %>% as.data.frame()
max_Zone_DMZPart_DC1 <- max(df_repstatus_zone[df_repstatus_zone$zone=="02_DMZ_Osny","max_totscore"],
                            df_repstatus_zone[df_repstatus_zone$zone=="04_Part_Osny","max_totscore"])
max_Zone_DMZPart_DC2 <- max(df_repstatus_zone[df_repstatus_zone$zone=="01_DMZ_Auze","max_totscore"],
                            df_repstatus_zone[df_repstatus_zone$zone=="03_Part_Auze","max_totscore"])
df_repstatus_zone[df_repstatus_zone$zone=="02_DMZ_Osny","max_totscore"] <- max_Zone_DMZPart_DC1
df_repstatus_zone[df_repstatus_zone$zone=="04_Part_Osny","max_totscore"] <- max_Zone_DMZPart_DC1
df_repstatus_zone[df_repstatus_zone$zone=="01_DMZ_Auze","max_totscore"] <- max_Zone_DMZPart_DC2
df_repstatus_zone[df_repstatus_zone$zone=="03_Part_Auze","max_totscore"] <- max_Zone_DMZPart_DC2

df_repstatus_GrHote <- df_repstatus_GrHote %>% left_join(df_repstatus_zone, by=c("zone")) %>% 
  mutate(st4_OK=max_totscore-totscore) %>% relocate("st4_OK",.before="totscore") %>% 
  mutate(p1_Crit = round(st1_Crit/max_totscore*100,0),
         p2_Warn = round(st2_Warn/max_totscore*100,0),
         p3_Inco = round(st3_Inco/max_totscore*100,0),
         p4_OK   = round(st4_OK/max_totscore*100,0)) %>% 
  mutate(p1c_Crit=p1_Crit,
         p2c_Warn=p1_Crit+p2_Warn,
         p3c_Inco=p1_Crit+p2_Warn+p3_Inco)

# quelques renommages...
df_repstatus_GrHote <- renommageGrHote(df_repstatus_GrHote)

df_repstatus_GrHote_Final <- df_repstatus_GrHote %>%
  select("zone","Gr1","Gr1_libcour","p1c_Crit","p2c_Warn","p3c_Inco")


########
####
# CALCUL MILIEU-JOUR de repartition statut alerte crit,warn,inc selon chaque zone...
df_repstatus_jour <- df %>%
  filter(moment_1h %in% c("07h","08h","09h","10h","11h","12h",
                          "13h","14h","15h","16h","17h","18h")) %>% 
  select(zone,Gr_Hotes,name_host,last_hard_state,poids) %>% 
  group_by(zone,Gr_Hotes,name_host,last_hard_state,poids) %>%
  mutate(last_hard_state=case_when(last_hard_state==2 ~ "st1_Crit_jour",  
                                   last_hard_state==1 ~ "st2_Warn_jour",
                                   last_hard_state==3 ~ "st3_Inco_jour", 
                                   TRUE ~ "X")) %>% arrange(zone,Gr_Hotes) %>%
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame() %>% select(-poids) %>% 
  pivot_wider(names_from=last_hard_state, values_from = sum_poids) %>% as.data.frame() %>% 
  relocate("st1_Crit_jour","st2_Warn_jour","st3_Inco_jour",.after="name_host")
df_repstatus_jour[is.na(df_repstatus_jour)] <- 0 
df_repstatus_jour <- df_repstatus_jour %>% rowwise() %>% 
  mutate(totscore_jour = sum(c_across("st1_Crit_jour":"st3_Inco_jour"))) %>% as.data.frame()

df_repstatus_jour <- df_repstatus_jour %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

# repartition status niveau appli
df_repstatus_jour_appli <- df_repstatus_jour %>% group_by(zone,Gr1,Gr2) %>% 
  summarise(across(c("st1_Crit_jour","st2_Warn_jour","st3_Inco_jour","totscore_jour"),
                   ~ sum(.x, na.rm = TRUE))) %>% as.data.frame()

# repartition status niveau GrHote
df_repstatus_jour_GrHote <- df_repstatus_jour %>% group_by(zone,Gr1) %>% 
  summarise(across(c("st1_Crit_jour","st2_Warn_jour","st3_Inco_jour","totscore_jour"),
                   ~ sum(.x, na.rm = TRUE))) %>% as.data.frame()

df_repstatus_jour_zone <- df_repstatus_jour_GrHote %>% group_by(zone) %>%
  summarise(max_totscore_jour=max(totscore_jour)) %>% ungroup() %>% as.data.frame()
max_Zone_DMZPart_DC1 <- max(df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="02_DMZ_Osny","max_totscore_jour"],
                            df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="04_Part_Osny","max_totscore_jour"])
max_Zone_DMZPart_DC2 <- max(df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="01_DMZ_Auze","max_totscore_jour"],
                            df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="03_Part_Auze","max_totscore_jour"])
df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="02_DMZ_Osny","max_totscore_jour"] <- max_Zone_DMZPart_DC1
df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="04_Part_Osny","max_totscore_jour"] <- max_Zone_DMZPart_DC1
df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="01_DMZ_Auze","max_totscore_jour"] <- max_Zone_DMZPart_DC2
df_repstatus_jour_zone[df_repstatus_jour_zone$zone=="03_Part_Auze","max_totscore_jour"] <- max_Zone_DMZPart_DC2

df_repstatus_jour_GrHote <- df_repstatus_jour_GrHote %>% left_join(df_repstatus_jour_zone, by=c("zone")) %>% 
  mutate(st4_OK_jour=max_totscore_jour-totscore_jour) %>% relocate("st4_OK_jour",.before="totscore_jour") %>% 
  mutate(p1_Crit_jour = round(st1_Crit_jour/max_totscore_jour*100,0),
         p2_Warn_jour = round(st2_Warn_jour/max_totscore_jour*100,0),
         p3_Inco_jour = round(st3_Inco_jour/max_totscore_jour*100,0),
         p4_OK_jour   = round(st4_OK_jour/max_totscore_jour*100,0)) %>% 
  mutate(p1c_Crit_jour=p1_Crit_jour,
         p2c_Warn_jour=p1_Crit_jour+p2_Warn_jour,
         p3c_Inco_jour=p1_Crit_jour+p2_Warn_jour+p3_Inco_jour)

# quelques renommages...
df_repstatus_jour_GrHote <- renommageGrHote(df_repstatus_jour_GrHote)

df_repstatus_jour_GrHote_Final <- df_repstatus_jour_GrHote %>%
  select("zone","Gr1","Gr1_libcour","p1c_Crit_jour","p2c_Warn_jour","p3c_Inco_jour")


###########################
###########################

df_synth_zone <- df %>% arrange(zone,Gr_Hotes,name_host,moment_1h) %>%
  group_by(zone,Gr_Hotes,name_host,moment_1h) %>% 
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame() %>% 
  pivot_wider(names_from=moment_1h, values_from = sum_poids) %>% as.data.frame()


# rearrangement des colonnes...
listCol <- colnames(df_synth_zone) %>% as.data.frame() %>%
  rename(nomcol=".") %>% 
  filter(!(nomcol %in% c("zone","Gr_Hotes","name_host"))) %>%
  arrange(nomcol) %>% as.vector() %>% unlist()
df_synth_zone <- df_synth_zone %>%
  relocate(all_of(listCol),.after="name_host")

# rearrangement des colonnes...
# df_synth_zone <- df_synth_zone %>%
#   relocate("00h","01h","02h","03h","04h","05h",
#            "06h","07h","08h","09h","10h","11h",
#            "12h","13h","14h","15h","16h","17h",
#            "18h","19h","20h","21h","22h","23h",.after="name_host")


df_synth_zone <- df_synth_zone %>% rowwise() %>% 
  mutate(totscore      = sum(c_across("00h":"23h"),na.rm = TRUE),
         totscore_jour = sum(c_across("07h":"18h"),na.rm = TRUE)) %>% 
# fusion avec les 4 dataframes df_nbhote, df_nbserv, df_nbhote_jour et df_nbserv_jour
  left_join(df_nbhote, by=c("name_host")) %>% 
  left_join(df_nbserv, by=c("name_host")) %>%
  left_join(df_nbhote_jour, by=c("name_host")) %>% 
  left_join(df_nbserv_jour, by=c("name_host")) %>% 
# adaptation si nbservjour=NA apres la fusion...
  mutate(nbhote_jour=ifelse(is.na(nbhote_jour),replace_na(0),nbhote_jour)) %>%
  mutate(nbserv_jour=ifelse(is.na(nbserv_jour),replace_na(0),nbserv_jour)) %>% 
  mutate(totscoremoy     =round(totscore/nbserv,0),
         totscoremoy_jour=ifelse(nbserv_jour!=0,round(totscore_jour/nbserv_jour,0),0)) %>% 
  relocate(nbhote,nbserv,totscoremoy,.after=totscore) %>% 
  relocate(nbhote_jour,nbserv_jour,totscoremoy_jour,.after=totscore_jour) %>%
  mutate(rapport=round((totscoremoy_jour*2)/totscoremoy,1)) %>% 
  select(-totscoremoy,-totscoremoy_jour) %>% as.data.frame()


# Vue <- df_synth_zone %>% filter(str_detect(name_host,"esxi"))

df_synth_zone <- df_synth_zone %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

# detection des host non pris en compte et qui necessite alors un
# enrichissement du fichier Agreg_REFERENCE_Zone_GrHote_Hote.csv
# VueVide <- df_synth_zone %>% filter(is.na(Gr1))


################
################

df_synth_zone_appli_1 <- df_synth_zone %>% group_by(zone,Gr1,Gr2) %>% 
  summarise(across(c("00h":"23h","totscore","nbhote","nbserv"),
                   ~ sum(.x, na.rm = TRUE))) %>%
  mutate(totscoremoy=round(totscore/nbserv,0))

df_synth_zone_appli_2 <- df_synth_zone %>% group_by(zone,Gr1,Gr2) %>% 
  summarise(across(c("07h":"18h","totscore_jour","nbhote_jour","nbserv_jour"),
                   ~ sum(.x, na.rm = TRUE))) %>% select(-"07h":-"18h") %>% 
  mutate(totscoremoy_jour=ifelse(nbserv_jour!=0,round(totscore_jour/nbserv_jour,0),0))

df_synth_zone_appli <- df_synth_zone_appli_1 %>%
  left_join(df_synth_zone_appli_2, by=c("zone","Gr1","Gr2")) %>% 
  mutate(rapport=round((totscoremoy_jour*2)/totscoremoy,1)) %>% 
  select(-totscoremoy,-totscoremoy_jour) %>% as.data.frame()

rm(df_synth_zone_appli_1,df_synth_zone_appli_2)


################
################


df_synth_zone_GrHote_1 <- df_synth_zone_appli %>% group_by(zone,Gr1) %>% 
  summarise(across(c("00h":"23h","totscore","nbhote","nbserv"),
                   ~ sum(.x, na.rm = TRUE))) %>% 
  mutate(totscoremoy=round(totscore/nbserv,0))

df_synth_zone_GrHote_2 <- df_synth_zone_appli %>% group_by(zone,Gr1) %>% 
  summarise(across(c("07h":"18h","totscore_jour","nbhote_jour","nbserv_jour"),
                   ~ sum(.x, na.rm = TRUE))) %>% select(-"07h":-"18h") %>% 
  mutate(totscoremoy_jour=ifelse(nbserv_jour!=0,round(totscore_jour/nbserv_jour,0),0))

df_synth_zone_GrHote <- df_synth_zone_GrHote_1 %>%
  left_join(df_synth_zone_GrHote_2, by=c("zone","Gr1")) %>% 
  mutate(rapport=round((totscoremoy_jour*2)/totscoremoy,1)) %>% 
  select(-totscoremoy,-totscoremoy_jour) %>% as.data.frame()

rm(df_synth_zone_GrHote_1,df_synth_zone_GrHote_2)


# quelques renommages...
df_synth_zone_GrHote <- renommageGrHote(df_synth_zone_GrHote)

df_synth_zone_GrHote2 <- df_synth_zone_GrHote %>% rowwise() %>%
  mutate(hmax = max(c_across("00h":"23h"))) %>% ungroup() %>% as.data.frame()

colnames(df_synth_zone_GrHote2) <- c("zone","Gr1","Gr1_libcour",
    "h00h","h01h","h02h","h03h","h04h","h05h","h06h","h07h","h08h","h09h","h10h","h11h",
    "h12h","h13h","h14h","h15h","h16h","h17h","h18h","h19h","h20h","h21h","h22h","h23h",
    "totscore","nbhote","nbserv",  "totscore_jour","nbhote_jour","nbserv_jour",  "rapport","hmax")

df_synth_zone_GrHote2 <- df_synth_zone_GrHote2 %>%
  mutate(p00h = round(h00h/hmax*100,0), p01h = round(h01h/hmax*100,0),
         p02h = round(h02h/hmax*100,0), p03h = round(h03h/hmax*100,0),
         p04h = round(h04h/hmax*100,0), p05h = round(h05h/hmax*100,0),
         p06h = round(h06h/hmax*100,0), p07h = round(h07h/hmax*100,0),
         p08h = round(h08h/hmax*100,0), p09h = round(h09h/hmax*100,0),
         p10h = round(h10h/hmax*100,0), p11h = round(h11h/hmax*100,0),
         p12h = round(h12h/hmax*100,0), p13h = round(h13h/hmax*100,0),
         p14h = round(h14h/hmax*100,0), p15h = round(h15h/hmax*100,0),
         p16h = round(h16h/hmax*100,0), p17h = round(h17h/hmax*100,0),
         p18h = round(h18h/hmax*100,0), p19h = round(h19h/hmax*100,0),
         p20h = round(h20h/hmax*100,0), p21h = round(h21h/hmax*100,0),
         p22h = round(h22h/hmax*100,0), p23h = round(h23h/hmax*100,0)) %>% 
  select("zone","Gr1","Gr1_libcour","hmax","p00h":"p23h")

df_synth_zone_GrHote <- df_synth_zone_GrHote %>%
  left_join(df_synth_zone_GrHote2, by=c("zone","Gr1","Gr1_libcour"))

rm(df_synth_zone_GrHote2)

# Fusion avec df_repstatus_GrHote_Final pour fill en background-color la colonne score...
df_synth_zone_GrHote <- df_synth_zone_GrHote %>%
  left_join(df_repstatus_GrHote_Final, by=c("zone","Gr1","Gr1_libcour"))
# Fusion avec df_repstatus_jour_GrHote_Final pour fill en background-color la colonne score...
df_synth_zone_GrHote <- df_synth_zone_GrHote %>%
  left_join(df_repstatus_jour_GrHote_Final, by=c("zone","Gr1","Gr1_libcour"))


###


df_synth_zone_GrHote_RES <- df_synth_zone_GrHote %>% 
  mutate(zone=ifelse(zone=="03_Part_Auze","01_DMZ_Auze",zone)) %>% 
  mutate(zone=ifelse(zone=="04_Part_Osny","02_DMZ_Osny",zone)) %>% group_by(zone) %>% 
  summarise(totscore=sum(totscore),          nbhote=sum(nbhote),           nbserv=sum(nbserv),
            totscore_jour=sum(totscore_jour),nbhote_jour=sum(nbhote_jour), nbserv_jour=sum(nbserv_jour)) %>% 
  ungroup() %>% as.data.frame() %>% 
  mutate(rapport=round((totscore_jour/nbserv_jour)*2 / (totscore/nbserv), 1))


# ecriture du dataframe GrHote dans un .csv aux 2 endroits
write.csv2(df_synth_zone_GrHote,paste0(rep_gen,"tableau_synthese_GrHote.csv"), row.names = FALSE)
write.csv2(df_synth_zone_GrHote,paste0(fileGen,"tableau_synthese_GrHote.csv"), row.names = FALSE)

write.csv2(df_synth_zone_GrHote_RES,paste0(rep_gen,"tableau_synthese_GrHote_RESUME.csv"), row.names = FALSE)


# recuperation des resultats RES dhier
date_jour_hier <- str_replace_all(as.character(Sys.Date()-ddays(2)),"-","")
rep_gen_hier <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/Fichiers_date_",date_jour_hier,"/")

df_synth_zone_GrHote_RES_hier <- read.csv2(paste0(rep_gen_hier,"tableau_synthese_GrHote_RESUME.csv"))
df_synth_zone_GrHote_RES_hier <- df_synth_zone_GrHote_RES_hier %>% 
  rename(totscore_dhier     =totscore,      nbhote_dhier     =nbhote,       nbserv_dhier     =nbserv,
         totscore_jour_dhier=totscore_jour, nbhote_jour_dhier=nbhote_jour,  nbserv_jour_dhier=nbserv_jour,
         rapport_dhier      =rapport)

df_synth_zone_GrHote_RES <- df_synth_zone_GrHote_RES %>% 
  left_join(df_synth_zone_GrHote_RES_hier, by=c("zone")) %>% 
  mutate(dif_totscore     =as.character(round(totscore-totscore_dhier,1)),
         dif_nbhote       =as.character(round(nbhote-nbhote_dhier,1)),
         dif_nbserv       =as.character(round(nbserv-nbserv_dhier,1)),
         dif_totscore_jour=as.character(round(totscore_jour-totscore_jour_dhier,1)),
         dif_nbhote_jour  =as.character(round(nbhote_jour-nbhote_jour_dhier,1)),
         dif_nbserv_jour  =as.character(round(nbserv_jour-nbserv_jour_dhier,1)),
         dif_rapport      =as.character(round(rapport-rapport_dhier,1)) ) %>% 
  mutate(dif_totscore     =ifelse(str_sub(dif_totscore,1,1)=="-",dif_totscore,paste0("+",dif_totscore)),
         dif_nbhote       =ifelse(str_sub(dif_nbhote,1,1)=="-",dif_nbhote,paste0("+",dif_nbhote)),
         dif_nbserv       =ifelse(str_sub(dif_nbserv,1,1)=="-",dif_nbserv,paste0("+",dif_nbserv)),
         dif_totscore_jour=ifelse(str_sub(dif_totscore_jour,1,1)=="-",dif_totscore_jour,paste0("+",dif_totscore_jour)),
         dif_nbhote_jour  =ifelse(str_sub(dif_nbhote_jour,1,1)=="-",dif_nbhote_jour,paste0("+",dif_nbhote_jour)),
         dif_nbserv_jour  =ifelse(str_sub(dif_nbserv_jour,1,1)=="-",dif_nbserv_jour,paste0("+",dif_nbserv_jour)),
         dif_rapport      =ifelse(str_sub(dif_rapport,1,1)=="-",dif_rapport,paste0("+",dif_rapport)) )  


# encodage de limage du phare
# library(base64enc)
# img_phare_encode <- base64encode(paste0(fileGen,"img_phare_synthese_GrHote.png"))
# write.csv2(img_phare_encode,paste0(fileGen,"img_phare_synthese_GrHote_encode.png"), row.names = FALSE)


######################################################################
######################################################################
######################################################################
######################################################################

######################################################################
######################################################################
######################################################################
######################################################################

######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

creadatelib <- function(date) {
  djour <- str_sub(date_ref,9,10) ; dmois <- str_sub(date_ref,6,7) ; dan <- str_sub(date_ref,1,4)
  dmois <- case_when(
    dmois=="01" ~ "janvier", dmois=="02" ~ "f&eacute;vrier", dmois=="03" ~ "mars",
    dmois=="04" ~ "avril",   dmois=="05" ~ "mai",            dmois=="06" ~ "juin",
    dmois=="07" ~ "juillet", dmois=="08" ~ "ao&ucirc;t",     dmois=="09" ~ "septembre",
    dmois=="10" ~ "octobre", dmois=="11" ~ "novembre",       dmois=="12" ~ "d&eacute;cembre",
    TRUE ~ "X"
  )
  return(paste0(djour," ",dmois," ",dan))
}
date_ref_lib <- creadatelib(dateref)

#####
#####

creatabhtml <- function(df) {
  codehtml <- ""
  if (nrow(df)>0){
    for (i in 1:nrow(df)){
      for (j in 1:ncol(df)){
        print(paste0(i," et ",j," : ",df[i,j]))
        if ( j==3 ){
          codehtml <- paste0(codehtml,"<tr>",
                  "<td style='width:auto;' class='col1'>",df[i,j],"</td>",
                  
                  # "<td style='text-align:right;'>",df[i,"totscore"],"</td>",
                  "<td style='text-align:right;
                      background: linear-gradient(to right, 
                      rgba(255,0,0,1) 0%,                 rgba(255,0,0,1)",df[i,60],"%,
                      rgba(255,155,0,1)",df[i,60]+1,"%,   rgba(255,155,0,1)",df[i,61],"%,
                      rgba(180,180,180,1)",df[i,61]+1,"%, rgba(180,180,180,1)",df[i,62],"%,
                      rgba(255,255,255,1)",df[i,62]+1,"%, rgba(255,255,255,1) 100%);'>",df[i,"totscore"],"</td>",
                  "<td style='text-align:right;'>",df[i,"nbhote"],"</td>",
                  "<td style='text-align:right;'>",df[i,"nbserv"],"</td>",
                  
                  # "<td style='text-align:right;'>",df[i,"totscore_jour"],"</td>",
                  "<td style='text-align:right;
                      background: linear-gradient(to right, 
                      rgba(255,0,0,1) 0%, rgba(255,0,0,1)",df[i,63],"%,
                      rgba(255,155,0,1)",df[i,63]+1,"%,   rgba(255,155,0,1)",df[i,64],"%,
                      rgba(180,180,180,1)",df[i,64]+1,"%, rgba(180,180,180,1)",df[i,65],"%,
                      rgba(255,255,255,1)",df[i,65]+1,"%, rgba(255,255,255,1) 100%);'>",df[i,"totscore_jour"],"</td>",
                  "<td style='text-align:right;'>",df[i,"nbhote_jour"],"</td>",
                  "<td style='text-align:right;'>",df[i,"nbserv_jour"],"</td>",
                  
                  "<td style='text-align:right; background-color:Khaki;'>",df[i,"rapport"],"</td>")
        }
        if ( j!=3 & j>3 & j<=27 ){
          if (df[i,j]!=0){
            
            # codehtml <- paste0(codehtml,"<td class='cell'>",df[i,j],"</td>")
            
            codehtml <- paste0(codehtml,
                               "<td class='cell'
                                    style='background: linear-gradient(to top, 
                                           rgba(221,160,221,1) 0%,                
                                           rgba(221,160,221,1)",df[i,j+32],"%,    
                                           rgba(255,255,255,1)",df[i,j+32]+1,"%,
                                           rgba(255,255,255,1) 100%);'>",df[i,j],"</td>")

          } else {
            codehtml <- paste0(codehtml,"<td class='cell'
                                             style='background-color:white;'></td>")
          }
        }
        if ( j==27 ){codehtml <- paste0(codehtml,"</tr>")}
        if ( j>27 ){ codehtml <- codehtml }
      }
    }
  } else {
    codehtml <- paste0(codehtml,"<tr><td colspan=25>absence de tableau</td></tr>")
  }
  rm(i,j)
  return(codehtml)
}

# filtre sur DC1 DMZ et Partenaire
df_DC1_z000 <- df_synth_zone_GrHote %>% filter(zone %in% c("02_DMZ_Osny","04_Part_Osny"))
corps_tab_DC1_z000 <- creatabhtml(df_DC1_z000)
# filtre sur DC2 DMZ et Partenaire
df_DC2_z000 <- df_synth_zone_GrHote %>% filter(zone %in% c("01_DMZ_Auze","03_Part_Auze"))
corps_tab_DC2_z000 <- creatabhtml(df_DC2_z000)

#####

# filtre sur DC1 Interne
df_DC1_z050 <- df_synth_zone_GrHote %>% filter(zone %in% c("06_ZoneInt_Osny"))
corps_tab_DC1_z050 <- creatabhtml(df_DC1_z050)
# filtre sur DC2 Interne
df_DC2_z050 <- df_synth_zone_GrHote %>% filter(zone %in% c("05_ZoneInt_Auze"))
corps_tab_DC2_z050 <- creatabhtml(df_DC2_z050)

#####

# filtre sur DC1 SIA
df_DC1_z100 <- df_synth_zone_GrHote %>% filter(zone %in% c("08_ZoneSIA_Osny"))
corps_tab_DC1_z100 <- creatabhtml(df_DC1_z100)
# filtre sur DC2 SIA
df_DC2_z100 <- df_synth_zone_GrHote %>% filter(zone %in% c("07_ZoneSIA_Auze"))
corps_tab_DC2_z100 <- creatabhtml(df_DC2_z100)

#####
#####

head <- paste0("<head>",
                  "<meta charset='utf-8'/>",
                  "<title>ENVOI INFOS SYNTHESE Zones du SI</title>",
               
                  "<link rel='stylesheet'
                         href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css' 
                         crossorigin='anonymous' 
                         referrerpolicy='no-referrer' />",
               
                  "<style>",
                      " * { margin:0;
                            padding:0;
                            box-sizing: border-box; }",
               
                      "body {background: linear-gradient(to bottom, 
                                                          rgb(14,21,60) 10%,
                                                          rgb(30,37,91) 50%, 
                                                          rgb(76,122,169) 80%);}", 
               
                      # ".tabletitre {border:         none;
                      #               text-align:     left;
                      #               vertical-align: top;}",
               
               
                      ".image {
                               # position :relative;
                               display: flex;
                               justify-content: center;
                               background-size: auto;}", 
               
                      "img {border: 3px white solid;
                            border-radius: 50px 10px 50px 10px;}",
               
                      ".titreGen {
                                  # position:absolute;
                                  color:white;
                                  font-style: italic;
                                  top:5%;
                                  left:15%;
                                  /* transform: translate(-80%,-50%); */}",
               
               
                      ".moon-title1 {transform : translateX(100px) translateY(110px);}",
                      ".moon-title2 {transform : translateX(100px) translateY(130px);}",
               
                      ".etat0 {position:relative;
                               font-size:40px;
                               opacity:1;}", 
                      ".etat0:hover {opacity:0;}",
               
                      ".etat1 {position:relative;
                               font-size:40px;
                               left:-50px;
                               opacity:0;}",
                      ".etat1:hover {left:0px;
                                     opacity:1;
                                     transition: all 800ms ease-in-out;}",
               
               
               
                      ".moon-title2 .etat0 {display: inline;
                                         opacity:1;}",
                      ".moon-title2:hover .etat0 {display: none;
                                               opacity:0;}",
               
                      ".moon-title2 .etat1 {display: none;}",
                      ".moon-title2:hover .etat1 {display: inline;}",
               
                      ".partie-cellule {display:relative;}",
               
                      ".grid-container {display: grid;
                                        grid-template-columns: auto auto;
                                        gap: 5px 15px;}",
               
                      ".grid-item-titre {width:auto; # au lieu de 600px;
                                         height:auto;}",
               
                      ".grid-item {width:auto; # au lieu de 600px;
                                   height:auto;
                                   overflow: auto;}",
               
                      ".grid-item > details[open] {height:400px;}",
               
               
                      "details:hover {opacity:1;}",
               
                      "details {transition: all 1s ease-in-out;
                                max-height:50px;
                                translate:-300px;
                                transform:skewX(-20deg);
                                opacity:0;}",
                      "details[open] {max-height:400px;
                                      translate:0px;
                                      transform:skewX(0deg);
                                      opacity:1;}",
               
                      "details summary {transition: all 1s ease-in-out;
                                        translate:300px;
                                        transform:skewX(20deg);
                                        background-color:silver;}",
                      "details[open] summary {translate:0px;
                                              transform:skewX(0deg);
                                              width:1400px;
                                              background-color:grey;}",
               
                      "details table {#transition: all 1s ease-in-out;
                                      #translate:300px;
                                      #transform:skewX(-20deg);
                                      animation: mymove 1.5s linear;
                                      animation-iteration-count: mymove 1;}",
                      "details[open] table {translate:0px;
                                            transform:skewX(0deg);
                                            width:1400px;}",
               
                      "@keyframes mymove {
                        0% {transform:skewX(-20deg);translate:-300px;}
                        50% {transform:skewX(5deg);translate:100px;}
                        75% {transform:skewX(-3deg);translate:-50px;}
                        90% {transform:skewX(1deg);translate:20px;}
                        100% {transform:skewX(0deg);translate:0px;}
                      }",
               
                      # "details table {top: -50px;
                      #                 opacity:0.5;
                      #                 transition: all 1s ease-in-out;}",
                      # "details[open] table {top:0px;
                      #                       opacity:1;}",
               
                      # ".grid-item > details > table       {top:-50px;}",             
                      # ".grid-item > details[open] > table {top:0px;}",
               
                      "summary {list-style: none;}",
               
                      "table {background-color:white;}",
               
                      "tr, td {border:          1px solid black;
                               padding:         5px;
                               border-collapse: collapse;
                               text-align:      center;}",
               
                      "thead tr > :first-child {position: sticky;
                                                z-index: 2; left: 0; top: 0;}",
                      "thead td  {position: sticky;
                                  z-index: 1; top: 0;}",
               
                      "tbody.interne tr > :first-child {position: sticky;
                                                        z-index: 1; left: 0;
                                                        border: 2px solid black;
                                                        background-color: #fff;}",
               
                      ".cell {font-size : 15px;
                              padding: 2px;}",
               
                      ".colzone {text-align:     center;
                                 vertical-align: bottom;
                                 font-size:      15px;
                                 padding :       2px;
                                 writing-mode:   vertical-lr;
                                 transform:      rotate(180deg);}",
               
                      "td.colzone:nth-child(even) {background-color: #FFFFFF;}",
               
                      ".col1 {text-align: left;
                              # width:120px;
                              white-space: nowrap;}",
               
                      ".sup {font-size: 15px;}",
               
                      ".grid-item-titre:nth-child(odd) td.relief:hover {z-index:2;
                                                                        transform: scale(1.2);
                                                                        background-color: #add8e6;}",
                      ".grid-item-titre:nth-child(even) td.relief:hover {z-index:2;
                                                                         transform: scale(1.2);
                                                                         background-color: #ffe4e1;}",
               
                      ".caseRed {color:            rgb(200,0,0);
                                 font-weight:      bold;
                                 background-color: rgb(255,228,196);}",
               
                      ".ligneTot {font-weight:      bold;
                                  background-color: rgb(239,228,176);}",
                  "</style>",
               "</head>")

titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>EPEX <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")

titre2 <- paste0("<p style='font-size:20px'>-- Synth&egrave;se des principales Zones du SI --</p>")

# recuperation des images encodees dans des variables
img_phare_synth_encode.png <- read.csv2(paste0(fileGen,"img_phare_synthese_GrHote_encode.png"))
img_phare_encode <- paste0("<p><img src='data:image/png;base64,",
                            img_phare_synth_encode.png,
                           "'alt='image phare'/></p>")

entete_corps <- ""
if (nrow(df_synth_zone_GrHote)>0){
  for (lg in 1:ncol(df_synth_zone_GrHote)-7-25-6){
    if (lg>=4) {
        entete_corps <- paste0(entete_corps,"<td style='width:30px; 
                                                        border-bottom:0px;
                                                        border-top:0px;' 
                                                 class='ligneTot colzone'>",
                                paste0(str_sub(colnames(df_synth_zone_GrHote)[lg],1,2),"<sup>h</sup>"),
                                "</td>")
        print(colnames(df_synth_zone_GrHote)[lg])
    }
  }
}
rm(lg)

entete <- paste0("<thead>",
                    "<tr>",
                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Tan;'
                           class='ligneTot col1'>Brique /<br /> &nbsp;Zone /<br /> &nbsp;&nbsp;Quartier</td>",

                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:white;'
                           class='ligneTot colzone'>score</td>",

                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Sand;
                                  text-align:center;'
                           class='ligneTot col1'>Vol.<br /> h&ocirc;te<br /> en<br /> alerte</td>",
                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Sand;
                                  text-align:center;'
                           class='ligneTot col1'>Vol.<br /> service<br /> en<br /> alerte</td>",

                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:white;
                                  text-align:center;'
                           class='ligneTot colzone'><i>7<sup>h</sup>-19<sup>h</sup></i><br />
                                                    score</td>",

                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Sand;
                                  text-align:center;'
                           class='ligneTot col1'>Vol.<br /> h&ocirc;te<br /> en<br /> alerte<br />
                                                 <i>7<sup>h</sup>-19<sup>h</sup></i></td>",
                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Sand;
                                  text-align:center;'
                           class='ligneTot col1'>Vol.<br /> service<br /> en<br /> alerte<br />
                                                 <i>7<sup>h</sup>-19<sup>h</sup></i></td>",

                      "<td style='border-bottom:3px solid black; border-right:3px solid black;
                                  background-color:Khaki;
                                  text-align:center;'
                           class='ligneTot col1'>rapport<br /> pres.<br /> alerte</td>",
                    entete_corps,
                    "</tr>",
                 "</thead>")


#####
#####

crea2tabhtml <- function(encadrement,partie_interne_html) {
  partie_externe_html <- paste0(
    "<div class='grid-item'>",
        "<details>",
            "<summary>&nbsp;<i class='fa-solid fa-eye'></i> / 
                            <i class='fa-sharp fa-solid fa-eye-slash'></i>
             </summary>",
            "<table>",
                encadrement,
                    "<tbody class='interne'>",partie_interne_html,"</tbody>",
                encadrement,
            "</table>",
      "</details>",
    "</div>")
  return(partie_externe_html)
}

corps_tab_DC1_z000 <- crea2tabhtml(entete,corps_tab_DC1_z000)
corps_tab_DC2_z000 <- crea2tabhtml(entete,corps_tab_DC2_z000)

corps_tab_DC1_z050 <- crea2tabhtml(entete,corps_tab_DC1_z050)
corps_tab_DC2_z050 <- crea2tabhtml(entete,corps_tab_DC2_z050)

corps_tab_DC1_z100 <- crea2tabhtml(entete,corps_tab_DC1_z100)
corps_tab_DC2_z100 <- crea2tabhtml(entete,corps_tab_DC2_z100)

#####
#####

crea_TabIndicSynthZone_html <- function(color,backgr_color,libzone,zone) {
  # on recupere le numero de ligne de la zone recherchee
  numligne <- as.integer(str_sub(zone,2,2))
  if (numligne>2){numligne<-numligne-2}
  print(numligne)
  
  code_html <- paste0(
  "<div class='grid-item-titre' style='margin-bottom:0px; font-weight:bold; padding: 3px;
                                       border: 1px solid ",color," border-radius:5px 5px;
                                       background-color: ",backgr_color," color: ",color," '>",
    libzone,"<br />",
    "<table style='margin-bottom:0px; font-weight:bold; padding: 3px;
                   border: 1px solid ",color," border-radius:5px 5px;
                   background-color:#fff; color: ",color," '>",
        "<tr>
            <td style='border: 0px;'></td>
            <td style='border: 2px solid black;'>score</td>
            <td style='border: 2px solid black;'>h&ocirc;te <br /> en alerte</td>
            <td style='border: 2px solid black;'>serv. <br /> en alerte</td>
            <td style='border: 2px solid black;'>&Delta; score <br />(<i>J / J-1</i>)</td>
            <td style='border: 2px solid black;'>&Delta; h&ocirc;te <br />en alerte <br />(<i>J / J-1</i>)</td>
            <td style='border: 2px solid black;'>&Delta; serv. <br />en alerte <br />(<i>J / J-1</i>)</td>
        </tr>",
        "<tr>",
            "<td style='border: 2px solid black;'>p&eacute;riode 0<sup>h</sup>-24<sup>h</sup></td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'totscore'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'nbhote'],"</td>",
            "<td style='border-right: 2px solid black;'>",df_synth_zone_GrHote_RES[numligne,'nbserv'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'dif_totscore'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'dif_nbhote'],"</td>",              
            "<td style='border-right: 2px solid black;'>",df_synth_zone_GrHote_RES[numligne,'dif_nbserv'],"</td>",
        "</tr>",
        "<tr>",
            "<td style='border: 2px solid black;'>p&eacute;riode 7<sup>h</sup>-19<sup>h</sup></td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'totscore_jour'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'nbhote_jour'],"</td>",
            "<td style='border-right: 2px solid black;'>",df_synth_zone_GrHote_RES[numligne,'nbserv_jour'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'dif_totscore_jour'],"</td>",
            "<td>",df_synth_zone_GrHote_RES[numligne,'dif_nbhote_jour'],"</td>",
            "<td style='border-right: 2px solid black;'>",df_synth_zone_GrHote_RES[numligne,'dif_nbserv_jour'],"</td>",
        "</tr>",
        "<tr>",
            "<td style='border: 2px solid black;'>",
                "rapport pres. alerte <br />
                 ( <i>7<sup>h</sup>-19<sup>h</sup> / 0<sup>h</sup>-24<sup>h</sup></i> )",
            "</td>",
            "<td colspan=3 style='font-size:30px;
                                  border-right: 2px solid black;
                                  border-bottom: 2px solid black;'
                           class='relief'>",
                df_synth_zone_GrHote_RES[numligne,'rapport'],"</td>",
            "<td colspan=3 style='font-size:30px; 
                                  border-right: 2px solid black;
                                  border-bottom: 2px solid black;'>",
                df_synth_zone_GrHote_RES[numligne,'dif_rapport'],"</td>",
        "</tr>",
    "</table>",
  "</div>")
  return(code_html)
}

IndicSynth_DC1_z000 <- crea_TabIndicSynthZone_html("#006db0;","#add8e6;",
                                                   "DC1 / DMZ & Partenaire ...","02_DMZ_Osny")
IndicSynth_DC2_z000 <- crea_TabIndicSynthZone_html("#fd6c9e;","#ffe4e1;",
                                                   "DC2 / DMZ & Partenaire ...","01_DMZ_Auze")

IndicSynth_DC1_z050 <- crea_TabIndicSynthZone_html("#006db0;","#add8e6;",
                                                   "DC1 / Interne ...","06_ZoneInt_Osny")
IndicSynth_DC2_z050 <- crea_TabIndicSynthZone_html("#fd6c9e;","#ffe4e1;",
                                                   "DC2 / Interne ...","05_ZoneInt_Auze")

IndicSynth_DC1_z100 <- crea_TabIndicSynthZone_html("#006db0;","#add8e6;",
                                                   "DC1 / SIA ...","08_ZoneSIA_Osny")
IndicSynth_DC2_z100 <- crea_TabIndicSynthZone_html("#fd6c9e;","#ffe4e1;",
                                                   "DC2 / SIA ...","07_ZoneSIA_Auze")


#####
#####

# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
              head,
              "<body>",
              
                "<div class='image'>",
              
                    "<div class='titreGen'>",
                        "<p class='moon-title1' style='font-size:30px'>-- Synth&egrave;se des principales Zones du SI --</p>",
                
                        "<p class='moon-title2' style='margin:0px; font-size:40px; line-height: 30%;'>",
                            "<strong class='etat0'>MOON Light</strong>",
                            "<strong class='etat1'>MOnitoring ON Light</strong>",
                        "</p>",
                        img_phare_encode,
                    "</div>",
                    
                "</div>",
                
                
              # "<div>",img_phare_encode,"</div>",
              # 
              # "<table class='tabletitre'>",
              #     "<tr>",
              #         "<td class='tabletitre'>",titre1,titre2,"</td>",
              #     "</tr>",
              # "</table>",
              
              "<div class='partie-cellule'>",
              
              "<p style='color:white; padding:10px;'>
                  date de r&eacute;f&eacute;rence : ",date_ref_lib,
              "</p>",
 
              "<div class='grid-container'>",
              
                IndicSynth_DC1_z000,  IndicSynth_DC2_z000,
                corps_tab_DC1_z000,   corps_tab_DC2_z000,

                IndicSynth_DC1_z050,  IndicSynth_DC2_z050,
                corps_tab_DC1_z050,   corps_tab_DC2_z050,

                IndicSynth_DC1_z100,  IndicSynth_DC2_z100,
                corps_tab_DC1_z100,   corps_tab_DC2_z100,
              
              "</div>",         
              
              "<p style='color:white; padding:10px;'>
                  date de r&eacute;f&eacute;rence : ",date_ref_lib,
              "</p>",
              "<p style='color:white; padding:10px;'>
                  Cr&eacute;ation : <i>4nt0n10 S3D3N0</i>
               </p>",
              
              "</div>",
              
              "</body>",
              "</html>")


######################################################################
######################################################################
######################################################################
######################################################################

setwd(rep_gen)

file <- "CorpsMel_synthese_GrHote.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="CorpsMel_synthese_GrHote.html",append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)
setwd(fileGen)
file.remove(paste0(fileGen,"CorpsMel_synthese_GrHote.html"))
cat(tab,file="CorpsMel_synthese_GrHote.html",append=TRUE)


######################################################################
######################################################################
######################################################################
######################################################################

system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min__Synthese\\EnvoiMail_ViaPowershell_SynthZone.ps1"')











