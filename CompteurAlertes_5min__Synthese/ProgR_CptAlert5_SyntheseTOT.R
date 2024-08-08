
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
# plus besoin de creer le repertoire ci-dessous car cree auparavant par le progR ProgR_CptAlert5_Synthese
# dir.create(rep_gen, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# Creation du repertoire des datas
rep_data <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/")

#second endroit generalise pour envoi du ficher (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/"



# transformation en json
library(jsonlite)

file.remove(paste0(fileGen,"date_json.json"))

date_json <- toJSON(date_ref)
date_json <- prettify(date_json)
# enregistrement du JSON
write_json(date_json,paste0(fileGen,"date_json.json"))

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

######################################################################
######################################################################

AgregGrHotes <- read.csv2(paste0(fileGen,"Agreg_REFERENCE_Zone_GrHote_Hote.csv"),sep=",") %>% 
  select(-zone,-nb_obs) %>% separate("Gr_Hotes", into=c("Gr1","Gr2"), sep = " // ")

# lecture du fichier global
df <- read.csv2(paste0(rep_gen,listing$liste_fich[1]))

vectheure <- c("_00h00","_00h05","_00h10","_00h15","_00h20","_00h25","_00h30","_00h35","_00h40","_00h45","_00h50","_00h55",
               "_01h00","_01h05","_01h10","_01h15","_01h20","_01h25","_01h30","_01h35","_01h40","_01h45","_01h50","_01h55",
               "_02h00","_02h05","_02h10","_02h15","_02h20","_02h25","_02h30","_02h35","_02h40","_02h45","_02h50","_02h55",
               "_03h00","_03h05","_03h10","_03h15","_03h20","_03h25","_03h30","_03h35","_03h40","_03h45","_03h50","_03h55",
               "_04h00","_04h05","_04h10","_04h15","_04h20","_04h25","_04h30","_04h35","_04h40","_04h45","_04h50","_04h55",
               "_05h00","_05h05","_05h10","_05h15","_05h20","_05h25","_05h30","_05h35","_05h40","_05h45","_05h50","_05h55",
               "_06h00","_06h05","_06h10","_06h15","_06h20","_06h25","_06h30","_06h35","_06h40","_06h45","_06h50","_06h55",
               "_07h00","_07h05","_07h10","_07h15","_07h20","_07h25","_07h30","_07h35","_07h40","_07h45","_07h50","_07h55",
               "_08h00","_08h05","_08h10","_08h15","_08h20","_08h25","_08h30","_08h35","_08h40","_08h45","_08h50","_08h55",
               "_09h00","_09h05","_09h10","_09h15","_09h20","_09h25","_09h30","_09h35","_09h40","_09h45","_09h50","_09h55",
               "_10h00","_10h05","_10h10","_10h15","_10h20","_10h25","_10h30","_10h35","_10h40","_10h45","_10h50","_10h55",
               "_11h00","_11h05","_11h10","_11h15","_11h20","_11h25","_11h30","_11h35","_11h40","_11h45","_11h50","_11h55",
               "_12h00","_12h05","_12h10","_12h15","_12h20","_12h25","_12h30","_12h35","_12h40","_12h45","_12h50","_12h55",
               "_13h00","_13h05","_13h10","_13h15","_13h20","_13h25","_13h30","_13h35","_13h40","_13h45","_13h50","_13h55",
               "_14h00","_14h05","_14h10","_14h15","_14h20","_14h25","_14h30","_14h35","_14h40","_14h45","_14h50","_14h55",
               "_15h00","_15h05","_15h10","_15h15","_15h20","_15h25","_15h30","_15h35","_15h40","_15h45","_15h50","_15h55",
               "_16h00","_16h05","_16h10","_16h15","_16h20","_16h25","_16h30","_16h35","_16h40","_16h45","_16h50","_16h55",
               "_17h00","_17h05","_17h10","_17h15","_17h20","_17h25","_17h30","_17h35","_17h40","_17h45","_17h50","_17h55",
               "_18h00","_18h05","_18h10","_18h15","_18h20","_18h25","_18h30","_18h35","_18h40","_18h45","_18h50","_18h55",
               "_19h00","_19h05","_19h10","_19h15","_19h20","_19h25","_19h30","_19h35","_19h40","_19h45","_19h50","_19h55",
               "_20h00","_20h05","_20h10","_20h15","_20h20","_20h25","_20h30","_20h35","_20h40","_20h45","_20h50","_20h55",
               "_21h00","_21h05","_21h10","_21h15","_21h20","_21h25","_21h30","_21h35","_21h40","_21h45","_21h50","_21h55",
               "_22h00","_22h05","_22h10","_22h15","_22h20","_22h25","_22h30","_22h35","_22h40","_22h45","_22h50","_22h55",
               "_23h00","_23h05","_23h10","_23h15","_23h20","_23h25","_23h30","_23h35","_23h40","_23h45","_23h50","_23h55")

date_ref_court <- str_sub(date_ref,3,10)
vectheure_df <- crossing(date_ref_court,vectheure) %>% as.data.frame() %>%
  unite(date_ref_court:vectheure,col="moment",sep="", na.rm = TRUE, remove = TRUE)
# 288 Obs normalement ici car 12 creneaux de 5 min par heure x 24 = 288


# restriction variables...
df_TOT <- df %>% select(zone,Gr_Hotes,name_host,description,moment,last_hard_state) %>% 
  unite(zone:description,col="tot",sep=" // ", na.rm = TRUE, remove = FALSE) %>% 
  relocate(tot)

df_TOT_list <- df_TOT %>% select(-tot) %>% group_by(zone,Gr_Hotes,name_host,description) %>% 
  summarise(nb_alert=n()) %>% ungroup() %>% as.data.frame() %>% select(-nb_alert) %>% 
  unite(zone:description,col="moment",sep=" // ", na.rm = TRUE, remove = TRUE) %>% 
  unlist(use.names=FALSE)

df_bis <- df %>% select(zone,Gr_Hotes,name_host,description)
vectheure_df <- crossing(df_bis,vectheure_df) %>% as.data.frame()
#vectheure_df <- crossing(df_TOT_list,vectheure_df) %>% as.data.frame() %>% rename(tot=df_TOT_list)
gc()

df_TOT <- df_TOT %>% full_join(vectheure_df, by=c("zone","Gr_Hotes","name_host","description","moment")) %>% 
  arrange(zone,Gr_Hotes,name_host,description,moment) %>% 
  unite(zone:description,col="tot",sep=" // ", na.rm = TRUE, remove = FALSE) %>% 
  relocate(tot)
#df_TOT <- df_TOT %>% full_join(vectheure_df, by=c("tot","moment")) %>% arrange(tot, moment)
gc()

# reconnaissance de certaines maintenances recurrentes par insertion codes 4 ou 5 puis 
# via 2 couleurs bleues
df_TOT <- df_TOT %>%
  # QoE-APIM-Requete-DL 
  mutate(last_hard_state=ifelse(str_sub(description,1,19)=="QoE-APIM-Requete-DL" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,14) %in% c("02h30","02h35","02h40","02h45","02h50","02h55",
                                                               "03h00","03h05","03h10","03h15","03h20","03h25"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,19)=="QoE-APIM-Requete-DL" & is.na(last_hard_state) &
                                  str_sub(moment,10,14) %in% c("02h30","02h35","02h40","02h45","02h50","02h55",
                                                               "03h00","03h05","03h10","03h15","03h20","03h25"),
                                5,last_hard_state)) %>% 
  # QoE-COLTRANE 
  mutate(last_hard_state=ifelse(str_sub(description,1,12)=="QoE-COLTRANE" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,12)=="QoE-COLTRANE" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h"),
                                5,last_hard_state)) %>% 
  # QoE-FIGARO
  mutate(last_hard_state=ifelse(str_sub(description,1,20)=="QoE-FIGARO-Catalogue" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("00h","23h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-FIGARO-Catalogue" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("00h","23h"),
                                5,last_hard_state)) %>% 
  # QoE-HARMONICA 
  mutate(last_hard_state=ifelse(str_sub(description,1,13)=="QoE-HARMONICA" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("20h","21h","22h","23h","00h","01h","02h","03h","04h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,13)=="QoE-HARMONICA" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("20h","21h","22h","23h","00h","01h","02h","03h","04h"),
                                5,last_hard_state)) %>% 
  # QoE-RORCAL
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-RORCAL" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("05h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-RORCAL" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("05h"),
                                5,last_hard_state)) %>% 
  # QoE-SIRUS
  mutate(last_hard_state=ifelse(str_sub(description,1,9)=="QoE-SIRUS" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h","05h","06h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,9)=="QoE-SIRUS" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h","05h","06h"),
                                5,last_hard_state)) %>% 
  # QoE-WS-Atis-BRPP
  mutate(last_hard_state=ifelse(str_sub(description,1,16)=="QoE-WS-Atis_BRPP" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,14) %in% c("01h30","01h35","01h40","01h45","01h50","01h55",
                                                               "02h00","02h05","02h10","02h15","02h20","02h25",
                                                               "02h30","02h35","02h40","02h45","02h50","02h55",
                                                               "03h00","03h05","03h10","03h15","03h20","03h25",
                                                               "03h30","03h35","03h40","03h45","03h50","03h55"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,16)=="QoE-WS-Atis_BRPP" & is.na(last_hard_state) &
                                  str_sub(moment,10,14) %in% c("01h30","01h35","01h40","01h45","01h50","01h55",
                                                               "02h00","02h05","02h10","02h15","02h20","02h25",
                                                               "02h30","02h35","02h40","02h45","02h50","02h55",
                                                               "03h00","03h05","03h10","03h15","03h20","03h25",
                                                               "03h30","03h35","03h40","03h45","03h50","03h55"),
                                5,last_hard_state))

# suppression des observations identiques... !! important
df_TOT <- df_TOT[!duplicated(df_TOT), ]

df_TOT <- df_TOT %>% pivot_wider(names_from=moment, values_from = last_hard_state) %>%
  as.data.frame() %>% filter(!is.na(description))
df_TOT[is.na(df_TOT)] <- 0 


# TEST DIVERS....
# typeof(df_TOT$`24-06-08_00h00`)
# df_TOT_test <- df_TOT
# for (col in 6:ncol(df_TOT_test)){
#   long <- length(df_TOT_test[[col]])
#   print(paste0(col," - ",long))
#   df_TOT_test[[col]] <- unlist(df_TOT_test[[col]])
# }
# df_TOT$`24-06-08_00h00` <- unlist(df_TOT$`24-06-08_00h00`)
# # for (col in 6:ncol(df_TOT_test)){
# for (col in 277:279){
#   print(paste0("column:",col))
#   for (line in 1:nrow(df_TOT_test)){
#     print(typeof(df_TOT_test[line,col]))
#     if (length(df_TOT_test[line,col])!=1){
#       print(paste0(col," - ",line))
#     }
#     # print(df_TOT_test[line,col])
#     # # if (length(df_TOT_test[line:col])!=1){
#     # print(paste0(col," - ",line))
#     # }
#   }
# }



listCol <- colnames(df_TOT) %>% as.data.frame() %>% rename(listCol=".") %>% 
  filter(!(listCol %in% c("tot","zone","Gr_Hotes","name_host","description"))) %>% mutate(listCol=str_sub(listCol,9,14)) %>% 
  filter(!(str_sub(listCol,5,6) %in% c("05","10","15","20","25",
                                       "35","40","45","50","55"))) %>% 
  mutate(listCol=paste0(" ",str_sub(listCol,2,6)," "))

df_TOT2 <- df_TOT %>% select(-tot)

df_TOT2 <- df_TOT2 %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

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
  # pour raccourcir le libelle des services de type pool
  df_out <- df_out %>% 
    mutate(description = ifelse( str_sub(description,1,13)=="Pool-/Common/" & str_to_lower(str_sub(name_host,1,9))=="pd-hlb01-",
                                 str_sub(description,14,str_length(description)),description))
  # plus traitement particulier pour mieux identifier la zone Partenaire...
  df_out <- df_out %>% 
    mutate(Gr1_libcour = ifelse(zone %in% c("03_Part_Auze","04_Part_Osny"),
                                paste0(Gr1_libcour," (zone Part.)"),Gr1_libcour))
  return(df_out)
}

df_TOT2 <- renommageGrHote(df_TOT2) %>% select(-Gr1) %>% rename(Gr1=Gr1_libcour)
df_TOT2 <- df_TOT2 %>% mutate(Gr1 = ifelse(is.na(Gr1),"",Gr1),Gr2 = ifelse(is.na(Gr2),"",Gr2))

# ecriture du dataframe QoE dans un .csv aux 2 endroits
write.csv2(df_TOT,paste0(rep_gen,"tableau_TOT.csv"), row.names = FALSE)
write.csv2(df_TOT2,paste0(fileGen,"tableau_TOT.csv"), row.names = FALSE)


###########################
# Transformation en JSON...

df_TOT2_JSON <- df_TOT2

listCol_JSON <- colnames(df_TOT2_JSON) %>% as.data.frame() %>% 
  rename(listCol=".") %>% filter(!(listCol %in% c("zone","Gr1","Gr2","name_host","description"))) %>% 
  separate("listCol",into=c("jour","heure"),sep="_") %>% select(-jour) %>%
  mutate(heure=paste0("_",heure)) %>% as.vector() %>% unlist() %>% unname()

colnames(df_TOT2_JSON)[6:ncol(df_TOT2_JSON)] <- c(listCol_JSON)

df_TOT2_JSON <- df_TOT2_JSON %>% 
  pivot_longer(c(listCol_JSON),names_to = "heure") %>%
  mutate(status="status") %>% select(-heure) %>% 
  pivot_wider(names_from="status",values_from = "value", values_fn=list) %>% 
  as.data.frame()

library(jsonlite)
df_TOT2_json <- toJSON(df_TOT2_JSON)
df_TOT2_json <- prettify(df_TOT2_json)

# enregistrement du JSON
write_json(df_TOT2_json,paste0(rep_gen,"df_global.json"))
# suppression de lancien fichier de la veille present dans file_gen puis ecriture
file.remove(paste0(fileGen,"df_global.json"))
write_json(df_TOT2_json,paste0(fileGen,"df_global.json"))


################################################
################################################
################################################
# pour le tableau simplifie a mettre dans le mel
listCol_2 <- colnames(df_TOT) %>% as.data.frame() %>% 
  rename(listCol=".") %>% filter(!(listCol %in% c("tot","zone","Gr_Hotes","name_host","description")))

# listCol_2[[1]][1]

vec <- unlist(listCol_2[[1]])
df_TOT_2 <- df_TOT %>%  
  pivot_longer(cols = all_of(vec), values_to = "status") %>% 
  mutate(tr30min = case_when(
    str_sub(name,13,13) %in% c("0","1","2") ~ paste0("_",str_sub(name,10,12),"00"),
    str_sub(name,13,13) %in% c("3","4","5") ~ paste0("_",str_sub(name,10,12),"30"),
    TRUE ~ "X")) %>% select(-name) %>% unique() %>%
  mutate(status_car=paste0("_",as.character(status))) %>% select(-status) %>%
  pivot_wider(names_from=tr30min, values_from = status_car,values_fn = list) %>% as.data.frame() %>% 
  select(-tot)

# length(toString(unlist(df_TOT_2[[9]][1])))
gc()

for (i in 1:nrow(df_TOT_2)){
  for (j in 5:ncol(df_TOT_2)){
    df_TOT_2[i,j] <- toString(unlist(df_TOT_2[[j]][i]))
  }
}
gc()

for (i in 1:nrow(df_TOT_2)){
  for (j in 5:ncol(df_TOT_2)){
    if (str_detect(df_TOT_2[i,j],"_2")) {
      df_TOT_2[i,j] <- 2
    } else if (str_detect(df_TOT_2[i,j],"_1")) {
      df_TOT_2[i,j] <- 1
    } else if (str_detect(df_TOT_2[i,j],"_3")) {
      df_TOT_2[i,j] <- 3
    } else if (str_detect(df_TOT_2[i,j],"_4")) {
      df_TOT_2[i,j] <- 4
    } else if (str_detect(df_TOT_2[i,j],"_5")) {
      df_TOT_2[i,j] <- 5
    } else {
      df_TOT_2[i,j] <- 0
    }
  }
}
rm(vec,i,j)
gc()

df_TOT_2 <- df_TOT_2 %>% left_join(AgregGrHotes, by=c("name_host")) %>% 
  relocate("Gr1",.before="name_host") %>% relocate("Gr2",.before="name_host") %>% 
  select(-Gr_Hotes) %>% as.data.frame()

df_TOT_2 <- renommageGrHote(df_TOT_2) %>% select(-Gr1) %>% rename(Gr1=Gr1_libcour)
df_TOT_2 <- df_TOT_2 %>% mutate(Gr1 = ifelse(is.na(Gr1),"",Gr1), Gr2 = ifelse(is.na(Gr2),"",Gr2))

###########################
# Transformation en JSON...

df_TOT_2_JSON <- df_TOT_2

# listCol_JSON_2 <- colnames(df_TOT_2_JSON) %>% as.data.frame() %>% 
#   rename(listCol=".") %>% filter(!(listCol %in% c("zone","Gr1","Gr2","name_host","description"))) %>% 
#   separate("listCol",into=c("jour","heure"),sep="_") %>% select(-jour) %>%
#   mutate(heure=paste0("_",heure)) %>% as.vector() %>% unlist() %>% unname()

listCol_JSON_2 <- colnames(df_TOT_2_JSON) %>% as.data.frame() %>% 
  rename(listCol=".") %>% filter(!(listCol %in% c("zone","Gr1","Gr2","name_host","description"))) %>%
  unlist() %>% unname()

colnames(df_TOT_2_JSON)[6:ncol(df_TOT_2_JSON)] <- c(listCol_JSON_2)

df_TOT_2_JSON <- df_TOT_2_JSON %>% 
  pivot_longer(c(listCol_JSON_2),names_to = "heure") %>%
  mutate(status="status") %>% select(-heure) %>% mutate(value=as.integer(value)) %>% 
  pivot_wider(names_from="status",values_from = "value", values_fn=list) %>% 
  as.data.frame()

library(jsonlite)
df_TOT_2_json <- toJSON(df_TOT_2_JSON)
df_TOT_2_json <- prettify(df_TOT_2_json)

# enregistrement du JSON
write_json(df_TOT_2_json,paste0(rep_gen,"df_global_heure.json"))
# suppression de lancien fichier de la veille present dans file_gen puis ecriture
file.remove(paste0(fileGen,"df_global_heure.json"))
write_json(df_TOT_2_json,paste0(fileGen,"df_global_heure.json"))


######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

head <- paste0("<head>",
               "<meta charset='utf-8'/>",
               "<title>ENVOI INFOS SYNTHESE GLOBALE</title>",
               "<style>",
               ".tabletitre {border:         none;
                             text-align:     left;
                             vertical-align: top;}",
               
               "tr, td {border:          1px solid black;
                        padding:         5px;
                        border-collapse: collapse;
                        text-align:      center;}",
               
               "thead tr > :first-child  {position: sticky; z-index: 2; left: 0; top: 0;}",
               "thead tr > :nth-child(2) {position: sticky; z-index: 2; left: 100px; top: 0;}",
               "thead tr > :nth-child(3) {position: sticky; z-index: 2; left: 200px; top: 0;}",
               "thead tr > :nth-child(4) {position: sticky; z-index: 2; left: 270px; top: 0;}",
               "thead tr > :nth-child(5) {position: sticky; z-index: 2; left: 400px; top: 0;}",
               "thead td  {position: sticky;
                           z-index: 1; top: 0;}",
               "tbody tr > :first-child  {position: sticky; z-index: 1; left: 0;
                                          border: 2px solid black; background-color: #fff;}",
               "tbody tr > :nth-child(2) {position: sticky; z-index: 1; left: 100px;
                                          border: 2px solid black; background-color: #fff;}",
               "tbody tr > :nth-child(3) {position: sticky; z-index: 1; left: 200px;
                                          border: 2px solid black; background-color: #fff;}",
               "tbody tr > :nth-child(4) {position: sticky; z-index: 1; left: 270px;
                                          border: 2px solid black; background-color: #fff;}",
               "tbody tr > :nth-child(5) {position: sticky; z-index: 1; left: 400px;
                                          border: 2px solid black; background-color: #fff;}",
               
               ".cell {font-size : 5px;
                       padding: 2px;}",
               
               ".colzone {text-align:     center;
                          vertical-align: bottom;
                          font-size:      15px;
                          padding :       2px;
                          writing-mode:   vertical-lr;
                          transform:      rotate(180deg);}",
               
               "td.colzone:nth-child(even) {background-color: #FFFFFF;}",
               
               ".col1 {text-align: left;
                       width:100px;
                       white-space: nowrap;}",
               
               ".sup {font-size: 15px;}",
               
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

titre2 <- paste0("<p style='font-size:20px'>-- Synth&egrave;se Globale (niv. fin) --</p>")

entete_corps <- ""
if (nrow(listCol)>0){
  for (lg in 1:nrow(listCol)){
    entete_corps <- paste0(entete_corps,"<td style='width:30px; 
                                                    border-bottom:0px;
                                                    border-top:0px;' 
                                             colspan=6 class='ligneTot colzone'>",
                           listCol$listCol[lg],
                           "</td>")
  }
}
rm(lg)

entete <- paste0("<thead>",
                 "<tr>",
                 "<td style='border-bottom:3px solid black; border-right :3px solid black;' 
                      class='ligneTot col1'>Zone SI</td>",
                 "<td style='border-bottom:3px solid black; border-right :3px solid black;' 
                      class='ligneTot col1'>Brique</td>",
                 "<td style=' border-bottom:3px solid black; border-right :3px solid black;' 
                      class='ligneTot col1'>Appli</td>",
                 "<td style='border-bottom:3px solid black; border-right :3px solid black;' 
                      class='ligneTot col1'>H&ocirc;te</td>",
                 "<td style='border-bottom:3px solid black; border-right :3px solid black;' 
                      class='ligneTot col1'>Service</td>",
                 entete_corps,
                 "</tr>",
                 "</thead>")

corps_tab <- ""

if (nrow(df_TOT2)>0){
  # for (i in 1:nrow(df_TOT2)){
  for (i in 1:10){  # restriction sur les 5 premieres lignes...
    for (j in 1:ncol(df_TOT2)){
      print(paste0(i," et ",j," : ",df_TOT2[i,j]))
      if ( j==1 ){
        corps_tab <- paste0(corps_tab,"<tr>","<td class='col1'>",df_TOT2[i,j],"</td>")
      }
      if ( j %in% c(2,3,4,5) ){
        corps_tab <- paste0(corps_tab,"<td class='col1'>",df_TOT2[i,j],"</td>")
      }
      if ( !(j %in% c(1,2,3,4,5)) ){
        if (df_TOT2[i,j]==1){
          corps_tab <- paste0(corps_tab,"<td style='background-color:orange;' class='cell'>",df_TOT2[i,j],"</td>")
        }
        if (df_TOT2[i,j]==2){
          corps_tab <- paste0(corps_tab,"<td style='background-color:red;' class='cell'>",df_TOT2[i,j],"</td>")
        }
        if (df_TOT2[i,j]==3){
          corps_tab <- paste0(corps_tab,"<td style='background-color:grey;' class='cell'>",df_TOT2[i,j],"</td>")
        } 
        if (df_TOT2[i,j]==4){
          corps_tab <- paste0(corps_tab,"<td style='background-color:#6495ed;' class='cell'>",df_TOT2[i,j],"</td>")
        }  
        if (df_TOT2[i,j]==5){
          corps_tab <- paste0(corps_tab,"<td style='background-color:#87ceeb;' class='cell'>",df_TOT2[i,j],"</td>")
        } 
        if (!(df_TOT2[i,j] %in% c(1,2,3,4,5)) & j %in% c(    6, 18, 30, 42, 54, 66,
                                                            78, 90,102,114,126,138,
                                                           150,162,174,186,198,210,
                                                           222,234,246,258,270,282)){
          corps_tab <- paste0(corps_tab,"<td style='border:0px; 
                                                    border-left:1px solid black;' 
                                                    class='cell'>",df_TOT2[i,j],"</td>")
        } else if (!(df_TOT2[i,j] %in% c(1,2,3,4,5)) & j==293){
          corps_tab <- paste0(corps_tab,"<td style='border:0px;
                                                    border-right:1px solid black;'
                                                    class='cell'>",df_TOT2[i,j],"</td>")          
        } else if (!(df_TOT2[i,j] %in% c(1,2,3,4,5))){
          corps_tab <- paste0(corps_tab,"<td style='border:0px;' class='cell'>",df_TOT2[i,j],"</td>")
        }   
        
      }
      if ( j==ncol(df_TOT2) ){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=293>absence de tableau</td></tr>")
}
rm(i,j)


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
              head,
              "<body>",
              
              "<table class='tabletitre'>",
              "<tr>",
              "<td class='tabletitre'>",#titre1,
                                        titre2,"</td>",
              "</tr>",
              "</table>",
              
              "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
              "<table>", #style='border-collapse:collapse;' 
                  entete, 
                  "<tbody>",
                      corps_tab,
                  "</tbody>",
                  entete, 
              "</table>",
              
              "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
              "</body>",
              "</html>")

######################################################################
######################################################################

# setwd(rep_gen)

# file <- "CorpsMel_SyntheseTOT.html"
# file.create(file, showWarnings = TRUE)
# cat(tab,file="CorpsMel_SyntheseTOT.html",append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)
# setwd(fileGen)
# file.remove(paste0(fileGen,"CorpsMel_SyntheseTOT.html"))
# cat(tab,file="CorpsMel_SyntheseTOT.html",append=TRUE)


# fichier zip
# file.remove(paste0(fileGen,"CorpsMel_SyntheseTOT_Docs.zip"))
# zip("CorpsMel_SyntheseQoE_Docs.zip","CorpsMel_SyntheseTOT.html")
# zip("CorpsMel_SyntheseQoE_Docs.zip","tableau_TOT.csv")

######################################################################
######################################################################

# definition du tableau simplifie dans le corps du mel

entete_corps_MEL <- ""
if (nrow(listCol)>0){
  for (lg in 1:nrow(listCol)){
    entete_corps_MEL <- paste0(entete_corps_MEL,"<td style='width:20px; 
                                                            border-bottom:0px;
                                                            border-top:0px;' 
                                                     class='ligneTot colzone'>",
                               listCol$listCol[lg],
                               "</td>")
  }
}
rm(lg)

entete_MEL <- paste0("<thead>",
                     "<tr>",
                        "<td style='width:120px; border-bottom:3px solid black; border-right :3px solid black;' 
                             class='ligneTot col1'>Zone SI</td>",
                        "<td style='width:200px; border-bottom:3px solid black; border-right :3px solid black;' 
                             class='ligneTot col1'>Brique</td>",
                        "<td style='width:200px; border-bottom:3px solid black; border-right :3px solid black;' 
                             class='ligneTot col1'>Appli</td>",
                        "<td style='width:200px; border-bottom:3px solid black; border-right :3px solid black;' 
                             class='ligneTot col1'>H&ocirc;te</td>",
                        "<td style='border-bottom:3px solid black; border-right :3px solid black;' 
                             class='ligneTot col1'>Service</td>",
                        entete_corps_MEL,
                     "</tr>",
                     "</thead>")

corps_tab_MEL <- ""

if (nrow(df_TOT_2)>0){
  # for (i in 1:nrow(df_TOT_2)){
    for (i in 1:10){  # restriction sur les 5 premieres lignes...
    for (j in 1:ncol(df_TOT_2)){
      print(paste0(i," et ",j," : ",df_TOT_2[i,j]))
      if ( j==1 ){
        corps_tab_MEL <- paste0(corps_tab_MEL,"<tr>","<td class='col1'>",df_TOT_2[i,j],"</td>")
      }
      if ( j %in% c(2,3,4,5) ){
        corps_tab_MEL <- paste0(corps_tab_MEL,"<td class='col1'>",df_TOT_2[i,j],"</td>")
      }
      if ( !(j %in% c(1,2,3,4,5)) ){
        if (df_TOT_2[i,j]==1){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:orange;' class='cell'>",df_TOT_2[i,j],"</td>")
        } else if (df_TOT_2[i,j]==2){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:red;' class='cell'>",df_TOT_2[i,j],"</td>")
        } else if (df_TOT_2[i,j]==3){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#808080;' class='cell'>",df_TOT_2[i,j],"</td>")
        } else if (df_TOT_2[i,j]==4){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#6495ed;' class='cell'>",df_TOT_2[i,j],"</td>")
        } else if (df_TOT_2[i,j]==5){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#87ceeb;' class='cell'>",df_TOT_2[i,j],"</td>")
        } else if (!(df_TOT_2[i,j] %in% c(1,2,3,4,5))){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='border:0px;' class='cell'>",df_TOT_2[i,j],"</td>")
        }   
      }
      if ( j==ncol(df_TOT_2) ){corps_tab_MEL <- paste0(corps_tab_MEL,"</tr>")}
    }
  }
} else {
  corps_tab_MEL <- paste0(corps_tab_MEL,"<tr><td colspan=52>absence de tableau</td></tr>")
}
rm(i,j)


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab_MEL <- paste0("<!DOCTYPE html>",
                  "<html>",
                  head,
                  "<body>",
                  
                  "<table class='tabletitre'>",
                      "<tr>",
                          "<td class='tabletitre'>",#titre1,
                                                    titre2,"</td>",
                      "</tr>",
                  "</table>",
                  
                  "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
                  
                  "<table>", #style='border-collapse:collapse;' 
                      entete_MEL, 
                      "<tbody>",
                          corps_tab_MEL,
                      "</tbody>",
                      entete_MEL, 
                  "</table>",
                  
                  "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
                  
                  "</body>",
                  "</html>")

######################################################################
######################################################################

# setwd(rep_gen)

# file <- "CorpsMel_SyntheseTOT_MEL.html"
# file.create(file, showWarnings = TRUE)
# cat(tab_MEL,file="CorpsMel_SyntheseTOT_MEL.html",append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)
# setwd(fileGen)
# file.remove(paste0(fileGen,"CorpsMel_SyntheseTOT_MEL.html"))
# cat(tab_MEL,file="CorpsMel_SyntheseTOT_MEL.html",append=TRUE)


######################################################################
######################################################################

setwd(fileGen)

# creation fichier zip
file.remove(paste0(fileGen,"CorpsMel_SyntheseTOT_Docs.zip"))
zip("CorpsMel_SyntheseTOT_Docs.zip","date_json.json")
zip("CorpsMel_SyntheseTOT_Docs.zip","df_global.json")
zip("CorpsMel_SyntheseTOT_Docs.zip","df_global_heure.json")


system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min__Synthese\\EnvoiMail_ViaPowershell_SynthTOT.ps1"')

