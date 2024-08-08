
library(RMariaDB)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

# decalage egal de 2 en heure en ete et 1 en heure en hiver
decalage_heure <- 2

date_jour <- Sys.time()
part_heure <- as.character(hour(date_jour))
part_minute <- as.character(minute(date_jour))

if (str_length(part_heure)==1){part_heure <- paste0("0",part_heure)}
if (str_length(part_minute)==1){part_minute <- paste0("0",part_minute)}
lib_time <- paste0("_",part_heure,"h",part_minute)

# pour tester
# lib_time <- "_01h30"

rm(part_heure,part_minute)

part_date  <- as.character(Sys.Date()-ddays(1))
part_date2 <- str_remove_all(part_date,"-")

# pour tester...
# part_date2 <- "20240530"

rep_fich <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/HistoCentreon_User/Fichiers_date_",part_date2,"/")
setwd(rep_fich)

# rep_sas <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_vers_Z050/")


##########
##########
# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_fich) %>% as.data.frame() %>% 
     rename(liste_fich=".") %>% arrange(liste_fich)

listing_centreonSIP <- listing %>%
     filter(str_detect(liste_fich,"CentreonSIP_User") & str_detect(liste_fich,"csv")) %>%
     arrange(liste_fich)


lib_time <- "_01h05"

##########
##########
# PARTIE 1
# copie de tous les fichiers dans le repertoire de reference

for (i in 1:nrow(listing_centreonSIP)){
     cat(paste0("numero ",i," ",listing_centreonSIP$liste_fich[i],"\n"))
     # concatenation des fichiers de type datareq
     if (i==1) {
          file.copy(paste0(rep_fich,listing_centreonSIP$liste_fich[i]),
                    paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv"))
     } else {
          df_centreonSIP_jr <- read.csv2(paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv"))
          df_copy <- read.csv2(paste0(rep_fich,listing_centreonSIP$liste_fich[i]))
          df_centreonSIP_jr <- rbind(df_centreonSIP_jr,df_copy)
          write.csv2(df_centreonSIP_jr,paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv"),row.names=FALSE)
     }
     # insertion du fichier unitaire dans un .zip puis suppression
     #zip(paste0("df_centreonSIP_user_jr_",part_date,lib_time,".zip"),
     #    paste0(listing_centreonSIP$liste_fich[i]))
     # file.remove(paste0(rep_fich,listing_datareq$liste_fich[i]))
}
rm(i,df_copy)

library(zip)
for (i in 1:nrow(listing_centreonSIP)){
  # cat(paste0(i,"/n"))
  print(i)
  if (i==1) {
    zip(paste0("df_centreonSIP_user_jr_",part_date,lib_time,".zip"),
        paste0(listing_centreonSIP$liste_fich[i]))
  } else {
    zip_append(paste0("df_centreonSIP_user_jr_",part_date,lib_time,".zip"),
        paste0(listing_centreonSIP$liste_fich[i]))
  }
}


# nettoyage RAM...
gc()

# export...
# lib_time <- "_01h30"

# TRANSFERT des fichiers dans le repertoire sas vers la z050
# df_centreonSIP_jr <- read.csv2(paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv"))
# 
# write.csv2(df_centreonSIP_jr,
#            paste0(rep_sas,"df_centreonSIP_user_jr_",part_date,lib_time,".csv"),
#            row.names=FALSE)

##############
# ANALYSES....
##############

lib_time <- "_01h05"

Agreg <- read.csv2(paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv")) %>% 
  arrange(contact_alias,session_id,POSIXct_moment_extract)

# library(dplyr)
# library(tidyr)
# library(stringr)
# part_date <- "2024-01-07"
# lib_time <- "_01h05"
# rep_fich <- "C:/Users/R_ycg8l6/ProgrammesR_Gepex/HistoCentreon_User/Fichiers_date_20240107/"
# Agreg <- read.csv2(paste0(rep_fich,"df_centreonSIP_user_jr_",part_date,lib_time,".csv")) %>%
#      arrange(contact_alias,session_id,POSIXct_moment_extract)

Agreg2 <- Agreg %>% filter(diff_tps_sec<=900) %>% # a partir de 15min soit 900s, on considere que ce n'est plus actif
  mutate(jour=str_sub(POSIXct_moment_extract,1,10),
         hm_connect=str_replace(str_sub(POSIXct_moment_extract,12,16),":","m")) %>% 
  mutate(hm_connect=paste0("h",hm_connect),
         statut="X")

Agreg3 <- Agreg2 %>% select(contact_name,session_id,jour,hm_connect,statut) %>% 
  arrange(jour,hm_connect,contact_name,session_id,statut)


heure <- c("h00","h01","h02","h03","h04","h05","h06","h07","h08","h09","h10","h11",
           "h12","h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23")
minut <- c("m00","m05","m10","m15","m20","m25","m30","m35","m40","m45","m50","m55")
moment <- crossing(heure,minut) %>% 
  mutate(cle="user",hm_connect=paste0(heure,minut), statut="_") %>% 
  select(-heure,-minut)
moment <- pivot_wider(moment, names_from=hm_connect, values_from = statut) %>%
  as.data.frame() %>% arrange(cle)
rm(heure,minut)

liste_user <- Agreg3 %>% group_by(jour,contact_name) %>% summarise(duree_min=n()*5) %>% 
  ungroup() %>% as.data.frame() %>% mutate(cle="user") %>% 
  right_join(moment,by=c("cle"))

Agreg4 <- pivot_wider(Agreg3, names_from=hm_connect, values_from = statut) %>%
  as.data.frame() %>% arrange(contact_name) %>% select(-jour)

Agreg5 <- liste_user %>% right_join(Agreg4, by=c("contact_name"))

# retrait des colonnes en doublons se finissant par .x
colonnes <- colnames(Agreg5) %>% as.data.frame() %>% rename(listcol=".")
colonnesX <- colonnes %>% filter(str_detect(listcol,".x")) %>% as.vector()
Agreg5 <- Agreg5 %>% select(-colonnesX$listcol)
# renommage des colonnes en doublons se finissant par .y
colonnes <- colnames(Agreg5) %>% as.data.frame() %>% rename(listcol=".") %>% 
  mutate(listcol=str_replace(listcol,".y",""))
colnames(Agreg5) <- colonnes$listcol

colonnes <- colnames(Agreg5) %>% as.data.frame() %>% rename(listcol=".") %>% arrange(listcol)

Agreg5 <- Agreg5 %>% relocate(colonnes$listcol) %>% 
  relocate(jour,contact_name,session_id,duree_min) %>% select(-cle)

## modification globale sur les colonnes...
for (i in 1:nrow(Agreg5)){
  #h00
  if (is.na(Agreg5$h00m00[i])){ Agreg5$h00m00[i]="_" }; if (is.na(Agreg5$h00m05[i])){ Agreg5$h00m05[i]="_" }
  if (is.na(Agreg5$h00m10[i])){ Agreg5$h00m10[i]="_" }; if (is.na(Agreg5$h00m15[i])){ Agreg5$h00m15[i]="_" }
  if (is.na(Agreg5$h00m20[i])){ Agreg5$h00m20[i]="_" }; if (is.na(Agreg5$h00m25[i])){ Agreg5$h00m25[i]="_" }
  if (is.na(Agreg5$h00m30[i])){ Agreg5$h00m30[i]="_" }; if (is.na(Agreg5$h00m35[i])){ Agreg5$h00m35[i]="_" }
  if (is.na(Agreg5$h00m40[i])){ Agreg5$h00m40[i]="_" }; if (is.na(Agreg5$h00m45[i])){ Agreg5$h00m45[i]="_" }
  if (is.na(Agreg5$h00m50[i])){ Agreg5$h00m50[i]="_" }; if (is.na(Agreg5$h00m55[i])){ Agreg5$h00m55[i]="_" }
  #h01
  if (is.na(Agreg5$h01m00[i])){ Agreg5$h01m00[i]="_" }; if (is.na(Agreg5$h01m05[i])){ Agreg5$h01m05[i]="_" }
  if (is.na(Agreg5$h01m10[i])){ Agreg5$h01m10[i]="_" }; if (is.na(Agreg5$h01m15[i])){ Agreg5$h01m15[i]="_" }
  if (is.na(Agreg5$h01m20[i])){ Agreg5$h01m20[i]="_" }; if (is.na(Agreg5$h01m25[i])){ Agreg5$h01m25[i]="_" }
  if (is.na(Agreg5$h01m30[i])){ Agreg5$h01m30[i]="_" }; if (is.na(Agreg5$h01m35[i])){ Agreg5$h01m35[i]="_" }
  if (is.na(Agreg5$h01m40[i])){ Agreg5$h01m40[i]="_" }; if (is.na(Agreg5$h01m45[i])){ Agreg5$h01m45[i]="_" }
  if (is.na(Agreg5$h01m50[i])){ Agreg5$h01m50[i]="_" }; if (is.na(Agreg5$h01m55[i])){ Agreg5$h01m55[i]="_" }
  #h02
  if (is.na(Agreg5$h02m00[i])){ Agreg5$h02m00[i]="_" }; if (is.na(Agreg5$h02m05[i])){ Agreg5$h02m05[i]="_" }
  if (is.na(Agreg5$h02m10[i])){ Agreg5$h02m10[i]="_" }; if (is.na(Agreg5$h02m15[i])){ Agreg5$h02m15[i]="_" }
  if (is.na(Agreg5$h02m20[i])){ Agreg5$h02m20[i]="_" }; if (is.na(Agreg5$h02m25[i])){ Agreg5$h02m25[i]="_" }
  if (is.na(Agreg5$h02m30[i])){ Agreg5$h02m30[i]="_" }; if (is.na(Agreg5$h02m35[i])){ Agreg5$h02m35[i]="_" }
  if (is.na(Agreg5$h02m40[i])){ Agreg5$h02m40[i]="_" }; if (is.na(Agreg5$h02m45[i])){ Agreg5$h02m45[i]="_" }
  if (is.na(Agreg5$h02m50[i])){ Agreg5$h02m50[i]="_" }; if (is.na(Agreg5$h02m55[i])){ Agreg5$h02m55[i]="_" }
  #h03
  if (is.na(Agreg5$h03m00[i])){ Agreg5$h03m00[i]="_" }; if (is.na(Agreg5$h03m05[i])){ Agreg5$h03m05[i]="_" }
  if (is.na(Agreg5$h03m10[i])){ Agreg5$h03m10[i]="_" }; if (is.na(Agreg5$h03m15[i])){ Agreg5$h03m15[i]="_" }
  if (is.na(Agreg5$h03m20[i])){ Agreg5$h03m20[i]="_" }; if (is.na(Agreg5$h03m25[i])){ Agreg5$h03m25[i]="_" }
  if (is.na(Agreg5$h03m30[i])){ Agreg5$h03m30[i]="_" }; if (is.na(Agreg5$h03m35[i])){ Agreg5$h03m35[i]="_" }
  if (is.na(Agreg5$h03m40[i])){ Agreg5$h03m40[i]="_" }; if (is.na(Agreg5$h03m45[i])){ Agreg5$h03m45[i]="_" }
  if (is.na(Agreg5$h03m50[i])){ Agreg5$h03m50[i]="_" }; if (is.na(Agreg5$h03m55[i])){ Agreg5$h03m55[i]="_" }
  #h04
  if (is.na(Agreg5$h04m00[i])){ Agreg5$h04m00[i]="_" }; if (is.na(Agreg5$h04m05[i])){ Agreg5$h04m05[i]="_" }
  if (is.na(Agreg5$h04m10[i])){ Agreg5$h04m10[i]="_" }; if (is.na(Agreg5$h04m15[i])){ Agreg5$h04m15[i]="_" }
  if (is.na(Agreg5$h04m20[i])){ Agreg5$h04m20[i]="_" }; if (is.na(Agreg5$h04m25[i])){ Agreg5$h04m25[i]="_" }
  if (is.na(Agreg5$h04m30[i])){ Agreg5$h04m30[i]="_" }; if (is.na(Agreg5$h04m35[i])){ Agreg5$h04m35[i]="_" }
  if (is.na(Agreg5$h04m40[i])){ Agreg5$h04m40[i]="_" }; if (is.na(Agreg5$h04m45[i])){ Agreg5$h04m45[i]="_" }
  if (is.na(Agreg5$h04m50[i])){ Agreg5$h04m50[i]="_" }; if (is.na(Agreg5$h04m55[i])){ Agreg5$h04m55[i]="_" }
  #h05
  if (is.na(Agreg5$h05m00[i])){ Agreg5$h05m00[i]="_" }; if (is.na(Agreg5$h05m05[i])){ Agreg5$h05m05[i]="_" }
  if (is.na(Agreg5$h05m10[i])){ Agreg5$h05m10[i]="_" }; if (is.na(Agreg5$h05m15[i])){ Agreg5$h05m15[i]="_" }
  if (is.na(Agreg5$h05m20[i])){ Agreg5$h05m20[i]="_" }; if (is.na(Agreg5$h05m25[i])){ Agreg5$h05m25[i]="_" }
  if (is.na(Agreg5$h05m30[i])){ Agreg5$h05m30[i]="_" }; if (is.na(Agreg5$h05m35[i])){ Agreg5$h05m35[i]="_" }
  if (is.na(Agreg5$h05m40[i])){ Agreg5$h05m40[i]="_" }; if (is.na(Agreg5$h05m45[i])){ Agreg5$h05m45[i]="_" }
  if (is.na(Agreg5$h05m50[i])){ Agreg5$h05m50[i]="_" }; if (is.na(Agreg5$h05m55[i])){ Agreg5$h05m55[i]="_" }
  #h06
  if (is.na(Agreg5$h06m00[i])){ Agreg5$h06m00[i]="_" }; if (is.na(Agreg5$h06m05[i])){ Agreg5$h06m05[i]="_" }
  if (is.na(Agreg5$h06m10[i])){ Agreg5$h06m10[i]="_" }; if (is.na(Agreg5$h06m15[i])){ Agreg5$h06m15[i]="_" }
  if (is.na(Agreg5$h06m20[i])){ Agreg5$h06m20[i]="_" }; if (is.na(Agreg5$h06m25[i])){ Agreg5$h06m25[i]="_" }
  if (is.na(Agreg5$h06m30[i])){ Agreg5$h06m30[i]="_" }; if (is.na(Agreg5$h06m35[i])){ Agreg5$h06m35[i]="_" }
  if (is.na(Agreg5$h06m40[i])){ Agreg5$h06m40[i]="_" }; if (is.na(Agreg5$h06m45[i])){ Agreg5$h06m45[i]="_" }
  if (is.na(Agreg5$h06m50[i])){ Agreg5$h06m50[i]="_" }; if (is.na(Agreg5$h06m55[i])){ Agreg5$h06m55[i]="_" }
  #h07
  if (is.na(Agreg5$h07m00[i])){ Agreg5$h07m00[i]="_" }; if (is.na(Agreg5$h07m05[i])){ Agreg5$h07m05[i]="_" }
  if (is.na(Agreg5$h07m10[i])){ Agreg5$h07m10[i]="_" }; if (is.na(Agreg5$h07m15[i])){ Agreg5$h07m15[i]="_" }
  if (is.na(Agreg5$h07m20[i])){ Agreg5$h07m20[i]="_" }; if (is.na(Agreg5$h07m25[i])){ Agreg5$h07m25[i]="_" }
  if (is.na(Agreg5$h07m30[i])){ Agreg5$h07m30[i]="_" }; if (is.na(Agreg5$h07m35[i])){ Agreg5$h07m35[i]="_" }
  if (is.na(Agreg5$h07m40[i])){ Agreg5$h07m40[i]="_" }; if (is.na(Agreg5$h07m45[i])){ Agreg5$h07m45[i]="_" }
  if (is.na(Agreg5$h07m50[i])){ Agreg5$h07m50[i]="_" }; if (is.na(Agreg5$h07m55[i])){ Agreg5$h07m55[i]="_" }
  #h08
  if (is.na(Agreg5$h08m00[i])){ Agreg5$h08m00[i]="_" }; if (is.na(Agreg5$h08m05[i])){ Agreg5$h08m05[i]="_" }
  if (is.na(Agreg5$h08m10[i])){ Agreg5$h08m10[i]="_" }; if (is.na(Agreg5$h08m15[i])){ Agreg5$h08m15[i]="_" }
  if (is.na(Agreg5$h08m20[i])){ Agreg5$h08m20[i]="_" }; if (is.na(Agreg5$h08m25[i])){ Agreg5$h08m25[i]="_" }
  if (is.na(Agreg5$h08m30[i])){ Agreg5$h08m30[i]="_" }; if (is.na(Agreg5$h08m35[i])){ Agreg5$h08m35[i]="_" }
  if (is.na(Agreg5$h08m40[i])){ Agreg5$h08m40[i]="_" }; if (is.na(Agreg5$h08m45[i])){ Agreg5$h08m45[i]="_" }
  if (is.na(Agreg5$h08m50[i])){ Agreg5$h08m50[i]="_" }; if (is.na(Agreg5$h08m55[i])){ Agreg5$h08m55[i]="_" }
  #h09
  if (is.na(Agreg5$h09m00[i])){ Agreg5$h09m00[i]="_" }; if (is.na(Agreg5$h09m05[i])){ Agreg5$h09m05[i]="_" }
  if (is.na(Agreg5$h09m10[i])){ Agreg5$h09m10[i]="_" }; if (is.na(Agreg5$h09m15[i])){ Agreg5$h09m15[i]="_" }
  if (is.na(Agreg5$h09m20[i])){ Agreg5$h09m20[i]="_" }; if (is.na(Agreg5$h09m25[i])){ Agreg5$h09m25[i]="_" }
  if (is.na(Agreg5$h09m30[i])){ Agreg5$h09m30[i]="_" }; if (is.na(Agreg5$h09m35[i])){ Agreg5$h09m35[i]="_" }
  if (is.na(Agreg5$h09m40[i])){ Agreg5$h09m40[i]="_" }; if (is.na(Agreg5$h09m45[i])){ Agreg5$h09m45[i]="_" }
  if (is.na(Agreg5$h09m50[i])){ Agreg5$h09m50[i]="_" }; if (is.na(Agreg5$h09m55[i])){ Agreg5$h09m55[i]="_" }
  #h10
  if (is.na(Agreg5$h10m00[i])){ Agreg5$h10m00[i]="_" }; if (is.na(Agreg5$h10m05[i])){ Agreg5$h10m05[i]="_" }
  if (is.na(Agreg5$h10m10[i])){ Agreg5$h10m10[i]="_" }; if (is.na(Agreg5$h10m15[i])){ Agreg5$h10m15[i]="_" }
  if (is.na(Agreg5$h10m20[i])){ Agreg5$h10m20[i]="_" }; if (is.na(Agreg5$h10m25[i])){ Agreg5$h10m25[i]="_" }
  if (is.na(Agreg5$h10m30[i])){ Agreg5$h10m30[i]="_" }; if (is.na(Agreg5$h10m35[i])){ Agreg5$h10m35[i]="_" }
  if (is.na(Agreg5$h10m40[i])){ Agreg5$h10m40[i]="_" }; if (is.na(Agreg5$h10m45[i])){ Agreg5$h10m45[i]="_" }
  if (is.na(Agreg5$h10m50[i])){ Agreg5$h10m50[i]="_" }; if (is.na(Agreg5$h10m55[i])){ Agreg5$h10m55[i]="_" }
  #h11
  if (is.na(Agreg5$h11m00[i])){ Agreg5$h11m00[i]="_" }; if (is.na(Agreg5$h11m05[i])){ Agreg5$h11m05[i]="_" }
  if (is.na(Agreg5$h11m10[i])){ Agreg5$h11m10[i]="_" }; if (is.na(Agreg5$h11m15[i])){ Agreg5$h11m15[i]="_" }
  if (is.na(Agreg5$h11m20[i])){ Agreg5$h11m20[i]="_" }; if (is.na(Agreg5$h11m25[i])){ Agreg5$h11m25[i]="_" }
  if (is.na(Agreg5$h11m30[i])){ Agreg5$h11m30[i]="_" }; if (is.na(Agreg5$h11m35[i])){ Agreg5$h11m35[i]="_" }
  if (is.na(Agreg5$h11m40[i])){ Agreg5$h11m40[i]="_" }; if (is.na(Agreg5$h11m45[i])){ Agreg5$h11m45[i]="_" }
  if (is.na(Agreg5$h11m50[i])){ Agreg5$h11m50[i]="_" }; if (is.na(Agreg5$h11m55[i])){ Agreg5$h11m55[i]="_" }
  #h12
  if (is.na(Agreg5$h12m00[i])){ Agreg5$h12m00[i]="_" }; if (is.na(Agreg5$h12m05[i])){ Agreg5$h12m05[i]="_" }
  if (is.na(Agreg5$h12m10[i])){ Agreg5$h12m10[i]="_" }; if (is.na(Agreg5$h12m15[i])){ Agreg5$h12m15[i]="_" }
  if (is.na(Agreg5$h12m20[i])){ Agreg5$h12m20[i]="_" }; if (is.na(Agreg5$h12m25[i])){ Agreg5$h12m25[i]="_" }
  if (is.na(Agreg5$h12m30[i])){ Agreg5$h12m30[i]="_" }; if (is.na(Agreg5$h12m35[i])){ Agreg5$h12m35[i]="_" }
  if (is.na(Agreg5$h12m40[i])){ Agreg5$h12m40[i]="_" }; if (is.na(Agreg5$h12m45[i])){ Agreg5$h12m45[i]="_" }
  if (is.na(Agreg5$h12m50[i])){ Agreg5$h12m50[i]="_" }; if (is.na(Agreg5$h12m55[i])){ Agreg5$h12m55[i]="_" }
  #h13
  if (is.na(Agreg5$h13m00[i])){ Agreg5$h13m00[i]="_" }; if (is.na(Agreg5$h13m05[i])){ Agreg5$h13m05[i]="_" }
  if (is.na(Agreg5$h13m10[i])){ Agreg5$h13m10[i]="_" }; if (is.na(Agreg5$h13m15[i])){ Agreg5$h13m15[i]="_" }
  if (is.na(Agreg5$h13m20[i])){ Agreg5$h13m20[i]="_" }; if (is.na(Agreg5$h13m25[i])){ Agreg5$h13m25[i]="_" }
  if (is.na(Agreg5$h13m30[i])){ Agreg5$h13m30[i]="_" }; if (is.na(Agreg5$h13m35[i])){ Agreg5$h13m35[i]="_" }
  if (is.na(Agreg5$h13m40[i])){ Agreg5$h13m40[i]="_" }; if (is.na(Agreg5$h13m45[i])){ Agreg5$h13m45[i]="_" }
  if (is.na(Agreg5$h13m50[i])){ Agreg5$h13m50[i]="_" }; if (is.na(Agreg5$h13m55[i])){ Agreg5$h13m55[i]="_" }
  #h14
  if (is.na(Agreg5$h14m00[i])){ Agreg5$h14m00[i]="_" }; if (is.na(Agreg5$h14m05[i])){ Agreg5$h14m05[i]="_" }
  if (is.na(Agreg5$h14m10[i])){ Agreg5$h14m10[i]="_" }; if (is.na(Agreg5$h14m15[i])){ Agreg5$h14m15[i]="_" }
  if (is.na(Agreg5$h14m20[i])){ Agreg5$h14m20[i]="_" }; if (is.na(Agreg5$h14m25[i])){ Agreg5$h14m25[i]="_" }
  if (is.na(Agreg5$h14m30[i])){ Agreg5$h14m30[i]="_" }; if (is.na(Agreg5$h14m35[i])){ Agreg5$h14m35[i]="_" }
  if (is.na(Agreg5$h14m40[i])){ Agreg5$h14m40[i]="_" }; if (is.na(Agreg5$h14m45[i])){ Agreg5$h14m45[i]="_" }
  if (is.na(Agreg5$h14m50[i])){ Agreg5$h14m50[i]="_" }; if (is.na(Agreg5$h14m55[i])){ Agreg5$h14m55[i]="_" }
  #h15
  if (is.na(Agreg5$h15m00[i])){ Agreg5$h15m00[i]="_" }; if (is.na(Agreg5$h15m05[i])){ Agreg5$h15m05[i]="_" }
  if (is.na(Agreg5$h15m10[i])){ Agreg5$h15m10[i]="_" }; if (is.na(Agreg5$h15m15[i])){ Agreg5$h15m15[i]="_" }
  if (is.na(Agreg5$h15m20[i])){ Agreg5$h15m20[i]="_" }; if (is.na(Agreg5$h15m25[i])){ Agreg5$h15m25[i]="_" }
  if (is.na(Agreg5$h15m30[i])){ Agreg5$h15m30[i]="_" }; if (is.na(Agreg5$h15m35[i])){ Agreg5$h15m35[i]="_" }
  if (is.na(Agreg5$h15m40[i])){ Agreg5$h15m40[i]="_" }; if (is.na(Agreg5$h15m45[i])){ Agreg5$h15m45[i]="_" }
  if (is.na(Agreg5$h15m50[i])){ Agreg5$h15m50[i]="_" }; if (is.na(Agreg5$h15m55[i])){ Agreg5$h15m55[i]="_" }
  #h16
  if (is.na(Agreg5$h16m00[i])){ Agreg5$h16m00[i]="_" }; if (is.na(Agreg5$h16m05[i])){ Agreg5$h16m05[i]="_" }
  if (is.na(Agreg5$h16m10[i])){ Agreg5$h16m10[i]="_" }; if (is.na(Agreg5$h16m15[i])){ Agreg5$h16m15[i]="_" }
  if (is.na(Agreg5$h16m20[i])){ Agreg5$h16m20[i]="_" }; if (is.na(Agreg5$h16m25[i])){ Agreg5$h16m25[i]="_" }
  if (is.na(Agreg5$h16m30[i])){ Agreg5$h16m30[i]="_" }; if (is.na(Agreg5$h16m35[i])){ Agreg5$h16m35[i]="_" }
  if (is.na(Agreg5$h16m40[i])){ Agreg5$h16m40[i]="_" }; if (is.na(Agreg5$h16m45[i])){ Agreg5$h16m45[i]="_" }
  if (is.na(Agreg5$h16m50[i])){ Agreg5$h16m50[i]="_" }; if (is.na(Agreg5$h16m55[i])){ Agreg5$h16m55[i]="_" }
  #h17
  if (is.na(Agreg5$h17m00[i])){ Agreg5$h17m00[i]="_" }; if (is.na(Agreg5$h17m05[i])){ Agreg5$h17m05[i]="_" }
  if (is.na(Agreg5$h17m10[i])){ Agreg5$h17m10[i]="_" }; if (is.na(Agreg5$h17m15[i])){ Agreg5$h17m15[i]="_" }
  if (is.na(Agreg5$h17m20[i])){ Agreg5$h17m20[i]="_" }; if (is.na(Agreg5$h17m25[i])){ Agreg5$h17m25[i]="_" }
  if (is.na(Agreg5$h17m30[i])){ Agreg5$h17m30[i]="_" }; if (is.na(Agreg5$h17m35[i])){ Agreg5$h17m35[i]="_" }
  if (is.na(Agreg5$h17m40[i])){ Agreg5$h17m40[i]="_" }; if (is.na(Agreg5$h17m45[i])){ Agreg5$h17m45[i]="_" }
  if (is.na(Agreg5$h17m50[i])){ Agreg5$h17m50[i]="_" }; if (is.na(Agreg5$h17m55[i])){ Agreg5$h17m55[i]="_" }
  #h18
  if (is.na(Agreg5$h18m00[i])){ Agreg5$h18m00[i]="_" }; if (is.na(Agreg5$h18m05[i])){ Agreg5$h18m05[i]="_" }
  if (is.na(Agreg5$h18m10[i])){ Agreg5$h18m10[i]="_" }; if (is.na(Agreg5$h18m15[i])){ Agreg5$h18m15[i]="_" }
  if (is.na(Agreg5$h18m20[i])){ Agreg5$h18m20[i]="_" }; if (is.na(Agreg5$h18m25[i])){ Agreg5$h18m25[i]="_" }
  if (is.na(Agreg5$h18m30[i])){ Agreg5$h18m30[i]="_" }; if (is.na(Agreg5$h18m35[i])){ Agreg5$h18m35[i]="_" }
  if (is.na(Agreg5$h18m40[i])){ Agreg5$h18m40[i]="_" }; if (is.na(Agreg5$h18m45[i])){ Agreg5$h18m45[i]="_" }
  if (is.na(Agreg5$h18m50[i])){ Agreg5$h18m50[i]="_" }; if (is.na(Agreg5$h18m55[i])){ Agreg5$h18m55[i]="_" }
  #h19
  if (is.na(Agreg5$h19m00[i])){ Agreg5$h19m00[i]="_" }; if (is.na(Agreg5$h19m05[i])){ Agreg5$h19m05[i]="_" }
  if (is.na(Agreg5$h19m10[i])){ Agreg5$h19m10[i]="_" }; if (is.na(Agreg5$h19m15[i])){ Agreg5$h19m15[i]="_" }
  if (is.na(Agreg5$h19m20[i])){ Agreg5$h19m20[i]="_" }; if (is.na(Agreg5$h19m25[i])){ Agreg5$h19m25[i]="_" }
  if (is.na(Agreg5$h19m30[i])){ Agreg5$h19m30[i]="_" }; if (is.na(Agreg5$h19m35[i])){ Agreg5$h19m35[i]="_" }
  if (is.na(Agreg5$h19m40[i])){ Agreg5$h19m40[i]="_" }; if (is.na(Agreg5$h19m45[i])){ Agreg5$h19m45[i]="_" }
  if (is.na(Agreg5$h19m50[i])){ Agreg5$h19m50[i]="_" }; if (is.na(Agreg5$h19m55[i])){ Agreg5$h19m55[i]="_" }
  #h20
  if (is.na(Agreg5$h20m00[i])){ Agreg5$h20m00[i]="_" }; if (is.na(Agreg5$h20m05[i])){ Agreg5$h20m05[i]="_" }
  if (is.na(Agreg5$h20m10[i])){ Agreg5$h20m10[i]="_" }; if (is.na(Agreg5$h20m15[i])){ Agreg5$h20m15[i]="_" }
  if (is.na(Agreg5$h20m20[i])){ Agreg5$h20m20[i]="_" }; if (is.na(Agreg5$h20m25[i])){ Agreg5$h20m25[i]="_" }
  if (is.na(Agreg5$h20m30[i])){ Agreg5$h20m30[i]="_" }; if (is.na(Agreg5$h20m35[i])){ Agreg5$h20m35[i]="_" }
  if (is.na(Agreg5$h20m40[i])){ Agreg5$h20m40[i]="_" }; if (is.na(Agreg5$h20m45[i])){ Agreg5$h20m45[i]="_" }
  if (is.na(Agreg5$h20m50[i])){ Agreg5$h20m50[i]="_" }; if (is.na(Agreg5$h20m55[i])){ Agreg5$h20m55[i]="_" }
  #h21
  if (is.na(Agreg5$h21m00[i])){ Agreg5$h21m00[i]="_" }; if (is.na(Agreg5$h21m05[i])){ Agreg5$h21m05[i]="_" }
  if (is.na(Agreg5$h21m10[i])){ Agreg5$h21m10[i]="_" }; if (is.na(Agreg5$h21m15[i])){ Agreg5$h21m15[i]="_" }
  if (is.na(Agreg5$h21m20[i])){ Agreg5$h21m20[i]="_" }; if (is.na(Agreg5$h21m25[i])){ Agreg5$h21m25[i]="_" }
  if (is.na(Agreg5$h21m30[i])){ Agreg5$h21m30[i]="_" }; if (is.na(Agreg5$h21m35[i])){ Agreg5$h21m35[i]="_" }
  if (is.na(Agreg5$h21m40[i])){ Agreg5$h21m40[i]="_" }; if (is.na(Agreg5$h21m45[i])){ Agreg5$h21m45[i]="_" }
  if (is.na(Agreg5$h21m50[i])){ Agreg5$h21m50[i]="_" }; if (is.na(Agreg5$h21m55[i])){ Agreg5$h21m55[i]="_" }
  #h22
  if (is.na(Agreg5$h22m00[i])){ Agreg5$h22m00[i]="_" }; if (is.na(Agreg5$h22m05[i])){ Agreg5$h22m05[i]="_" }
  if (is.na(Agreg5$h22m10[i])){ Agreg5$h22m10[i]="_" }; if (is.na(Agreg5$h22m15[i])){ Agreg5$h22m15[i]="_" }
  if (is.na(Agreg5$h22m20[i])){ Agreg5$h22m20[i]="_" }; if (is.na(Agreg5$h22m25[i])){ Agreg5$h22m25[i]="_" }
  if (is.na(Agreg5$h22m30[i])){ Agreg5$h22m30[i]="_" }; if (is.na(Agreg5$h22m35[i])){ Agreg5$h22m35[i]="_" }
  if (is.na(Agreg5$h22m40[i])){ Agreg5$h22m40[i]="_" }; if (is.na(Agreg5$h22m45[i])){ Agreg5$h22m45[i]="_" }
  if (is.na(Agreg5$h22m50[i])){ Agreg5$h22m50[i]="_" }; if (is.na(Agreg5$h22m55[i])){ Agreg5$h22m55[i]="_" }
  #h23
  if (is.na(Agreg5$h23m00[i])){ Agreg5$h23m00[i]="_" }; if (is.na(Agreg5$h23m05[i])){ Agreg5$h23m05[i]="_" }
  if (is.na(Agreg5$h23m10[i])){ Agreg5$h23m10[i]="_" }; if (is.na(Agreg5$h23m15[i])){ Agreg5$h23m15[i]="_" }
  if (is.na(Agreg5$h23m20[i])){ Agreg5$h23m20[i]="_" }; if (is.na(Agreg5$h23m25[i])){ Agreg5$h23m25[i]="_" }
  if (is.na(Agreg5$h23m30[i])){ Agreg5$h23m30[i]="_" }; if (is.na(Agreg5$h23m35[i])){ Agreg5$h23m35[i]="_" }
  if (is.na(Agreg5$h23m40[i])){ Agreg5$h23m40[i]="_" }; if (is.na(Agreg5$h23m45[i])){ Agreg5$h23m45[i]="_" }
  if (is.na(Agreg5$h23m50[i])){ Agreg5$h23m50[i]="_" }; if (is.na(Agreg5$h23m55[i])){ Agreg5$h23m55[i]="_" }
}

Agreg6 <- Agreg5 %>% 
  mutate(h00 = paste0(h00m00,h00m05,h00m10,h00m15,h00m20,h00m25,h00m30,h00m35,h00m40,h00m45,h00m50,h00m55),
         h01 = paste0(h01m00,h01m05,h01m10,h01m15,h01m20,h01m25,h01m30,h01m35,h01m40,h01m45,h01m50,h01m55),
         h02 = paste0(h02m00,h02m05,h02m10,h02m15,h02m20,h02m25,h02m30,h02m35,h02m40,h02m45,h02m50,h02m55),
         h03 = paste0(h03m00,h03m05,h03m10,h03m15,h03m20,h03m25,h03m30,h03m35,h03m40,h03m45,h03m50,h03m55),
         h04 = paste0(h04m00,h04m05,h04m10,h04m15,h04m20,h04m25,h04m30,h04m35,h04m40,h04m45,h04m50,h04m55),
         h05 = paste0(h05m00,h05m05,h05m10,h05m15,h05m20,h05m25,h05m30,h05m35,h05m40,h05m45,h05m50,h05m55),
         h06 = paste0(h06m00,h06m05,h06m10,h06m15,h06m20,h06m25,h06m30,h06m35,h06m40,h06m45,h06m50,h06m55),
         h07 = paste0(h07m00,h07m05,h07m10,h07m15,h07m20,h07m25,h07m30,h07m35,h07m40,h07m45,h07m50,h07m55),
         h08 = paste0(h08m00,h08m05,h08m10,h08m15,h08m20,h08m25,h08m30,h08m35,h08m40,h08m45,h08m50,h08m55),
         h09 = paste0(h09m00,h09m05,h09m10,h09m15,h09m20,h09m25,h09m30,h09m35,h09m40,h09m45,h09m50,h09m55),
         h10 = paste0(h10m00,h10m05,h10m10,h10m15,h10m20,h10m25,h10m30,h10m35,h10m40,h10m45,h10m50,h10m55),
         h11 = paste0(h11m00,h11m05,h11m10,h11m15,h11m20,h11m25,h11m30,h11m35,h11m40,h11m45,h11m50,h11m55),
         h12 = paste0(h12m00,h12m05,h12m10,h12m15,h12m20,h12m25,h12m30,h12m35,h12m40,h12m45,h12m50,h12m55),
         h13 = paste0(h13m00,h13m05,h13m10,h13m15,h13m20,h13m25,h13m30,h13m35,h13m40,h13m45,h13m50,h13m55),
         h14 = paste0(h14m00,h14m05,h14m10,h14m15,h14m20,h14m25,h14m30,h14m35,h14m40,h14m45,h14m50,h14m55),
         h15 = paste0(h15m00,h15m05,h15m10,h15m15,h15m20,h15m25,h15m30,h15m35,h15m40,h15m45,h15m50,h15m55),
         h16 = paste0(h16m00,h16m05,h16m10,h16m15,h16m20,h16m25,h16m30,h16m35,h16m40,h16m45,h16m50,h16m55),
         h17 = paste0(h17m00,h17m05,h17m10,h17m15,h17m20,h17m25,h17m30,h17m35,h17m40,h17m45,h17m50,h17m55),
         h18 = paste0(h18m00,h18m05,h18m10,h18m15,h18m20,h18m25,h18m30,h18m35,h18m40,h18m45,h18m50,h18m55),
         h19 = paste0(h19m00,h19m05,h19m10,h19m15,h19m20,h19m25,h19m30,h19m35,h19m40,h19m45,h19m50,h19m55),
         h20 = paste0(h20m00,h20m05,h20m10,h20m15,h20m20,h20m25,h20m30,h20m35,h20m40,h20m45,h20m50,h20m55),
         h21 = paste0(h21m00,h21m05,h21m10,h21m15,h21m20,h21m25,h21m30,h21m35,h21m40,h21m45,h21m50,h21m55),
         h22 = paste0(h22m00,h22m05,h22m10,h22m15,h22m20,h22m25,h22m30,h22m35,h22m40,h22m45,h22m50,h22m55),
         h23 = paste0(h23m00,h23m05,h23m10,h23m15,h23m20,h23m25,h23m30,h23m35,h23m40,h23m45,h23m50,h23m55))

Agreg7 <- Agreg6 %>% select(jour,contact_name,session_id,duree_min,
                            h00,h01,h02,h03,h04,h05,h06,h07,h08,h09,h10,h11,
                            h12,h13,h14,h15,h16,h17,h18,h19,h20,h21,h22,h23)


Agreg7 <- Agreg7 %>% mutate(contact_name=str_replace_all(contact_name,"é","e"))

library(readODS)
write_ods(Agreg7,paste0(rep_fich,"SortieODS_centreonSIP_user_jr_",part_date,lib_time,".ods"), sheet="Visu")






