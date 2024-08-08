
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)

date_jour <- Sys.time()

part_heure <- as.character(hour(date_jour))
part_minute <- as.character(minute(date_jour))

if (str_length(part_heure)==1){part_heure <- paste0("0",part_heure)}
if (str_length(part_minute)==1){part_minute <- paste0("0",part_minute)}
lib_time <- paste0("_",part_heure,"h",part_minute)

# recuperation de la partie heure precedente
lib_time_heure <- as.integer(str_sub(lib_time,2,3))-1
if (str_length(lib_time_heure)==1){lib_time_heure <- paste0("0",lib_time_heure)}

rm(part_heure,part_minute)

part_date  <- as.character(Sys.Date())
part_date2 <- str_remove_all(part_date,"-")

part_date_hier  <- as.character(Sys.Date()-ddays(1))
part_date2_hier <- str_remove_all(part_date_hier,"-")

# pour tester...
# part_date2 <- "20230707"

rep_fich <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/Fichiers_date_",part_date2,"/")
setwd(rep_fich)

rep_fich_hier <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/Fichiers_date_",part_date2_hier,"/")


##########
##########
# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_fich) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich)

listing_delta_group <- listing %>%
  filter(str_detect(liste_fich,"df_delta_group") & str_detect(liste_fich,"csv")) %>%
  arrange(liste_fich)
listing_regroup <- listing %>%
  filter(str_detect(liste_fich,"df_regroup") & str_detect(liste_fich,"csv")) %>%
  arrange(liste_fich)


# filtre complementaire sur la periode de temps considere
vect_periode <- c(paste0(lib_time_heure,"h00"), paste0(lib_time_heure,"h05"), paste0(lib_time_heure,"h10"),
                  paste0(lib_time_heure,"h15"), paste0(lib_time_heure,"h20"), paste0(lib_time_heure,"h25"),
                  paste0(lib_time_heure,"h30"), paste0(lib_time_heure,"h35"), paste0(lib_time_heure,"h40"),
                  paste0(lib_time_heure,"h45"), paste0(lib_time_heure,"h50"), paste0(lib_time_heure,"h55"))

listing_delta_group <- listing_delta_group %>% filter(str_sub(liste_fich,25,29) %in% vect_periode)
listing_regroup <- listing_regroup %>% filter(str_sub(liste_fich,21,25) %in% vect_periode)                                            


##########
##########
# PARTIE 1
# copie de tous les fichiers datareq dans le repertoire de reference
for (i in 1:nrow(listing_delta_group)){
  cat(paste0("numero ",i," ",listing_delta_group$liste_fich[i],"\n"))
  # concatenation des fichiers de type delta_group
  if (i==1) {
    file.copy(paste0(rep_fich,listing_delta_group$liste_fich[i]),
              paste0(rep_fich,"df_delta_group_jour_",part_date,lib_time,".csv"))
  } else {
    df_datareq <- read.csv2(paste0(rep_fich,"df_delta_group_jour_",part_date,lib_time,".csv"))
    df_copy <- read.csv2(paste0(rep_fich,listing_delta_group$liste_fich[i]))
    df_datareq <- rbind(df_datareq,df_copy)
    write.csv2(df_datareq,paste0(rep_fich,"df_delta_group_jour_",part_date,lib_time,".csv"),row.names=FALSE)
  }
  # insertion du fichier unitaire dans un .zip puis suppression
  #zip(paste0("df_delta_group_jour_",part_date,lib_time,".zip"),
  #    paste0(listing_delta_group$liste_fich[i]))
  file.remove(paste0(rep_fich,listing_delta_group$liste_fich[i]))
}
rm(i,df_copy)

write.csv2(df_datareq,
           paste0(rep_fich,"df_delta_group_jour_",part_date,lib_time,".csv"),
           row.names=FALSE)


##########
##########
# PARTIE 2
# copie de tous les fichiers status_DC1 ou status_DC2 dans le repertoire de reference
for (i in 1:nrow(listing_regroup)){
  cat(paste0("numero ",i," ",listing_regroup$liste_fich[i],"\n"))
  # concatenation des fichiers de type regroup
  if (i==1) {
    file.copy(paste0(rep_fich,listing_regroup$liste_fich[i]),
              paste0(rep_fich,"df_regroup_jour_",part_date,lib_time,".csv"))
  } else {
    df_status <- read.csv2(paste0(rep_fich,"df_regroup_jour_",part_date,lib_time,".csv"))
    df_copy <- read.csv2(paste0(rep_fich,listing_regroup$liste_fich[i]))
    df_status <- rbind(df_status,df_copy)
    write.csv2(df_status,paste0(rep_fich,"df_regroup_jour_",part_date,lib_time,".csv"),row.names=FALSE)
  }
  # insertion du fichier unitaire dans un .zip puis suppression
  #zip(paste0("df_regroup_jour_",part_date,lib_time,".zip"),
  #    paste0(listing_regroup$liste_fich[i]))
  file.remove(paste0(rep_fich,listing_regroup$liste_fich[i]))
}
rm(i,df_copy)

# lib_time <- "_01h30"

# ajout de la variables unite
df_datareq <- read.csv2(paste0(rep_fich,"df_regroup_jour_",part_date,lib_time,".csv"))
df_datareq <- df_datareq %>% 
  mutate(unite=1) %>% relocate(unite)

write.csv2(df_datareq,
           paste0(rep_fich,"df_regroup_jour_",part_date,lib_time,".csv"),
           row.names=FALSE)

####
####

# Remise en place des 2 fichiers resume de la journee dhier dans le bon repertoire

# pour le fichier de type delta_group
date_fichier1 <- str_sub(listing_delta_group$liste_fich[1],21,30)
if (date_fichier1==part_date_hier){
  file.copy(paste0(rep_fich,     listing_delta_group$liste_fich[1]),
            paste0(rep_fich_hier,"df_delta_group_resume_jour_",part_date_hier,".csv"))
  file.remove(paste0(rep_fich,listing_delta_group$liste_fich[1]))
}

# pour le fichier de type regroup
date_fichier1 <- str_sub(listing_regroup$liste_fich[1],17,26)
if (date_fichier1==part_date_hier){
  file.copy(paste0(rep_fich,     listing_regroup$liste_fich[1]),
            paste0(rep_fich_hier,"df_regroup_resume_jour_",part_date_hier,".csv"))
  file.remove(paste0(rep_fich,listing_regroup$liste_fich[1]))
}

