
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"


date_jour <- str_replace_all(as.character(Sys.Date()),"-","")
# Creation dun repertoire avec la date du jour
rep_gen <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/Fichiers_date_",date_jour,"/")
if (file.exists(rep_gen)) {
  cat("The folder already exists")
} else {
  dir.create(rep_gen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

###

# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
    rename(liste_fich=".") %>% arrange(liste_fich) %>% 
    mutate(lg=row_number()) %>% relocate(lg) %>% 
    filter(str_detect(liste_fich,"df_delta_group_2") | 
           str_detect(liste_fich,"df_regroup_2")) %>% 
    mutate(last=ifelse(str_sub(liste_fich,1,6)!=lead(str_sub(liste_fich,1,6)) | lg==max(lg),1,0))


# copie de tous les fichiers dans le repertoire gen
for (i in 1:nrow(listing)){
    cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
    file.copy(paste0(rep_sas,listing$liste_fich[i]),
              paste0(rep_gen,listing$liste_fich[i]))
}

# suppression de tous les fichiers dans le repertoire sas
for (i in 1:nrow(listing)){
    cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
    file.remove(paste0(rep_sas,listing$liste_fich[i]))
}


######################################################################
######################################################################
######################################################################
######################################################################

# filtre sur les 2 derniers fichiers de chaque type present dans le repertoire gen et 
# enregistrement sous un nom commun df_regroup et df_delta_group
listing_gen <- list.files(rep_gen) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"df_delta_group_2") | str_detect(liste_fich,"df_regroup_2")) %>% 
  mutate(last=ifelse(str_sub(liste_fich,1,6)!=lead(str_sub(liste_fich,1,6)) | lg==max(lg),1,0)) %>% 
  filter(last==1)

df_regroup <- read.csv2(paste0(rep_gen,listing_gen$liste_fich[2]))
df_delta_group <- read.csv2(paste0(rep_gen,listing_gen$liste_fich[1]))

# pour tester ...
# repertoire <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/Fichiers_date_20240415/"
# df_regroup <- read.csv2(paste0(repertoire,"df_regroup_24-04-15_17h35.csv"))
# df_delta_group <- read.csv2(paste0(repertoire,"df_delta_group_24-04-15_17h35.csv"))

detection <- FALSE
for (i in 1:nrow(df_regroup)){
  if (
      df_regroup$zone[i] %in% c("02_DMZ_Osny","06_ZoneInt_Osny","07_ZoneSIA_Auze","08_ZoneSIA_Osny") &&
      (
        # condition 1 : les critical sur les zones importantes si effectif >=5
        (df_regroup$diff[i]>=5 && df_regroup$diff_sur_statut[i]=="1_Crit")
        |
        # condition 2 : les non-critical sur les zones importantes si effectif >=10
        (df_regroup$diff[i]>=10 && df_regroup$diff_sur_statut[i]!="1_Crit")
        |
        # condition 3 : les critical sur la partie -Selenium si effectif >=2
        (df_regroup$diff[i]>=2 && df_regroup$Gr_Hotes[i]=="-Selenium" && df_regroup$diff_sur_statut[i]=="1_Crit")
      )
     ){
    detection <- TRUE
  }
}
print(detection)


# envoi mel notification avec Powershell
if (detection) {
  
  rep_genprog <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/")
  
  write.csv2(df_regroup, paste0(rep_genprog,"df_regroup.csv"), row.names=FALSE)
  write.csv2(df_delta_group, paste0(rep_genprog,"df_delta_group.csv"), row.names=FALSE)
  
  system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min_DetectionRupture\\EnvoiMail_ViaPowershell_DetectRupt.ps1"')
  
  file.remove(paste0(rep_genprog,df_regroup))
  file.remove(paste0(rep_genprog,df_delta_group))
  
} else {
  
  # suppression de tous les fichiers ayant ete lus dans le repertoire sas
  # mais se retrouvant a present dans le repertoire gen
  for (i in 1:nrow(listing)){
    cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
    file.remove(paste0(rep_gen,listing$liste_fich[i]))
  }
  
}





