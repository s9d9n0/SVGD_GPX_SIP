
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"

date_jour <- str_replace_all(as.character(Sys.Date()-ddays(1)),"-","")
# Creation dun repertoire avec la date du jour
rep_gen <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min_DetectionRupture/Fichiers_date_",date_jour,"/")

###

# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"df_delta_group_jour_") ) #%>% 
#mutate(last=ifelse(str_sub(liste_fich,1,6)!=lead(str_sub(liste_fich,1,6)) | lg==max(lg),1,0))


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









