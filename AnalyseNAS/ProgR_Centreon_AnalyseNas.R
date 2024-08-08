
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)


# determination journee de reference
date_jour <- Sys.time()
#date_jour_simple <- Sys.Date()
#tz <- Sys.timezone()

## Operation de decalage de l'heure
## mettre 1 lorsqu'on est en heure d'hiver et
## mettre 2 lorsqu'on est en heure d'ete
## decalage_heure <- 2

#date retardee de 1d
date_jour_1dmoins <- date_jour - ddays(1)

# recuperation de la partie date date_jour_1hmoins
annee_date_jour <- paste0(year(date_jour_1dmoins))
mois_date_jour <- ifelse(str_length(month(date_jour_1dmoins))==1,
                         paste0("0",month(date_jour_1dmoins)),
                         paste0(month(date_jour_1dmoins)))
jour_date_jour <- ifelse(str_length(day(date_jour_1dmoins))==1,
                         paste0("0",day(date_jour_1dmoins)),
                         paste0(day(date_jour_1dmoins)))

date_ref <- paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour)

# date_ref <- as.POSIXct(paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour," 00:00:00")) - dhours(decalage_heure)
rm(annee_date_jour,mois_date_jour,jour_date_jour,
   date_jour_1dmoins)


# pour tests ou corrections...
# date_ref <- "2024-04-29"

###

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"
rep_gen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/_AnalyseNas/"

rep_jour <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/_AnalyseNas/general/"

###

# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"ASv3_") & !(str_detect(liste_fich,"_15jrs"))) %>% 
  mutate(last=ifelse(lg==max(lg),1,0))

nom_json_data <- listing %>% filter(last!=1) %>%
  select(liste_fich) %>% as.character()
nom_json_logs <- listing %>% filter(last==1) %>%
  select(liste_fich) %>% as.character()

# pour tester...
# nom_fichier_retenu <- "Recensement_24-02-08_06h44.zip"

######################################################################
######################################################################
######################################################################
######################################################################

# Creation dun repertoire avec la date du jour
folder <- paste0(rep_gen,"Fichiers_date_",str_replace_all(date_ref,"-",""))
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# copie du fichier .zip selectionne, 
# suppression de ce meme fichier dans le repertoire sas de depart,
# dezippage des fichier dans le repertoire d'arrivee

# fichier data
file.copy(paste0(rep_sas,nom_json_data),paste0(folder,"/",nom_json_data))

file.remove(paste0(rep_jour,nom_json_data))
file.copy(paste0(rep_sas,nom_json_data),paste0(rep_jour,"/",nom_json_data))

file.remove(paste0(rep_sas,nom_json_data))

# fichier logs
file.copy(paste0(rep_sas,nom_json_logs),paste0(folder,"/",nom_json_logs))

file.remove(paste0(rep_jour,nom_json_logs))
file.copy(paste0(rep_sas,nom_json_logs),paste0(rep_jour,"/",nom_json_logs))

file.remove(paste0(rep_sas,nom_json_logs))


######################################################################
######################################################################
######################################################################
######################################################################

# PARTIE : ENVOI DU MEL

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\_AnalyseNas\\EnvoiMail_ViaPowershell.ps1"')







