
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"
rep_gen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/HistoVMW_EsxListeHost/"

###

# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
    rename(liste_fich=".") %>% arrange(liste_fich) %>% 
    mutate(lg=row_number()) %>% relocate(lg) %>% 
    filter(str_detect(liste_fich,"df_EsxiVM")) %>% 
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

# preparation fichier zip pour envoi via mel

date_jour <- Sys.time()
#date_jour_simple <- Sys.Date()
#tz <- Sys.timezone()

## Operation de decalage de l'heure
## mettre 1 lorsqu'on est en heure d'hiver et
## mettre 2 lorsqu'on est en heure d'ete
## decalage_heure <- 2

#date retardee de 12h
date_jour_12hmoins <- date_jour - dhours(12)

# recuperation de la partie date date_jour_12hmoins
annee_date_jour <- paste0(year(date_jour_12hmoins))
mois_date_jour <- ifelse(str_length(month(date_jour_12hmoins))==1,
                         paste0("0",month(date_jour_12hmoins)),
                         paste0(month(date_jour_12hmoins)))
jour_date_jour <- ifelse(str_length(day(date_jour_12hmoins))==1,
                         paste0("0",day(date_jour_12hmoins)),
                         paste0(day(date_jour_12hmoins)))

date_ref <- paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour)

# date_ref <- as.POSIXct(paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour," 00:00:00")) - dhours(decalage_heure)
rm(annee_date_jour,mois_date_jour,jour_date_jour)



######################################################################
######################################################################
######################################################################
######################################################################

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\HistoVMW_EsxListeHost\\EnvoiMail_ViaPowershell.ps1"')

# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM









