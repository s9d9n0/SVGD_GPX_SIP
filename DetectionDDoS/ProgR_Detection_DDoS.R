
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"
rep_gen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Detection_DDoS/"


date_jour <- Sys.time()

part_date <- as.character(Sys.Date())
part_heure <- as.character(hour(date_jour))
part_minute <- as.character(minute(date_jour))
part_seconde <- as.character(round(second(date_jour),0))

if (str_length(part_heure)==1){part_heure <- paste0("0",part_heure)}
if (str_length(part_minute)==1){part_minute <- paste0("0",part_minute)}
if (str_length(part_seconde)==1){part_seconde <- paste0("0",part_seconde)}
lib_date_jr <- paste0(part_date,"_",part_heure,"h",part_minute,"m",part_seconde,"s")

rm(part_date,part_heure,part_minute,part_seconde)

###


# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
    rename(liste_fich=".") %>% arrange(liste_fich) %>% 
    mutate(lg=row_number()) %>% relocate(lg) %>% 
    filter(str_detect(liste_fich,"df_Dns") | str_detect(liste_fich,"df_PaloAlto")) %>% 
    mutate(last=ifelse(str_sub(liste_fich,1,6)!=lead(str_sub(liste_fich,1,6)) | lg==max(lg),1,0))


nb_fichier <- nrow(listing) %>% as.integer()

# debut du if
if (nb_fichier>=1) {
    
# copie de tous les fichiers dans le repertoire gen
  for (i in 1:nrow(listing)){
      cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
      file.copy(paste0(rep_sas,listing$liste_fich[i]),
                paste0(rep_gen,listing$liste_fich[i]))
  }


# detection des derniers fichiers de chaque type
  dernier_PaloAlto <- listing$liste_fich[str_sub(listing$liste_fich,1,11)=="df_PaloAlto" & listing$last==1]
  dernier_Dns <- listing$liste_fich[str_sub(listing$liste_fich,1,6)=="df_Dns" & listing$last==1]

  cat(dernier_PaloAlto,"\n",dernier_Dns)

# lecture des fichiers presents dans le sas
  Requete <- read.csv2(paste0(rep_sas,dernier_PaloAlto))
  Requete2 <- read.csv2(paste0(rep_sas,dernier_Dns))
  #Requete <- read.csv2(paste0(rep_sas,"df_PaloAlto_",lib_date_jr,".csv"))
  #Requete2 <- read.csv2(paste0(rep_sas,"df_Dns_",lib_date_jr,".csv"))

# copie de ces fichiers vers le repertoire gen
  write.csv2(Requete,
              paste0(rep_gen,"df_PaloAlto_",lib_date_jr,".csv"),
              row.names=FALSE)
  write.csv2(Requete2,
              paste0(rep_gen,"df_Dns_",lib_date_jr,".csv"),
              row.names=FALSE)

  # suppression de tous les fichiers dans le repertoire sas
  for (i in 1:nrow(listing)){
    cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
    file.remove(paste0(rep_sas,listing$liste_fich[i]))
  }
  
######################################################################
######################################################################
######################################################################
######################################################################

  file.remove(paste0(rep_gen,"/df_PaloAlto.csv"))
  file.remove(paste0(rep_gen,"/df_Dns.csv"))
  
#  Selection des fichiers a mettre en PJ du mel
  file.copy(from=paste0(rep_gen,dernier_PaloAlto),
            to=paste0(rep_gen,"/df_PaloAlto.csv"))
  file.copy(from=paste0(rep_gen,dernier_Dns),
            to=paste0(rep_gen,"/df_Dns.csv"))


# envoi finalement avec Powershell
  system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Detection_DDoS\\EnvoiMail_ViaPowershell.ps1"')

    
# fin du if   
}


# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM
