
library(RMariaDB)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)

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


# decalage egal de 2 en heure en ete et 1 en heure en hiver
decalage_heure <- 2


##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----
##
## Connexion a la nouvelle base 50 ----
##
baseMdb <- dbConnect(MariaDB(),user="Gepex_lecture", password="Reporting",
                     dbname="centreon", host="xx.xx.xx.xx", port="3306")

# !! utilisation de la base centreon (tout court) et non centreon.storage

##################################################################

Req_Contact <- dbGetQuery(baseMdb, 
                      paste("SELECT contact_id, contact_name, contact_alias 
                             FROM centreon.contact
                            "))

Req_Session <- dbGetQuery(baseMdb, 
                      paste("SELECT session_id, user_id, current_page, 
                             FROM_UNIXTIME(last_reload,'%Y-%m-%d %H:%i:%S') as last_charg
                             FROM centreon.session
                            "))

##################################################################

##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################


Req_Contact <- Req_Contact %>% rename(user_id=contact_id)

# fusion entre les df
Req_Glob <- Req_Contact %>% 
     right_join(Req_Session, by=c("user_id")) %>% 
     mutate(POSIXct_date_charg=ymd_hms(last_charg)) %>% 
     mutate(POSIXct_date_charg=POSIXct_date_charg+dhours(decalage_heure)) %>% 
     select(-last_charg) %>% 
     mutate(moment_extract=paste0(str_sub(lib_date_jr,1,16),":00")) %>% 
     mutate(moment_extract=str_replace(moment_extract,"h",":")) %>% 
     mutate(moment_extract=str_replace(moment_extract,"_"," ")) %>% 
     mutate(POSIXct_moment_extract=ymd_hms(moment_extract)) %>% 
     mutate(diff_tps_sec=int_length(interval(POSIXct_date_charg,POSIXct_moment_extract))) %>% 
     select(-moment_extract) #%>% 
     #mutate(char=as.character(POSIXct_moment_extract))


########################################################
########################################################
# sauvegarde des resultats dans un repertoire Windows

# Creation dun repertoire avec la date du jour
folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/HistoCentreon_User/Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
     cat("The folder already exists")
} else {
     dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

write.csv2(Req_Glob, 
           paste0(folder,"/df_CentreonSIP_User_",str_sub(lib_date_jr,3,16),".csv"),
           row.names=FALSE)









