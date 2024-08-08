
## Chargement des packages utilises dans le programme ----
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/"

# Appel des fonctions utiles a la réalisations des graphiques
source(paste0(chemin,"ProgR_CptAlert5_GraphStatut_Fonctions.R"))

##########################################################
##########################################################

# determination journee de reference
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


# pour tests ou corrections...
# date_ref <- "2023-10-01"


##########################################################
##########################################################

# creation des 4 graphiques de type chronogramme agreges selon les statuts et les zones pour chaque DC

df_status <- read.csv2(paste0(chemin,"df_statusj_",date_ref,"_01h30.csv"))
# pour tester
# date_ref <- "2023-07-18"
# df_status <- read.csv2(paste0(chemin,"df_statusj_",date_ref,"_01h30.csv"))
    

df_status_Ens <- df_status %>% 
    filter(str_detect(zone,"Ensemble")) %>% 
    pivot_longer(.,cols=c("Critical","Warning","Unknown"),values_to="eff") %>% 
    as.data.frame()

df_status_Ens_BIS <- df_status_Ens %>% #filter(str_detect(zone,"dc1"))
    filter(#str_sub(moment,10,11) %in% c("00") & str_sub(moment,13,14) %in% c("00","05","10","15") &
           #str_sub(moment,10,11) %in% c("00","01","02","03","04","05",
           #                             "06","07","08","09","10","11") &
           str_detect(zone,"dc1")) %>% 
    mutate(eff_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),eff,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),moment,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),str_sub(moment,10,14),moment_label)) %>% 
    relocate(moment_label,.after="moment")

char <- fct_modif_label_x(df_status_Ens_BIS)
graph_dc1 <- fct_Crea_BarChart(df_status_Ens_BIS)
graph_dc1

#vue <- char %>% as.data.frame()

gc()

####
####

df_status_Ens_BIS <- df_status_Ens %>% #filter(str_detect(zone,"dc1"))
    filter(#str_sub(moment,10,11) %in% c("00") & str_sub(moment,13,14) %in% c("00","05","10","15") &
        #str_sub(moment,10,11) %in% c("00","01","02","03","04","05",
        #                             "06","07","08","09","10","11") &
        str_detect(zone,"dc2")) %>% 
    mutate(eff_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),eff,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),moment,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),str_sub(moment,10,14),moment_label)) %>% 
    relocate(moment_label,.after="moment")

char <- fct_modif_label_x(df_status_Ens_BIS)
graph_dc2 <- fct_Crea_BarChart(df_status_Ens_BIS)
graph_dc2

gc()

########################################
## Partie Declinaison entre les zones...
########################################

df_status_Zone <- df_status %>% 
    filter(!str_detect(zone,"Ensemble")) %>% group_by(moment,zone) %>% 
    summarise(Critical=sum(Critical),
              Warning=sum(Warning),
              Unknown=sum(Unknown),
              eff=sum(Critical,Warning,Unknown)) %>%
    ungroup() %>% as.data.frame()

df_status_Zone_BIS <- df_status_Zone %>% #filter(str_detect(zone,"dc1"))
    filter(#str_sub(moment,10,11) %in% c("00") & str_sub(moment,13,14) %in% c("00","05","10","15") &
        #str_sub(moment,10,11) %in% c("00","01","02","03","04","05",
        #                             "06","07","08","09","10","11") &
        str_detect(zone,"dc1")) %>% 
    mutate(eff_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),eff,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),moment,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),str_sub(moment,10,14),moment_label)) %>% 
    relocate(moment_label,.after="moment")

char <- fct_modif_label_x(df_status_Zone_BIS)
graph_Zonedc1 <- fct_Crea_BarChart_Zonedc1(df_status_Zone_BIS)
graph_Zonedc1

gc()

####
####

df_status_Zone <- df_status %>% 
    filter(!str_detect(zone,"Ensemble")) %>% group_by(moment,zone) %>% 
    summarise(Critical=sum(Critical),
              Warning=sum(Warning),
              Unknown=sum(Unknown),
              eff=sum(Critical,Warning,Unknown)) %>%
    ungroup() %>% as.data.frame()

df_status_Zone_BIS <- df_status_Zone %>% #filter(str_detect(zone,"dc1"))
    filter(#str_sub(moment,10,11) %in% c("00") & str_sub(moment,13,14) %in% c("00","05","10","15") &
        #str_sub(moment,10,11) %in% c("00","01","02","03","04","05",
        #                             "06","07","08","09","10","11") &
        str_detect(zone,"dc2")) %>% 
    mutate(eff_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),eff,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),moment,""),
           moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),str_sub(moment,10,14),moment_label)) %>% 
    relocate(moment_label,.after="moment")

char <- fct_modif_label_x(df_status_Zone_BIS)
graph_Zonedc2 <- fct_Crea_BarChart_Zonedc2(df_status_Zone_BIS)
graph_Zonedc2

gc()


# sauvegarde des 4 graphiques sur des fichiers .png

setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/Graph_Chronogrammes")

png(file="graph_dc1Status.png",width = 1300, height = 500)
graph_dc1
dev.off()
png(file="graph_dc1Zone.png",width = 1300, height = 500)
graph_Zonedc1
dev.off()

png(file="graph_dc2Status.png",width = 1300, height = 500)
graph_dc2
dev.off()
png(file="graph_dc2Zone.png",width = 1300, height = 500)
graph_Zonedc2
dev.off()

# encodage des 4 images
library(base64enc)
graph_dc1Status_encode <- base64encode("graph_dc1Status.png")
write.csv2(graph_dc1Status_encode,"graph_dc1Status_encode.csv", row.names = FALSE)
graph_dc1Zone_encode <- base64encode("graph_dc1Zone.png")
write.csv2(graph_dc1Zone_encode,"graph_dc1Zone_encode.csv", row.names = FALSE)

graph_dc2Status_encode <- base64encode("graph_dc2Status.png")
write.csv2(graph_dc2Status_encode,"graph_dc2Status_encode.csv", row.names = FALSE)
graph_dc2Zone_encode <- base64encode("graph_dc2Zone.png")
write.csv2(graph_dc2Zone_encode,"graph_dc2Zone_encode.csv", row.names = FALSE)


##########################################################
##########################################################
##########################################################
##########################################################

###########
# MEL 1
###########

chemin_graph <- "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min\\Graph_Chronogrammes\\"

# Creation de la page HTML
# head <- paste0("<head>",
#                "<meta charset='utf-8'/>",
#                "<title>CHRONOGRAMMES</title>",
#                "<style>",
#                "</style>",
#                "</head>")
# 
# 
# dc1titre <- paste0("<p style='font-size:20px, line-height:30%'><strong>Chronogrammes - DC1 dc1</strong></p>")
# imgdc11 <- paste0("<p><img src='",paste0(chemin_graph,"graph_dc1Status.png"),"'
#                             alt='Chronogramme dc1 par statut'/></p>")
# imgdc12 <- paste0("<p><img src='",paste0(chemin_graph,"graph_dc1Zone.png"),"'
#                             alt='Chronogramme dc1 par zone'/></p>")
# 
# dc2titre <- paste0("<p style='font-size:20px, line-height:30%'><strong>Chronogrammes - DC2 dc2</strong></p>")
# imgdc21 <- paste0("<p><img src='",paste0(chemin_graph,"graph_dc2Status.png"),"'
#                             alt='Chronogramme dc2 par statut'/></p>")
# imgdc22 <- paste0("<p><img src='",paste0(chemin_graph,"graph_dc2Zone.png"),"'
#                             alt='Chronogramme dc2 par zone'/></p>")
# 
# 
# # reconstitution du corps du mel par reunion des differentes parties de codes HTML
# pageHTML <- paste0("<!DOCTYPE html>",
#                    "<html>",
#                         head,
#                         "<body>",
#                           # partie dc1
#                           dc1titre,imgdc11,imgdc12,
#                           # partie dc2
#                           dc2titre,imgdc21,imgdc22,
#                         "</body>",
#                     "</html>")
# 
# file <- "Graph_Chronogrammes.html"
# file.create(file, showWarnings = TRUE)
# cat(pageHTML,file="Graph_Chronogrammes.html",append=TRUE)



##########################################################
##########################################################
##########################################################
##########################################################


###########
# MEL 2
###########

# recuperation des images encodees dans des variables
graph_dc1Status_encode <- read.csv2("graph_dc1Status_encode.csv")
graph_dc1Status_encode <- graph_dc1Status_encode$x[1]
graph_dc1Zone_encode <- read.csv2("graph_dc1Zone_encode.csv")
graph_dc1Zone_encode <- graph_dc1Zone_encode$x[1]

graph_dc2Status_encode <- read.csv2("graph_dc2Status_encode.csv")
graph_dc2Status_encode <- graph_dc2Status_encode$x[1]
graph_dc2Zone_encode <- read.csv2("graph_dc2Zone_encode.csv")
graph_dc2Zone_encode <- graph_dc2Zone_encode$x[1]



# Creation de la page HTML
head <- paste0("<head>",
               "<meta charset='utf-8'/>",
               "<title>CHRONOGRAMMES</title>",
                    "<style>",
                    "</style>",
               "</head>")


dc1titre <- paste0("<p style='font-size:20px, line-height:30%'><strong>Chronogrammes - DC1 dc1</strong></p>")
imgdc11_encode <- paste0("<p><img src='data:image/png;base64,",graph_dc1Status_encode,"'
                                   alt='Chronogramme dc1 par statut'/></p>")
imgdc12_encode <- paste0("<p><img src='data:image/png;base64,",graph_dc1Zone_encode,"'
                                   alt='Chronogramme dc1 par zone'/></p>")

dc2titre <- paste0("<p style='font-size:20px, line-height:30%'><strong>Chronogrammes - DC2 dc2</strong></p>")
imgdc21_encode <- paste0("<p><img src='data:image/png;base64,",graph_dc2Status_encode,"'
                                   alt='Chronogramme dc2 par statut'/></p>")
imgdc22_encode <- paste0("<p><img src='data:image/png;base64,",graph_dc2Zone_encode,"'
                                   alt='Chronogramme Auville par zone'/></p>")


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
pageHTML_encode <- paste0("<!DOCTYPE html>",
                            "<html>",
                                head,
                                "<body>",
                                    # partie dc1
                                    dc1titre,imgdc11_encode,imgdc12_encode,
                                    # partie dc2
                                    dc2titre,imgdc21_encode,imgdc22_encode,
                                "</body>",
                            "</html>")

file <- "Graph_Chronogrammes_zip.html"
file.create(file, showWarnings = TRUE)
cat(pageHTML_encode,file="Graph_Chronogrammes_zip.html",append=TRUE)


# fichier zip
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),paste0("Graph_Chronogrammes_zip.html"))

file.remove(paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))

file.copy(from=paste0(chemin_graph,"/Graph_Chronogrammes_",date_ref,".zip"),
          to=paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))

######################################################################
######################################################################
######################################################################
######################################################################


# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min\\EnvoiMail_ViaPowershell_Graph_01h55.ps1"')

# suppression du fichier .zip envoyé...
file.remove(paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))


# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM

# library(mailR) ne fonctionne pas sans lib.loc
# library(mailR, lib.loc="E:/Librairie_R_partagee/R/win-library/4.0")
library(mailR, lib.loc="C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_PackagesMail")


locmdp <- "E:/X_PackagesMailR/"
mdp_cpt <- read.table(paste0(locmdp,"ident_tls.txt")) %>% as.character()

#  Envoi du mel 1 final - graphiques dans corps du mel en SMTP authentifie
# send.mail(from = "Centreon-SIP@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste0("Chronogrammes DC1 / DC2 du ",date_ref),
#           body = paste0(chemin,"Graph_Chronogrammes/","Graph_Chronogrammes.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 587, 
#                       user.name = "PD0-SUPERVISION-SVC@ad.insee.intra", 
#                       passwd = mdp_cpt ),
#           authenticate = TRUE,
#           send = TRUE
# )

#  Envoi du mel 2 final - graphiques dans fichier zip en SMTP authentifie
# send.mail(from = "Centreon-SIP@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste0("ZIP Chronogrammes DC1 / DC2 du ",date_ref),
#           body = "Graphiques encodes sous le fichier zippe",
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 587, 
#                       user.name = "PD0-SUPERVISION-SVC@ad.insee.intra", 
#                       passwd = mdp_cpt ),
#           authenticate = TRUE,
#           send = TRUE,
#           attach.files = c(paste0("Graph_Chronogrammes_",date_ref,".zip"))
# )

# Envoi du mel 1 final - graphiques dans corps du mel
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste0("Chronogrammes DC1 / DC2 du ",date_ref),
#           body = paste0(chemin,"Graph_Chronogrammes/","Graph_Chronogrammes.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE
# )

# Envoi du mel 2 final - graphiques dans fichier zip
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste0("ZIP Chronogrammes DC1 / DC2 du ",date_ref),
#           body = "Graphiques encodes sous le fichier zippe",
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE,
#           attach.files = c(paste0("Graph_Chronogrammes_",date_ref,".zip"))
# )





