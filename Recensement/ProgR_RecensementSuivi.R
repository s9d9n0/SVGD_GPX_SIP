
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

#date retardee de 1h
date_jour_1hmoins <- date_jour - dhours(1)

# recuperation de la partie date date_jour_1hmoins
annee_date_jour <- paste0(year(date_jour_1hmoins))
mois_date_jour <- ifelse(str_length(month(date_jour_1hmoins))==1,
                         paste0("0",month(date_jour_1hmoins)),
                         paste0(month(date_jour_1hmoins)))
jour_date_jour <- ifelse(str_length(day(date_jour_1hmoins))==1,
                         paste0("0",day(date_jour_1hmoins)),
                         paste0(day(date_jour_1hmoins)))

date_ref <- paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour)

# date_ref <- as.POSIXct(paste0(annee_date_jour,"-",mois_date_jour,"-",jour_date_jour," 00:00:00")) - dhours(decalage_heure)
rm(annee_date_jour,mois_date_jour,jour_date_jour,
   date_jour_1hmoins)


# pour tests ou corrections...
# date_ref <- "2024-04-29"

###

#rep_sas <- "E:/X_Passerelle_depuis_Z100/"
rep_sas <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/X_Passerelle_depuis_Z100/"
rep_gen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_Recensement_Suivi/"

###

# liste des fichiers csv presents dans le repertoire sas et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_sas) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"Recensement")) %>% 
  mutate(last=ifelse(lg==max(lg),1,0))

nom_fichier_retenu <- listing %>% filter(last==1) %>%
  select(liste_fich) %>% as.character()

# pour tester...
# nom_fichier_retenu <- "Recensement_24-02-08_06h44.zip"

######################################################################
######################################################################
######################################################################
######################################################################

# Creation dun repertoire avec la date du jour
folder <- paste0(rep_gen,"Fichiers_date_",format(Sys.time(),'%Y%m%d'))
if (file.exists(folder)) {
  cat("The folder already exists")
} else {
  dir.create(folder, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# copie du fichier .zip selectionne, 
# suppression de ce meme fichier dans le repertoire sas de depart,
# dezippage des fichier dans le repertoire d'arrivee
file.copy(paste0(rep_sas,nom_fichier_retenu),
          paste0(folder,"/",nom_fichier_retenu))

file.remove(paste0(rep_sas,nom_fichier_retenu))

unzip(paste0(folder,"/",nom_fichier_retenu),
      exdir=folder)

######################################################################
######################################################################
######################################################################
######################################################################

listingZip <- list.files(folder) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,".csv$"))


## Envoi automatique des resultats globaux en HTML via un mel Outlook ----

nom_fichier_TabGlobal <- listingZip %>% filter(str_detect(liste_fich,"VueGlobale")) %>%
  select(liste_fich) %>% as.character()
TabGlobal <- read.csv2(paste0(folder,"/",nom_fichier_TabGlobal))


head <- paste0("<head>",
               "<meta charset='utf-8'/>",
               "<title>ENVOI INFOS RECENSEMENT CENTREON</title>",
                  "<style>",
                      ".tabletitre {border:         none;
                                    text-align:     left;
                                    vertical-align: top;}",
               
                      "tr, td {border:          1px solid black;
                               padding:         5px;
                               border-collapse: collapse;
                               text-align:      right;}",
               
                      ".colZone {text-align: left;}",
               
                      ".piedtable {border:      none;
                                   padding-top: 3px;
                                   text-align:  left;}",
               
                      "sup {font-size: 15px;}",
               
                      ".caseRed {color:            rgb(200,0,0);
                                 font-weight:      bold;
                                 background-color: rgb(255,228,196);}",
               
                      ".caseGray {background-color: Gainsboro;}",
               
                      ".ligneTot {font-weight:      bold;
                                  background-color: rgb(239,228,176);}",
               
                  "</style>",
               "</head>")

titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")
titre2 <- paste0("<p style='font-size:20px'>-- Recensement global Centreon --</p>")

entete <- paste0("<thead>",
                    "<tr>",
                      "<td class='ligneTot colZone'>Zones</td>",
                      "<td class='ligneTot'>Nbre<br>hotes</td>",
                      "<td class='ligneTot'>Nbre<br>services</td>",
                      "<td class='ligneTot'>Nbre<br>metriques</td>",
                      "<td class='ligneTot'>Nbre moyen<br>controles<br>par jour</td>",
                      "<td class='ligneTot'>Nbre moyen<br>controles<br>par jour et<br>par hote</td>",
                      "<td class='ligneTot'>Nbre moyen<br>controles<br>par jour et<br>par service</td>",
                      "<td class='ligneTot'>Nbre moyen<br>controles<br>par jour et<br>par metrique</td>",
                      "<td class='ligneTot'>Repartition<br>selon toutes zones</td>",
                      "<td class='ligneTot'>Repartition<br>uniquement<br>sur zones VIP</td>",
                    "</tr>",
                 "</thead>")


corps_tab <- ""

if (nrow(TabGlobal)>0){
  for (i in 1:nrow(TabGlobal)){
    for (j in 1:10){
      if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
      
      # traitement des premieres lignes du tableau
      if (j==1 && !str_detect(TabGlobal[i,1],"Ensemble") ){
          corps_tab <- paste0(corps_tab,"<td class='colZone'>",TabGlobal[i,j],"</td>")
      } else {
          if (!str_detect(TabGlobal[i,1],"Ensemble") && !(j==9 || j==10)){
            corps_tab <- paste0(corps_tab,"<td>",TabGlobal[i,j],"</td>")
          }
          if (!str_detect(TabGlobal[i,1],"Ensemble") && (j==9 || j==10) && is.na(TabGlobal[i,j]) ){
            corps_tab <- paste0(corps_tab,"<td></td>")
          } 
          if (!str_detect(TabGlobal[i,1],"Ensemble") && (j==9 || j==10) && !is.na(TabGlobal[i,j]) ){
            corps_tab <- paste0(corps_tab,"<td>",TabGlobal[i,j],"</td>")
          }
      }
      
      # traitement des dernieres lignes Ensemble
      if (j==1 && str_detect(TabGlobal[i,1],"Ensemble") ){
        corps_tab <- paste0(corps_tab,"<td class='ligneTot colZone'>",TabGlobal[i,j],"</td>")
      } 
      
      if (j!=1 && str_detect(TabGlobal[i,1],"Ensemble") ){
        if (str_detect(TabGlobal[i,1],"Ensemble") && !(j==9 || j==10)){
          corps_tab <- paste0(corps_tab,"<td class='ligneTot'>",TabGlobal[i,j],"</td>")
        }
        if (str_detect(TabGlobal[i,1],"Ensemble") && (j==9 || j==10)  && is.na(TabGlobal[i,j]) ){
          corps_tab <- paste0(corps_tab,"<td class='ligneTot'></td>")
        }
        if (str_detect(TabGlobal[i,1],"Ensemble") && (j==9 || j==10)  && !is.na(TabGlobal[i,j]) ){
          corps_tab <- paste0(corps_tab,"<td class='ligneTot'>",TabGlobal[i,j],"</td>")
        }
      }

      
      if (j==10){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=10>absence de datas</td></tr>")
}



# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# SANS LES IMAGES
tab_ss_img <- paste0("<!DOCTYPE html>",
                        "<html>",
                        head,
                        "<body>",
                     
                        # titre1,titre2,
                     
                          "<table class='tabletitre'>",
                            "<tr>",
                              "<td class='tabletitre'>",titre1,titre2,"</td>",
                            "</tr>",
                          "</table>",
                     
                        "<h2 style='margin-top: 10; margin-bottom: 5; color: DarkGoldenrod;'>
                              I-/ Tableau Global - Population Centreon selon les zones du SI
                         </h2>",
                     
                        "<table>",
                          entete,corps_tab,
                        "</table>",
                     
                     "</body>",
                     "</html>")


######################################################################
######################################################################
######################################################################
######################################################################

setwd(folder)

file.create("CorpsMel_Recensement.html", showWarnings = TRUE)
cat(tab_ss_img,file="CorpsMel_Recensement.html",append=TRUE)

######################################################################
######################################################################
######################################################################
######################################################################

# Preparation mel avec fichiers en PJ et envoi via Powershell

#second endroit generalise du fichier du jour html et du zip
fichierjour <- paste0(rep_gen,"General")

nom_fichier_Agreg <- list.files(folder) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"^Agreg")) %>%
  select(liste_fich)

nom_fichier_VueGlobale <- list.files(folder) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  mutate(lg=row_number()) %>% relocate(lg) %>% 
  filter(str_detect(liste_fich,"^VueGlobale")) %>%
  select(liste_fich) %>% as.character()

file.copy(from=paste0(folder,"/","CorpsMel_Recensement.html"),
          to=paste0(fichierjour,"/","CorpsMel_Recensement.html"))

file.copy(from=paste0(folder,"/",nom_fichier_Agreg$liste_fich[1]),
          to=paste0(fichierjour,"/","Agreg_NivGrHote.csv"))
file.copy(from=paste0(folder,"/",nom_fichier_Agreg$liste_fich[2]),
          to=paste0(fichierjour,"/","Agreg_NivHote.csv"))
file.copy(from=paste0(folder,"/",nom_fichier_Agreg$liste_fich[3]),
          to=paste0(fichierjour,"/","Agreg_NivService.csv"))
file.copy(from=paste0(folder,"/",nom_fichier_VueGlobale),
          to=paste0(fichierjour,"/","VueGlobale.csv"))

setwd(fichierjour)

zip("Recensement_Centreon.zip","Agreg_NivGrHote.csv")
zip("Recensement_Centreon.zip","Agreg_NivHote.csv")
zip("Recensement_Centreon.zip","Agreg_NivService.csv")
zip("Recensement_Centreon.zip","VueGlobale.csv")


# PARTIE : ENVOI DU MEL

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_Recensement_Suivi\\EnvoiMail_ViaPowershell.ps1"')


# puis suppression des diffrents fichiers envoyes...
file.remove("CorpsMel_Recensement.html")
file.remove("Recensement_Centreon.zip")
file.remove("Agreg_NivGrHote.csv")
file.remove("Agreg_NivHote.csv")
file.remove("Agreg_NivService.csv")
file.remove("VueGlobale.csv")


# tout a la fin, suppression de tous les fichiers .csv dans le repertoire de travail
# car present dans le .zip et pour faire de la place
setwd(folder)

for (i in 1:nrow(listingZip)){
  cat(paste0("numero ",i," ",listingZip$liste_fich[i],"\n"))
  file.remove(paste0(folder,"/",listingZip$liste_fich[i]))
}

