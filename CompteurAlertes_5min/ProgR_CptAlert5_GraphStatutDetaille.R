
## Chargement des packages utilises dans le programme ----
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/"

# Appel des fonctions utiles a la realisations des graphiques
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
# date_ref <- "2024-01-30"

# transformation en json
library(jsonlite)

file.remove(paste0(chemin,"Graph_Chronogrammes_Detailles/","date_json.json"))

date_json <- toJSON(date_ref)
date_json <- prettify(date_json)
# enregistrement du JSON
write_json(date_json,paste0(chemin,"Graph_Chronogrammes_Detailles/","date_json.json"))

##########################################################
##########################################################


# creation des 6 graphiques de type chronogramme agreges selon les statuts
# pour chaque croisement zone x DC

df_status <- read.csv2(paste0(chemin,"df_statusj_",date_ref,"_01h30.csv"))

df_status <- df_status %>% 
  group_by(moment,zone) %>% 
  summarize(Critical=sum(Critical),
         Warning=sum(Warning),
         Unknown=sum(Unknown)) %>% ungroup() %>%
  as.data.frame() %>% 
  pivot_longer(.,cols=c("Critical","Warning","Unknown"),values_to="eff") %>%
  as.data.frame()

df_status <- df_status %>% 
  mutate(eff_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),eff,""),
         eff_label=ifelse(eff_label=="0","",eff_label),
         moment_label=ifelse(str_sub(moment,13,14) %in% c("00","15","30","45"),moment,""),
         moment_label=ifelse(str_sub(moment,13,14) %in% c("15","30","45"),str_sub(moment,10,14),moment_label)) %>% 
  relocate(moment_label,.after="moment")
  

##############
#### dc1 ####
##############
# DMZ dc1 & PARTENAIRE dc1
df_status_DMZ_dc1 <- df_status %>% filter(zone=="02_DMZ_dc1" | zone=="04_Part_dc1") 
char <- fct_modif_label_x(df_status_DMZ_dc1)
graph_DMZ_dc1 <- fct_Crea_BarChart(df_status_DMZ_dc1)
graph_DMZ_dc1

gc()

####
# INTERNE dc1
df_status_INT_dc1 <- df_status %>% filter(zone=="06_ZoneInt_dc1")
char <- fct_modif_label_x(df_status_INT_dc1)
graph_INT_dc1 <- fct_Crea_BarChart(df_status_INT_dc1)
graph_INT_dc1

gc()

####
# SIA dc1
df_status_SIA_dc1 <- df_status %>% filter(zone=="08_ZoneSIA_dc1")
char <- fct_modif_label_x(df_status_SIA_dc1)
graph_SIA_dc1 <- fct_Crea_BarChart(df_status_SIA_dc1)
graph_SIA_dc1

gc()

###################
#### dc2 ####
###################
# DMZ dc2 & PARTENAIRE dc2
df_status_DMZ_dc2 <- df_status %>% filter(zone=="01_DMZ_dc2" | zone=="03_Part_dc2") 
char <- fct_modif_label_x(df_status_DMZ_dc2)
graph_DMZ_dc2 <- fct_Crea_BarChart(df_status_DMZ_dc2)
graph_DMZ_dc2

gc()

####
# INTERNE dc2
df_status_INT_dc2 <- df_status %>% filter(zone=="05_ZoneInt_dc2")
char <- fct_modif_label_x(df_status_INT_dc2)
graph_INT_dc2 <- fct_Crea_BarChart(df_status_INT_dc2)
graph_INT_dc2

gc()

####
# SIA dc2
df_status_SIA_dc2 <- df_status %>% filter(zone=="07_ZoneSIA_dc2")
char <- fct_modif_label_x(df_status_SIA_dc2)
graph_SIA_dc2 <- fct_Crea_BarChart(df_status_SIA_dc2)
graph_SIA_dc2

gc()


# sauvegarde des 6 graphiques sur des fichiers .png

setwd(dir = "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/Graph_Chronogrammes_Detailles")

png(file="graph_DMZ_dc1.png",width = 750, height = 300)
graph_DMZ_dc1
dev.off()
png(file="graph_INT_dc1.png",width = 750, height = 300)
graph_INT_dc1
dev.off()
png(file="graph_SIA_dc1.png",width = 750, height = 300)
graph_SIA_dc1
dev.off()

png(file="graph_DMZ_dc2.png",width = 750, height = 300)
graph_DMZ_dc2
dev.off()
png(file="graph_INT_dc2.png",width = 750, height = 300)
graph_INT_dc2
dev.off()
png(file="graph_SIA_dc2.png",width = 750, height = 300)
graph_SIA_dc2
dev.off()

# encodage des 6 images
library(base64enc)
graph_DMZ_dc1_encode <- base64encode("graph_DMZ_dc1.png")
write.csv2(graph_DMZ_dc1_encode,"graph_DMZ_dc1_encode.csv", row.names = FALSE)
graph_INT_dc1_encode <- base64encode("graph_INT_dc1.png")
write.csv2(graph_INT_dc1_encode,"graph_INT_dc1_encode.csv", row.names = FALSE)
graph_SIA_dc1_encode <- base64encode("graph_SIA_dc1.png")
write.csv2(graph_SIA_dc1_encode,"graph_SIA_dc1_encode.csv", row.names = FALSE)

graph_DMZ_dc2_encode <- base64encode("graph_DMZ_dc2.png")
write.csv2(graph_DMZ_dc2_encode,"graph_DMZ_dc2_encode.csv", row.names = FALSE)
graph_INT_dc2_encode <- base64encode("graph_INT_dc2.png")
write.csv2(graph_INT_dc2_encode,"graph_INT_dc2_encode.csv", row.names = FALSE)
graph_SIA_dc2_encode <- base64encode("graph_SIA_dc2.png")
write.csv2(graph_SIA_dc2_encode,"graph_SIA_dc2_encode.csv", row.names = FALSE)


##########################################################
##########################################################
##########################################################
##########################################################


###########
# MEL
###########

chemin_graph <- "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min\\Graph_Chronogrammes_Detailles\\"

# recuperation des images encodees dans des variables
graph_DMZ_dc1_encode <- read.csv2("graph_DMZ_dc1_encode.csv")
graph_DMZ_dc1_encode <- graph_DMZ_dc1_encode$x[1]
graph_INT_dc1_encode <- read.csv2("graph_INT_dc1_encode.csv")
graph_INT_dc1_encode <- graph_INT_dc1_encode$x[1]
graph_SIA_dc1_encode <- read.csv2("graph_SIA_dc1_encode.csv")
graph_SIA_dc1_encode <- graph_SIA_dc1_encode$x[1]

graph_DMZ_dc2_encode <- read.csv2("graph_DMZ_dc2_encode.csv")
graph_DMZ_dc2_encode <- graph_DMZ_dc2_encode$x[1]
graph_INT_dc2_encode <- read.csv2("graph_INT_dc2_encode.csv")
graph_INT_dc2_encode <- graph_INT_dc2_encode$x[1]
graph_SIA_dc2_encode <- read.csv2("graph_SIA_dc2_encode.csv")
graph_SIA_dc2_encode <- graph_SIA_dc2_encode$x[1]


# Creation de la page HTML
head <- paste0("<head>",
               "<meta charset='utf-8'/>",
               "<title>CHRONOGRAMMES</title>",
               "<style>",
                  ".entete {text-align: center;
                            font-size: 30px;
                            line-height:30%;
                    }",
                  ".colzone {text-align: center;
                             font-size: 25px;
                             writing-mode: vertical-lr;
                             transform: rotate(180deg);
                    }",
               "</style>",
               "</head>")


dc1titre <- paste0("<p class='entete'><strong>Chronogrammes - DC1 dc1</strong></p>")
imgdc1_DMZ_encode <- paste0("<p><img src='data:image/png;base64,",graph_DMZ_dc1_encode,"'
                                   alt='Chronogramme dc1 DMZ'/></p>")
imgdc1_INT_encode <- paste0("<p><img src='data:image/png;base64,",graph_INT_dc1_encode,"'
                                   alt='Chronogramme dc1 INT'/></p>")
imgdc1_SIA_encode <- paste0("<p><img src='data:image/png;base64,",graph_SIA_dc1_encode,"'
                                   alt='Chronogramme dc1 SIA'/></p>")

dc2titre <- paste0("<p class='entete'><strong>Chronogrammes - DC2 dc2</strong></p>")
imgdc2_DMZ_encode <- paste0("<p><img src='data:image/png;base64,",graph_DMZ_dc2_encode,"'
                                   alt='Chronogramme dc2 DMZ'/></p>")
imgdc2_INT_encode <- paste0("<p><img src='data:image/png;base64,",graph_INT_dc2_encode,"'
                                   alt='Chronogramme dc2 INT'/></p>")
imgdc2_SIA_encode <- paste0("<p><img src='data:image/png;base64,",graph_SIA_dc2_encode,"'
                                   alt='Chronogramme dc2 SIA'/></p>")


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
pageHTML_encode <- paste0("<!DOCTYPE html>",
                          "<html>",
                            head,
                            "<body>",
                              "<table>",
                                "<thead>",
                                  "<tr>",
                                    "<th style='width:10%;'>","</th>",
                                    "<th>",dc1titre,"</th>",
                                    "<th>",dc2titre,"</th>",
                                  "</tr>",
                                "</thead>",
                                "<tbody>",
                                  "<tr>",
                                    "<td style='width:10%;'>","<p class='colzone'>zones <strong>DMZ</strong>/<strong>Partenaires</strong></p>","</td>",
                                    "<td>",imgdc1_DMZ_encode,"</td>",
                                    "<td>",imgdc2_DMZ_encode,"</td>",
                                  "</tr>",
                                  "<tr>",
                                    "<td style='width:10%;'>","<p class='colzone'>zone <strong>Interne</strong></p>","</td>",
                                    "<td>",imgdc1_INT_encode,"</td>",
                                    "<td>",imgdc2_INT_encode,"</td>",
                                  "</tr>",
                                  "<tr>",
                                    "<td style='width:10%;'>","<p class='colzone'>zone <strong>Sia</strong></p>","</td>",
                                    "<td>",imgdc1_SIA_encode,"</td>",
                                    "<td>",imgdc2_SIA_encode,"</td>",
                                  "</tr>",
                                "</tbody>",                      
                              "</table>",
                            "</body>",
                          "</html>")

file <- "Graph_Chronogrammes_zip.html"
file.create(file, showWarnings = TRUE)
cat(pageHTML_encode,file="Graph_Chronogrammes_zip.html",append=TRUE)


# fichier zip
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),paste0("Graph_Chronogrammes_zip.html"))
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_DMZ_dc1.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_INT_dc1.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_SIA_dc1.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_DMZ_dc2.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_INT_dc2.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"graph_SIA_dc2.png")
zip(paste0("Graph_Chronogrammes_",date_ref,".zip"),"date_json.json")

# file.remove(paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))

file.copy(from=paste0(chemin_graph,"/Graph_Chronogrammes_",date_ref,".zip"),
          to=paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))

######################################################################
######################################################################
######################################################################
######################################################################


# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min\\EnvoiMail_ViaPowershell_Graph_02h00.ps1"')

# suppression du fichier .zip envoye...
file.remove(paste0(chemin_graph,"/Graph_Chronogrammes_jour_total.zip"))



