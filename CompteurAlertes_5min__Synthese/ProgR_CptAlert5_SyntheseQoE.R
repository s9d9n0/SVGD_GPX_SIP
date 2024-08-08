
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(RMariaDB)
library(lubridate)
library(ggplot2)

date_ref <- as.character(Sys.Date()-ddays(1))
date_jour <- str_replace_all(as.character(Sys.Date()-ddays(1)),"-","")

# pour tester...
# date_ref <- "2024-05-03"
# date_jour <- "20240503"

# Creation dun repertoire avec la date du jour
rep_gen <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/Fichiers_date_",date_jour,"/")
# plus besoin de creer le repertoire ci-dessous car cree auparavant par le progR ProgR_CptAlert5_Synthese
# dir.create(rep_gen, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# Creation du repertoire des datas
rep_data <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/")

#second endroit generalise pour envoi du ficher (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/"

###


# liste des fichiers csv presents dans le repertoire data et 
# enregistrement dans un dataframe listing
listing <- list.files(rep_data) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich) %>% 
  filter(str_detect(liste_fich,"df_datajreq_") & 
         str_detect(liste_fich,date_ref) & str_detect(liste_fich,"_01h30")) 

# transfert du fichier dans le repertoire synthese
file.copy(paste0(rep_data,listing$liste_fich[1]),
          paste0(rep_gen,listing$liste_fich[1]))

######################################################################
######################################################################

# lecture du fichier global
df <- read.csv2(paste0(rep_gen,listing$liste_fich[1]))

vectheure <- c("_00h00","_00h05","_00h10","_00h15","_00h20","_00h25","_00h30","_00h35","_00h40","_00h45","_00h50","_00h55",
               "_01h00","_01h05","_01h10","_01h15","_01h20","_01h25","_01h30","_01h35","_01h40","_01h45","_01h50","_01h55",
               "_02h00","_02h05","_02h10","_02h15","_02h20","_02h25","_02h30","_02h35","_02h40","_02h45","_02h50","_02h55",
               "_03h00","_03h05","_03h10","_03h15","_03h20","_03h25","_03h30","_03h35","_03h40","_03h45","_03h50","_03h55",
               "_04h00","_04h05","_04h10","_04h15","_04h20","_04h25","_04h30","_04h35","_04h40","_04h45","_04h50","_04h55",
               "_05h00","_05h05","_05h10","_05h15","_05h20","_05h25","_05h30","_05h35","_05h40","_05h45","_05h50","_05h55",
               "_06h00","_06h05","_06h10","_06h15","_06h20","_06h25","_06h30","_06h35","_06h40","_06h45","_06h50","_06h55",
               "_07h00","_07h05","_07h10","_07h15","_07h20","_07h25","_07h30","_07h35","_07h40","_07h45","_07h50","_07h55",
               "_08h00","_08h05","_08h10","_08h15","_08h20","_08h25","_08h30","_08h35","_08h40","_08h45","_08h50","_08h55",
               "_09h00","_09h05","_09h10","_09h15","_09h20","_09h25","_09h30","_09h35","_09h40","_09h45","_09h50","_09h55",
               "_10h00","_10h05","_10h10","_10h15","_10h20","_10h25","_10h30","_10h35","_10h40","_10h45","_10h50","_10h55",
               "_11h00","_11h05","_11h10","_11h15","_11h20","_11h25","_11h30","_11h35","_11h40","_11h45","_11h50","_11h55",
               "_12h00","_12h05","_12h10","_12h15","_12h20","_12h25","_12h30","_12h35","_12h40","_12h45","_12h50","_12h55",
               "_13h00","_13h05","_13h10","_13h15","_13h20","_13h25","_13h30","_13h35","_13h40","_13h45","_13h50","_13h55",
               "_14h00","_14h05","_14h10","_14h15","_14h20","_14h25","_14h30","_14h35","_14h40","_14h45","_14h50","_14h55",
               "_15h00","_15h05","_15h10","_15h15","_15h20","_15h25","_15h30","_15h35","_15h40","_15h45","_15h50","_15h55",
               "_16h00","_16h05","_16h10","_16h15","_16h20","_16h25","_16h30","_16h35","_16h40","_16h45","_16h50","_16h55",
               "_17h00","_17h05","_17h10","_17h15","_17h20","_17h25","_17h30","_17h35","_17h40","_17h45","_17h50","_17h55",
               "_18h00","_18h05","_18h10","_18h15","_18h20","_18h25","_18h30","_18h35","_18h40","_18h45","_18h50","_18h55",
               "_19h00","_19h05","_19h10","_19h15","_19h20","_19h25","_19h30","_19h35","_19h40","_19h45","_19h50","_19h55",
               "_20h00","_20h05","_20h10","_20h15","_20h20","_20h25","_20h30","_20h35","_20h40","_20h45","_20h50","_20h55",
               "_21h00","_21h05","_21h10","_21h15","_21h20","_21h25","_21h30","_21h35","_21h40","_21h45","_21h50","_21h55",
               "_22h00","_22h05","_22h10","_22h15","_22h20","_22h25","_22h30","_22h35","_22h40","_22h45","_22h50","_22h55",
               "_23h00","_23h05","_23h10","_23h15","_23h20","_23h25","_23h30","_23h35","_23h40","_23h45","_23h50","_23h55")

date_ref_court <- str_sub(date_ref,3,10)
vectheure_df <- crossing(date_ref_court,vectheure) %>% as.data.frame() %>%
  unite(date_ref_court:vectheure,col="moment",sep="", na.rm = TRUE, remove = TRUE)
# 288 Obs normalement ici car 12 creneaux de 5 min par heure x 24 = 288



df_QoE <- df %>% select(zone,Gr_Hotes,name_host,description,moment,last_hard_state) %>% 
  filter(Gr_Hotes=="-Selenium") %>% filter(str_sub(description,1,7)!="QoETEST") %>%
  arrange(description,moment) %>% select(-zone,-Gr_Hotes,-name_host)

df_QoE_list <- df_QoE %>% group_by(description) %>% 
  summarise(nb_alert=n()) %>% ungroup() %>% as.data.frame() %>% select(-nb_alert) %>% 
  unlist(use.names=FALSE)

vectheure_df <- crossing(df_QoE_list,vectheure_df) %>% as.data.frame() %>% rename(description=df_QoE_list)


df_QoE <- df_QoE %>% 
  full_join(vectheure_df, by=c("description","moment")) %>% arrange(description, moment)
  
# reconnaissance de certaines maintenances recurrentes par insertion codes 4 ou 5 puis 
# via 2 couleurs bleues
df_QoE <- df_QoE %>%
# QoE-APIM-Requete-DL 
  mutate(last_hard_state=ifelse(str_sub(description,1,19)=="QoE-APIM-Requete-DL" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,14) %in% c("02h30","02h35","02h40","02h45","02h50","02h55",
                                                             "03h00","03h05","03h10","03h15","03h20","03h25"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,19)=="QoE-APIM-Requete-DL" & is.na(last_hard_state) &
                                str_sub(moment,10,14) %in% c("02h30","02h35","02h40","02h45","02h50","02h55",
                                                             "03h00","03h05","03h10","03h15","03h20","03h25"),
                                5,last_hard_state)) %>% 
# QoE-COLTRANE 
  mutate(last_hard_state=ifelse(str_sub(description,1,12)=="QoE-COLTRANE" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,12)=="QoE-COLTRANE" & is.na(last_hard_state) &
                                str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h"),
                                5,last_hard_state)) %>% 
# QoE-FIGARO
  mutate(last_hard_state=ifelse(str_sub(description,1,20)=="QoE-FIGARO-Catalogue" & last_hard_state %in% c(1,2,3) &
                                  str_sub(moment,10,12) %in% c("00h","23h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-FIGARO-Catalogue" & is.na(last_hard_state) &
                                  str_sub(moment,10,12) %in% c("00h","23h"),
                                5,last_hard_state)) %>% 
# QoE-HARMONICA 
  mutate(last_hard_state=ifelse(str_sub(description,1,13)=="QoE-HARMONICA" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,12) %in% c("20h","21h","22h","23h","00h","01h","02h","03h","04h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,13)=="QoE-HARMONICA" & is.na(last_hard_state) &
                                str_sub(moment,10,12) %in% c("20h","21h","22h","23h","00h","01h","02h","03h","04h"),
                                5,last_hard_state)) %>% 
# QoE-RORCAL
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-RORCAL" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,12) %in% c("05h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,10)=="QoE-RORCAL" & is.na(last_hard_state) &
                                str_sub(moment,10,12) %in% c("05h"),
                                5,last_hard_state)) %>% 
# QoE-SIRUS
  mutate(last_hard_state=ifelse(str_sub(description,1,9)=="QoE-SIRUS" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h","05h","06h"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,9)=="QoE-SIRUS" & is.na(last_hard_state) &
                                str_sub(moment,10,12) %in% c("00h","01h","02h","03h","04h","05h","06h"),
                                5,last_hard_state)) %>% 
# QoE-WS-Atis-BRPP
  mutate(last_hard_state=ifelse(str_sub(description,1,16)=="QoE-WS-Atis_BRPP" & last_hard_state %in% c(1,2,3) &
                                str_sub(moment,10,14) %in% c("01h30","01h35","01h40","01h45","01h50","01h55",
                                                             "02h00","02h05","02h10","02h15","02h20","02h25",
                                                             "02h30","02h35","02h40","02h45","02h50","02h55",
                                                             "03h00","03h05","03h10","03h15","03h20","03h25",
                                                             "03h30","03h35","03h40","03h45","03h50","03h55"),
                                4,last_hard_state)) %>% 
  mutate(last_hard_state=ifelse(str_sub(description,1,16)=="QoE-WS-Atis_BRPP" & is.na(last_hard_state) &
                                str_sub(moment,10,14) %in% c("01h30","01h35","01h40","01h45","01h50","01h55",
                                                             "02h00","02h05","02h10","02h15","02h20","02h25",
                                                             "02h30","02h35","02h40","02h45","02h50","02h55",
                                                             "03h00","03h05","03h10","03h15","03h20","03h25",
                                                             "03h30","03h35","03h40","03h45","03h50","03h55"),
                                5,last_hard_state))


df_QoE <- df_QoE %>% pivot_wider(names_from=moment, values_from = last_hard_state) %>%
  as.data.frame() %>% filter(!is.na(description))
df_QoE[is.na(df_QoE)] <- 0 

# simplification libelle...
df_QoE <- df_QoE %>% mutate(description=str_sub(description,5,str_length(description)))


listCol <- colnames(df_QoE) %>% as.data.frame() %>% rename(listCol=".") %>% 
  filter(listCol!="description") %>% mutate(listCol=str_sub(listCol,9,14)) %>% 
  filter(!(str_sub(listCol,5,6) %in% c("05","10","15","20","25",
                                       "35","40","45","50","55"))) %>% 
  mutate(listCol=paste0(" ",str_sub(listCol,2,6)," "))

# ecriture du dataframe QoE dans un .csv aux 2 endroits
write.csv2(df_QoE,paste0(rep_gen,"tableau_QoE.csv"), row.names = FALSE)
write.csv2(df_QoE,paste0(fileGen,"tableau_QoE.csv"), row.names = FALSE)


################################################
# pour le tableau simplifie a mettre dans le mel
listCol_2 <- colnames(df_QoE) %>% as.data.frame() %>% 
  rename(listCol=".") %>% filter(listCol!="description")

# listCol_2[[1]][1]

vec <- unlist(listCol_2[[1]])
df_QoE_2 <- df_QoE %>%  
  pivot_longer(cols = all_of(vec), values_to = "status") %>% 
  mutate(tr30min = case_when(
    str_sub(name,13,13) %in% c("0","1","2") ~ paste0("_",str_sub(name,10,12),"00"),
    str_sub(name,13,13) %in% c("3","4","5") ~ paste0("_",str_sub(name,10,12),"30"),
    TRUE ~ "X")) %>% select(-name) %>% unique() %>%
  mutate(status_car=paste0("_",as.character(status))) %>% select(-status) %>%
  pivot_wider(names_from=tr30min, values_from = status_car,values_fn = list) %>% as.data.frame()

# length(toString(unlist(df_QoE_2[[2]][3])))
gc()

for (i in 1:nrow(df_QoE_2)){
  for (j in 2:ncol(df_QoE_2)){
    df_QoE_2[i,j] <- toString(unlist(df_QoE_2[[j]][i]))
  }
}
gc()

for (i in 1:nrow(df_QoE_2)){
  for (j in 2:ncol(df_QoE_2)){
    if (str_detect(df_QoE_2[i,j],"_2")) {
      df_QoE_2[i,j] <- 2
    } else if (str_detect(df_QoE_2[i,j],"_1")) {
      df_QoE_2[i,j] <- 1
    } else if (str_detect(df_QoE_2[i,j],"_3")) {
      df_QoE_2[i,j] <- 3
    } else if (str_detect(df_QoE_2[i,j],"_4")) {
      df_QoE_2[i,j] <- 4
    } else if (str_detect(df_QoE_2[i,j],"_5")) {
      df_QoE_2[i,j] <- 5
    } else {
      df_QoE_2[i,j] <- 0
    }
  }
}
rm(vec,i,j)
gc()

######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

head <- paste0("<head>",
                  "<meta charset='utf-8'/>",
                  "<title>ENVOI INFOS SYNTHESE QoE</title>",
                  "<style>",
                      ".tabletitre {border:         none;
                                    text-align:     left;
                                    vertical-align: top;}",
               
                      "tr, td {border:          1px solid black;
                               padding:         5px;
                               border-collapse: collapse;
                               text-align:      center;}",
 
                      "thead tr > :first-child {position: sticky;
                                                z-index: 2; left: 0; top: 0;}",
                      "thead td  {position: sticky;
                                  z-index: 1; top: 0;}",
                      "tbody tr > :first-child {position: sticky;
                                                z-index: 1; left: 0;
                                                border: 2px solid black;
                                                background-color: #fff;}",
               
                      ".cell {font-size : 5px;
                              padding: 2px;}",
               
                      ".colzone {text-align:     center;
                                 vertical-align: bottom;
                                 font-size:      15px;
                                 padding :       2px;
                                 writing-mode:   vertical-lr;
                                 transform:      rotate(180deg);}",
               
                      "td.colzone:nth-child(even) {background-color: #FFFFFF;}",
               
                      ".col1 {text-align: left;
                              white-space: nowrap;}",
               
                      ".sup {font-size: 15px;}",
               
                      ".caseRed {color:            rgb(200,0,0);
                                 font-weight:      bold;
                                 background-color: rgb(255,228,196);}",
               
                      ".ligneTot {font-weight:      bold;
                                  background-color: rgb(239,228,176);}",
                  "</style>",
               "</head>")

titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>EPEX <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")

titre2 <- paste0("<p style='font-size:20px'>-- Synth&egrave;se QoE --</p>")

entete_corps <- ""
if (nrow(listCol)>0){
  for (lg in 1:nrow(listCol)){
      entete_corps <- paste0(entete_corps,"<td style='width:30px; 
                                                      border-bottom:0px;
                                                      border-top:0px;' 
                                               colspan=6 class='ligneTot colzone'>",
                                              listCol$listCol[lg],
                                          "</td>")
  }
}
rm(lg)

entete <- paste0("<thead>",
                    "<tr>",
                      "<td style='border-bottom:3px solid black;
                                  border-right :3px solid black;' 
                           class='ligneTot col1'>Libelle QoE</td>",
                      entete_corps,
                    "</tr>",
                 "</thead>")

corps_tab <- ""

if (nrow(df_QoE)>0){
  for (i in 1:nrow(df_QoE)){
    for (j in 1:ncol(df_QoE)){
          print(paste0(i," et ",j," : ",df_QoE[i,j]))
      if ( j==1 ){
        corps_tab <- paste0(corps_tab,"<tr>","<td class='col1'>",df_QoE[i,j],"</td>")
      }
      if ( j!=1 ){
        if (df_QoE[i,j]==1){
          corps_tab <- paste0(corps_tab,"<td style='background-color:orange;' class='cell'>",df_QoE[i,j],"</td>")
        }
        if (df_QoE[i,j]==2){
          corps_tab <- paste0(corps_tab,"<td style='background-color:red;' class='cell'>",df_QoE[i,j],"</td>")
        }
        if (df_QoE[i,j]==3){
          corps_tab <- paste0(corps_tab,"<td style='background-color:grey;' class='cell'>",df_QoE[i,j],"</td>")
        } 
        if (df_QoE[i,j]==4){
          corps_tab <- paste0(corps_tab,"<td style='background-color:#6495ed;' class='cell'>",df_QoE[i,j],"</td>")
        }  
        if (df_QoE[i,j]==5){
          corps_tab <- paste0(corps_tab,"<td style='background-color:#87ceeb;' class='cell'>",df_QoE[i,j],"</td>")
        } 
        if (!(df_QoE[i,j] %in% c(1,2,3,4,5)) & j %in% c(  2, 14, 26, 38, 50, 62,
                                                         74, 86, 98,110,122,134,
                                                        146,158,170,182,194,206,
                                                        218,230,242,254,266,278)){
          corps_tab <- paste0(corps_tab,"<td style='border:0px; 
                                                    border-left:1px solid black;' 
                                                    class='cell'>",df_QoE[i,j],"</td>")
        } else if (!(df_QoE[i,j] %in% c(1,2,3,4,5)) & j==289){
          corps_tab <- paste0(corps_tab,"<td style='border:0px;
                                                    border-right:1px solid black;'
                                                    class='cell'>",df_QoE[i,j],"</td>")          
        } else if (!(df_QoE[i,j] %in% c(1,2,3,4,5))){
          corps_tab <- paste0(corps_tab,"<td style='border:0px;' class='cell'>",df_QoE[i,j],"</td>")
        }   
 
      }
      if ( j==ncol(df_QoE) ){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=289>absence de tableau</td></tr>")
}
rm(i,j)


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                head,
                "<body>",
              
                  "<table class='tabletitre'>",
                    "<tr>",
                      "<td class='tabletitre'>",#titre1,
                                                titre2,"</td>",
                    "</tr>",
                  "</table>",
              
                  "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
                  "<table>", #style='border-collapse:collapse;' 
                      entete, 
                      "<tbody>",
                        corps_tab,
                      "</tbody>",
                      entete, 
                  "</table>",
              
                  "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
                "</body>",
              "</html>")


######################################################################
######################################################################
######################################################################
######################################################################

setwd(rep_gen)

# file <- "CorpsMel_SyntheseQoE.html"
# file.create(file, showWarnings = TRUE)
# cat(tab,file="CorpsMel_SyntheseQoE.html",append=TRUE)

fileName <- paste0("SyntheseQoE_",date_ref_court,".html")
file.create(fileName, showWarnings = TRUE)
cat(tab,file=fileName,append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)
setwd(fileGen)
file.remove(paste0(fileGen,"SyntheseQoE.html"))
cat(tab,file="SyntheseQoE.html",append=TRUE)


######################################################################
######################################################################

# definition du tableau simplifie dans le corps du mel

entete_corps_MEL <- ""
if (nrow(listCol)>0){
  for (lg in 1:nrow(listCol)){
    entete_corps_MEL <- paste0(entete_corps_MEL,"<td style='width:20px; 
                                                            border-bottom:0px;
                                                            border-top:0px;' 
                                                     class='ligneTot colzone'>",
                           listCol$listCol[lg],
                           "</td>")
  }
}
rm(lg)

entete_MEL <- paste0("<thead>",
                 "<tr>",
                 "<td style='border-bottom:3px solid black;
                                  border-right :3px solid black;' 
                           class='ligneTot col1'>Libelle QoE</td>",
                 entete_corps_MEL,
                 "</tr>",
                 "</thead>")

corps_tab_MEL <- ""

if (nrow(df_QoE_2)>0){
  for (i in 1:nrow(df_QoE_2)){
    for (j in 1:ncol(df_QoE_2)){
      print(paste0(i," et ",j," : ",df_QoE_2[i,j]))
      if ( j==1 ){
        corps_tab_MEL <- paste0(corps_tab_MEL,"<tr>","<td class='col1'>",df_QoE_2[i,j],"</td>")
      }
      if ( j!=1 ){
        if (df_QoE_2[i,j]==1){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:orange;' class='cell'>",df_QoE_2[i,j],"</td>")
        } else if (df_QoE_2[i,j]==2){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:red;' class='cell'>",df_QoE_2[i,j],"</td>")
        } else if (df_QoE_2[i,j]==3){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#808080;' class='cell'>",df_QoE_2[i,j],"</td>")
        } else if (df_QoE_2[i,j]==4){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#6495ed;' class='cell'>",df_QoE_2[i,j],"</td>")
        } else if (df_QoE_2[i,j]==5){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='background-color:#87ceeb;' class='cell'>",df_QoE_2[i,j],"</td>")
        } else if (!(df_QoE_2[i,j] %in% c(1,2,3,4,5))){
          corps_tab_MEL <- paste0(corps_tab_MEL,"<td style='border:0px;' class='cell'>",df_QoE_2[i,j],"</td>")
        }   
      }
      if ( j==ncol(df_QoE_2) ){corps_tab_MEL <- paste0(corps_tab_MEL,"</tr>")}
    }
  }
} else {
  corps_tab_MEL <- paste0(corps_tab_MEL,"<tr><td colspan=49>absence de tableau</td></tr>")
}
rm(i,j)


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab_MEL <- paste0("<!DOCTYPE html>",
                "<html>",
                    head,
                    "<body>",
              
                    "<table class='tabletitre'>",
                      "<tr>",
                        "<td class='tabletitre'>",#titre1,
                                                  titre2,"</td>",
                      "</tr>",
                    "</table>",
              
                    "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
                    "<table>", #style='border-collapse:collapse;' 
                        entete_MEL, 
                        "<tbody>",
                            corps_tab_MEL,
                        "</tbody>",
                        entete_MEL, 
                    "</table>",
              
                    "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
                    "</body>",
              "</html>")


setwd(rep_gen)

file <- "SyntheseQoE_MEL.html"
file.create(file, showWarnings = TRUE)
cat(tab_MEL,file="SyntheseQoE_MEL.html",append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)
setwd(fileGen)
file.remove(paste0(fileGen,"SyntheseQoE_MEL.html"))
cat(tab_MEL,file="SyntheseQoE_MEL.html",append=TRUE)


######################################################################
######################################################################

# fichier zip

setwd(rep_gen)
zip("SyntheseQoE_Docs.zip",fileName)
zip("SyntheseQoE_Docs.zip",file)
zip("SyntheseQoE_Docs.zip","tableau_QoE.csv")
file.remove(paste0(fileGen,"SyntheseQoE_Docs.zip"))
file.copy(from = paste0(rep_gen,"SyntheseQoE_Docs.zip"),
          to = paste0(fileGen,"SyntheseQoE_Docs.zip"))


######################################################################
######################################################################

system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min__Synthese\\EnvoiMail_ViaPowershell_SynthQoE.ps1"')


