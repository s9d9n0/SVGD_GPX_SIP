
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
dir.create(rep_gen, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# Creation du repertoire des datas
rep_data <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/")

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

# traitement et agregation...
df <- read.csv2(paste0(rep_gen,listing$liste_fich[1])) %>% 
  mutate(status = case_when(
    last_hard_state==1 ~ "_1_Warning",
    last_hard_state==2 ~ "_2_Critical",
    last_hard_state==3 ~ "_3_Unknown",
    TRUE ~ "4_autres")) %>% relocate(status, .after=last_hard_state)

df_synth_corp <- df %>% group_by(zone,status) %>% 
  summarise(sum_unite=sum(unite), sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame() 
  
df_synth_corp_unite <- df_synth_corp %>% select(-sum_poids) %>% 
  pivot_wider(names_from=status, values_from = sum_unite) %>% as.data.frame() %>% 
  rename("unite_Warning"="_1_Warning","unite_Critical"="_2_Critical","unite_Unknown"="_3_Unknown")
df_synth_corp_poids <- df_synth_corp %>% select(-sum_unite) %>% 
  pivot_wider(names_from=status, values_from = sum_poids) %>% as.data.frame() %>% 
  rename("poids_Warning"="_1_Warning","poids_Critical"="_2_Critical","poids_Unknown"="_3_Unknown")
  
df_synth_corp <- df_synth_corp_unite %>% left_join(df_synth_corp_poids, by=c("zone"))
rm(df_synth_corp_unite,df_synth_corp_poids)

vectzone <- data.frame(zone = c("01_DMZ_dc2",     "02_DMZ_dc1",
                                "03_Part_dc2",    "04_Part_dc1", 
                                "05_ZoneInt_dc2", "06_ZoneInt_dc1",
                                "07_ZoneSIA_dc2", "08_ZoneSIA_dc1"))

df_synth_corp <- vectzone %>% full_join(df_synth_corp, by=c("zone"))
df_synth_corp[is.na(df_synth_corp)] <- 0 
rm(vectzone)


df_synth_corp <- df_synth_corp %>% rowwise() %>% 
  mutate(unite_Total=sum(unite_Warning,unite_Critical,unite_Unknown),
         poids_Total=sum(poids_Warning,poids_Critical,poids_Unknown))

df_synth_dc2 <- df_synth_corp %>% filter(str_detect(zone,"dc2")) %>% group_by %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% as.data.frame() %>% 
  mutate(zone="Ensemble DC2") %>% relocate(zone)

df_synth_dc1 <- df_synth_corp %>% filter(str_detect(zone,"dc1")) %>% group_by %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% as.data.frame() %>% 
  mutate(zone="Ensemble DC1") %>% relocate(zone) 

df_synth_Tot <- df_synth_corp %>% group_by %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>% ungroup() %>% as.data.frame() %>% 
  mutate(zone="Ensemble SI") %>% relocate(zone)

df_synth <- rbind(df_synth_corp, df_synth_Tot, df_synth_dc2, df_synth_dc1)
rm(df_synth_corp, df_synth_Tot, df_synth_dc2, df_synth_dc1)

# ordonnancement des colonnes...
df_synth <- df_synth %>% 
  relocate(unite_Warning, .after=zone) %>% 
  relocate(unite_Critical, .after=unite_Warning) %>% 
  relocate(unite_Unknown, .after=unite_Critical) %>% 
  relocate(poids_Warning, .after=unite_Unknown) %>% 
  relocate(poids_Critical, .after=poids_Warning) %>% 
  relocate(poids_Unknown, .after=poids_Critical) %>% 
  relocate(unite_Total, .after=poids_Unknown) %>% 
  relocate(poids_Total, .after=unite_Total) 

write.csv2(df_synth, paste0(rep_gen,"synth_jour_",date_jour,".csv"), row.names=FALSE)


######################################################################
######################################################################
######################################################################
######################################################################

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

zone <- c("01_DMZ_dc2",     "02_DMZ_dc1",     "03_Part_dc2",    "04_Part_dc1", 
          "05_ZoneInt_dc2", "06_ZoneInt_dc1", "07_ZoneSIA_dc2", "08_ZoneSIA_dc1")

date_ref_court <- str_sub(date_ref,3,10)
vectheurezone_df <- crossing(date_ref_court,vectheure, zone) %>% as.data.frame() %>%
  unite(date_ref_court:vectheure,col="moment",sep="", na.rm = TRUE, remove = TRUE)
# 2304 Obs normalement ici car 12 creneaux de 5 min par heure x 24 = 288 x 8 zones




# Fonction
fct_modif_label_x <- function(nb_modalite,df){
  ########################################
  # DEBUT Creation du vecteur moment label
  moment_vect <- df %>% select(moment,moment_label) %>%
    mutate(ligne=row_number()) %>% 
    mutate(mult=ligne %% nb_modalite) %>% filter(mult==1) %>%
    mutate(ligne=row_number()) %>% 
    select(ligne,moment,moment_label)
  
  virgule <- c(',')
  for (i in 1:(nrow(moment_vect)-2)){
    virgule <- append(virgule,',')
  }
  virgule <- virgule %>% as.data.frame() %>%
    rename(moment_label = ".") %>% 
    mutate(ligne=row_number(),moment="") %>% select(ligne,moment,moment_label) 
  
  moment_vect <- rbind(moment_vect,virgule) %>% arrange(ligne)
  
  char <- c()
  for (i in 1:nrow(moment_vect)){
    char <- append(char,moment_vect$moment_label[i])
    cat(moment_vect$moment_label[i],"\n")
  }
  names(char) <- moment_vect$moment
  
  return (char)
  # structure attendue du char...
  # char <- c('23-06-28_00h00' = '23-06-28_00h00',
  #           ',',
  #           '23-06-28_00h05' = '_',
  #           ',',
  #           '23-06-28_00h10' = '23-06-28_00h10',
  #           ',',
  #           '23-06-28_00h15' = '_')
  
  # FIN  Creation du vecteur moment label
  ########################################    
}

###################################################
###################################################
# Partie realisation graphique TCD moment x zone...

df_moment_zone <- df %>% select(moment,zone,poids) %>% group_by(moment,zone) %>% 
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame()

df_moment_zone <- df_moment_zone %>% 
  full_join(vectheurezone_df, by=c("moment","zone")) %>% arrange(moment,zone) #%>% 
  #pivot_wider(names_from=zone, values_from = sum_poids) %>% as.data.frame()
df_moment_zone[is.na(df_moment_zone)] <- 0 

df_moment_zone <- df_moment_zone %>% 
  mutate(moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),
                             moment,""),
         moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),
                             str_sub(moment,10,14),moment_label)) %>% 
  relocate(moment_label, .after="moment")


char <- fct_modif_label_x(8, df_moment_zone)
outgraph <- ggplot(df_moment_zone, 
                   aes(x=moment,y=sum_poids, group=zone)) + 
            geom_line(aes(color=zone),size=1.2)+
            theme_light() +  
            scale_x_discrete(labels=char) +
            scale_y_continuous(breaks=seq(0,100000,by=100), 
                               minor_breaks = seq(0 , 100000, 50)) +
            scale_color_manual(values=c("#ffd1d4", "#a3b7ca",
                                        "#ffb7c5", "#7593af",
                                        "#ffa0c5", "#476f95",
                                        "#c095e4", "#194a7a")) +
            theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
                  axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=80, face="italic"),
        
                  axis.title.y = element_text(color = "black", size=10, face="italic"),
                  axis.text.y = element_text(color="black", size=8),
        
                  panel.border = element_blank(),
                  legend.position="bottom") +  
            labs(x="moment de la journee", y="score")
outgraph
gc()

# sauvegarde du graphique sur fichier .png
setwd(rep_gen)
png(file="graph_MomentZone.png",width = 1300, height = 500)
outgraph
dev.off()

# encodage image
library(base64enc)
graph_MomentZone_encode <- base64encode("graph_MomentZone.png")
write.csv2(graph_MomentZone_encode,"graph_MomentZone.csv", row.names = FALSE)


###################################################
###################################################
# Partie realisation graphique TCD moment x statut... (hors zones 00, 25, 50 DC2)

df_2 <- read.csv2(paste0(rep_gen,listing$liste_fich[1])) %>% 
  filter(!(zone %in% c("01_DMZ_dc2","03_Part_dc2","05_ZoneInt_dc2"))) %>% 
  mutate(status = case_when(
    last_hard_state==1 ~ "_2_Warning",
    last_hard_state==2 ~ "_1_Critical",
    last_hard_state==3 ~ "_3_Unknown",
    TRUE ~ "4_autres")) %>% relocate(status, .after=last_hard_state)

status <- c("_1_Critical","_2_Warning","_3_Unknown")

date_ref_court <- str_sub(date_ref,3,10)
vectheurestatus_df <- crossing(date_ref_court,vectheure, status) %>% as.data.frame() %>%
  unite(date_ref_court:vectheure,col="moment",sep="", na.rm = TRUE, remove = TRUE)
# 864 Obs normalement ici car 12 creneaux de 5 min par heure x 24 = 288 x 3 zones


df_moment_statut <- df_2 %>% select(moment,status,poids) %>% group_by(moment,status) %>% 
  summarise(sum_poids=sum(poids)) %>% ungroup() %>% as.data.frame()

df_moment_statut <- df_moment_statut %>% 
  full_join(vectheurestatus_df, by=c("moment","status")) %>% arrange(moment,status) #%>% 
#pivot_wider(names_from=zone, values_from = sum_poids) %>% as.data.frame()
df_moment_statut[is.na(df_moment_statut)] <- 0 

df_moment_statut <- df_moment_statut %>% 
  mutate(moment_label=ifelse(str_sub(moment,13,14)%in% c("00","15","30","45"),
                             moment,""),
         moment_label=ifelse(str_sub(moment,13,14)%in% c("15","30","45"),
                             str_sub(moment,10,14),moment_label)) %>% 
  relocate(moment_label, .after="moment")

char <- fct_modif_label_x(3, df_moment_statut)
outgraph <- ggplot(df_moment_statut, 
                   aes(x=moment,y=sum_poids,fill=factor(status, 
                       levels=c("_3_Unknown","_2_Warning","_1_Critical")))) + 
  geom_col(position="stack", width = 1) +
  scale_fill_manual(name="legende des statuts :", 
                    values = c("_1_Critical" = "red",
                               "_2_Warning" = "orange",
                               "_3_Unknown" = "grey"),
                    labels = c("_1_Critical" = "Critical",
                               "_2_Warning" = "Warning",
                               "_3_Unknown" = "Unknown")) +
  scale_x_discrete(labels = char) +
  scale_y_continuous(breaks=seq(0,100000,by=100), 
                     minor_breaks = seq(0 , 100000, 50)) +
  theme_light() +
  theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
        axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=88, face="italic"),
        
        axis.title.y = element_text(color = "black", size=10, face="italic"),
        axis.text.y = element_text(color="black", size=8),
        
        panel.border = element_blank(),
        legend.position="bottom") +
  labs(x="moment de la journee", y="score") #+
  #geom_text(aes(label = sum_poids, fontface=4), size=2, vjust=0, hjust=1, angle=80, position="stack")
outgraph
gc()


# sauvegarde du graphique sur fichier .png
setwd(rep_gen)
png(file="graph_MomentStatus.png",width = 1300, height = 500)
outgraph
dev.off()

# encodage image
library(base64enc)
graph_MomentStatus_encode <- base64encode("graph_MomentStatus.png")
write.csv2(graph_MomentStatus_encode,"graph_MomentStatus.csv", row.names = FALSE)


######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

head <- paste0("<head>",
                  "<meta charset='utf-8'/>",
                  "<title>ENVOI INFOS SYNTHESE</title>",
                  "<style>",
                    ".tabletitre {border:         none;
                                  text-align:     left;
                                  vertical-align: top;}",
               
                    "tr, td {border:          1px solid black;
                             padding:         5px;
                             border-collapse: collapse;
                             text-align:      center;}",
               
                    ".col1 {text-align: left;}",           
               
                    "sup {font-size: 15px;}",
               
                    ".caseRed {color:            rgb(200,0,0);
                               font-weight:      bold;
                               background-color: rgb(255,228,196);}",
               
                    ".ligneTot {font-weight:      bold;
                                background-color: rgb(239,228,176);}",
                  "</style>",
              "</head>")

titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")

titre2 <- paste0("<p style='font-size:20px'>-- Synth&egrave;se des alertes --</p>")

entete <- paste0("<thead>",
                    "<tr>",
                      "<td class='ligneTot'>Zone</td>",
                      "<td class='ligneTot'>Warning <br />(unite)</td>",
                      "<td class='ligneTot'>Critical <br />(unite)</td>",
                      "<td class='ligneTot'>Inconnu <br />(unite)</td>",
                      "<td class='ligneTot'>Warning <br />(poids)</td>",
                      "<td class='ligneTot'>Critical <br />(poids)</td>",
                      "<td class='ligneTot'>Inconnu <br />(poids)</td>",
                      "<td class='ligneTot'>Total <br />(unite)</td>",
                      "<td class='ligneTot'>Total <br />(poids)</td>",
                    "</tr>",
                 "</thead>")

corps_tab <- ""

if (nrow(df_synth)>0){
  for (i in 1:nrow(df_synth)){
    for (j in 1:9){
      if ( i<9 & j==1 ){
        corps_tab <- paste0(corps_tab,"<tr>")
        corps_tab <- paste0(corps_tab,"<td class='col1'>",df_synth[i,j],"</td>")
      }
      if (i<9 & j!=1){
        corps_tab <- paste0(corps_tab,"<td>",df_synth[i,j],"</td>")
      }  
      if (i<9 & j==ncol(df_synth)){corps_tab <- paste0(corps_tab,"</tr>")} 
      
      
      if ( i %in% c(9,10,11) & j==1 ){
        corps_tab <- paste0(corps_tab,"<tr>")
        corps_tab <- paste0(corps_tab,"<td class='ligneTot col1'>",df_synth[i,j],"</td>")
      }
      if ( i %in% c(9,10,11) & j!=1 ){
        corps_tab <- paste0(corps_tab,"<td class='ligneTot'>",df_synth[i,j],"</td>")
      }      
      if (i %in% c(9,10,11) & j==ncol(df_synth)){corps_tab <- paste0(corps_tab,"</tr>")}
    }
  }
} else {
  corps_tab <- paste0(corps_tab,"<tr><td colspan=9>absence de tableau</td></tr>")
}
rm(i,j)



# recuperation des images encodees dans des variables
graph_MomentZone_encode <- read.csv2("graph_MomentZone.csv")
graph_MomentZone_encode <- graph_MomentZone_encode$x[1]

graph_MomentStatus_encode <- read.csv2("graph_MomentStatus.csv")
graph_MomentStatus_encode <- graph_MomentStatus_encode$x[1]


titre_MomentZone <- paste0("<p style='font-size:20px, line-height:30%'>
                                <strong>Chronogramme selon les zones du SI <br />
                                        (hors zones 00, 25, 50 de DC2)</strong>
                            </p>")
MomentZone_encode <- paste0("<p><img src='data:image/png;base64,",graph_MomentZone_encode,"'
                                 alt='Chronogramme selon les zones du SI'/>
                             </p>")

titre_MomentStatus <- paste0("<p style='font-size:20px, line-height:30%'>
                                <strong>Chronogramme selon les statuts d'alerte <br />
                                        (hors zones 00, 25, 50 de DC2)</strong>
                            </p>")
MomentStatus_encode <- paste0("<p><img src='data:image/png;base64,",graph_MomentStatus_encode,"'
                                 alt='Chronogramme selon les zones du SI'/>
                             </p>")



# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                  head,
                  "<body>",
              
                      "<table class='tabletitre'>",
                          "<tr>",
                              "<td class='tabletitre'>",titre1,titre2,"</td>",
                          "</tr>",
                      "</table>",
              
                      "<p> date de r&eacute;f&eacute;rence : ",date_ref,"</p>",
              
                      "<table>",
                          entete, corps_tab,
                      "</table>",
              
                      titre_MomentZone, MomentZone_encode,
              
                      titre_MomentStatus, MomentStatus_encode,
              
                  "</body>",
              "</html>")


######################################################################
######################################################################
######################################################################
######################################################################

setwd(rep_gen)

file <- "CorpsMel_Synthese.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="CorpsMel_Synthese.html",append=TRUE)

#second endroit generalise pour envoi du ficher (niveau general)

fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min__Synthese/"
setwd(fileGen)
file.remove(paste0(fileGen,"CorpsMel_Synthese.html"))
cat(tab,file="CorpsMel_Synthese.html",append=TRUE)

###################
###################
## LES TRANSFERTS...
###################
###################

# transfert tableau df_synth
write.csv2(df_synth,paste0(rep_gen,"tableau_synthese.csv"), row.names = FALSE)

file.remove(paste0(fileGen,"tableau_synthese.csv"))
write.csv2(df_synth,paste0(fileGen,"tableau_synthese.csv"), row.names = FALSE)

# transfert image MomentZone dans le repertoire envoi
file.remove(paste0(fileGen,"graph_MomentZone.png"))
file.copy(paste0(rep_gen,"graph_MomentZone.png"),
          paste0(fileGen,"graph_MomentZone.png"))

file.remove(paste0(fileGen,"graph_MomentStatus.png"))
file.copy(paste0(rep_gen,"graph_MomentStatus.png"),
          paste0(fileGen,"graph_MomentStatus.png"))


# fichier zip
file.remove(paste0(fileGen,"CorpsMel_Synthese_Docs.zip"))
zip("CorpsMel_Synthese_Docs.zip","CorpsMel_Synthese.html")
zip("CorpsMel_Synthese_Docs.zip","tableau_synthese.csv")
zip("CorpsMel_Synthese_Docs.zip","graph_MomentStatus.png")
zip("CorpsMel_Synthese_Docs.zip","graph_MomentZone.png")


######################################################################
######################################################################

system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\CompteurAlertes_5min__Synthese\\EnvoiMail_ViaPowershell_Synth.ps1"')



