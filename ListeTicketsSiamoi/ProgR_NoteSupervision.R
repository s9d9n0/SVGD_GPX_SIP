
#.libPaths()
## Chargement des packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(lubridate)

# library(rmarkdown)
# library(knitr)

########################################################################################
########################################################################################
########################################################################################

# definition du chemin global
chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Liste_TicketsSiamoi_Gepex/"

date <- format(Sys.time(),'%Y-%m-%d')
# date <- "2024-06-28"
 
 
#travail autour de la date pour une meilleure presentation sur la sortie HTML
date_jour <- str_sub(date,9,10)
date_mois <- str_sub(date,6,7)
date_an <- str_sub(date,1,4)


if (date_mois=="01"){date_mois_lib <- " janvier "}
if (date_mois=="02"){date_mois_lib <- " f&eacute;vrier "}
if (date_mois=="03"){date_mois_lib <- " mars "}
if (date_mois=="04"){date_mois_lib <- " avril "}
if (date_mois=="05"){date_mois_lib <- " mai "}
if (date_mois=="06"){date_mois_lib <- " juin "}
if (date_mois=="07"){date_mois_lib <- " juillet "}
if (date_mois=="08"){date_mois_lib <- " ao&ucirc;t "}
if (date_mois=="09"){date_mois_lib <- " septembre "}
if (date_mois=="10"){date_mois_lib <- " octobre "}
if (date_mois=="11"){date_mois_lib <- " novembre "}
if (date_mois=="12"){date_mois_lib <- " d&eacute;cembre "}

date_pres <- paste0(date_jour,date_mois_lib,date_an)

cheminSiamoi <- "E:/InfoCom_entre_CentreonSiamoi/Siamoi_vers_Centreon/listing_etat_tickets_gepex/"
cheminSiamoi <- paste0(cheminSiamoi,date,"/")

###

# liste des fichiers csv presents dans le repertoire cheminSiamoi et 
# enregistrement dans un dataframe listing
listing <- list.files(cheminSiamoi) %>% as.data.frame() %>% 
    rename(liste_fich=".") %>% arrange(liste_fich)

# copie de tous les fichiers dans le repertoire chemin
for (i in 1:nrow(listing)){
    cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
    file.copy(paste0(cheminSiamoi,listing$liste_fich[i]),
              paste0(chemin,listing$liste_fich[i]))
}

rm(listing,i)
rm(date_jour,date_mois,date_an,date_mois_lib)
rm(cheminSiamoi)

########################################################################################
########################################################################################
########################################################################################

cheminDuJourPart <- paste0(chemin,"Fichiers_date_",date)
cheminDuJourGen  <- paste0(chemin,"Fichiers_date_du_jour")

# Creation des 2 repertoires :
#  - 1 particulier, avec ref a un jour precis contenant les 2 fichiers origines
#  - 1 general,     avec ref au jour courant avec fichiers renommes
if (file.exists(cheminDuJourPart)) {
    cat("Le repertoire particulier existe deja\n")
} else {
    dir.create(cheminDuJourPart, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

if (file.exists(cheminDuJourGen)) {
    cat("Le repertoire general existe deja\n")
} else {
    dir.create(cheminDuJourGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

# definition des variables contenant les noms des fichiers origines
nomFich_TickOuverts <- paste0("Tickets_supervision_ouverts_",date,".csv")
nomFich_TickResolus <- paste0("Tickets_supervision_resolus_",date,".csv")

# deplacement des 2 fichiers origines dans le repertoire particulier
file.copy(from = paste0(chemin,nomFich_TickOuverts), to = cheminDuJourPart, overwrite = TRUE)
file.copy(from = paste0(chemin,nomFich_TickResolus), to = cheminDuJourPart, overwrite = TRUE)
# deplacement des 2 fichiers origines dans le repertoire general
file.copy(from = paste0(chemin,nomFich_TickOuverts), to = cheminDuJourGen, overwrite = TRUE)
file.copy(from = paste0(chemin,nomFich_TickResolus), to = cheminDuJourGen, overwrite = TRUE)

# renommage dans le repertoire general des 2 fichiers avec retrait du suffixe comportant la precision de la date
setwd(cheminDuJourGen)
file.rename(from = nomFich_TickOuverts, to = "ListTickSup_OUVR.csv")
file.rename(from = nomFich_TickResolus, to = "ListTickSup_FERM.csv")

# suppression dans le repertoire arrivee des 2 fichiers origine
setwd(chemin)
file.remove(nomFich_TickOuverts)
file.remove(nomFich_TickResolus)

########################################################################################
########################################################################################
########################################################################################

# fonction de recodage des modalite des acteurs en charge des tickets
recodActeurs <- function(df,typetick){
    df_out <- df %>% 
        mutate(acteur = case_when(
            str_detect(acteur,"xxx") ~ "xxx / SYST",
            str_detect(acteur,"xxx") ~ "xxx / Admin AD",
            str_detect(acteur,"xxx") ~ "xxx / RES",
            str_detect(acteur,"xxx") ~ "xxx / PTIL",
            str_detect(acteur,"xxx") ~ "xxx / RIAP",
            str_detect(acteur,"xxx") ~ "xxx / INTEGR",
            str_detect(acteur,"xxx") ~ "xxx / DBA",
            str_detect(acteur,"xxx") ~ "xxx / Messag",
            str_detect(acteur,"xxx") ~ "xxx / GPX",
            str_detect(acteur,"xxx") ~ "xxx / Si@moi",
            
            str_detect(acteur,"xxx") ~ "xxx / P3I RES",
            str_detect(acteur,"xxx") ~ "xxx / P3I SYST",
           
            str_detect(acteur,"xxx") ~ "Equipe BRPP (44)",
            str_detect(acteur,"xxx") ~ "Equipe Sirene (44)",
            str_detect(acteur,"xxx") ~ "Equipe SIRUS (44)",
            str_detect(acteur,"xxx") ~ "Equipe OCAPI (44)",
            str_detect(acteur,"xxx") ~ "Equipe ICA (44)",
            str_detect(acteur,"xxx") ~ "Equipe Sirene4 (44)",
            str_detect(acteur,"xxx") ~ "Equipe Harmonica (44)",
            
            str_detect(acteur,"xxx") ~ "Equipe EMPL (45)",
            str_detect(acteur,"xxx") ~ "Equipe EPU (45)",
            str_detect(acteur,"xxx") ~ "Equipe DOT (75)",

            TRUE ~ acteur))
    if (typetick=="OUVR"){
        df_out <- df_out %>% filter(acteur!="xxx / GPX")
    }
    return (df_out)
}

setwd(cheminDuJourGen)
ListTickOUVR <- read.csv2(paste0(cheminDuJourGen,"/","ListTickSup_OUVR.csv"),encoding="UTF-8")
ListTickFERM <- read.csv2(paste0(cheminDuJourGen,"/","ListTickSup_FERM.csv"),encoding="UTF-8")

ListTickFERM <- ListTickFERM %>% arrange(desc(date_realisation)) %>% 
  mutate(date_inc_date=as_datetime(ymd_hms(date_inc)),
         date_rea_date=as_datetime(ymd_hms(date_realisation)),
         duree=difftime(date_rea_date,date_inc_date,units="secs")) %>% 
  mutate(duree_int=as.integer(str_replace(duree," secs",""))) %>% 
  mutate(jour=duree_int%/%86400, jour_reste=duree_int%%86400,
         heur=jour_reste%/%3600, heur_reste=jour_reste%%3600,
         min=heur_reste%/%60,    sec=heur_reste%%60) %>% select(-jour_reste,-heur_reste) %>% 
  mutate(jour=ifelse(jour<10,paste0("0",jour),paste0("",jour)),
         heur=ifelse(heur<10,paste0("0",heur),paste0("",heur)),
         min=ifelse(min<10,paste0("0",min),paste0("",min)),
         sec=ifelse(sec<10,paste0("0",sec),paste0("",sec)) ) %>% 
  mutate(duree_str=paste0(jour,"j_",heur,"h_",min,"m")) %>% 
  select(-date_inc_date,-date_rea_date,-duree,-duree_int,-jour,-heur,-min,-sec) %>% 
  mutate(duree_str=ifelse(str_sub(duree_str,1,7)=="00j_00h",
                          paste0("________",str_sub(duree_str,8,12)),
                          duree_str)) %>% 
  mutate(duree_str=ifelse(str_sub(duree_str,1,3)=="00j",
                        paste0("____",str_sub(duree_str,5,12)),
                        duree_str)) %>% 
  mutate(duree_strf=str_trim(str_replace_all(duree_str,"_"," "),side="left")) %>% select(-duree_str) %>% 
  rename(duree_str=duree_strf)
  

# renommage particulier de la 1ere colonne des 2 dataframes
colnames(ListTickOUVR)[1] <- "num_tick"
colnames(ListTickFERM)[1] <- "num_tick"


setwd(chemin)

# Operations de traitement du dataframe listant les tickets ouverts
source("ProgR_Trait_ListTickOUVR_HTML.R", encoding="UTF8")

# Operations de traitement du dataframe listant les tickets recemments fermes
source("ProgR_Trait_ListTickFERM_HTML.R", encoding="UTF8")

# Generation de la page HTML !! ENVOI MEL NE FONCTIONNE PLUS DEPUIS MISE EN PLACE NTLM
#setwd(chemin)
#source("ProgR_GenerationHTML.R", encoding="UTF8")

# Generation de la page HTML avec envoi du -doc zippe
setwd(chemin)
source("ProgR_GenerationHTML_PJ.R", encoding="UTF8")

########################################################################################
########################################################################################
########################################################################################


# render(input=paste0(chemin,"/ProgR_GenerationHTML_test.Rmd"), output_file=paste0("Note_Supervision_",date,".html"),
#        output_dir = chemin, encoding="UTF-8")

# render(input=paste0(chemin,"/ProgR_GenerationPDF_v2.Rmd"), output_file=paste0("Note_Supervision_",date,".pdf"),
#        output_dir = chemin, encoding="UTF-8")

# render(input=paste0(chemin,"/ProgR_GenerationPDF_v2_test.Rmd"), output_file=paste0("Note_Supervision_",date,".pdf"),
#        output_dir = chemin, encoding="UTF-8")

# render(input=paste0(chemin,"/ProgR_GenerationODT_v2.Rmd"), output_file=paste0("Note_Supervision_",date,".odt"),
#        output_dir = chemin, encoding="UTF-8")





