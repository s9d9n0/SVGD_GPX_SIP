
library(RMariaDB)
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
library(readODS)

partie_mois <- "_2024_05"

rep_fich <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/HistoCentreon_User/",partie_mois,"/")
setwd(rep_fich)
partie_mois2 <- paste0(str_sub(partie_mois,1,5),"-",str_sub(partie_mois,7,8))

VisuMois <- read_ods(paste0(rep_fich,"CentreonSIP_user",partie_mois2,".ods"), sheet="Visu")

###

transfo_heureminute <- function(var){
  jour <- var %/% 1440
  jour_reste=var%%1440
  heure=jour_reste%/%60
  minute=jour_reste%%60
  if (jour<10)  { jour   <- paste0("0",jour)  }  else { jour   <- paste0("",jour) }
  if (heure<10) { heure  <- paste0("0",heure) }  else { heure  <- paste0("",heure) }
  if (minute<10){ minute <- paste0("0",minute) } else { minute <- paste0("",minute) }
  resultat <- paste0(jour,"j ",heure,"h ",minute,"m")
  return (resultat)
}

# transfo_heureminute(1440)

###

Agreg_Jour <- VisuMois %>% select(jour,contact_name,duree_min) %>% unique() %>%
  group_by(jour) %>% summarise(duree_mois=sum(duree_min)) %>% ungroup() %>% as.data.frame()

Agreg_Contact <- VisuMois %>% select(jour,contact_name,duree_min) %>% unique() %>%
  group_by(contact_name) %>% summarise(duree_mois=sum(duree_min)) %>% ungroup() %>% as.data.frame() %>% 
  arrange(desc(duree_mois)) %>% 
  mutate(site=case_when(
          contact_name %in% c("xxx",
                              "xxx",
                              "xxx") ~ "xxx / G4",
          contact_name %in% c("xxx",
                              "xxx",
                              "xxx",
                              "xxx",
                              "xxx",
                              "xxx") ~ "xxx / sup",
          contact_name %in% c("xxx",
                              "xxx",
                              "xxx",
                              "xxx",
                              "xxx") ~ "xxx / RIAP",
          contact_name %in% c("xxx",
                              "xxx") ~ "xxx / Integ",
          contact_name %in% c("xxx") ~ "xxx / Messag",
          contact_name %in% c("xxx") ~ "xxx / DBA",
          contact_name %in% c("xxx",
                              "xxx",
                              "xxx",
                              "xxx") ~ "xxx / Obs",
          contact_name %in% c("xxx") ~ "xxx / Res",          
          contact_name %in% c("xxx") ~ "xxx / Si@moi",  
          contact_name %in% c("xxx") ~ "xxx / SEF", 
          contact_name %in% c("xxx",
                              "xxx",
                              "xxx",
                              "xxx") ~ "Ext. au xxx",
          TRUE ~ "Autre")) %>% relocate(site)

Agreg_Equipe <- Agreg_Contact %>% 
  group_by(site) %>% summarise(duree_mois=sum(duree_mois)) %>% ungroup() %>% as.data.frame() %>% 
  arrange(desc(duree_mois))

Agreg_Jour <- Agreg_Jour %>% mutate(duree_label=lapply(duree_mois,transfo_heureminute))
Agreg_Equipe <- Agreg_Equipe %>% mutate(duree_label=lapply(duree_mois,transfo_heureminute))
Agreg_Contact <- Agreg_Contact %>% mutate(duree_label=lapply(duree_mois,transfo_heureminute))


write_ods(Agreg_Jour,paste0(rep_fich,"AGREG_centreonSIP",partie_mois2,".ods"), 
          sheet="AgregJour", append = TRUE)
write_ods(Agreg_Equipe,paste0(rep_fich,"AGREG_centreonSIP",partie_mois2,".ods"),
          sheet="AgregEquipe", append = TRUE)
write_ods(Agreg_Contact,paste0(rep_fich,"AGREG_centreonSIP",partie_mois2,".ods"),
          sheet="AgregAgent", append = TRUE)










