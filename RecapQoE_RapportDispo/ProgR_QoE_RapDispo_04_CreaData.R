
## Creation des datas pour remplissage automatique des fichiers Calc ----

nb_min_mois_7j7 <- 24*60*days_in_month(as.Date(paste0(mois_ref,"-01")))

nb_samdim <- vectplus2 %>% distinct(lib_jour,jour_bis) %>%
  filter(lib_jour %in% c("sam.","dim.")) %>% nrow() %>% as.integer()
nb_min_mois_5j7 <- (19-7)*60*days_in_month(as.Date(paste0(mois_ref,"-01"))) - (19-7)*60*nb_samdim

###

ResumCoupuresQoE_Durees <- read.csv2(paste0("ResumCoupuresQoE_Durees_",str_sub(mois_ref,6,7),".csv")) %>% 
  select(service_description,output_final,periode,duree_tot_min,duree_tot_min_5j7,moment_crit,moment_ok) %>%
  filter(service_description==nom_QoE) %>% 
  filter(duree_tot_min!=0) %>%
  filter(str_sub(output_final,1,4)!="WARN") %>% 
  # ajout du zero si jour < 10
  mutate(moment_crit=ifelse(str_sub(moment_crit,7,7)==" ",
                            paste0(str_sub(moment_crit,1,5),"0",str_sub(moment_crit,6,str_length(moment_crit))),
                            moment_crit),
         moment_ok=ifelse(str_sub(moment_ok,7,7)==" ",
                          paste0(str_sub(moment_ok,1,5),"0",str_sub(moment_ok,6,str_length(moment_ok))),
                          moment_ok)) %>% 
  
  # ajout du zero si heure_crit < 10
  mutate(moment_crit=ifelse(str_sub(moment_crit,10,10)==":",
                            paste0(str_sub(moment_crit,1,8),"0",str_sub(moment_crit,9,12)),
                            moment_crit), 
         moment_ok=ifelse(str_sub(moment_ok,10,10)==":",
                          paste0(str_sub(moment_ok,1,8),"0",str_sub(moment_ok,9,12)),
                          moment_ok)) %>% 
  mutate(coup_5j7=ifelse(str_sub(moment_crit,1,4) %in% c("sam.","dim.") | 
                           str_sub(moment_crit,8,9) %in% c("19","20","21","22","23","00","01","02","03","04","05","06"),
                         0,
                         1))


IncidentExclusionQoE <- read_ods(paste0(chemin_racine,"/Incidents_et_Exclusions_",str_sub(mois_ref,1,4),".ods"),
                                 sheet=paste0("ListeMois_",str_sub(mois_ref,6,7))) %>% 
  select(Type,Service,moment_DEB,moment_FIN,periode,
         duree7j7_min,duree7j7_lib,
         duree5j7_min,duree5j7_lib,commentaire) %>% 
  filter(Service==nom_QoE) %>% 
  rename(service_description=Service)


Res_data <- ResumCoupuresQoE_Durees %>% left_join(IncidentExclusionQoE,by=c("service_description","periode")) %>% 
  mutate(duree_tot_min_5j7=ifelse(is.na(duree_tot_min_5j7),duree5j7_min,duree_tot_min_5j7)) %>% 
  mutate(Type=ifelse(is.na(Type),"Inc",Type))


######################################################################
######################################################################
######################################################################
######################################################################

####################
# definition du mois
lib_mois_ref <- mois_ref %>% as.data.frame() %>% rename(mois_ref=".") %>% 
  mutate(lib_mois_ref=case_when(str_sub(mois_ref,6,7)=="01" ~ "- JANVIER "  , str_sub(mois_ref,6,7)=="02" ~ "- FEVRIER ",
                                str_sub(mois_ref,6,7)=="03" ~ "- MARS "     , str_sub(mois_ref,6,7)=="04" ~ "- AVRIL ",
                                str_sub(mois_ref,6,7)=="05" ~ "- MAI "      , str_sub(mois_ref,6,7)=="06" ~ "- JUIN ",
                                str_sub(mois_ref,6,7)=="07" ~ "- JUILLET "  , str_sub(mois_ref,6,7)=="08" ~ "- AOUT ",
                                str_sub(mois_ref,6,7)=="09" ~ "- SEPTEMBRE ", str_sub(mois_ref,6,7)=="10" ~ "- OCTOBRE ",
                                str_sub(mois_ref,6,7)=="11" ~ "- NOVEMBRE  ", str_sub(mois_ref,6,7)=="12" ~ "- DECEMBRE ",
                                TRUE ~ "")) %>%
  mutate(lib_mois_ref=paste0(lib_mois_ref,str_sub(mois_ref,1,4)," -")) %>% 
  select(lib_mois_ref) %>% as.character()

############################################################################################
# partie 1
# definition des taux indisponibilite, des effectifs de coupures et des durees en 7j7 et 5j7

Res_data_Incident <- Res_data %>% filter(Type=="Inc") %>% 
  mutate(duree_tot_min=ifelse(is.na(duree_tot_min),0,duree_tot_min),
         duree_tot_min_5j7=ifelse(is.na(duree_tot_min_5j7),0,duree_tot_min_5j7))

if(nrow(Res_data_Incident)==0){
  taux_7j7 <- "100,0"
  coup_7j7 <- 0
  ldur_7j7 <- "< 1 min"
  
  taux_5j7 <- "100,0"
  coup_5j7 <- 0
  ldur_5j7 <- "< 1 min"
} else {
  taux_7j7 <- round((nb_min_mois_7j7 - sum(Res_data_Incident$duree_tot_min))/nb_min_mois_7j7*100,1) %>% 
    as.character() %>% str_replace("\\.",",")
  if (!(str_detect(taux_7j7,","))){
    taux_7j7 <- paste0(taux_7j7,",0")
  }
  coup_7j7 <- nrow(Res_data_Incident)
  ldur_7j7 <- Res_data_Incident %>% select(duree_tot_min) %>% sum() %>% as.data.frame() %>% rename(min=".") %>% 
    mutate(jour=min%/%1440, jour_reste=min%%1440,
           heure=jour_reste%/%60, minute=round(jour_reste%%60,0)) %>% 
    select(-jour_reste) %>%
    mutate(ldur_7j7=ifelse(jour!=0,
                           paste0(jour,"j ",heure,"h ",minute,"min"),
                           ifelse(jour==0 & heure!=0 & minute!=0,
                                  paste0(heure,"h ",minute,"min"),
                                  ifelse(jour==0 & heure==0 & minute!=0,
                                         paste0(minute,"min"),
                                         ifelse(jour!=0 & heure!=0 & minute==0,
                                                paste0(jour,"j ",heure,"h"),
                                                ifelse(jour==0 & heure!=0 & minute==0,
                                                       paste0(heure,"h"),
                                                       ifelse(jour!=0 & heure==0 & minute==0,
                                                              paste0(jour,"j"),
                                                              "< 1 min"))))))) %>%
    select(ldur_7j7) %>% as.character()
  
  taux_5j7 <- round((nb_min_mois_5j7 - sum(Res_data_Incident$duree_tot_min_5j7))/nb_min_mois_5j7*100,1) %>% 
    as.character() %>% str_replace("\\.",",")
  if (!(str_detect(taux_5j7,","))){
    taux_5j7 <- paste0(taux_5j7,",0")
  }
  coup_5j7 <- as.integer(sum(Res_data_Incident$coup_5j7))
  ldur_5j7 <- Res_data_Incident %>% select(duree_tot_min_5j7) %>% sum() %>% as.data.frame() %>% rename(min=".") %>% 
    mutate(jour=min%/%1440, jour_reste=min%%1440,
           heure=jour_reste%/%60, minute=round(jour_reste%%60,0)) %>% 
    select(-jour_reste) %>%
    mutate(ldur_5j7=ifelse(jour!=0,
                           paste0(jour,"j ",heure,"h ",minute,"min"),
                           ifelse(jour==0 & heure!=0 & minute!=0,
                                  paste0(heure,"h ",minute,"min"),
                                  ifelse(jour==0 & heure==0 & minute!=0,
                                         paste0(minute,"min"),
                                         ifelse(jour!=0 & heure!=0 & minute==0,
                                                paste0(jour,"j ",heure,"h"),
                                                ifelse(jour==0 & heure!=0 & minute==0,
                                                       paste0(heure,"h"),
                                                       ifelse(jour!=0 & heure==0 & minute==0,
                                                              paste0(jour,"j"),
                                                              "< 1 min"))))))) %>%
    select(ldur_5j7) %>% as.character()
}

lg7j7 <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,periode="A 7j7",
                    taux_dispo=taux_7j7,nb_coup=coup_7j7,lib_duree=ldur_7j7)
lg5j7 <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,periode="B 5j7",
                    taux_dispo=taux_5j7,nb_coup=coup_5j7,lib_duree=ldur_5j7)

Res_data_Final <- rbind(lg7j7,lg5j7)

rm(taux_7j7,coup_7j7,ldur_7j7,lg7j7, taux_5j7,coup_5j7,ldur_5j7,lg5j7)


############################################################################################
# partie 2
# reprise des elements information pour cadre Incident

Res_data_Incident <- Res_data %>% filter(Type=="Inc")

if(dim(Res_data_Incident)[1]>0){
  Cadre_Incident <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                               type="Incident",
                               duree_7j7=Res_data_Incident$duree_tot_min,
                               jour=paste0(str_sub(Res_data_Incident$moment_crit,1,4)," ",str_sub(Res_data_Incident$moment_crit,6,7)),
                               indisp7j7=Res_data_Incident$duree7j7_lib,
                               indisp5j7=Res_data_Incident$duree5j7_lib,
                               commentaire=Res_data_Incident$commentaire) %>% filter(duree_7j7>=60)
} else {
  Cadre_Incident <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                               type="Incident",
                               duree_7j7="",
                               jour="",
                               indisp7j7="",
                               indisp5j7="",
                               commentaire="")
}

# restriction aux 5 premieres lignes et ajout de lignes vides si < 5
if(nrow(Cadre_Incident)>=5){
  Cadre_Incident <- Cadre_Incident %>% 
    arrange(desc(as.integer(duree_7j7))) %>% 
    head(5)
} else {
  df_vide <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                        type="Incident",
                        duree_7j7=c(rep("",5-nrow(Cadre_Incident))),
                        jour=c(rep("",5-nrow(Cadre_Incident))),
                        indisp7j7=c(rep("",5-nrow(Cadre_Incident))),
                        indisp5j7=c(rep("",5-nrow(Cadre_Incident))),
                        commentaire=c(rep("",5-nrow(Cadre_Incident))))
  Cadre_Incident <- Cadre_Incident %>% 
    arrange(desc(as.integer(duree_7j7)))
  Cadre_Incident <- rbind(Cadre_Incident,df_vide)
  rm(df_vide)
}


############################################################################################
# partie 3
# reprise des elements information pour cadre Exclusion

Res_data_Exclusion <- Res_data %>% filter(Type=="Exclusion")

if(dim(Res_data_Exclusion)[1]>0){
  Cadre_Exclusion <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                                type="Exclusion",
                                periode_exclusion=Res_data_Exclusion$periode,
                                commentaire=Res_data_Exclusion$commentaire)
} else {
  Cadre_Exclusion <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                                type="Exclusion",
                                periode_exclusion="",
                                commentaire="")
}

# restriction aux 5 premieres lignes et ajout de lignes vides si < 5
if(nrow(Cadre_Exclusion)>=5){
  Cadre_Exclusion <- head(Cadre_Exclusion,5)
} else {
  df_vide <- data.frame(mois=lib_mois_ref,application=SousRepertoire,service_description=nom_QoE,
                        type="Exclusion",
                        periode_exclusion=c(rep("",5-nrow(Cadre_Exclusion))),
                        commentaire=c(rep("",5-nrow(Cadre_Exclusion))))
  Cadre_Exclusion <- rbind(Cadre_Exclusion,df_vide)
  rm(df_vide)
}

Res_data_Final_ <- data.frame(Res_data_Final,
                              type="",duree_7j7="",jour="",indisp7j7="",indisp5j7="",
                              periode_exclusion="",commentaire="")

Cadre_Incident_ <- data.frame(Cadre_Incident,
                              periode="",taux_dispo="",nb_coup="",lib_duree="",
                              periode_exclusion="") %>% 
  relocate(periode:lib_duree,.after="service_description") %>%
  relocate(periode_exclusion,.before="commentaire")

Cadre_Exclusion_ <- data.frame(Cadre_Exclusion,
                               periode="",taux_dispo="",nb_coup="",lib_duree="",
                               duree_7j7="",jour="",indisp7j7="",indisp5j7="") %>% 
  relocate(periode:lib_duree,.after="service_description") %>%
  relocate(duree_7j7:indisp5j7,.after="type")

Res_data_Final <- rbind(Res_data_Final_,Cadre_Incident_,Cadre_Exclusion_)

rm(Res_data_Final_,
   Res_data_Incident,Cadre_Incident,Cadre_Incident_,
   Res_data_Exclusion,Cadre_Exclusion,Cadre_Exclusion_)


#######################
# Export de la table data
# Creation dun repertoire avec la date du jour
Doss <- paste0(chemin_racine,"/",SousRepertoire)
if (file.exists(Doss)) {
  cat("Le dossier existe deja")
} else {
  dir.create(Doss, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}
setwd(dir = Doss)

write_ods(Res_data_Final,paste0(Doss,"/",nom_QoE,"_Data_Calc.ods"), sheet=nom_QoE)

setwd(dir = chemin_racine)


# brouillon

