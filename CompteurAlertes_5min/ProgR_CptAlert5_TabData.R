
## Chargement des packages utilises dans le programme ----
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/"

##########################################################
##########################################################

# DEBUT creation du vecteur des heureXminute, au cas ou pas dinfo pendant 5min
vect_heure <- seq(0,23,1) %>% as.character()
for (i in 1:(length(vect_heure))){
    # cat(paste0("num ",i,"\n"))
    if (str_length(vect_heure[i])==1) {
        vect_heure[i]=paste0("v_0",vect_heure[i],"h")
    } else { 
        vect_heure[i]=paste0("v_",vect_heure[i],"h")
    }
}

vect_minute <- seq(0,55,5)
for (i in 1:(length(vect_minute))){
    # cat(paste0("num ",i,"\n"))
    if (str_length(vect_minute[i])==1){
        vect_minute[i]=paste0("0",vect_minute[i])
    } else {
        vect_minute[i]=paste0("",vect_minute[i])
    }
}

# croisement des 2 vecteurs
vect <- c()
for (i in 1:(length(vect_heure))){
    for (j in 1:(length(vect_minute))){
        vect <- append(vect,paste0(vect_heure[i],vect_minute[j]))
    }
}
vect <- vect %>% as.data.frame() %>% rename(heure_v=".")
vect
rm(vect_heure,vect_minute,i,j)
# FIN creation du vecteur des heureXminute, au cas ou pas dinfo pendant 5min


##########################################################
##########################################################

jour_ref <- "2024-07-12"

df_datajreq <- read.csv2(paste0(chemin,"df_datajreq_",jour_ref,"_01h30.csv")) %>% 
  filter(moment_1h %in% c("07h","08h","09h","10h","11h","12h",
                          "13h","14h","15h","16h","17h","18h"))

# premier regroupement et retraitement
dfdata <- df_datajreq %>% 
    group_by(moment,zone,name_host,Gr_Hotes,acknowledged,last_hard_state) %>% 
    summarise(eff=n()) %>%
    ungroup() %>% as.data.frame() %>%  
    unite(acknowledged,last_hard_state,col="etat") %>% 
    pivot_wider(names_from = etat, values_from = eff) %>%
    as.data.frame() %>%
    relocate(c("0_1","0_2","0_3","1_1","1_2","1_3"),.after=Gr_Hotes) %>% 
    rename(nAcq_Warn="0_1",nAcq_Crit="0_2",nAcq_Inc="0_3",
           Acq_Warn="1_1", Acq_Crit="1_2", Acq_Inc="1_3")

# filtre sur les non-acquittements
dfdata_nAcq_oAcq <- dfdata %>% 
    #filter(!is.na(nAcq_Warn) | !is.na(nAcq_Crit) | !is.na(nAcq_Inc)) %>% 
    #select(!(Acq_Warn | Acq_Crit | Acq_Inc)) %>% 
    # arrange(zone,name_host,moment) %>% 
    pivot_longer(.,cols=c("nAcq_Inc","nAcq_Crit","nAcq_Warn",
                          "Acq_Inc", "Acq_Crit", "Acq_Warn"),values_to="eff") %>% 
    as.data.frame() %>%
    mutate(jour=str_sub(moment,1,8),
           heure=str_sub(moment,9,14),
           heure_v=paste0("v",heure)) %>% 
    relocate(c(jour,heure,heure_v),.after=moment) %>% 
    mutate(eff=ifelse(is.na(eff),0,eff)) %>% 
    mutate(eff_pond=case_when(
                        name %in% c("nAcq_Inc","Acq_Inc") ~ eff*0.5,
                        name %in% c("nAcq_Warn","Acq_Warn") ~ eff,
                        name %in% c("nAcq_Crit","Acq_Crit") ~ eff*2) )

# Agregation pour connaitre le nbre alerte sur 24h
Agreg <- dfdata_nAcq_oAcq %>% group_by(zone,name_host,Gr_Hotes,name) %>%
    summarise(tot=sum(eff)) %>%
    ungroup() %>% as.data.frame()
Agreg2 <- dfdata_nAcq_oAcq %>% group_by(zone,name_host,Gr_Hotes) %>%
    summarise(totjour=sum(eff),totjour_pond=sum(eff_pond)) %>%
    ungroup() %>% as.data.frame()
Agreg <- Agreg %>% left_join(Agreg2, by=c("zone","name_host","Gr_Hotes"))
rm(Agreg2)

# completude avec les eventuelles tranches de 5min vides 
# grace a la jointure avec vect...
dfdata_nAcq_oAcq <- dfdata_nAcq_oAcq %>%
    right_join(vect,by=c("heure_v")) %>% 
    mutate(jour=ifelse(is.na(jour),jour[1],jour),
           moment=ifelse(is.na(moment),paste0(jour[1],heure),moment),
           zone=ifelse(is.na(zone),"_",zone),
           name_host=ifelse(is.na(name_host),"_",name_host),
           Gr_Hotes=ifelse(is.na(Gr_Hotes),"_",Gr_Hotes),
           name=ifelse(is.na(name),"nAcq_Inc",name))

# transposition pour colonnes 5min par 5min...
dfdata_nAcq_oAcq <- dfdata_nAcq_oAcq %>%
    select(!(moment | heure | eff_pond)) %>% arrange(jour,heure_v) %>% 
    pivot_wider(names_from = heure_v, values_from = eff) %>%
    as.data.frame() %>% 
    arrange(zone,Gr_Hotes,name_host)

# ajout des colonnes tot et totjour issues de la table Agreg
dfdata_nAcq_oAcq <- dfdata_nAcq_oAcq %>% 
    left_join(Agreg, by=c("zone","name_host","Gr_Hotes","name")) %>% 
    relocate(c(tot,totjour,totjour_pond),.after=name)
rm(Agreg)

dfdata_FINAL <- dfdata_nAcq_oAcq
# val <- dfdata_nAcq_FINAL[[6]][144]
# val


# test 
#dfdata_FINAL[[9]] <- dfdata_FINAL[[9]] %>% replace_na("") #%>%
    #str_replace_all("0","") %>% as.integer()


for(col in 9:(ncol(dfdata_FINAL))){
    dfdata_FINAL[[col]] <- dfdata_FINAL[[col]] %>% replace_na("")
}



# for(col in 9:(ncol(dfdata_nAcq_FINAL))){
#     for (lig in 1:(nrow(dfdata_nAcq_FINAL))){
#         cat("coord :",col,":",lig,"-> ")
#         if (is.na(dfdata_nAcq_FINAL[[col]][lig]) | dfdata_nAcq_FINAL[[col]][lig]==0){
#             cat("NA","\n")
#             dfdata_nAcq_FINAL[[col]][lig] <- ""
#         } else {
#             cat("\n")
#         }
#     }
# }
# rm(col,lig)

write.csv2(dfdata_FINAL,
           paste0(chemin,"resultat_du_jour_",jour_ref,".csv"),
           row.names = FALSE)

# trop long si gros fichier
# library(readODS)
# write_ods(dfdata_nAcq_FINAL,
#           paste0(chemin,"resultat_du_jour_",jour_ref,".ods"))

##########################################################
##########################################################

nbHote_Alerte <- dfdata_FINAL %>%
    select(zone,name_host) %>% unique() %>%
    group_by(zone,name_host) %>% summarize(nbHote_Alerte=n()) %>%
    ungroup() %>% as.data.frame() %>% nrow() %>% as.numeric() %>%
    as.data.frame() %>% rename(nbHote_Alerte_parZone=".") %>% 
    mutate(zone="09_Ensemble") %>% relocate(zone)

nbServ_Alerte <- df_datajreq %>% 
    #filter(acknowledged==0) %>%
    select(zone,name_host,description) %>% unique() %>%
    group_by(zone,name_host,description) %>% summarize(nbServ_Alerte=n()) %>%
    ungroup() %>% as.data.frame() %>% nrow() %>% as.numeric() %>%
    as.data.frame() %>% rename(nbServ_Alerte_parZone=".") %>% 
    mutate(zone="09_Ensemble") %>% relocate(zone)

##

Zone <- data.frame(zone = c("01_DMZ_Auze","02_DMZ_Osny",
                            "03_Part_Auze","04_Part_Osny",
                            "05_ZoneInt_Auze","06_ZoneInt_Osny",
                            "07_ZoneSIA_Auze","08_ZoneSIA_Osny"))

nbHote_Alerte_parZone <- dfdata_FINAL %>%
    select(zone,name_host) %>% unique() %>%
    group_by(zone) %>% summarize(nbHote_Alerte_parZone=n()) %>%
    ungroup() %>% as.data.frame() %>% 
    right_join(Zone,by=c("zone")) %>% 
    mutate(nbHote_Alerte_parZone=ifelse(is.na(nbHote_Alerte_parZone),0,nbHote_Alerte_parZone)) %>% 
    arrange(zone)

nbServ_Alerte_parZone <- df_datajreq %>% 
    #filter(acknowledged==0) %>%
    select(zone,name_host,description) %>% unique() %>%
    group_by(zone) %>% summarize(nbServ_Alerte_parZone=n()) %>%
    ungroup() %>% as.data.frame() %>% 
    right_join(Zone,by=c("zone")) %>% 
    mutate(nbServ_Alerte_parZone=ifelse(is.na(nbServ_Alerte_parZone),0,nbServ_Alerte_parZone)) %>% 
    arrange(zone)


nbHote_Alerte_parZone <- rbind(nbHote_Alerte_parZone,nbHote_Alerte) %>% 
    rename(nbHote_en_Alerte=nbHote_Alerte_parZone)

nbServ_Alerte_parZone <- rbind(nbServ_Alerte_parZone,nbServ_Alerte) %>% 
    rename(nbServ_en_Alerte=nbServ_Alerte_parZone)

########

library(readODS)

feuil_Hote <- read_ods(path = paste0(chemin,"resultatHisto_nbAlerte_parZone.ods"),
                       sheet="Hote")
nbHote_Alerte_parZone_transp <- nbHote_Alerte_parZone %>% 
    pivot_wider(names_from = zone, values_from = nbHote_en_Alerte) %>% 
    as.data.frame() %>% 
    mutate(date_jour=jour_ref) %>% 
    relocate(date_jour)
feuil_Hote <- rbind(feuil_Hote,nbHote_Alerte_parZone_transp)
write_ods(feuil_Hote, paste0(chemin,"resultatHisto_nbAlerte_parZone.ods"),
          sheet="Hote", update = TRUE, append = TRUE)

feuil_Serv <- read_ods(path = paste0(chemin,"resultatHisto_nbAlerte_parZone.ods"),
                       sheet="Serv")
nbServ_Alerte_parZone_transp <- nbServ_Alerte_parZone %>% 
    pivot_wider(names_from = zone, values_from = nbServ_en_Alerte) %>% 
    as.data.frame() %>% 
    mutate(date_jour=jour_ref) %>% 
    relocate(date_jour)
feuil_Serv <- rbind(feuil_Serv,nbServ_Alerte_parZone_transp)
write_ods(feuil_Serv, paste0(chemin,"resultatHisto_nbAlerte_parZone.ods"),
          sheet="Serv", update = TRUE, append = TRUE)

rm(feuil_Hote,nbHote_Alerte_parZone_transp,
   feuil_Serv,nbServ_Alerte_parZone_transp)


########

nb_Alerte_parZone <- nbHote_Alerte_parZone %>%
    left_join(nbServ_Alerte_parZone, by=c("zone"))

write_ods(nb_Alerte_parZone,
          paste0(chemin,"nb_Alerte_parZone_",jour_ref,".ods"),
          sheet=paste0("cpt_",jour_ref))

# write_ods(nbHote_Alerte_parZone,
#           paste0(chemin,"nb_Alerte_parZone_",jour_ref,".ods"),
#           sheet=paste0("Hote_",jour_ref))
# write_ods(nbServ_Alerte_parZone,
#           paste0(chemin,"nb_Alerte_parZone_",jour_ref,".ods"),
#           sheet=paste0("Serv_",jour_ref), append = TRUE)


#############
##########
#######
####
# ETUDE
####
#######
##########
#############

# Vue_07_23 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-23_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_23=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_23))
# 
# Vue_07_24 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-24_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_24=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_24))
# 
# Vue_07_25 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-25_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_25=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_25))
# 
# Vue_07_26 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-26_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_26=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_26))
# 
# Vue_07_27 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-27_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_27=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_27))
# 
# Vue_07_28 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-28_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_28=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_28))
# 
# Vue_07_29 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-29_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_29=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_29))
# 
# Vue_07_30 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-30_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_30=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_30))
# 
# Vue_07_31 <- read.csv2(paste0(chemin,"df_datajreq_2023-07-31_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_07_31=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_07_31))
# 
# Vue_08_01 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-01_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_01=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_01))
# 
# Vue_08_02 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-02_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_02=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_02))
# 
# Vue_08_03 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-03_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_03=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_03))
# 
# Vue_08_04 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-04_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_04=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_04))
# 
# Vue_08_05 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-05_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_05=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_05))
# 
# Vue_08_06 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-06_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_06=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_06))
# 
# Vue_08_07 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-07_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_07=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_07))
# 
# Vue_08_08 <- read.csv2(paste0(chemin,"df_datajreq_2023-08-08_01h30.csv")) %>%
#     filter(zone=="07_ZoneSIA_Auze") %>% group_by(name_host,description) %>% 
#     summarise(nb_eff_08_08=n()) %>% ungroup() %>% as.data.frame() %>% arrange(desc(nb_eff_08_08))
# 
# 
# Vue_Fuz <- Vue_07_23 %>% 
#     full_join(Vue_07_24, by=c("name_host","description")) %>% full_join(Vue_07_25, by=c("name_host","description")) %>% 
#     full_join(Vue_07_26, by=c("name_host","description")) %>% full_join(Vue_07_27, by=c("name_host","description")) %>% 
#     full_join(Vue_07_28, by=c("name_host","description")) %>% full_join(Vue_07_29, by=c("name_host","description")) %>%   
#     full_join(Vue_07_30, by=c("name_host","description")) %>% full_join(Vue_07_31, by=c("name_host","description")) %>% 
#     full_join(Vue_08_01, by=c("name_host","description")) %>% full_join(Vue_08_02, by=c("name_host","description")) %>% 
#     full_join(Vue_08_03, by=c("name_host","description")) %>% full_join(Vue_08_04, by=c("name_host","description")) %>% 
#     full_join(Vue_08_05, by=c("name_host","description")) %>% full_join(Vue_08_06, by=c("name_host","description")) %>% 
#     full_join(Vue_08_07, by=c("name_host","description")) %>% full_join(Vue_08_08, by=c("name_host","description"))
#     
# Vue_Fuz2 <- Vue_Fuz %>% group_by(name_host) %>% 
#     summarise(nb_eff_07_23_h=sum(nb_eff_07_23,na.rm = T), nb_eff_07_24_h=sum(nb_eff_07_24,na.rm = T),
#               nb_eff_07_25_h=sum(nb_eff_07_25,na.rm = T), nb_eff_07_26_h=sum(nb_eff_07_26,na.rm = T),
#               nb_eff_07_27_h=sum(nb_eff_07_27,na.rm = T), nb_eff_07_28_h=sum(nb_eff_07_28,na.rm = T),
#               nb_eff_07_29_h=sum(nb_eff_07_29,na.rm = T), nb_eff_07_30_h=sum(nb_eff_07_30,na.rm = T),
#               nb_eff_07_31_h=sum(nb_eff_07_31,na.rm = T),
#               nb_eff_08_01_h=sum(nb_eff_08_01,na.rm = T), nb_eff_08_02_h=sum(nb_eff_08_02,na.rm = T),
#               nb_eff_08_03_h=sum(nb_eff_08_03,na.rm = T), nb_eff_08_04_h=sum(nb_eff_08_04,na.rm = T),
#               nb_eff_08_05_h=sum(nb_eff_08_05,na.rm = T), nb_eff_08_06_h=sum(nb_eff_08_06,na.rm = T),
#               nb_eff_08_07_h=sum(nb_eff_08_07,na.rm = T), nb_eff_08_08_h=sum(nb_eff_08_08,na.rm = T)) %>%
#     ungroup() %>% as.data.frame()


# remplacement des NA par 0
# Vue_Fuz_csv <- Vue_Fuz %>% 
#     mutate(nb_eff_07_23=coalesce(nb_eff_07_23,0), nb_eff_07_24=coalesce(nb_eff_07_24,0),
#            nb_eff_07_25=coalesce(nb_eff_07_25,0), nb_eff_07_26=coalesce(nb_eff_07_26,0),
#            nb_eff_07_27=coalesce(nb_eff_07_27,0), nb_eff_07_28=coalesce(nb_eff_07_28,0),
#            nb_eff_07_29=coalesce(nb_eff_07_29,0), nb_eff_07_30=coalesce(nb_eff_07_30,0),
#            nb_eff_07_31=coalesce(nb_eff_07_31,0), 
#            nb_eff_08_01=coalesce(nb_eff_08_01,0), nb_eff_08_02=coalesce(nb_eff_08_02,0),
#            nb_eff_08_03=coalesce(nb_eff_08_03,0), nb_eff_08_04=coalesce(nb_eff_08_04,0),
#            nb_eff_08_05=coalesce(nb_eff_08_05,0), nb_eff_08_06=coalesce(nb_eff_08_06,0),
#            nb_eff_08_07=coalesce(nb_eff_08_07,0), nb_eff_08_08=coalesce(nb_eff_08_08,0) )

# remplacement des NA par "" lors de l'export
#write.csv2(Vue_Fuz,file=paste0(chemin,"Vue_Fuz.csv"),na="")

          