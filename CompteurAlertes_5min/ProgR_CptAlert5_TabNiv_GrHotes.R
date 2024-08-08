
library(lubridate)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)


chemin <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/CompteurAlertes_5min/"

####
## Visualisation...
####

fichier_ref <- "df_datajreq_2023-08-06_01h30.csv"

# modalites possibles
# 01_DMZ_dc2     02_DMZ_dc1
# 03_Part_dc2    04_Part_dc1
# 05_ZoneInt_dc2 06_ZoneInt_dc1
# 07_ZoneSIA_dc2 08_ZoneSIA_dc1
  
Zone_filter <- "07_ZoneSIA_Auze"

df_datajreq <- read.csv2(paste0(chemin,fichier_ref))

jour_ref <- str_sub(fichier_ref,13,22)

####

# modification modalite moment avec ajout de"m_" en prefixe
df_visu_data <- df_datajreq %>% 
    mutate(moment2 = paste0("m_",moment)) %>% relocate(moment2) %>% 
    select(-moment) %>% rename(moment=moment2)

# agregation par moment X groupe d'hote, recodification des statuts et transposition courte
# creation indicatrice d'un nbre d'alerte sup. ou égal a 10
df_visu_data <- df_visu_data %>% group_by(moment,zone,Gr_Hotes,last_hard_state) %>% 
    summarise(nbobs=n()) %>% ungroup() %>% as.data.frame() %>% 
    filter(zone==Zone_filter) %>% arrange(Gr_Hotes) %>%
    mutate(last_hard_state=case_when(
        last_hard_state==1 ~ "A_Warning",
        last_hard_state==2 ~ "B_Critical",
        last_hard_state==3 ~ "C_Inconnu")
    ) %>% pivot_wider(names_from = last_hard_state, values_from = nbobs) %>%
    rowwise() %>% 
    mutate(tot_alerte=sum(A_Warning,B_Critical,C_Inconnu,na.rm=TRUE)) %>% 
    mutate(indic_alerte_sup10=ifelse(tot_alerte>=10,1,0)) %>%
    arrange(desc(tot_alerte)) %>% as.data.frame()

# Agregation par groupe d'hote avec ajout de de compteurs et d'indicateurs de position differents
df_visu_data_Agreg <- df_visu_data %>% group_by(Gr_Hotes) %>% 
    summarise(tot_alert_warn=sum(A_Warning,na.rm=TRUE),
              tot_alert_crit=sum(B_Critical,na.rm=TRUE),
              tot_alert_inco=sum(C_Inconnu,na.rm=TRUE),
              tot_alert_gr_hote=sum(tot_alerte),
              q1_alert=round(quantile(tot_alerte,probs=0.25),0),
              q2_alert=round(quantile(tot_alerte,probs=0.5),0),
              q3_alert=round(quantile(tot_alerte,probs=0.75),0),
              moy_alert=round(mean(tot_alerte),0),
              max_alert=max(tot_alerte),
              nb_tr5min=n(),
              nb_tr5min_sup10=sum(indic_alerte_sup10)) %>% ungroup() %>% as.data.frame() %>% 
    arrange(desc(tot_alert_gr_hote))

# jointure entre les 2 tables
df_visu_data <- df_visu_data %>% left_join(df_visu_data_Agreg, by=c("Gr_Hotes")) %>% 
    select(-tot_alerte,-indic_alerte_sup10) %>%
    arrange(desc(tot_alert_gr_hote),moment) 
rm(df_visu_data_Agreg)

# simplification de 3 variables en une variable "profil"
df_visu_data_bis <- df_visu_data %>% unite(A_Warning,B_Critical,C_Inconnu,col="profil",sep=" - ") %>%
    mutate(profil=str_replace_all(profil,"NA"," "))

# nbre profils differents par groupe d'hote
df_visu_data_ter <- df_visu_data_bis %>% group_by(Gr_Hotes,profil) %>% 
    summarise(eff_profil=n()) %>% ungroup() %>% as.data.frame()

df_visu_data_qua <- df_visu_data_ter %>% group_by(Gr_Hotes) %>% 
    summarise(eff_profil_diff=n()) %>% ungroup() %>% as.data.frame()

# jointure entre les 2 tables et transposition courte
df_visu_data_qui <- df_visu_data_bis %>% 
    left_join(df_visu_data_qua, by=c("Gr_Hotes")) %>%
    pivot_wider(names_from = moment, values_from = profil) %>% as.data.frame()

# suppression des NA
for(col in 15:(ncol(df_visu_data_qui))){
    df_visu_data_qui[[col]] <- df_visu_data_qui[[col]] %>% replace_na("")
}
rm(col)

# creation d'une variable score d'alerte et tri decroissant sur cette derniere
df_visu_data_qui <- df_visu_data_qui %>%
    rowwise() %>% 
    mutate(score_alert=sum(tot_alert_warn*2,tot_alert_crit*3,tot_alert_inco*1)) %>%
    relocate(score_alert,.after="Gr_Hotes") %>% 
    as.data.frame() %>% 
    arrange(desc(score_alert))


# Export en .csv

write.csv2(df_visu_data_qui,
           paste0(chemin,"Synthese_GrHote_",Zone_filter,"_",jour_ref,".csv"),
           row.names = FALSE)


zoom <- df_datajreq %>% 
    filter(zone==Zone_filter & Gr_Hotes=="-RepartiteursCharge // -RepartiteursCharge_Auzeville") %>% 
    group_by(name_host,description) %>% summarise(nb_obs=n()) %>% ungroup() %>% as.data.frame()




###############################################
###############################################
###############################################
###############################################

# PREPARATION POUR SORTIE CHRONOGRAMME HTML

Tab_GrHote <- read.csv2(paste0(chemin,fichier_ref))

# modalites possibles
# 01_DMZ_dc2     02_DMZ_dc1
# 03_Part_dc2    04_Part_dc1
# 05_ZoneInt_dc2 06_ZoneInt_dc1
# 07_ZoneSIA_dc2 08_ZoneSIA_dc1

Zone_filter <- "07_ZoneSIA_Auze"

# modification modalite moment avec ajout de"m_" en prefixe
Tab_GrHote <- Tab_GrHote %>% 
    mutate(moment2 = paste0("m_",moment)) %>% relocate(moment2) %>% 
    select(-moment) %>% rename(moment=moment2)

# agregation par moment X groupe d'hote, recodification des statuts et transposition courte
Tab_GrHote <- Tab_GrHote %>% group_by(moment,zone,Gr_Hotes,last_hard_state) %>% 
    summarise(nbobs=n()) %>% ungroup() %>% as.data.frame() %>% 
    filter(zone==Zone_filter) %>% arrange(moment,Gr_Hotes) %>%
    pivot_wider(names_from = moment, values_from = nbobs) %>% as.data.frame() %>% 
    mutate(last_hard_state=case_when(
        last_hard_state==1 ~ "A_Warning",
        last_hard_state==2 ~ "B_Critical",
        last_hard_state==3 ~ "C_Inconnu")
    ) %>% rename(name=last_hard_state) %>% select(-zone)

# Agregation par groupe d'hote pour connaitre le nbre de groupe d'hote different sur la période consideree
Tab_GrHote_bis <- Tab_GrHote %>% group_by(Gr_Hotes) %>% 
    summarise(nb=n()) %>% ungroup() %>% as.data.frame() %>% select(-nb)

# Creation vecteur de statut Inconnu, Critical, Warning pour obtention de tous les croisements possibles
name <- rep(c("C_Inconnu","B_Critical","A_Warning"),nrow(Tab_GrHote_bis))
Tab_GrHote_bis <- cbind(Tab_GrHote_bis,name) %>% arrange(Gr_Hotes,desc(name))
rm(name)

# jointure entre les 2 tables
Tab_GrHote <- Tab_GrHote %>% right_join(Tab_GrHote_bis, by=c("Gr_Hotes","name")) %>% 
    arrange(Gr_Hotes,desc(name))
rm(Tab_GrHote_bis)

# remplacement des NA par des zeros
for(col in 3:(ncol(Tab_GrHote))){
    Tab_GrHote[[col]] <- Tab_GrHote[[col]] %>% replace_na(0)
}


# calcul du maximum sur chaque ligne
Tab_GrHote_bis <- Tab_GrHote %>% 
    group_by(Gr_Hotes,name) %>% 
    summarise(max_alert=max(c_across(starts_with("m_")))) %>% 
    relocate(max_alert) %>% as.data.frame()

# jointure entre les 2 tables
Tab_GrHote <- Tab_GrHote %>% right_join(Tab_GrHote_bis, by=c("Gr_Hotes","name")) %>% 
    arrange(Gr_Hotes,desc(name)) %>% relocate(max_alert,.after="name")
rm(Tab_GrHote_bis)


##
#vue <- ls() %>% as.data.frame()

############################################################
# Programme de generation du fichier HTML et d'envoi par mel

# PARTIE : CREATION DU HTML



tab <- Tab_GrHote %>% group_by(Gr_Hotes) %>% 
    summarise(nbobs=n()) %>% ungroup() %>% as.data.frame() %>% 
    select(Gr_Hotes)
class(tab)

#tab$Gr_Hotes[2]

vect <- c()
for (i in 1:(nrow(tab))){
    vect <- append(vect,tab$Gr_Hotes[i])
}
vect[]

fct_decoupe_GrHote <-function(df){
    liste_df <- list()
    for (i in 1:3){
        df_out <- df %>% filter(Gr_Hotes==vect[i])
        liste_df[[i]] <- df_out
    }
    return (liste_df)
}
list_result <- fct_decoupe_GrHote(Tab_GrHote)

class(list_result[[1]])
dim(list_result[[1]])




Tab_GrHote_extrait <- Tab_GrHote %>% 
    filter(Gr_Hotes %in% c("-RepartiteursCharge // -RepartiteursCharge_dc2",
                           "Centreon_Plateforme // Centreon_Zone100_dc2",
                           "Centreon_Plateforme // Centreon_Zone50_dc2",
                           "-ESX_VxRail",
                           "-Nas"))


Tab_GrHote_extrait <- Tab_GrHote_extrait %>% 
    mutate(lg=row_number()%%3) %>% relocate(lg)

###----
entete_table <- paste0("<thead>",
                          "<tr>",
                            "<td class='ligneTot'>Gr. Hotes</td>",
                            "<td class='ligneTot'>statut</td>",
                            "<td class='ligneTot'></td>",
                            "<td class='ligneTot' colspan=3>00<sup>h00</sup></td>",
                            #"<td class='ligneTot'>00h05</td>","<td class='ligneTot'>00h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>00h20</td>","<td class='ligneTot'>00h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>00h35</td>","<td class='ligneTot'>00h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>00h50</td>","<td class='ligneTot'>00h55</td>",
                       
                            "<td class='ligneTot' colspan=3>01<sup>h00</sup></td>",
                            #"<td class='ligneTot'>01h05</td>","<td class='ligneTot'>01h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>01h20</td>","<td class='ligneTot'>01h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>01h35</td>","<td class='ligneTot'>01h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>01h50</td>","<td class='ligneTot'>01h55</td>",
                       
                            "<td class='ligneTot' colspan=3>02<sup>h00</sup></td>",
                            #"<td class='ligneTot'>02h05</td>","<td class='ligneTot'>02h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>02h20</td>","<td class='ligneTot'>02h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>02h35</td>","<td class='ligneTot'>02h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>02h50</td>","<td class='ligneTot'>02h55</td>",
                       
                            "<td class='ligneTot' colspan=3>03<sup>h00</sup></td>",
                            #"<td class='ligneTot'>03h05</td>","<td class='ligneTot'>03h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>03h20</td>","<td class='ligneTot'>03h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>03h35</td>","<td class='ligneTot'>03h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>03h50</td>","<td class='ligneTot'>03h55</td>",
                       
                            "<td class='ligneTot' colspan=3>04<sup>h00</sup></td>",
                            #"<td class='ligneTot'>04h05</td>","<td class='ligneTot'>04h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>04h20</td>","<td class='ligneTot'>04h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>04h35</td>","<td class='ligneTot'>04h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>04h50</td>","<td class='ligneTot'>04h55</td>",
                       
                            "<td class='ligneTot' colspan=3>05<sup>h00</sup></td>",
                            #"<td class='ligneTot'>05h05</td>","<td class='ligneTot'>05h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>05h20</td>","<td class='ligneTot'>05h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>05h35</td>","<td class='ligneTot'>05h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>05h50</td>","<td class='ligneTot'>05h55</td>",
                       
                            "<td class='ligneTot' colspan=3>06<sup>h00</sup></td>",
                            #"<td class='ligneTot'>06h05</td>","<td class='ligneTot'>06h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>06h20</td>","<td class='ligneTot'>06h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>06h35</td>","<td class='ligneTot'>06h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>06h50</td>","<td class='ligneTot'>06h55</td>",
                       
                            "<td class='ligneTot' colspan=3>07<sup>h00</sup></td>",
                            #"<td class='ligneTot'>07h05</td>","<td class='ligneTot'>07h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>07h20</td>","<td class='ligneTot'>07h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>07h35</td>","<td class='ligneTot'>07h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>07h50</td>","<td class='ligneTot'>07h55</td>",
                       
                            "<td class='ligneTot' colspan=3>08<sup>h00</sup></td>",
                            #"<td class='ligneTot'>08h05</td>","<td class='ligneTot'>08h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>08h20</td>","<td class='ligneTot'>08h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>08h35</td>","<td class='ligneTot'>08h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>08h50</td>","<td class='ligneTot'>08h55</td>",
                       
                            "<td class='ligneTot' colspan=3>09<sup>h00</sup></td>",
                            #"<td class='ligneTot'>09h05</td>","<td class='ligneTot'>09h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>09h20</td>","<td class='ligneTot'>09h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>09h35</td>","<td class='ligneTot'>09h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>09h50</td>","<td class='ligneTot'>09h55</td>",
                       
                            "<td class='ligneTot' colspan=3>10<sup>h00</sup></td>",
                            #"<td class='ligneTot'>10h05</td>","<td class='ligneTot'>10h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>10h20</td>","<td class='ligneTot'>10h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>10h35</td>","<td class='ligneTot'>10h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>10h50</td>","<td class='ligneTot'>10h55</td>",
                       
                            "<td class='ligneTot' colspan=3>11<sup>h00</sup></td>",
                            #"<td class='ligneTot'>11h05</td>","<td class='ligneTot'>11h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>11h20</td>","<td class='ligneTot'>11h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>11h35</td>","<td class='ligneTot'>11h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>11h50</td>","<td class='ligneTot'>11h55</td>",
                       
                            "<td class='ligneTot' colspan=3>12<sup>h00</sup></td>",
                            #"<td class='ligneTot'>12h05</td>","<td class='ligneTot'>12h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>12h20</td>","<td class='ligneTot'>12h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>12h35</td>","<td class='ligneTot'>12h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>12h50</td>","<td class='ligneTot'>12h55</td>",
                       
                            "<td class='ligneTot' colspan=3>13<sup>h00</sup></td>",
                            #"<td class='ligneTot'>13h05</td>","<td class='ligneTot'>13h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>13h20</td>","<td class='ligneTot'>13h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>13h35</td>","<td class='ligneTot'>13h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>13h50</td>","<td class='ligneTot'>13h55</td>",
                       
                            "<td class='ligneTot' colspan=3>14<sup>h00</sup></td>",
                            #"<td class='ligneTot'>14h05</td>","<td class='ligneTot'>14h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>14h20</td>","<td class='ligneTot'>14h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>14h35</td>","<td class='ligneTot'>14h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>14h50</td>","<td class='ligneTot'>14h55</td>",
                       
                            "<td class='ligneTot' colspan=3>15<sup>h00</sup></td>",
                            #"<td class='ligneTot'>15h05</td>","<td class='ligneTot'>15h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>15h20</td>","<td class='ligneTot'>15h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>15h35</td>","<td class='ligneTot'>15h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>15h50</td>","<td class='ligneTot'>15h55</td>",
                       
                            "<td class='ligneTot' colspan=3>16<sup>h00</sup></td>",
                            #"<td class='ligneTot'>16h05</td>","<td class='ligneTot'>16h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>16h20</td>","<td class='ligneTot'>16h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>16h35</td>","<td class='ligneTot'>16h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>16h50</td>","<td class='ligneTot'>16h55</td>",
                       
                            "<td class='ligneTot' colspan=3>17<sup>h00</sup></td>",
                            #"<td class='ligneTot'>17h05</td>","<td class='ligneTot'>17h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>17h20</td>","<td class='ligneTot'>17h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>17h35</td>","<td class='ligneTot'>17h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>17h50</td>","<td class='ligneTot'>17h55</td>",
                       
                            "<td class='ligneTot' colspan=3>18<sup>h00</sup></td>",
                            #"<td class='ligneTot'>18h05</td>","<td class='ligneTot'>18h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>18h20</td>","<td class='ligneTot'>18h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>18h35</td>","<td class='ligneTot'>18h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>18h50</td>","<td class='ligneTot'>18h55</td>",
                       
                            "<td class='ligneTot' colspan=3>19<sup>h00</sup></td>",
                            #"<td class='ligneTot'>19h05</td>","<td class='ligneTot'>19h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>19h20</td>","<td class='ligneTot'>19h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>19h35</td>","<td class='ligneTot'>19h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>19h50</td>","<td class='ligneTot'>19h55</td>",
                       
                            "<td class='ligneTot' colspan=3>20<sup>h00</sup></td>",
                            #"<td class='ligneTot'>20h05</td>","<td class='ligneTot'>20h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>20h20</td>","<td class='ligneTot'>20h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>20h35</td>","<td class='ligneTot'>20h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>20h50</td>","<td class='ligneTot'>20h55</td>",
                       
                            "<td class='ligneTot' colspan=3>21<sup>h00</sup></td>",
                            #"<td class='ligneTot'>21h05</td>","<td class='ligneTot'>21h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>21h20</td>","<td class='ligneTot'>21h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>21h35</td>","<td class='ligneTot'>21h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>21h50</td>","<td class='ligneTot'>21h55</td>",
                       
                            "<td class='ligneTot' colspan=3>22<sup>h00</sup></td>",
                            #"<td class='ligneTot'>22h05</td>","<td class='ligneTot'>22h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>22h20</td>","<td class='ligneTot'>22h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>22h35</td>","<td class='ligneTot'>22h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>22h50</td>","<td class='ligneTot'>22h55</td>",
                       
                            "<td class='ligneTot' colspan=3>23<sup>h00</sup></td>",
                            #"<td class='ligneTot'>23h05</td>","<td class='ligneTot'>23h10</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h15</sup></td>",
                            #"<td class='ligneTot'>23h20</td>","<td class='ligneTot'>23h25</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h30</sup></td>",
                            #"<td class='ligneTot'>23h35</td>","<td class='ligneTot'>23h40</td>",
                            "<td class='ligneTot' colspan=3>&nbsp;&nbsp;<sup>h45</sup></td>",
                            #"<td class='ligneTot'>23h50</td>","<td class='ligneTot'>23h55</td>",
                          "</tr>",
                       "</thead>")


###----
table_corps <- ""
if (nrow(Tab_GrHote_extrait)>0){
    for (i in 1:nrow(Tab_GrHote_extrait)){
        for (j in 2:292){
            if (j==2){table_corps <- paste0(table_corps,"<tr style='height:",Tab_GrHote_extrait[i,4],"px;'>")}
            if (j==2 && Tab_GrHote_extrait[i,1]==1){
                table_corps <- paste0(table_corps,"<td class='col234' rowspan=3>",Tab_GrHote_extrait[i,j],"</td>")
            }
            if (j>2 && j<5) {table_corps <- paste0(table_corps,"<td class='col234'>",Tab_GrHote_extrait[i,j],"</td>")}
            if (j>=5 && Tab_GrHote_extrait[i,3]=="C_Inconnu"){
                    table_corps <- paste0(table_corps,"<td>",
                            "<div style='height:",Tab_GrHote_extrait[i,j],"px;
                                         width:5px;
                                         background-color:grey;
                                         border-color:grey;'>
                            </div>
                            </td>")
            }
            if (j>=5 && Tab_GrHote_extrait[i,3]=="B_Critical"){
                table_corps <- paste0(table_corps,"<td>",
                                      "<div style='height:",Tab_GrHote_extrait[i,j],"px;
                                         width:5px;
                                         background-color:red;
                                         border-color:red;'>
                            </div>
                            </td>")
            }
            if (j>=5 && Tab_GrHote_extrait[i,3]=="A_Warning"){
                table_corps <- paste0(table_corps,"<td>",
                                      "<div style='height:",Tab_GrHote_extrait[i,j],"px;
                                         width:5px;
                                         background-color:orange;
                                         border-color:orange;'>
                            </div>
                            </td>")
                # if (Tab_GrHote_extrait[i,j]==0){
                #     table_corps <- paste0(table_corps,"<td></td>")
                # } else { 
                #     table_corps <- paste0(table_corps,"<td>",
                #             "<table>
                #                 <tr>
                #                     <td style='height:",Tab_GrHote_extrait[i,j],"px;
                #                                background-color:orange;
                #                                border-color:orange;'></td>
                #                 </tr>
                #             </table></td>")
                # }
            }
            if (j==292){table_corps <- paste0(table_corps,"</tr>")}
        }
    }
} else {
    table_corps <- paste0(table_corps,"<tr><td colspan=7>absence d'alertes</td></tr>")
}


tab <- paste0(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset='utf-8'/>",
    "<title>Histo Alertes</title>",
    "<style>",
    " body {margin:  5px;
            padding: 0px;}",
    
    "table {width: 100%;
            bottom:0px;}",
    
    # "td.bas {position : relative;
    #          bottom : 0px;
    #          margin-bottom : 0px;
    #          padding-bottom : 0px;}",
    
    ".date, .eff, .infotri {text-align: right;}",
    
    ".tabletitre {border:         none;
                  vertical-align: top;}",
    

    "tr, td {border:          1px solid black;
             padding:         2px;
             border-collapse: collapse;
             text-align:      center;
             vertical-align:  bottom;}",
    
    "td.col234 {vertical-align: middle;}",
    
    "sup {font-size: 12px;}",
    
    ".caseRed {color:            rgb(200,0,0);
               font-weight:      bold;
               background-color: rgb(255,228,196);}",
    
    ".caseGray {background-color: Gainsboro;}",
    
    ".ligneTot {font-weight:      bold;
                background-color: rgb(239,228,176);}",
    "</style>",
    "</head>",
    "<body>",
    
    "<table>",
        entete_table, table_corps,
    "</table>",
    "<p>source : Base de donn&eacute;es `Centreon`</p>",
    
    "</body>",
    "</html>"
)

outputHTML <- paste0("HistoJour_",Zone_filter,"_",jour_ref,".html")
file.create(outputHTML, showWarnings = TRUE)
cat(tab,file=outputHTML,append=TRUE)







