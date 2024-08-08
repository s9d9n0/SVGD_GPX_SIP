
##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
##
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture",
                     password="Reporting",
                     dbname="centreon_storage", 
                     host="xx.xx.xx.xx",
                     port="3306")

##################################################################

## Au niveau de la table LOGS et sur les 24 dernieres heures ----
Req_00j <- dbGetQuery(baseMdb, 
            paste("SELECT *, FROM_UNIXTIME(ctime,'%Y-%m-%d %H:%i:%S') AS chaine_date
                   FROM logs
                   WHERE ctime > UNIX_TIMESTAMP('",date_jour_24hmoins,"') 
                   ORDER BY ctime DESC
                  "))
##################################################################

## Au niveau de la table LOGS et sur les 24 heures du jour precedent ----
Req_01j <- dbGetQuery(baseMdb, 
            paste("SELECT *, FROM_UNIXTIME(ctime,'%Y-%m-%d %H:%i:%S') AS chaine_date
                   FROM logs
                   WHERE ctime <= UNIX_TIMESTAMP('",date_jour_24hmoins,"') AND
                         ctime > UNIX_TIMESTAMP('",date_jour_48hmoins,"')
                   ORDER BY ctime DESC
                  "))
##################################################################

## Au niveau de la table HOSTS et afin de recuperer notamment les noms et ip ----
Hosts <- dbGetQuery(baseMdb, 
            paste("SELECT host_id, address, name, enabled, instance_id
                   FROM hosts
                  "))
##################################################################

##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################


##################################################
##                                              ##
## 2/ PARTIE DEFINITION DES FONCTIONS UTILISEES ##
##                                              ##
################################################## ----

## DEFINITION de fct_jointure_avec_Hosts :
## Fonction de jointure entre les variables de la table LOG et
## celles de la table HOSTS ----
fct_jointure_avec_Hosts <- function(df){
    df_join <- df %>% left_join(Hosts, by=c("host_id")) %>%
        relocate("instance_id",.before="instance_name") %>%
        mutate(host_name=str_replace(name,".part.insee$|.insee.intra$|.ad.insee.intra$|.insee.fr$","")) %>% 
        relocate(host_name,.after="host_id") %>% select(-name) %>% 
        relocate("address",.after="host_name") %>%
        relocate("enabled",.after="address") %>%
        rename("host_enabled" = "enabled") %>%
        mutate(zone = case_when(
            instance_name %in% c("Pollerdc200","Poller1dc200") ~ "dc2_DMZ",
            instance_name %in% c("Pollerdc100","Poller1dc100") ~ "dc1_DMZ",
            instance_name %in% c("Pollerdc225") ~ "dc2_PART",
            instance_name %in% c("Pollerdc125") ~ "dc1_PART",
            instance_name %in% c("Pollerdc250","Poller1dc250", "Poller2dc250","Poller3dc250") ~ "dc2_PROD",
            instance_name %in% c("Pollerdc150","Poller1dc150", "Poller2dc150","Rsdc150") ~ "dc1_PROD",
            TRUE ~ "Autre")
        ) %>% relocate("zone",.after="instance_name") %>% 
        mutate(datacenter = case_when(
            str_sub(zone,1,3)=="Auz" ~ "dc2",
            str_sub(zone,1,3)=="Osn" ~ "dc1",
            TRUE ~ "Autre")
        ) %>% relocate("datacenter",.after="zone") %>% 
        # quelques suppressions de variables finalement inutiles...
        select(-address,-issue_id,-retry,-type) %>% 
        relocate("service_description",.after="service_id") %>% relocate("output",.after="status") %>% 
        relocate("msg_type",.after="output") %>% relocate("notification_cmd",.after="msg_type") %>% relocate("notification_contact",.after="notification_cmd") %>% 
        # quelques recodifications de la variable notification_contact...
        mutate(notification_contact=str_replace(notification_contact,"_SIAR","")) %>% 
        mutate(notification_contact=str_replace(notification_contact,"ListeSiamoi_Notif_","AboNotifSiamoi_")) %>% 
        mutate(notification_contact = case_when(
            notification_contact=="xxx"            ~ "01_xxx",
            notification_contact=="xxx"            ~ "02_xxx",
            notification_contact=="Balf_Serv_Expl" ~ "Balf xxx",
            notification_contact=="Bal_Sup"        ~ "Balf xxx",
            notification_contact=="webservice"     ~ "WebService xxx",
            TRUE ~ notification_contact)
        ) %>% 
        # modification modalites de la variable notification_cmd...
        mutate(notification_cmd = case_when(
            notification_cmd=="host-notify-by-email"                ~ "Notif_Hote_par_Mel",
            notification_cmd=="service-notify-by-email"             ~ "Notif_Serv_par_Mel",
            str_sub(notification_cmd,1,21)=="service-notify-by-sms" ~ "Notif_Serv_par_SMS",
            TRUE ~ notification_cmd)
        ) %>% 
        mutate(output=str_replace(output,"CRITICAL","CRIT"))
    return (df_join)
}



##################################################################


## DEFINITION de fct_transfo_var_temps :
## Fonction permettant de transformer avec une precision a la minute au lieu de la seconde ----
fct_transfo_var_temps <- function(df){
    df_corr <- df %>% mutate(annee=year(chaine_date_corr), mois=month(chaine_date_corr), jour=day(chaine_date_corr),  
                             heure=hour(chaine_date_corr), min=minute(chaine_date_corr)) %>% 
        mutate(mois=ifelse(mois<10, paste0("0",as.character(mois)), as.character(mois)),
               jour=ifelse(jour<10, paste0("0",as.character(jour)), as.character(jour)),
               heure=ifelse(heure<10, paste0("0",as.character(heure)), as.character(heure)), 
               min=ifelse(min<10, paste0("0",as.character(min)), as.character(min))) %>%
        mutate(date_char=paste0(annee,"-",mois,"-",jour," ",heure,":",min))
    return (df_corr)
}


##################################################################


## DEFINITION de fct_agreg :
## Fonction permettant dagreger les observations de notifications
## selon la modalite de reception et l'individu Serv/Hote considere ----


fct_agreg_melserv <- function(df){
    ###
    # Etape 1 : selection notification par MEL d'alertes Services
    df_corr <- df %>% filter(notification_cmd=="Notif_Serv_par_Mel") %>% select(-notification_cmd) %>% 
        arrange(date_char,zone,host_name,service_description,output,notification_contact) %>% 
        mutate(indic_ident=ifelse(lag(date_char)==date_char &
                                  lag(zone)==zone &
                                  lag(host_name)==host_name &
                                  lag(service_description)==service_description &
                                  lag(output)==output,
                                  1,0)) %>%
        # traitement de la 1ere ligne...
        mutate(indic_ident=ifelse(is.na(indic_ident) &
                                  lead(date_char)==date_char &
                                  lead(zone)==zone & lead(host_name)==host_name &
                                  lead(service_description)==service_description &
                                  lead(output)==output,
                                  1,indic_ident)) %>% 
        mutate(list_contact_notif="")

    ###
    # Etape 2 : renseignement variable list_contact_notif
    list <- ""
    for (i in 1:nrow(df_corr)){
        if (i==1){
            list <- df_corr$notification_contact[i]
            df_corr$list_contact_notif[i] <- list
        } else if (df_corr$indic_ident[i]==1){
            list <- paste0(list," // ",df_corr$notification_contact[i])
            df_corr$list_contact_notif[i] <- list
        } else {
            list <- df_corr$notification_contact[i]
            df_corr$list_contact_notif[i] <- df_corr$notification_contact[i]
        }
    }
    
    rm(list,i)
    
    cat("nombre de lignes de la table 1 :", nrow(df_corr), "obs.\n")
    
    ###
    # Etape 3 : filtres et creation de la table df_corr_2
    df_corr_2 <- df_corr %>% mutate(nb_notifie=str_count(list_contact_notif,"//")+1) %>% 
        filter( (nb_notifie>=lead(nb_notifie) & indic_ident==1) | 
                (nb_notifie==1 & indic_ident==1) ) %>% 
        filter( !(row_number()==1 & 
                  lead(date_char)==date_char &
                  lead(zone)==zone & lead(host_name)==host_name &
                  lead(service_description)==service_description &
                  lead(output)==output) ) %>% 
        select(-msg_type,-notification_contact,-chaine_date_corr)

    cat("nombre de lignes de la table 2 :", nrow(df_corr_2), "obs.\n")

    ###
    # Etape 4 : creation de la table finale df_corr_3 avec selection des variables pertinentes
    df_corr_3 <- df_corr_2 %>% select(date_char,zone,host_name,
                                      service_description,status,output,
                                      starts_with("list_contact_notif"))
        
    list_out <- list(df_corr,df_corr_2,df_corr_3)
    return (list_out)
}

# Req_00j_notif_2 <- fct_agreg_melserv(Req_00j_notif)[[3]]



fct_agreg_smsserv <- function(df){
    ###
    # Etape 1 : selection notification par SMS d'alertes Services
    df_corr <- df %>% filter(notification_cmd=="Notif_Serv_par_SMS") %>% select(-notification_cmd) %>% 
        arrange(date_char,zone,host_name,service_description,output,notification_contact)
    
    ###
    # Etape 2 : creation de la table finale df_corr_2 avec selection des variables pertinentes
    df_corr_2 <- df_corr %>% select(date_char,zone,host_name,
                                    service_description,status,output,
                                    notification_contact) %>%
                             rename(liste_contact_notif=notification_contact)
    
    list_out <- list(df_corr,df_corr_2)
    return (list_out)
}  
    
# Req_00j_notif_2 <- fct_agreg_smsserv(Req_00j_notif)[[2]]
    


fct_agreg_melhote <- function(df){
    ###
    # Etape 1 : selection notification par MEL d'alertes Hotes
    df_corr <- df %>% filter(notification_cmd=="Notif_Hote_par_Mel") %>% select(-notification_cmd) %>% 
        arrange(date_char,zone,host_name,service_description,output,notification_contact)
    
    ###
    # Etape 2 : creation de la table finale df_corr_2 avec selection des variables pertinentes
    df_corr_2 <- df_corr %>% select(date_char,zone,host_name,
                                    service_description,status,output,
                                    notification_contact) %>%
        rename(liste_contact_notif=notification_contact)
    
    list_out <- list(df_corr,df_corr_2)
    return (list_out)
}  

# Req_00j_notif_2 <- fct_agreg_melhote(Req_00j_notif)[[2]]







###########################################
# Autres fonctions non utilisees finalement...


fct_agreg <- function(df){
    ###
    # Etape 1 : detection chgt evenement notifie et creation variable list_contact_notif
    df_corr <- df %>% 
        arrange(date_char,zone,host_name,service_description,output,notification_cmd,notification_contact) %>% 
        mutate(indic_ident=ifelse(lag(date_char)==date_char & lag(zone)==zone & lag(host_name)==host_name &
                                  lag(service_description)==service_description & lag(output)==output & lag(notification_cmd)==notification_cmd,
                                  1,0)) %>%
        # traitement de la 1ere ligne...
        mutate(indic_ident=ifelse(is.na(indic_ident) &
                                  lead(date_char)==date_char & lead(zone)==zone & lead(host_name)==host_name &
                                  lead(service_description)==service_description & lead(output)==output & lead(notification_cmd)==notification_cmd,
                                  1,indic_ident)) %>% 
        mutate(list_contact_notif="")
    
    ###
    # Etape 2 : renseignement variable list_contact_notif
    list <- ""
    for (i in 1:nrow(df_corr)){
        if (i==1){
            list <- df_corr$notification_contact[i]
            df_corr$list_contact_notif[i] <- list
        } else if (df_corr$indic_ident[i]==1){
            list <- paste0(list," // ",df_corr$notification_contact[i])
            df_corr$list_contact_notif[i] <- list
        } else {
            list <- df_corr$notification_contact[i]
            df_corr$list_contact_notif[i] <- df_corr$notification_contact[i]
        }
    }
    
    rm(list,i)
    
    ###
    # Etape 3 : corrections ulterieures avec remise de notification_contact dans list_contact_notif dans certains cas
    df_corr <- df_corr %>% 
                    mutate(list_contact_notif = case_when(
                            str_sub(notification_contact,1,14)=="AboNotifSiamoi" | 
                            notification_contact %in% c("Balf GEPEX","WebService GEPEX") ~ notification_contact,
                            TRUE ~ list_contact_notif)) %>% 
                    mutate(indic_ident = case_when(
                            str_sub(notification_contact,1,14)=="AboNotifSiamoi" | 
                            notification_contact %in% c("Balf GEPEX","WebService GEPEX") ~ 0,
                            TRUE ~ indic_ident))
    
    cat("nombre de lignes de la table :", nrow(df_corr), "obs.\n")
    
    ###
    # Etape 4 : filtre, corrections ulterieures de indic_ident et transposition selon le mode de notification 
    # creation de la table df_corr_2
    df_corr_2 <- df_corr %>% mutate(nb_notifie=str_count(list_contact_notif,"//")+1) %>% 
        filter(nb_notifie>=lead(nb_notifie) | nb_notifie==1 | row_number()==nrow(.)) %>% 
        mutate(indic_ident=0) %>% 
        mutate(indic_ident=ifelse(list_contact_notif=="01_Jean-Sev. L." & str_detect(lead(list_contact_notif),"01_Jean-Sev. L.") &
                                  lead(date_char)==date_char & lead(zone)==zone & lead(host_name)==host_name &
                                  lead(service_description)==service_description & lead(output)==output & lead(notification_cmd)==notification_cmd,
                                  1,0)) %>% 
        filter(indic_ident==0) %>%
        mutate(list_contact_notif=str_replace_all(list_contact_notif,"(01_|02_|03_|04_|05_|06_|07_|08_)","")) %>% 
        pivot_wider(names_from=notification_cmd, values_from = list_contact_notif) %>% as.data.frame() 
    
    cat("apres transposition selon le mode de notification, le nombre de lignes de la table est de :", nrow(df_corr_2), "obs.\n")
    
    ###
    # Etape 5 : simplification par reunion entre notif AboSiamoi et BalfGepex 
    # creation de la table df_corr_3
    df_corr_3 <- df_corr_2 %>% 
         mutate(indic_ident=ifelse(str_sub(Notif_Serv_par_Mel,1,14)=="AboNotifSiamoi" &
                                   str_sub(lead(Notif_Serv_par_Mel),1,10)=="Balf GEPEX" &
                                   lead(date_char)==date_char & lead(zone)==zone & lead(host_name)==host_name &
                                   lead(service_description)==service_description & lead(output)==output,
                                   1,0)) %>% 
         mutate(Notif_Serv_par_Mel=ifelse(indic_ident==1,paste0(Notif_Serv_par_Mel," // ",lead(Notif_Serv_par_Mel)),Notif_Serv_par_Mel)) %>% 
         filter(!(lag(indic_ident)==1 & Notif_Serv_par_Mel=="Balf GEPEX"))
     
    cat("apres reunion entre notifs' AboSiamoi et BalfGepex, le nombre de lignes de la table est de :", nrow(df_corr_3), "obs.\n")

    ###
    # Etape 6 : creation de la table finale df_corr_4 avec selection des variables pertinentes
    df_corr_4 <- df_corr_3 %>% select(date_char,zone,host_name,
                                      service_description,output,
                                      starts_with("Notif_"))
    
    list_out <- list(df_corr,df_corr_2,df_corr_3,df_corr_4)
    return (list_out)
}


##################################################################


