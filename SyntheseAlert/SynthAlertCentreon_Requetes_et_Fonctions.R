
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
            paste("SELECT *,
                          FROM_UNIXTIME(ctime,'%Y-%m-%d %H:%i:%S') AS chaine_date
                   FROM logs
                   WHERE ctime > UNIX_TIMESTAMP('",date_jour_24hmoins,"') 
                   ORDER BY ctime DESC
                  "))
##################################################################

## Au niveau de la table LOGS et sur les 24 heures du jour precedent ----
Req_01j <- dbGetQuery(baseMdb, 
            paste("SELECT *,
                          FROM_UNIXTIME(ctime,'%Y-%m-%d %H:%i:%S') AS chaine_date
                   FROM logs
                   WHERE ctime <= UNIX_TIMESTAMP('",date_jour_24hmoins,"') AND
                         ctime > UNIX_TIMESTAMP('",date_jour_48hmoins,"')
                   ORDER BY ctime DESC
                  "))
##################################################################

## Au niveau de la table HOSTS et afin de recuperer notamment les noms et ip ----
Hosts <- dbGetQuery(baseMdb, 
            paste("SELECT host_id, address, name, enabled, instance_id
                   FROM hosts", sep=""))
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
            instance_name %in% c("Pollerdc250","Poller1dc250",
                                 "Poller2dc250","Poller3dc250") ~ "dc2_PROD",
            instance_name %in% c("Pollerdc150","Poller1dc150",
                                 "Poller2dc150","Rsdc150") ~ "dc1_PROD",
            TRUE ~ "Autre")
        ) %>% relocate("zone",.after="instance_name") %>% 
        mutate(datacenter = case_when(
            str_sub(zone,1,3)=="Auz" ~ "dc2",
            str_sub(zone,1,3)=="Osn" ~ "dc1",
            TRUE ~ "Autre")
        ) %>% relocate("datacenter",.after="zone")
    return (df_join)
}


##################################################################


## DEFINITION de fct_ajout_var_temps :
## Fonction permettant d'ajouter des variables de temps supplementaire
## calendrier - annee - mois - jour - heure - min - sec - num de la semaine
## et demiheure equivalent a heure mais selon des tranches de 30 min ----
fct_ajout_var_temps <- function(df){
    df_corr <- df %>% mutate(calendrier=paste0(year(chaine_date_corr),"-",
                                               month(chaine_date_corr),"-",
                                               day(chaine_date_corr)),
                             annee=year(chaine_date_corr), 
                             mois=month(chaine_date_corr),
                             jour=day(chaine_date_corr),  
                             heure=hour(chaine_date_corr),
                             min=minute(chaine_date_corr),
                             sec=second(chaine_date_corr),
                             sem=week(chaine_date_corr)) %>% 
        mutate(mois=ifelse(mois<10, paste0("0",as.character(mois)), as.character(mois))) %>%
        mutate(jour=ifelse(jour<10, paste0("0",as.character(jour)), as.character(jour))) %>%
        mutate(calendrier=paste0(annee,"-",mois,"-",jour)) %>% 
        mutate(heure=ifelse(heure<10, paste0("0",as.character(heure)), as.character(heure))) %>%
        mutate(min=ifelse(min<10, paste0("0",as.character(min)), as.character(min))) %>%
        mutate(sec=ifelse(sec<10, paste0("0",as.character(sec)), as.character(sec))) %>%   
        mutate(tr_demiheure=ifelse(as.numeric(min)<30,"00","30")) %>% 
        mutate(demiheure=paste0(heure,":",tr_demiheure)) %>% 
        select(-tr_demiheure)
    return (df_corr)
}


##################################################################


## DEFINITION de fct_agreg_notif_initiale :
## Fonction d'agregation initiale selon quelques variables ----
fct_agreg_notif_initiale <- function(df){
    df_agreg <- df %>% select(-log_id) %>% unique() %>%
        group_by(calendrier,annee,mois,sem,jour,
                 demiheure,heure,min,sec,chaine_date_corr,
                 datacenter,zone,instance_name,host_name,
                 service_description,type,status,output) %>%
        rename(service=service_description) %>% 
        summarize(nb_notif=n()) %>% ungroup() %>% as.data.frame()
    return (df_agreg)
}


##################################################################


## DEFINITION de fct_agreg_1var :
## Fonction d'agregation sur la variable calendrier et 1 var. comme demiheure ----
fct_agreg_1var <- function(df,varagreg,nomcol,nbjour){
    df_agreg <- df %>% group_by(calendrier,{{varagreg}}) %>%
        summarize(nb_notif=sum(nb_notif)) %>% ungroup() %>% as.data.frame() %>%
        mutate(var=round(nb_notif/nbjour,0)) %>% select(-nb_notif)
    colnames(df_agreg)[3] <- nomcol
    return (df_agreg)
}


##################################################################


## DEFINITION de fct_agreg_5var :
## Fonction d'agregation sur 5 variables (calendrier + 5 autres var.) ----
# fct_agreg_5var <- function(df,varagreg1,varagreg2,varagreg3,varagreg4,varagreg5,nomcol,nbjour){
#     df_agreg <- df %>% group_by(calendrier,
#                                 {{varagreg1}},{{varagreg2}},{{varagreg3}},
#                                 {{varagreg4}},{{varagreg5}}) %>%
#         summarize(nb_notif=sum(nb_notif)) %>% ungroup() %>% as.data.frame() %>%
#         mutate(var=round(nb_notif/nbjour,0)) %>% select(-nb_notif)
#     colnames(df_agreg)[7] <- nomcol
#     return (df_agreg)
# }


##################################################################


## DEFINITION de fct_agreg_6var :
## Fonction d'agregation sur 6 variables (calendrier + 5 autres var.) ----
fct_agreg_6var <- function(df,varagreg1,varagreg2,varagreg3,varagreg4,varagreg5,varagreg6,nomcol,nbjour){
    df_agreg <- df %>% group_by(calendrier,
                                {{varagreg1}},{{varagreg2}},{{varagreg3}},{{varagreg4}},{{varagreg5}},{{varagreg6}}) %>%
        summarize(nb_notif=sum(nb_notif)) %>% ungroup() %>% as.data.frame() %>%
        mutate(var=round(nb_notif/nbjour,0)) %>% select(-nb_notif)
    colnames(df_agreg)[8] <- nomcol
    return (df_agreg)
}


##################################################################


## DEFINITION de fct_ope_demiheure :
## Fonction definissant les operations pour obtenir
## un resume par demiheure ----
fct_ope_demiheure <- function(dfA,dfB){
    demiheure <-c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30",
                  "04:00","04:30","05:00","05:30","06:00","06:30","07:00","07:30",
                  "08:00","08:30","09:00","09:30","10:00","10:30","11:00","11:30",
                  "12:00","12:30","13:00","13:30","14:00","14:30","15:00","15:30",
                  "16:00","16:30","17:00","17:30","18:00","18:30","19:00","19:30",
                  "20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30")
    demiheure <- as.data.frame(demiheure)
    rownames(demiheure) <- NULL
    
    jour_02j <- Req_01j_demiheure %>% select(calendrier) %>% head(1) 
    rownames(jour_02j) <- NULL
    jour_01j <- Req_00j_demiheure %>% select(calendrier) %>% head(1) 
    rownames(jour_01j) <- NULL
    jour_00j <- Req_00j_demiheure %>% select(calendrier) %>% tail(1) 
    rownames(jour_00j) <- NULL
    
    jour_heure_02j <- cbind(jour_02j,demiheure)
    jour_heure_01j <- cbind(jour_01j,demiheure)
    jour_heure_00j <- cbind(jour_00j,demiheure)
    jour_heure <- rbind(jour_heure_02j,jour_heure_01j,jour_heure_00j)
    
    #####
    
    dern_jour <- jour_00j %>% as.character() 
    
    Req_00j_interm <- dfA %>%
        full_join(jour_heure,by=c("calendrier","demiheure")) %>% 
        cbind(dern_jour,heure_lancement_demiheure) %>% 
        arrange(calendrier,demiheure) %>% 
        mutate(lignecoupure=ifelse(calendrier==dern_jour,ifelse(demiheure==heure_lancement_demiheure,1,NA),NA)) %>% 
        fill(lignecoupure,.direction=c("down"))
    dfA <- Req_00j_interm %>% 
        filter(is.na(lignecoupure)) %>% 
        select(-dern_jour,-heure_lancement_demiheure,-lignecoupure) %>% 
        slice_tail(n=48) %>% 
        mutate(nb_notif_00j=ifelse(is.na(nb_notif_00j),0,nb_notif_00j))
    
    ####
    
    dern_jour <- jour_01j %>% as.character() 
    
    Req_01j_interm <- dfB %>%
        full_join(jour_heure,by=c("calendrier","demiheure")) %>% 
        cbind(dern_jour,heure_lancement_demiheure) %>% 
        arrange(calendrier,demiheure) %>% 
        mutate(lignecoupure=ifelse(calendrier==dern_jour,ifelse(demiheure==heure_lancement_demiheure,1,NA),NA)) %>% 
        fill(lignecoupure,.direction=c("down"))
    dfB <- Req_01j_interm %>% 
        filter(is.na(lignecoupure)) %>% 
        select(-calendrier,-dern_jour,-heure_lancement_demiheure,-lignecoupure) %>% 
        slice_tail(n=48) %>% 
        mutate(nb_notif_01j=ifelse(is.na(nb_notif_01j),0,nb_notif_01j))
    
    ####
    
    df_out <- dfA %>%
        full_join(dfB,by=c("demiheure")) %>% 
        slice_tail(n=48)
    
    Req_par_demiheure_TOT <- df_out %>% 
        summarize(calendrier="Ensemble",
                  demiheure="Ensemble",
                  nb_notif_00j=sum(nb_notif_00j),
                  nb_notif_01j=sum(nb_notif_01j))
    df_out <- rbind(df_out,Req_par_demiheure_TOT)
    
    rm(dfA,dfB,Req_par_demiheure_TOT)
    
    return (df_out)
}


##################################################################


## DEFINITION de transfo_table_aliasdest :
## Fonction permettant de passer a un format large et de recuperer 
## les differentes heures de notifications figurent selon le format h1 // h2 // h3... ----
transfo_table_aliasdc <- function(df){
    # transformation au format long avec pivot_wider, 
    # chaque colonne correspond ? un destinataire
    df <- df %>% unite(heure, min, sec, col = "heure", sep = ":") %>% 
        pivot_wider(names_from = datacenter,
                    values_from = heure,
                    values_fn = list)
    
    # initialisation variables, long = nbre lignes de la table
    #                           m = matrice contenant les infos heure
    #                           list_bal = vecteur contenant les destinataires
    long <- numeric(nrow(df))
    m <- matrix(".", nrow = nrow(df), ncol = 1)
    
    list_bal <- colnames(df)
    list_bal <- list_bal[-(1:3)]
    length(list_bal)
    
    # initialisation d'un liste colW qui contiendra pour chaque elt
    # une colonne du data.frame, par contre chaque ligne de ce data.frame est 
    # elle-m?me une liste
    colW <- list()
    for (k in 1:length(list_bal)){
        colW[[k]] <- df %>% select(list_bal[k]) %>% as.data.frame()
    }
    
    # dim(colW[[2]])
    # vue <- colW[[2]][1][5,][[1]][1]
    
    # initialisation d'un liste tabW qui contiendra pour chaque elt
    # une colonne au format data.frame mais avec une chaine de caractere
    # sur chaque ligne et chaque notif est separee par le symbole // 
    tabW <- list()
    for (k in 1:length(list_bal)){
        for (i in 1:nrow(df)){
            long[i] <- length(colW[[k]][1][i,][[1]])
            m <- matrix(".", nrow = nrow(df), ncol = max(long))
        }
        for (i in 1:nrow(df)){
            if (long[i]!=0) {
                for (j in 1:long[i]){
                    #print(colW[[k]][1][i,][[1]][j])
                    m[i,j] <- colW[[k]][1][i,][[1]][j]
                }
            }
        }
        mdf <- as.data.frame(m)
        col_mdf <- paste0(colnames(mdf),collapse=",")
        mdf <- unite(mdf,col_mdf,sep=" // ")
        mdf2 <- mdf %>% 
            mutate(moment=str_replace_all(col_mdf," // \\.","")) %>%
            mutate(moment=str_replace_all(moment,"\\.","")) %>% select(-col_mdf)
        colnames(mdf2) <- list_bal[k]
        tabW[[k]] <- mdf2
    }
    
    rm(mdf,mdf2)
    # vue <- tabW[[1]]
    
    # reconstitution de la table souhaitee
    df_final <- df %>% select(calendrier,host_name,service)
    for (k in 1:length(list_bal)){
        df_final <- df_final %>% cbind(tabW[[k]])
    }
    return(df_final)
}


##################################################################


## DEFINITION DE transfo_duree_A :
## Fonction permettant de traduire en duree le temps ecoule entre 2 notifications
## chaque duree est ecrite selon la convention xh xm xs 
## et les differentes durees figurent selon la meme convention d1 // d2 // d3... ----
transfo_duree_A <- function(df){
    # transformation au format long avec pivot_wider, 
    # chaque colonne correspond ? un destinataire
    tab <- df %>% 
        select(calendrier,host_name,service,chaine_date_corr,datacenter) %>% 
        unique() %>% 
        arrange(datacenter,host_name,service,chaine_date_corr) %>% 
        group_by(datacenter,host_name,service) %>% 
        mutate(date_2 = ymd_hms(chaine_date_corr),
               date_1 = ymd_hms(lag(chaine_date_corr))) %>% 
        mutate(dur=int_length(interval(date_1,date_2))) %>%  
        mutate(duree_heure=dur%/%3600,
               duree_heure_reste=dur%%3600,
               duree_minute=duree_heure_reste%/%60,
               duree_seconde=duree_heure_reste%%60) %>% 
        select(-date_2,-date_1,-duree_heure_reste) %>%
        mutate(duree=paste0(duree_heure,"h ",duree_minute,"m ",duree_seconde,"s")) %>%
        select(-duree_heure:-duree_seconde) %>% 
        filter(!is.na(dur)) %>% 
        arrange(chaine_date_corr,host_name,service) %>% 
        select(-chaine_date_corr,-dur) %>% 
        pivot_wider(names_from = datacenter,
                    values_from = duree,
                    values_fn = list) %>% 
        ungroup() %>% as.data.frame()
    
    # initialisation variables, long = nbre lignes de la table
    #                           m = matrice contenant les infos heure
    #                           list_bal = vecteur contenant les destinataires
    long <- numeric(nrow(tab))
    m <- matrix(".", nrow = nrow(tab), ncol = 1)
    
    list_bal <- colnames(tab)
    list_bal <- list_bal[-(1:3)]
    length(list_bal)
    
    # initialisation d'un liste colW qui contiendra pour chaque elt
    # une colonne du data.frame, par contre chaque ligne de ce data.frame est 
    # elle-m?me une liste
    colW <- list()
    for (k in 1:length(list_bal)){
        colW[[k]] <- tab %>% select(list_bal[k]) %>% as.data.frame()
    }
    
    # dim(colW[[6]])
    # vue <- colW[[1]][1][1,][[1]][4]
    
    # initialisation d'un liste tabW qui contiendra pour chaque elt
    # une colonne au format data.frame mais avec une chaine de caractere
    # sur chaque ligne et chaque notif est separee par le symbole // 
    tabW <- list()
    for (k in 1:length(list_bal)){
        for (i in 1:nrow(tab)){
            long[i] <- length(colW[[k]][1][i,][[1]])
            m <- matrix(".", nrow = nrow(tab), ncol = max(long))
        }
        for (i in 1:nrow(tab)){
            if (long[i]!=0) {
                for (j in 1:long[i]){
                    #print(colW[[k]][1][i,][[1]][j])
                    m[i,j] <- colW[[k]][1][i,][[1]][j]
                }
            }
        }
        mdf <- as.data.frame(m)
        col_mdf <- paste0(colnames(mdf),collapse=",")
        mdf <- unite(mdf,col_mdf,sep=" // ")
        mdf2 <- mdf %>% 
            mutate(moment=str_replace_all(col_mdf," // \\.","")) %>%
            mutate(moment=str_replace_all(moment,"\\.","")) %>% select(-col_mdf)
        colnames(mdf2) <- list_bal[k]
        tabW[[k]] <- mdf2
    }
    
    rm(mdf,mdf2)
    # vue <- tabW[[1]]
    
    # reconstitution de la table souhaitee
    df_final <- tab %>% select(calendrier,host_name,service)
    for (k in 1:length(list_bal)){
        df_final <- df_final %>% cbind(tabW[[k]])
    }
    return(df_final)
}


##################################################################


## DEFINITION DE transfo_duree_B :
## Fonction permettant de renommer les colonnes de durees,
## puis de fusionner avec la table agregee contenant les heures de notifications
## presentees selon le meme format (" // "),
## puis de reordonner les colonnes logiquement ----
transfo_duree_B <- function(df_notif,df_dur){                 
    # changement du nom des colonnes du data.frame duree
    # (pour preparation de la fusion ulterieure)
    Vue <- colnames(df_dur)
    Vue <- Vue[-(1:3)]
    vectdur <- c("duree_")
    Vue <- data.frame(vectdur,Vue) %>% unite(.,vectdur,Vue,col="nomdur",sep="")
    
    colnames(df_dur) <- c("calendrier","host_name","service",Vue$nomdur)
    
    rm(Vue,vectdur)
    
    # fusion entre les 2 tables et reordonnancement de colonnes
    df_out <- df_notif %>% mutate(numligne=row_number()) %>% 
        relocate(numligne,.before=calendrier) %>% 
        left_join(df_dur, by=c("calendrier","host_name","service")) %>% 
        relocate(OSNY,.after=service) %>% 
        relocate(duree_OSNY,.after=OSNY)
    
    # # operations de reordonnancement des autres colonnes
    Vue <- colnames(df_out)
    Vue <- Vue[-(1:6)] %>% as.data.frame() %>%
        rename(nomcol=".") %>%
        mutate(dur=ifelse(substr(nomcol,1,6)=="duree_","O","N")) %>%
        mutate(var=ifelse(dur=="N",nomcol,substr(nomcol,7,str_length(nomcol)))) %>%
        arrange(var,dur) %>%
        mutate(numligne=row_number()) %>%
        select(-dur,-var)
    for (i in 1:nrow(Vue)){
        df_out <- df_out %>% relocate(Vue$nomcol[i],.after=last_col())
    }

    # # operations de nettoyage des cases avec NA suite a la fusion entre les 2 data.frame
    for (j in 1:nrow(df_out)){
        for (k in 4:ncol(df_out)){
            if (is.na(df_out[j,k])) {
                df_out[j,k] <- ""
            }
        }
    }
    
    return (df_out)
}


##################################################################


## DEFINITION DE transfo_duree_C :
## Fonction permettant d'obtenir en sortie une liste de 2 data.frame
## 1 data.frame en pos.1 pour le dc OSNY et
## 1 data.frame en pos.2 pour le dc AUZEVILLE
## la fonction permet egalement de simplifier certaines lignes ou le serveur
## apparait en doublon en y apportant de l'information comme
## (j-1) au niveau des heures ou (entre j-1 et j) au niveau des durees
## Enfin, elle traduit en seconde toutes les durees et fournit la duree min et max 
## entre 2 notifs pour chaque serveur ----
transfo_duree_C <- function(df){
    #selection des lignes concernant les notifs de la balf de la supervision
    df_sup <- df %>% filter(OSNY!="") %>% 
        select(numligne, calendrier, host_name, service,
               OSNY, duree_OSNY)
    
    #detection des serveurs apparaissant 1 fois
    Vue1x <- as.data.frame(table(df_sup$host_name)) %>% 
        filter(Freq==1) %>% rename(host_name=Var1) %>% select(-Freq) %>% 
        left_join(df_sup, by=c("host_name"))
    
    #detection des serveurs apparaissant 2 fois et plus
    Vue2x <- as.data.frame(table(df_sup$host_name)) %>% 
        filter(Freq>=2) %>% rename(listhost_name=Var1) %>% select(listhost_name)
    #corrections/modifications/suppressions de certaines lignes
    df_sup2 <- df_sup %>% filter(host_name %in% (Vue2x$listhost_name)) %>% 
        arrange(host_name) %>% 
        # 1er type de correction
        mutate(condition=ifelse(is.na(duree_OSNY) & !str_detect(OSNY,"//"),
                                "O",
                                NA)) %>% 
        mutate(corr=ifelse(lag(condition)=="O",
                           paste0(lag(OSNY)," (j-1) // ",OSNY),
                           NA)) %>% 
        mutate(OSNY=ifelse(!is.na(corr),
                           corr,
                           OSNY)) %>% 
        filter(!is.na(duree_OSNY)) %>% 
        # 2eme type de correction
        mutate(condition=ifelse(host_name==lag(host_name) & str_detect(duree_OSNY,"//"),
                                "O",
                                NA)) %>% 
        mutate(corr=ifelse(condition=="O",
                           str_replace(duree_OSNY," // "," (entre j-1 et j) // "),
                           NA)) %>%               
        mutate(duree_OSNY=ifelse(!is.na(corr),
                                 corr,
                                 duree_OSNY)) %>% 
        select(-condition,-corr)
    
    df_out <- rbind(Vue1x,df_sup2) %>% 
        arrange(numligne) %>% select(-numligne) %>% 
        mutate(nb_notif=str_count(OSNY,"//")+1) %>% 
        mutate(nb_duree=str_count(duree_OSNY,"//")+1) %>%
        # plus quelques recodages pour assurer une coherence densemble 
        # au niveau des variables nb_duree et duree_OSNY
        mutate(nb_duree=ifelse(duree_OSNY=="",NA,nb_duree)) %>% 
        mutate(duree_OSNY=ifelse(duree_OSNY=="",NA,duree_OSNY)) %>% 
        #prepa suite operations
        mutate(var=str_split(duree_OSNY," // ",n=nb_duree)) %>% 
        mutate(min=0,max=0)
    
    for (i in 1:nrow(df_out)){
        dureesec <- ""
        print(i)
        if (is.na(df_out$nb_duree[i])){
            df_out$dureesec[i] <- "0"
        }
        if (!is.na(df_out$nb_duree[i])){ 
            for (k in 1:df_out$nb_duree[i]){ 
                #print(df_out$var[[i]][k])
                val<-df_out$var[[i]][k]
                #heure
                place_h=str_locate(val,"h")[1,1]
                heure_en_sec<-as.numeric(str_sub(val,1,place_h-1))*3600
                #print(heure_en_sec)
                #minute
                place_m=str_locate(val,"m")[1,1]
                min_en_sec<-as.numeric(str_sub(val,place_h+1,place_m-1))*60
                #print(min_en_sec)
                #seconde
                place_s=str_locate(val,"s")[1,1]
                sec_en_sec<-as.numeric(str_sub(val,place_m+1,place_s-1))
                #print(sec_en_sec)
                #conversion en seconde
                sec=heure_en_sec+min_en_sec+sec_en_sec
                print(sec)
                
                if (df_out$nb_duree[i]>=1){
                    dureesec <- paste0(dureesec," // ",sec)
                    df_out$dureesec[i] <- str_sub(dureesec,5,str_length(dureesec))
                    print(dureesec)
                }
                #initialisation des valeurs min et max
                if (k==1){
                    min <- sec
                    max <- sec
                    print(min)
                    print(max)
                }
                if (sec<=min){
                    print(i)
                    min <- sec
                    df_out$min[i] <- sec
                }
                if (sec>=max){
                    print(i)
                    max <- sec
                    df_out$max[i] <- sec
                }
            }
        }
    }
    
    df_out <- df_out %>% 
        mutate(duree_OSNY=ifelse(nb_notif==1,NA,duree_OSNY)) %>% 
        mutate(nb_duree=ifelse(nb_notif==1,NA,nb_duree)) %>% 
        mutate(min=ifelse(nb_notif==1,NA,min)) %>% 
        mutate(max=ifelse(nb_notif==1,NA,max)) %>% 
        mutate(dureesec=ifelse(nb_notif==1,NA,dureesec)) %>% 
        select(-var) %>% 
        relocate(calendrier)
    
    df_out <- df_out %>% mutate(dureesec=ifelse(is.na(dureesec),"",dureesec)) %>%
                mutate(duree_OSNY=ifelse(is.na(duree_OSNY),"",duree_OSNY)) %>%
                rename(dureemin=min,dureemax=max)



    # recherche des lignes a retenir pour le 2nd data.frame (notifs envoyees ailleurs que sur la balf supervision)
    
    # definition des colonnes o? au moins une notif a ete envoyee (hors balf_supervision)
    SelectCol <- df %>% select(-(2:6)) %>% colnames() %>% as.data.frame() %>% rename(liste=".")
    SelectCol <- SelectCol %>% mutate(numligne=row_number()) %>% 
                    filter(!(numligne==1)) %>%
                    filter(!(str_detect(liste,"duree_"))) %>%
                    filter(!(str_detect(liste,"nb_notif_"))) %>% select(-numligne)
    # restriction aux lignes ou au moins un destinataire particulier a ete notifie
    Vue <- df %>% select(SelectCol$liste) %>% unite(.,colnames(.),col="Agreg",sep="",remove=FALSE) %>% 
                                mutate(numligne=row_number()) %>% 
                                mutate(ligneagarder=ifelse(str_length(Agreg)!=0,"O","N")) %>% 
                                select(numligne,ligneagarder) %>% filter(ligneagarder=="O") %>% 
                                select(-ligneagarder)
    
    df_out2 <- df %>% right_join(Vue, by=c("numligne")) %>% select(-numligne)
    

    # df_out2 <- df %>% filter(OSNY=="" | ) %>% 
    #     select(-numligne,-OSNY,-duree_OSNY)
    
    list_out <- list(df_out,df_out2)
    return (list_out)
}


##################################################################












