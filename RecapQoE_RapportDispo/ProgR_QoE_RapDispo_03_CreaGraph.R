
## Creation des graphiques ----

Res_graph <- Result_agreg  %>% filter(service_description==nom_QoE) %>% mutate(ordre=row_number())

nb_jour_mois <- days_in_month(as.Date(paste0(mois_ref,"-01")))


Exclusion_Serv <- Uniquement_Excl %>% filter(Service==nom_QoE) %>% 
  mutate(quinz=as.integer(str_sub(date,6,7)),
         milieu_mois=case_when(nb_jour_mois==28 ~ 14,
                               nb_jour_mois==29 ~ 14,
                               nb_jour_mois==30 ~ 15,
                               nb_jour_mois==31 ~ 15),
         quinzaine=ifelse(quinz<=milieu_mois,"quinz1","quinz2")) %>% 
  select(-quinz,-milieu_mois)

Exclusion_Serv_quinz1 <- Exclusion_Serv %>% filter(quinzaine=="quinz1")
Exclusion_Serv_quinz2 <- Exclusion_Serv %>% filter(quinzaine=="quinz2")

Res_graph <- Res_graph %>% left_join(Exclusion_Serv,by=c("date"))


table(Res_graph$jour_bis)
# la table Res_graph doit normalement contenir 48  pts par jour
# traitement du cas special des QoE
# QoE-BRPP-Backup-France_Connect_1min, 
# QoE-ELIRE-Interne, 
# QoE-GMC3-Externe, 
# QoE-PILOT-Agents, 
# QoE-Si@moi-V2


vect <- c(paste0("0",as.character(seq(1,9,1))),as.character(seq(10,31,1)))
if (nb_jour_mois==28){vecteur_label_x <- c(vect[1:28],"01")}
if (nb_jour_mois==29){vecteur_label_x <- c(vect[1:29],"01")}
if (nb_jour_mois==30){vecteur_label_x <- c(vect[1:30],"01")}
if (nb_jour_mois==31){vecteur_label_x <- c(vect[1:31],"01")}

lib_jour1 <- Res_graph %>% select(lib_jour,jour_bis) %>% filter(jour_bis==1) %>%
  select(lib_jour) %>% head(1) %>% as.character()

# si le service n'a pas debute au tout debut du mois, entrer le libelle du jour 1 du mois
 lib_jour1 <- "sam."

 jsem <- case_when(lib_jour1=="lun." ~ c("lun.","mar.","mer.","jeu.","ven.","sam.","dim."),
                   lib_jour1=="mar." ~ c("mar.","mer.","jeu.","ven.","sam.","dim.","lun."),
                   lib_jour1=="mer." ~ c("mer.","jeu.","ven.","sam.","dim.","lun.","mar."),
                   lib_jour1=="jeu." ~ c("jeu.","ven.","sam.","dim.","lun.","mar.","mer."),
                   lib_jour1=="ven." ~ c("ven.","sam.","dim.","lun.","mar.","mer.","jeu."),
                   lib_jour1=="sam." ~ c("sam.","dim.","lun.","mar.","mer.","jeu.","ven."),
                   lib_jour1=="dim." ~ c("dim.","lun.","mar.","mer.","jeu.","ven.","sam."))
 vectplus <- paste0(jsem," ",vecteur_label_x)
 
 rm(vect,jsem)
 
 vectheure <- c("00:00","00:30","01:00","01:30","02:00","02:30","03:00","03:30","04:00","04:30","05:00","05:30",
                "06:00","06:30","07:00","07:30","08:00","08:30","09:00","09:30","10:00","10:30","11:00","11:30",
                "12:00","12:30","13:00","13:30","14:00","14:30","15:00","15:30","16:00","16:30","17:00","17:30",
                "18:00","18:30","19:00","19:30","20:00","20:30","21:00","21:30","22:00","22:30","23:00","23:30")
 
 vectplus2 <- crossing(vectplus,vectheure) %>% as.data.frame() %>% 
   mutate(libelle_jour1=lib_jour1,num_jour=str_sub(vectplus,6,7)) %>% 
   filter(!(num_jour=="01" & libelle_jour1!=str_sub(vectplus,1,4))) %>% 
   arrange(num_jour,vectheure) %>% 
   unite(vectplus,vectheure,col="date",sep=" ",remove=TRUE) %>% 
   select(-libelle_jour1,-num_jour) %>% 
   mutate(lib_jour=str_sub(date,1,4),
          jour_bis=as.integer(str_sub(date,6,7)),
          heure_bis=as.integer(str_sub(date,9,10)),
          min_bis=as.integer(str_sub(date,12,13)),
          heure=str_sub(date,9,13))
 
 Res_graph <- Res_graph %>% right_join(vectplus2,by=c("lib_jour","jour_bis","date","heure_bis","min_bis","heure")) %>% 
   arrange(jour_bis,heure_bis,min_bis) %>% 
   fill(host_id:mois_bis,.direction=c("down")) %>%
   mutate(ordre=row_number())
 
 
 # Ensemble des points du mois
 # nb_pts_x <- case_when(nb_jour_mois==28 ~ 1344, nb_jour_mois==29 ~ 1392,
 #                       nb_jour_mois==30 ~ 1440, nb_jour_mois==31 ~ 1488) # multiple de 48 car nb demi-heure dans 1 jour
 
 # points de la 1ere quinzaine du mois uniquement
 nb_pts_x_quinz1 <- case_when(nb_jour_mois==28 ~ 672, nb_jour_mois==29 ~ 672,
                              nb_jour_mois==30 ~ 720, nb_jour_mois==31 ~ 720) # multiple de 48 car nb demi-heure dans 1 jour
 if (nb_jour_mois==28){vect_label_x_quinz1 <- vectplus[1:15]}
 if (nb_jour_mois==29){vect_label_x_quinz1 <- vectplus[1:15]}
 if (nb_jour_mois==30){vect_label_x_quinz1 <- vectplus[1:16]}
 if (nb_jour_mois==31){vect_label_x_quinz1 <- vectplus[1:16]}
 
 # points de la 2nde quinzaine du mois uniquement
 nb_pts_x_quinz2 <- case_when(nb_jour_mois==28 ~ 672, nb_jour_mois==29 ~ 720,
                              nb_jour_mois==30 ~ 720, nb_jour_mois==31 ~ 768) # multiple de 48 car nb demi-heure dans 1 jour
 if (nb_jour_mois==28){vect_label_x_quinz2 <- vectplus[15:28]}
 if (nb_jour_mois==29){vect_label_x_quinz2 <- vectplus[15:29]}
 if (nb_jour_mois==30){vect_label_x_quinz2 <- vectplus[16:30]}
 if (nb_jour_mois==31){vect_label_x_quinz2 <- vectplus[16:31]}
 
 
 # Creation des tables Res_graph_quinz1 et Res_graph_quinz2
 if(nb_jour_mois<=29){
   Res_graph_quinz1 <- Res_graph %>% filter(jour_bis %in% seq(1,14)) %>% mutate(ordre=row_number())
 } else {
   Res_graph_quinz1 <- Res_graph %>% filter(jour_bis %in% seq(1,15)) %>% mutate(ordre=row_number())
 }
 
 if(nb_jour_mois==28){
   Res_graph_quinz2 <- Res_graph %>% filter(jour_bis %in% seq(15,28)) %>% mutate(ordre=row_number())
 }
 if (nb_jour_mois==29){
   Res_graph_quinz2 <- Res_graph %>% filter(jour_bis %in% seq(15,29)) %>% mutate(ordre=row_number())
 } 
 if (nb_jour_mois==30){
   Res_graph_quinz2 <- Res_graph %>% filter(jour_bis %in% seq(16,30)) %>% mutate(ordre=row_number())
 }
 if (nb_jour_mois==31){
   Res_graph_quinz2 <- Res_graph %>% filter(jour_bis %in% seq(16,31)) %>% mutate(ordre=row_number())
 }
 
 # fonction de detection des moments de debut et de fin des periodes en mode 5j7 et 7h-19h
 Detect_debfin_5j7 <-function(df,var_ymin,var_ymax){
   df_out <- df %>% select(service_description,lib_jour,heure,ordre) %>% 
     mutate(indic_5J7=ifelse(heure %in% c("19:00","19:30","20:00","20:30","21:00","21:30",
                                          "22:00","22:30","23:00","23:30","00:00","00:30",
                                          "01:00","01:30","02:00","02:30","03:00","03:30",
                                          "04:00","04:30","05:00","05:30","06:00","06:30") |
                               lib_jour %in% c("sam.","dim."),"Oui","Non")) %>%
     filter(indic_5J7=="Oui") %>%
     mutate(indic_lag=ordre-lag(ordre), indic_lead=lead(ordre)-ordre) %>% 
     filter(ordre==min(ordre) | ordre==max(ordre) | indic_lag!=1 | indic_lead!=1) %>% 
     select(-indic_5J7,-indic_lag,-indic_lead) %>%
     mutate(lg=row_number()%%2)
   lg_impair <- df_out %>% filter(lg==1) %>% select(service_description,ordre) %>% rename(ordmin=ordre)
   lg_pair <- df_out %>% filter(lg==0) %>% select(ordre) %>% rename(ordmax=ordre)
   df_out <- cbind.data.frame(lg_impair,lg_pair) %>% 
     cbind.data.frame(ymin=var_ymin,ymax=var_ymax,ordre=1,disponibilite=1)
   return(df_out)
 }
 
 quinz1_5J7  <- Detect_debfin_5j7(Res_graph_quinz1,100,105)
 quinz2_5J7  <- Detect_debfin_5j7(Res_graph_quinz2,100,105)
 

##########################################
##########################################

 # graphique DISPO UNIQUEMENT quinzaine num 1
 graphDISPO_quinz1 <- ggplot(NULL, aes(x=ordre, y=disponibilite)) +
   geom_line(data = Res_graph_quinz1, size = 0.8, color = "brown") +
   
   scale_x_continuous(name = "", breaks = seq(0, nb_pts_x_quinz1, 48), 
                      labels = vect_label_x_quinz1) +
   scale_y_continuous(name = "", limits = c(0,105), breaks = seq(0, 100, 10)) + 
   
   theme_light() +
   theme(axis.text.x = element_text(color="black", size=12, angle = 30, face="italic"),
         axis.text.y = element_text(color="brown", size=12),
         panel.grid.minor.x = element_line(color = "white", size = 0.25, linetype = "dotted"),
         panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"),
         panel.border = element_blank(),
         legend.position="none")
 
 graphDISPO_quinz1
 
 ##########################################
 ##########################################
 # graphique TEMPS UNIQUEMENT quinzaine num 1
 graphTEMPS_quinz1 <-  ggplot(data = Res_graph_quinz1, aes(x = ordre)) +
   geom_line(mapping = aes(y = temps), size = 0.8, color = "blue") +
   geom_line(mapping = aes(y = warn), size = 0.5, color = "orange", linetype = "longdash") +
   geom_line(mapping = aes(y = crit), size = 0.5, color = "red", linetype = "longdash") +
   
   scale_x_continuous(name = "", breaks = seq(0, nb_pts_x_quinz1, 48),
                      labels = vect_label_x_quinz1) +
   scale_y_continuous(name = "") + 
   
   theme_light() +
   theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
         axis.text.x = element_text(color="black", size=12, angle = 30, face="italic"),
         axis.title.y = element_text(color = "blue", size=10, face="italic"),
         axis.text.y = element_text(color="blue", size=12),
         panel.grid.minor.x = element_line(color = "white", size = 0.25, linetype = "dotted"),
         panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"),
         panel.border = element_blank(),
         legend.position="none") 
 
 graphTEMPS_quinz1
 
 ##########################################
 ##########################################
 # graphique DISPO UNIQUEMENT quinzaine num 2
 graphDISPO_quinz2 <- ggplot(NULL, aes(x=ordre, y=disponibilite)) +
   geom_line(data = Res_graph_quinz2, size = 0.8, color = "brown") +
   
   scale_x_continuous(name = "", breaks = seq(0, nb_pts_x_quinz2, 48), 
                      labels = c(vect_label_x_quinz2,vectplus[length(vectplus)])) +
   scale_y_continuous(name = "", limits = c(0,105), breaks = seq(0, 100, 10)) + 
   
   theme_light() +
   theme(axis.text.x = element_text(color="black", size=12, angle = 30, face="italic"),
         axis.text.y = element_text(color="brown", size=12),
         panel.grid.minor.x = element_line(color = "white", size = 0.25, linetype = "dotted"),
         panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"),
         panel.border = element_blank(),
         legend.position="none")
 
 graphDISPO_quinz2
 
 ##########################################
 ##########################################
 # graphique TEMPS UNIQUEMENT quinzaine num 2
 graphTEMPS_quinz2 <-  ggplot(data = Res_graph_quinz2, aes(x = ordre)) +
   geom_line(mapping = aes(y = temps), size = 0.8, color = "blue") +
   geom_line(mapping = aes(y = warn), size = 0.5, color = "orange", linetype = "longdash") +
   geom_line(mapping = aes(y = crit), size = 0.5, color = "red", linetype = "longdash") +
   
   scale_x_continuous(name = "", breaks = seq(0, nb_pts_x_quinz2, 48),
                      labels =  c(vect_label_x_quinz2,vectplus[length(vectplus)])) +
   scale_y_continuous(name = "") + 
   
   theme_light() +
   theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
         axis.text.x = element_text(color="black", size=12, angle = 30, face="italic"),
         axis.title.y = element_text(color = "blue", size=10, face="italic"),
         axis.text.y = element_text(color="blue", size=12),
         panel.grid.minor.x = element_line(color = "white", size = 0.25, linetype = "dotted"),
         panel.grid.minor.y = element_line(color = "grey", size = 0.25, linetype = "dotted"),
         panel.border = element_blank(),
         legend.position="none") 
 
 graphTEMPS_quinz2

 
 ##################################################################
 ##################################################################
 ##################################################################
 
 # Ajout des zones rectangulaires au niveau des graphiques de DISPO
 # qui permet de representer les temps darrets recurrents
 if (nom_QoE %in% c("QoE-COLTRANE-Compte","QoE-COLTRANE-Enquete","QoE-COLTRANE-Enquete2",
                    "QoE-ESANE-Diffusion","QoE-ESANE-Diffusion_mineure","QoE-ESANE-Diffusion_mineure2",
                    "QoE-ESANE-Metropole","QoE-ESANE-Metropole_mineure","QoE-ESANE-Metropole_mineure2", 
                    "QoE-FIGARO-Catalogue",
                    "QoE-HARMONICA-Consultation","QoE-HARMONICA-Expertise",
                    "QoE-PILOT-Agents",
                    "QoE-REPONSE_Authentification",
                    "QoE-SIRUS-Externe-Recherche_par_Denom","QoE-SIRUS-Externe-Recherche_par_Ident",
                    "QoE-WS-Atis_BRPP")){
   indic_TArec <- "TArec_PRESENT"
 } else{
   indic_TArec <- "TArec_ABSENT"
 }
 
 
 fct_suiteope_prisecompte_TArec <- function(df){
   df <- df %>% filter(TArec=="Oui") %>%
     mutate(indic_lag=ordre-lag(ordre), indic_lead=lead(ordre)-ordre) %>%
     filter(ordre==min(ordre) | ordre==max(ordre) | indic_lag!=1 | indic_lead!=1) %>%
     select(-TArec,-indic_lag,-indic_lead) %>%
     mutate(lg=row_number()%%2)
   lg_impair <- df %>% filter(lg==1) %>%
     select(service_description,ordre) %>% rename(ordmin=ordre)
   lg_pair <- df %>% filter(lg==0) %>%
     select(ordre) %>% rename(ordmax=ordre)
   df <- cbind.data.frame(lg_impair,lg_pair) %>%
     cbind.data.frame(ymin=0,ymax=100,ordre=1,disponibilite=1)
   return(df)
 }
 
 
 fct_reco_periode_TArec <- function(df_quinz){
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,12)=="QoE-COLTRANE"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,heure,ordre) %>% 
       mutate(TArec=ifelse(heure %in% c("00:00","00:30","01:00","01:30",
                                        "02:00","02:30","03:00","03:30",
                                        "04:00","04:30"),"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
   }
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,19)=="QoE-ESANE-Diffusion"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,heure,ordre) %>% 
       mutate(TArec=ifelse((heure %in% c("20:00","20:30","21:00","21:30","22:00","22:30",
                                         "23:00","23:30") &
                              lib_jour %in% c("ven.")) |
                             (heure %in% c("00:00","00:30","01:00","01:30","02:00","02:30",
                                           "03:00","03:30","04:00","04:30","05:00","05:30",
                                           "06:00","06:30","07:00") &
                                lib_jour %in% c("lun.") |
                                lib_jour %in% c("sam.","dim.")),"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
     
   }
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,19)=="QoE-ESANE-Metropole"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,heure,ordre) %>% 
       mutate(TArec=ifelse((heure %in% c("20:00","20:30","21:00","21:30","22:00","22:30",
                                         "23:00","23:30") &
                              lib_jour %in% c("ven.")) |
                             (heure %in% c("00:00","00:30","01:00","01:30","02:00","02:30",
                                           "03:00","03:30","04:00","04:30","05:00","05:30",
                                           "06:00","06:30","07:00") &
                                lib_jour %in% c("lun.") |
                                lib_jour %in% c("sam.","dim.")),"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
     
   }
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,20)=="QoE-FIGARO-Catalogue"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,jour_bis,heure,ordre) %>% 
       mutate(TArec=ifelse(heure %in% c("23:00","23:30","00:00","00:30"),"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
   }
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,13)=="QoE-HARMONICA"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,jour_bis,heure,ordre) %>% 
       mutate(TArec=ifelse(heure %in% c("21:00","21:30","22:00","22:30","23:00","23:30",
                                        "00:00","00:30","01:00","01:30","02:00","02:30",
                                        "03:00","03:30") |
                             jour_bis<=6 ,"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
   }
   
   if (indic_TArec=="TArec_PRESENT" & str_sub(nom_QoE,1,9)=="QoE-SIRUS"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,heure,ordre) %>% 
       mutate(TArec=ifelse(heure %in% c("00:00","00:30","01:00","01:30","02:00","02:30",
                                        "03:00","03:30","04:00","04:30","05:00","05:30",
                                        "06:00","06:30")
                           ,"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
   }
   
   if (indic_TArec=="TArec_PRESENT" & nom_QoE=="QoE-WS-Atis_BRPP"){
     df_out <- df_quinz %>% 
       select(service_description,lib_jour,heure,ordre) %>% 
       mutate(TArec=ifelse(heure %in% c("01:30","02:00","02:30","03:00") ,"Oui","Non"))
     df_out <- fct_suiteope_prisecompte_TArec(df_out)
   }
   return(df_out)
 }
 
 
 fct_reco_periode_TAponc <- function(df_quinz){
   df_out <- df_quinz %>% filter(!is.na(Service)) %>% mutate(lg=row_number()%%2)
   lg_impair <- df_out %>% filter(lg==1) %>% select(service_description,ordre) %>% rename(ordmin=ordre)
   lg_pair <- df_out %>% filter(lg==0) %>% select(ordre) %>% rename(ordmax=ordre)
   df_out <- cbind.data.frame(lg_impair,lg_pair) %>% cbind.data.frame(ymin=0,ymax=100,ordre=1,disponibilite=1)
   return(df_out)
 }
 
 
 
 #########
 # quinz 1
 if (indic_TArec=="TArec_PRESENT"){ 
   TArec <- fct_reco_periode_TArec(Res_graph_quinz1)
 }
 if (nrow(Exclusion_Serv_quinz1)!=0){
   TAponc <- fct_reco_periode_TAponc(Res_graph_quinz1)
 }
 
 if (indic_TArec=="TArec_PRESENT" & nrow(Exclusion_Serv_quinz1)!=0) {
   graphDISPO_quinz1 <- graphDISPO_quinz1 +
     geom_rect(data = quinz1_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TArec, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_recurrent"),alpha=0.5) +
     geom_rect(data = TAponc, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_ponctuel"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_recurrent"="PaleGreen",
                                  "TA_ponctuel"="SpringGreen"))
 } else if (indic_TArec=="TArec_ABSENT" & nrow(Exclusion_Serv_quinz1)!=0) {
   graphDISPO_quinz1 <- graphDISPO_quinz1 +
     geom_rect(data = quinz1_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TAponc, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_ponctuel"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_ponctuel"="SpringGreen"))
 } else if (indic_TArec=="TArec_PRESENT" & nrow(Exclusion_Serv_quinz1)==0) {
   graphDISPO_quinz1 <- graphDISPO_quinz1 +
     geom_rect(data = quinz1_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TArec, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_recurrent"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_recurrent"="PaleGreen"))
 } else {
   graphDISPO_quinz1 <- graphDISPO_quinz1 +
     geom_rect(data = quinz1_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender")) 
 }
 graphDISPO_quinz1
 
 
 # correction à la mano' en cas de bugs si nbre impair
 # Res_graph_quinz2 <- Res_graph_quinz2 %>% 
 #   mutate(Service=ifelse(date %in% c("mar. 16 18:00","jeu. 18 00:30"),"QoE-RP-Questionnaire",NA),
 #          commentaire=ifelse(date %in% c("mar. 16 18:00","jeu. 18 00:30"),"Faux positif.",NA),
 #          quinzaine=ifelse(date %in% c("mar. 16 18:00","jeu. 18 00:30"),"quinz2",NA))
 
 
 #########
 # quinz 2
 if (indic_TArec=="TArec_PRESENT"){
   TArec <- fct_reco_periode_TArec(Res_graph_quinz2)
 }
 if (nrow(Exclusion_Serv_quinz2)!=0){
   TAponc <- fct_reco_periode_TAponc(Res_graph_quinz2)
 }
 
 if (indic_TArec=="TArec_PRESENT" & nrow(Exclusion_Serv_quinz2)!=0) {
   graphDISPO_quinz2 <- graphDISPO_quinz2 +
     geom_rect(data = quinz2_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TArec, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_recurrent"),alpha=0.5) +
     geom_rect(data = TAponc, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_ponctuel"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_recurrent"="PaleGreen",
                                  "TA_ponctuel"="SpringGreen"))
 } else if (indic_TArec=="TArec_ABSENT" & nrow(Exclusion_Serv_quinz2)!=0) {
   graphDISPO_quinz2 <- graphDISPO_quinz2 +
     geom_rect(data = quinz2_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TAponc, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_ponctuel"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_ponctuel"="SpringGreen"))
 } else if (indic_TArec=="TArec_PRESENT" & nrow(Exclusion_Serv_quinz2)==0) {
   graphDISPO_quinz2 <- graphDISPO_quinz2 +
     geom_rect(data = quinz2_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     geom_rect(data = TArec, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="TA_recurrent"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender",
                                  "TA_recurrent"="PaleGreen"))
 } else {
   graphDISPO_quinz2 <- graphDISPO_quinz2 +
     geom_rect(data = quinz2_5J7, 
               mapping = aes(xmin=ordmin, xmax=ordmax,
                             ymin=ymin, ymax=ymax, fill="nuit_we"),alpha=0.5) +
     scale_fill_manual(values = c("nuit_we"="lavender")) 
 }
 graphDISPO_quinz2
 
 
 
 #######################
 # Export des graphiques
 # Creation dun repertoire avec la date du jour
 Doss <- paste0(chemin_racine,"/",SousRepertoire)
 if (file.exists(Doss)) {
   cat("Le dossier existe deja")
 } else {
   dir.create(Doss, showWarnings = TRUE, recursive = FALSE, mode = "0777")
 }
 setwd(dir = Doss)
 
 ggsave(paste0(nom_QoE,"_graphDISPO_quinz1.png"),plot = graphDISPO_quinz1,
        width = 430, height = 100, unit="mm", dpi="screen")
 
 ggsave(paste0(nom_QoE,"_graphTEMPS_quinz1.png"),plot = graphTEMPS_quinz1,
        width = 430, height = 48, unit="mm", dpi="screen")
 
 ggsave(paste0(nom_QoE,"_graphDISPO_quinz2.png"),plot = graphDISPO_quinz2,
        width = 430, height = 100, unit="mm", dpi="screen")
 
 ggsave(paste0(nom_QoE,"_graphTEMPS_quinz2.png"),plot = graphTEMPS_quinz2,
        width = 430, height = 48, unit="mm", dpi="screen")
 
 
 ExtraitData <- Result %>% filter(service_description==nom_QoE) %>%
   mutate(temps=round(as.numeric(temps),3)) %>% 
   select(alias,service_description,moment,warn,crit,temps,etape,disponibilite)
 write.csv2(ExtraitData,paste0(Doss,"/",nom_QoE,"_ExtraitData.csv"),row.names = FALSE)
 
 ExtraitData_Graph <- Result_agreg %>% filter(service_description==nom_QoE) %>% 
   select(alias,service_description,annee,mois_bis,jour_bis,date,heure,
          temps,etape,disponibilite) %>% 
   rename(mois=mois_bis,jour=jour_bis)
 write.csv2(ExtraitData_Graph,paste0(Doss,"/",nom_QoE,"_ExtraitDataGraph.csv"),row.names = FALSE)
 
 setwd(dir = chemin_racine)
 
 
 # brouillon

