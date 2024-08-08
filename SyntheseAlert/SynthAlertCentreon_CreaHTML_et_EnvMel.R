
# encodage des 3 images du mel
# library(base64enc)
# doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon/code/"
# 
# Schtroumpf_encode <- base64encode(paste0(doss,"Schtroumpf_xxs.png"))
# write.csv2(Schtroumpf_encode,
#            paste0(doss, "Img_encode64_Schtroumpf.csv"),
#            row.names = FALSE)
# 
# Gargamel_encode <- base64encode(paste0(doss,"Gargamel.png"))
# write.csv2(Gargamel_encode,
#            paste0(doss, "Img_encode64_Gargamel.csv"),
#            row.names = FALSE)
# 
# Schtroumpf_village_perdu_avecMessageCEI_compress_encode <- base64encode(paste0(doss,"Schtroumpf_village_perdu_avecMessageCEI_compress.JPG"))
# write.csv2(Schtroumpf_village_perdu_avecMessageCEI_compress_encode,
#            paste0(doss, "Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"),
#            row.names = FALSE)

##

# recuperation des images encodees dans des variables
doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon_New/code/"

img64_Schtroumpf <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf.csv"))
img64_Schtroumpf <- img64_Schtroumpf$x[1]

img64_Gargamel <- read.csv2(paste0(doss,"Img_encode64_Gargamel.csv"))
img64_Gargamel <- img64_Gargamel$x[1]

img64_Schtroumpf_village_perdu <- read.csv2(paste0(doss,"Img_encode64_Schtroumpf_village_perdu_avecMessageCEI_compress.csv"))
img64_Schtroumpf_village_perdu <- img64_Schtroumpf_village_perdu$x[1]



######################################################################
######################################################################
######################################################################
######################################################################
## Envoi automatique des resultats en HTML via un mel Outlook ----

# En introduction, quelques modifications sur la table contenant les donnees a afficher

Req_par_demiheure_save <- Req_par_demiheure
# Req_par_demiheure <- Req_par_demiheure_save

Req_par_demiheure <- Req_par_demiheure %>% 
    mutate(demiheure=ifelse(str_sub(demiheure,1,1)=="0",
                            paste0(" ",str_sub(demiheure,2,str_length(demiheure))),
                            demiheure)) %>% 
    mutate(demiheure=ifelse(calendrier!="Ensemble",
                            paste0(str_sub(demiheure,1,2),"<sup><i>h",str_sub(demiheure,4,5),"</i></sup>"),
                            demiheure))

Req_par_demiheure <- Req_par_demiheure %>%
    mutate(cal_1=paste0(str_sub(calendrier,9,10),"-",str_sub(calendrier,6,7))) %>% 
    mutate(cal_2=case_when(
              str_sub(cal_1,4,5)=="01" ~ "janv.", str_sub(cal_1,4,5)=="02" ~ "fevr.",
              str_sub(cal_1,4,5)=="03" ~ "mars",  str_sub(cal_1,4,5)=="04" ~ "avr.",
              str_sub(cal_1,4,5)=="05" ~ "mai",   str_sub(cal_1,4,5)=="06" ~ "juin",
              str_sub(cal_1,4,5)=="07" ~ "juil.", str_sub(cal_1,4,5)=="08" ~ "aout",
              str_sub(cal_1,4,5)=="09" ~ "sept.", str_sub(cal_1,4,5)=="10" ~ "oct.",
              str_sub(cal_1,4,5)=="11" ~ "nov.",  str_sub(cal_1,4,5)=="12" ~ "dec."))

Req_par_demiheure <- Req_par_demiheure %>% 
    mutate(calendrier=ifelse(calendrier!="Ensemble",
                             paste0(str_sub(cal_1,1,2)," ",cal_2),
                             calendrier)) %>% select(-cal_1:-cal_2)


# creation indicatrice de coloration des cases ou une evolution sensible entre J0 et J1 est detectee
Req_par_demiheure <- Req_par_demiheure %>%
    mutate(nb_notif_00j_Osny_Inco_num=as.integer(nb_notif_00j_Osny_Inco),
           nb_notif_01j_Osny_Inco_num=as.integer(nb_notif_01j_Osny_Inco)) %>% 
    mutate(red_Osny_Inco=ifelse((is.na(nb_notif_01j_Osny_Inco_num) & nb_notif_00j_Osny_Inco_num>10) |
                           (nb_notif_01j_Osny_Inco_num<100 & nb_notif_00j_Osny_Inco_num-nb_notif_01j_Osny_Inco_num>10) |
                           (nb_notif_01j_Osny_Inco_num>=100 & nb_notif_00j_Osny_Inco_num/nb_notif_01j_Osny_Inco_num>1.1),
                           "Oui","Non")) %>% 
    mutate(red_Osny_Inco=ifelse(is.na(red_Osny_Inco),"Non",red_Osny_Inco)) %>% 
    select(-nb_notif_00j_Osny_Inco_num,-nb_notif_01j_Osny_Inco_num) %>% 
  
    mutate(nb_notif_00j_Osny_Warn_num=as.integer(nb_notif_00j_Osny_Warn),
           nb_notif_01j_Osny_Warn_num=as.integer(nb_notif_01j_Osny_Warn)) %>% 
    mutate(red_Osny_Warn=ifelse((is.na(nb_notif_01j_Osny_Warn_num) & nb_notif_00j_Osny_Warn_num>10) |
                            (nb_notif_01j_Osny_Warn_num<100 & nb_notif_00j_Osny_Warn_num-nb_notif_01j_Osny_Warn_num>10) |
                            (nb_notif_01j_Osny_Warn_num>=100 & nb_notif_00j_Osny_Warn_num/nb_notif_01j_Osny_Warn_num>1.1),
                            "Oui","Non")) %>% 
    mutate(red_Osny_Warn=ifelse(is.na(red_Osny_Warn),"Non",red_Osny_Warn)) %>% 
    select(-nb_notif_00j_Osny_Warn_num,-nb_notif_01j_Osny_Warn_num) %>%
  
    mutate(nb_notif_00j_Osny_Crit_num=as.integer(nb_notif_00j_Osny_Crit),
           nb_notif_01j_Osny_Crit_num=as.integer(nb_notif_01j_Osny_Crit)) %>% 
    mutate(red_Osny_Crit=ifelse((is.na(nb_notif_01j_Osny_Crit_num) & nb_notif_00j_Osny_Crit_num>10) |
                            (nb_notif_01j_Osny_Crit_num<100 & nb_notif_00j_Osny_Crit_num-nb_notif_01j_Osny_Crit_num>10) |
                            (nb_notif_01j_Osny_Crit_num>=100 & nb_notif_00j_Osny_Crit_num/nb_notif_01j_Osny_Crit_num>1.1),
                            "Oui","Non")) %>% 
    mutate(red_Osny_Crit=ifelse(is.na(red_Osny_Crit),"Non",red_Osny_Crit)) %>% 
    select(-nb_notif_00j_Osny_Crit_num,-nb_notif_01j_Osny_Crit_num) %>%
  
  
    mutate(nb_notif_00j_Auzeville_Inco_num=as.integer(nb_notif_00j_Auzeville_Inco),
           nb_notif_01j_Auzeville_Inco_num=as.integer(nb_notif_01j_Auzeville_Inco)) %>% 
    mutate(red_Auzeville_Inco=ifelse((is.na(nb_notif_01j_Auzeville_Inco_num) & nb_notif_00j_Auzeville_Inco_num>10) |
                            (nb_notif_01j_Auzeville_Inco_num<100 & nb_notif_00j_Auzeville_Inco_num-nb_notif_01j_Auzeville_Inco_num>10) |
                            (nb_notif_01j_Auzeville_Inco_num>=100 & nb_notif_00j_Auzeville_Inco_num/nb_notif_01j_Auzeville_Inco_num>1.1),
                            "Oui","Non")) %>% 
    mutate(red_Auzeville_Inco=ifelse(is.na(red_Auzeville_Inco),"Non",red_Auzeville_Inco)) %>% 
    select(-nb_notif_00j_Auzeville_Inco_num,-nb_notif_01j_Auzeville_Inco_num) %>% 

    mutate(nb_notif_00j_Auzeville_Warn_num=as.integer(nb_notif_00j_Auzeville_Warn),
           nb_notif_01j_Auzeville_Warn_num=as.integer(nb_notif_01j_Auzeville_Warn)) %>% 
    mutate(red_Auzeville_Warn=ifelse((is.na(nb_notif_01j_Auzeville_Warn_num) & nb_notif_00j_Auzeville_Warn_num>10) |
                            (nb_notif_01j_Auzeville_Warn_num<100 & nb_notif_00j_Auzeville_Warn_num-nb_notif_01j_Auzeville_Warn_num>10) |
                            (nb_notif_01j_Auzeville_Warn_num>=100 & nb_notif_00j_Auzeville_Warn_num/nb_notif_01j_Auzeville_Warn_num>1.1),
                            "Oui","Non")) %>% 
    mutate(red_Auzeville_Warn=ifelse(is.na(red_Auzeville_Warn),"Non",red_Auzeville_Warn)) %>% 
    select(-nb_notif_00j_Auzeville_Warn_num,-nb_notif_01j_Auzeville_Warn_num) %>% 

    mutate(nb_notif_00j_Auzeville_Crit_num=as.integer(nb_notif_00j_Auzeville_Crit),
           nb_notif_01j_Auzeville_Crit_num=as.integer(nb_notif_01j_Auzeville_Crit)) %>% 
    mutate(red_Auzeville_Crit=ifelse((is.na(nb_notif_01j_Auzeville_Crit_num) & nb_notif_00j_Auzeville_Crit_num>10) |
                            (nb_notif_01j_Auzeville_Crit_num<100 & nb_notif_00j_Auzeville_Crit_num-nb_notif_01j_Auzeville_Crit_num>10) |
                            (nb_notif_01j_Auzeville_Crit_num>=100 & nb_notif_00j_Auzeville_Crit_num/nb_notif_01j_Auzeville_Crit_num>1.1),
                            "Oui","Non")) %>% 
    mutate(red_Auzeville_Crit=ifelse(is.na(red_Auzeville_Crit),"Non",red_Auzeville_Crit)) %>% 
    select(-nb_notif_00j_Auzeville_Crit_num,-nb_notif_01j_Auzeville_Crit_num)



Req_par_demiheure <- Req_par_demiheure %>%
    mutate(lag_calendrier=lag(calendrier),numligne=row_number()) %>% 
    mutate(lg=ifelse(calendrier!=lag_calendrier & calendrier!="Ensemble",numligne-1,0)) %>% 
    select(-lag_calendrier,-numligne)

# creation dune variable permettant de connaitre le numero de ligne avant le changement de jour
# puis suppression de la colonne dans le data.frame utilise
chgt_jour <- Req_par_demiheure %>% select(lg) %>% filter(lg!=0) %>% as.numeric()
Req_par_demiheure <- Req_par_demiheure %>% select(-lg)

# insertion colonnes vides
Req_par_demiheure <- Req_par_demiheure %>%
  mutate(col1="",col2="") %>% 
  relocate(col1,.after="demiheure") %>% 
  relocate(col2,.after="nb_notif_01j_Osny_Crit")
                        

######################################################################
######################################################################
######################################################################
######################################################################


head <- paste0("<head>",
      "<meta charset='utf-8'/>",
      "<title>VOL. ALERTES</title>",
      "<style>",
          "body {background-color: rgb(250,240,190);}",
               
          ".tabletitre {position:       relative;
                        border:         none;
                        text-align:     left;
                        vertical-align: top;
                        background-color: rgb(250,240,190);}",
                      
          ".tableau {position:relative;
                     left: 30px;
                     width:95%;
                     border-spacing: 2px 0px;}",
       
          ".FixHead { overflow: auto; height: 500px; }",
          ".FixHead thead > :first-child { position: sticky; top: 0px; z-index: 1; }",
          ".FixHead thead > :nth-child(2) { position: sticky; top: 30px; z-index: 1; }",
          ".FixHead thead > :nth-child(3) { position: sticky; top: 60px; z-index: 1; }",
          ".FixHead tbody > :nth-child(49) { position: sticky; bottom: 0px; z-index: 1; }",
      
          "tr, td {border:          1px solid black;
                   padding:         5px;
                   text-align:      center;
                   background-color: white;}",
               
          ".piedtable {border:      none;
                       padding-top: 3px;
                       text-align:  left;}",
               
          "sup {font-size: 15px;}",
               
          ".caseRed {color:            rgb(200,0,0);
                     font-weight:      bold;
                     background-color: rgb(255,228,196);}",
               
          ".caseOrange {color:            rgb(255,140,0);
                        font-weight:      bold;
                        background-color: rgb(255,229,180);}",
               
          ".caseGris {color:            rgb(115,115,115);
                      font-weight:      bold;
                      background-color: rgb(211,211,211);}",
               
               
          ".ligneTot {font-weight:      bold;
                      background-color: rgb(230,190,138);}", 
               
               
          ".ligneTot_caseRed {color:            rgb(200,0,0);
                              font-weight:      bold;
                              background-color: rgb(255,228,196);}",
               
          ".ligneTot_caseOrange {color:            rgb(255,140,0);
                                 font-weight:      bold;
                                 background-color: rgb(255,229,180);}",
               
          ".ligneTot_caseGris {color:            rgb(115,115,115);
                               font-weight:      bold;
                               background-color: rgb(211,211,211);}",
          "</style>",
      "</head>")


titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")

titre2 <- paste0("<p style='margin:0; 
                            font-size:30px; 
                            font-weight:bold;'>-- DELTA 10 --</p>")


cheminImage <- "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_SyntheseAlertCentreon_New\\"


# avec img encodees en base64
imageSchtroumpf <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf,"
                               alt='Photo de Schtroumpf'/></p>")

imageGargamel <- paste0("<p><img src='data:image/png;base64,",img64_Gargamel,"
                             alt='Photo de Gargamel'/></p>")

imageVillage <- paste0("<p><img src='data:image/png;base64,",img64_Schtroumpf_village_perdu,"
                            alt='Photo de Village'/></p>")


# imageSchtroumpf <- paste0("<p><img src='",cheminImage,"code\\Schtroumpf_xxs.png'
#                               alt='Photo de Schtroumpf'/></p>")
# 
# imageGargamel <- paste0("<p><img src='",cheminImage,"code\\Gargamel.png'
#                              alt='Photo de Gargamel'/></p>")
# 
# imageVillage <- paste0("<p><img src='",cheminImage,"code\\Schtroumpf_village_perdu_avecMessageCEI_compress.jpg'
#                            alt='Photo de Village'/></p>")

entete <- paste0("<thead>",
                    "<tr>",
        "<td class='ligneTot' colspan=2></td>",
        "<td style='border:0px; background-color:rgb(250,240,190);' rowspan=3></td>",
        "<td class='ligneTot' colspan=6>DC 1</td>",
        "<td style='border:0px; background-color:rgb(250,240,190);' rowspan=3></td>",
        "<td class='ligneTot' colspan=6>DC 2</td>",
                    "</tr>",
                    "<tr>",
        "<td class='ligneTot' rowspan=2>date r&eacute;f&eacute;rence</td>",
        "<td class='ligneTot' rowspan=2>tranche demiheure</td>", 
        
        "<td class='ligneTot_caseGris'   colspan=2>UNKNOWN</td>",
        "<td class='ligneTot_caseOrange' colspan=2>WARNING</td>",
        "<td class='ligneTot_caseRed'    colspan=2>CRITICAL</td>",
        
        "<td class='ligneTot_caseGris'   colspan=2>UNKNOWN</td>",
        "<td class='ligneTot_caseOrange' colspan=2>WARNING</td>",
        "<td class='ligneTot_caseRed'    colspan=2>CRITICAL</td>",
                    "</tr>",
                    "<tr>",
        "<td class='ligneTot_caseGris'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseGris'><i>24<br>heures pr&eacute;c.</i></td>",
        "<td class='ligneTot_caseOrange'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseOrange'><i>24<br>heures pr&eacute;c.</i></td>",
        "<td class='ligneTot_caseRed'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseRed'><i>24<br>heures pr&eacute;c.</i></td>",
        
        "<td class='ligneTot_caseGris'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseGris'><i>24<br>heures pr&eacute;c.</i></td>",
        "<td class='ligneTot_caseOrange'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseOrange'><i>24<br>heures pr&eacute;c.</i></td>",
        "<td class='ligneTot_caseRed'><i>24<br>dern. heures</i></td>",
        "<td class='ligneTot_caseRed'><i>24<br>heures pr&eacute;c.</i></td>",
                    "</tr>",
                 "</thead>")

pied <- paste0 ("<tfoot>",
                    "<tr>
                        <td class='piedtable' colspan=6>",imageVillage,"</td>
                    </tr>",
                 "</tfoot>")

gc()

corps_tab <- ""
for (i in 1:nrow(Req_par_demiheure)){
    if (i==1){corps_tab <- paste0("<tbody>",corps_tab)}
    #for (j in 1:ncol(Req_par_demiheure)){
    for (j in 1:16){
        if (j==1){corps_tab <- paste0(corps_tab,"<tr>")}
            if (i==nrow(Req_par_demiheure)){
                
                if (j==1){
                    corps_tab <- paste0(corps_tab,"<td class='ligneTot' colspan=2><h4>",Req_par_demiheure[i,j],"</h4></td>")
                } else {
                    if (j==2){
                        corps_tab <- corps_tab
                    } else {
                      if (( j==8 & Req_par_demiheure[i,19]=="Oui") |
                          (j==15 & Req_par_demiheure[i,22]=="Oui") ){
                        corps_tab <- paste0(corps_tab,"<td class='ligneTot_caseRed'><h4>",Req_par_demiheure[i,j],"</h4></td>")
                      } else if (( j==6 & Req_par_demiheure[i,18]=="Oui") |
                                 (j==13 & Req_par_demiheure[i,21]=="Oui") ) {
                        corps_tab <- paste0(corps_tab,"<td class='ligneTot_caseOrange'><h4>",Req_par_demiheure[i,j],"</h4></td>")
                      } else if (( j==4 & Req_par_demiheure[i,17]=="Oui") |
                                 (j==11 & Req_par_demiheure[i,20]=="Oui") ) {
                        corps_tab <- paste0(corps_tab,"<td class='ligneTot_caseGris'><h4>",Req_par_demiheure[i,j],"</h4></td>")
                      } else if (j==3 | j==10) {
                        corps_tab <- paste0(corps_tab,"<td style='border:0px; background-color:rgb(250,240,190);'></td>")
                      } else {
                        corps_tab <- paste0(corps_tab,"<td class='ligneTot'><h4>",Req_par_demiheure[i,j],"</h4></td>")
                      }
                    }
                }

            } else {
                
                # code de fusion des lignes dun meme jour
                if (i==1 & j==1){
                    corps_tab <- paste0(corps_tab,"<td style='vertical-align:top;' rowspan=",chgt_jour,">",
                                        Req_par_demiheure[i,j],"</td>")
                } else{
                    if (i==chgt_jour+1 & j==1){
                        corps_tab <- paste0(corps_tab,"<td style='vertical-align:top;' rowspan=",48-chgt_jour,">",
                                            Req_par_demiheure[i,j],"</td>")
                    } 
                }
                if (i==1 & j==3){
                  corps_tab <- paste0(corps_tab,"<td style='border:0px; background-color:rgb(250,240,190);' rowspan=",chgt_jour,">",Req_par_demiheure[i,j],"</td>")
                } else if (i==chgt_jour+1 & j==3) {
                      corps_tab <- paste0(corps_tab,"<td style='border:0px; background-color:rgb(250,240,190);' rowspan=",48-chgt_jour,">",Req_par_demiheure[i,j],"</td>")
                } else if (j==3) {
                      corps_tab <- corps_tab
                }
                if (i==1 & j==10){
                  corps_tab <- paste0(corps_tab,"<td style='border:0px; background-color:rgb(250,240,190);' rowspan=",chgt_jour,">",Req_par_demiheure[i,j],"</td>")
                } else if (i==chgt_jour+1 & j==10) {
                  corps_tab <- paste0(corps_tab,"<td style='border:0px; background-color:rgb(250,240,190);' rowspan=",48-chgt_jour,">",Req_par_demiheure[i,j],"</td>")
                } else if (j==10) {
                  corps_tab <- corps_tab
                }
          
                
                if (( j==8 & Req_par_demiheure[i,19]=="Oui") |
                    (j==15 & Req_par_demiheure[i,22]=="Oui") ){
                    corps_tab <- paste0(corps_tab,"<td class='caseRed'>",Req_par_demiheure[i,j],"</td>")
                } else if (( j==6 & Req_par_demiheure[i,18]=="Oui") |
                           (j==13 & Req_par_demiheure[i,21]=="Oui") ) {
                    corps_tab <- paste0(corps_tab,"<td class='caseOrange'>",Req_par_demiheure[i,j],"</td>")
                } else if (( j==4 & Req_par_demiheure[i,17]=="Oui") |
                           (j==11 & Req_par_demiheure[i,20]=="Oui") ) {
                    corps_tab <- paste0(corps_tab,"<td class='caseGris'>",Req_par_demiheure[i,j],"</td>")
                } else {
                    if (j!=1 & j!=3 & j!=10) {
                        corps_tab <- paste0(corps_tab,"<td>",Req_par_demiheure[i,j],"</td>")
                    }
                }
                
            }
        if (j==ncol(Req_par_demiheure)){corps_tab <- paste0(corps_tab,"</tr>")}
    }
    if (i==nrow(Req_par_demiheure)){corps_tab <- paste0(corps_tab,"</tbody>")} 
}

# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# tab <- paste0("<!DOCTYPE html>",
#               "<html>",
#                     head,
#                     "<body>",
#               
#                         # titre1,titre2,imageGargamel,
#               
#                         "<table class='tabletitre'>",
#                             "<tr>",
#                                 "<td class='tabletitre'>",#titre1,
#                                                           titre2,#imageSchtroumpf,
#                                 "</td>",
#                                 #"<td class='tabletitre'>",imageGargamel,"</td>",
#                             "</tr>",
#                         "</table>",
#               
#                         "<table class='tableau'>",
#                             entete,corps_tab,#pied,
#                         "</table>",
#                         
#                     "</body>",
#               "</html>")


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# SANS LES IMAGES
tab_ss_img <- paste0("<!DOCTYPE html>",
                     "<html>",
                        head,
                        "<body>",
              
                        # titre1,titre2,imageGargamel,
              
                            "<table class='tabletitre'>",
                                "<tr>",
                                    "<td class='tabletitre'>",#titre1,
                                                              titre2,"</td>",
                                "</tr>",
                            "</table>",
                     
                            "<div class='FixHead'>",
                                "<table class='tableau'>",
                                  entete,corps_tab,
                                "</table>",
                            "</div>",
              
                        "</body>",
                    "</html>")


######################################################################
######################################################################
######################################################################
######################################################################

# Creation dun fichier HTML dans le repertoire nouvellement cree par injection du contenu de la variable tab
# setwd(paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/Fichiers_date_",format(Sys.time(),'%Y%m%d')))
setwd(folder)

# if (file.exists(file)) {
#     cat("The file already exists")
# } else {
#     file.create(file, showWarnings = TRUE)
# }
file <- "Delta10_Alertes.html"
file.create(file, showWarnings = TRUE)
cat(tab_ss_img,file="Delta10_Alertes.html",append=TRUE)


###

#second endroit generalisee du fichier (niveau general)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseAlertCentreon_New/General"
if (file.exists(fileGen)) {
    cat("The folder already exists")
} else {
    dir.create(fileGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}
setwd(fileGen)

file.create("Delta10_Alertes.html", showWarnings = TRUE)
cat(tab_ss_img,file="Delta10_Alertes.html",append=TRUE)

# insertion dans un .zip
#zip("Acquittements_Listing.zip",paste0("CorpsMel_Notifications.html"))

#  Selection des fichiers a mettre en PJ du mel
# file.remove(paste0(fileGen,"/Delta10_Alertes.html"))
# file.copy(from=paste0(folder,"/Delta10_Alertes.html"),
#           to=paste0(fileGen,"/Delta10_Alertes.html"))

file.remove(paste0(fileGen,"/ResumAlert_24dernheures_Osny.csv"))
file.copy(from=paste0(folder,"/ResumAlert_24dernheures_Osny.csv"),
          to=paste0(fileGen,"/ResumAlert_24dernheures_Osny.csv"))

file.remove(paste0(fileGen,"/ResumAlert_24dernheures_Auzeville.csv"))
file.copy(from=paste0(folder,"/ResumAlert_24dernheures_Auzeville.csv"),
          to=paste0(fileGen,"/ResumAlert_24dernheures_Auzeville.csv"))

file.remove(paste0(fileGen,"/ListeAlert_24dernheures.csv"))
file.copy(from=paste0(folder,"/ListeAlert_24dernheures.csv"),
          to=paste0(fileGen,"/ListeAlert_24dernheures.csv"))

######################################################################
######################################################################
######################################################################
######################################################################

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_SyntheseAlertCentreon_New\\code\\EnvoiMail_ViaPowershell.ps1"')

# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM

# library(mailR) ne fonctionne pas sans lib.loc
#library(mailR, lib.loc="C:/Users/R_ycg8l6/ProgrammesR_Gepex/X_PackagesMail")


#locmdp <- "E:/X_PackagesMailR/"
#mdp_cpt <- read.table(paste0(locmdp,"ident_tls.txt")) %>% as.character()

#  Envoi du mel final en SMTP authentifie
# send.mail(from = "Centreon-SIP@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("Resume alertes Centreon du",format(Sys.time(),'%A %d %B %Y')),
#           body = paste0(folder,"/CorpsMel_Alertes.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 587, 
#                       user.name = "PD0-SUPERVISION-SVC@ad.insee.intra", 
#                       passwd = mdp_cpt ),
#           authenticate = TRUE,
#           send = TRUE,
#           attach.files = c(paste0(folder,"/ResumAlert_24dernheures.ods"))
# )


#  Envoi du mel final
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr","dg57-cei-suivi-de-l-exploitation@insee.fr"),
#           subject = paste("Resume alertes Centreon du",format(Sys.time(),'%A %d %B %Y')),
#           body = paste0(folder,"/CorpsMel_Alertes.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE,
#           attach.files = c(paste0(folder,"/ResumNotif_24dernheures.ods"),
#                            paste0(folder,"/ListeNotif_24dernheures.csv"))
# )

# test
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("Resume alertes Centreon du",format(Sys.time(),'%A %d %B %Y')),
#           body = paste0(folder,"/CorpsMel_Alertes.html"),
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE,
#           attach.files = c(paste0(folder,"/ResumAlert_24dernheures.ods"))
#           # attach.files = c(paste0(folder,"/ResumAlert_24dernheures.ods"),
#           #                  paste0(folder,"/ListeAlert_24dernheures.csv"))
# )




# TESTS TESTS TESTS TESTS






