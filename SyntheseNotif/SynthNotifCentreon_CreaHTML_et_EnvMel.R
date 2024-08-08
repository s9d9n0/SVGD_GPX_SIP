

# encodage des 3 images du mel
# library(base64enc)
# doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/code/"
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
doss <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/code/"

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


head <- paste0("<head>",
                    "<meta charset='utf-8'/>",
                    "<title>ENVOI VOLUMES ALERTES</title>",
                    "<style>",
                        ".tabletitre {border:         none;
                                      text-align:     left;
                                      vertical-align: top;}",
               
                        "tr, td {border: 1px solid black;
                                 padding:         5px;
                                 border-collapse: collapse;
                                 text-align:      center;}",
               
                        ".piedtable {border:      none;
                                     padding-top: 3px;
                                     text-align:  left;}",
               
                        "sup {font-size: 15px;}",
               
                        ".caseRed {color:            rgb(200,0,0);
                                   font-weight:      bold;
                                   background-color: rgb(255,228,196);}",
               
                        ".ligneTot {font-weight:      bold;
                                    background-color: rgb(239,228,176);}",
               
                        ".ligneTot_caseRed {color:            rgb(200,0,0);
                                            font-weight:      bold;
                                            background-color: rgb(239,228,176);}",
                    "</style>",
               "</head>")


titre1 <- paste0("<p style='font-size:20px, line-height:30%'><strong>The GARGAMEL Project</strong><br>
                  <strong>G</strong>&eacute;n&eacute;ration <strong>A</strong>utomatique de <strong>R</strong>etours
                  du <strong>G</strong>epex <strong>A</strong>ccessibles par <strong>MEL</strong>
                  </p>")
titre2 <- paste0("<p style='font-size:20px'>-- Volume de notifications envoy&eacute;es --</p>")


cheminImage <- "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_SyntheseNotifCentreon\\"

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
                        "<td class='ligneTot'>date</td>",
                        "<td class='ligneTot'>zone</td>",
                        "<td class='ligneTot'>hote</td>",
                        "<td class='ligneTot'>service</td>",
                        "<td class='ligneTot'>statut</td>",
                        "<td class='ligneTot'>commentaire</td>",
                        "<td class='ligneTot'>liste groupe(s) notifi&eacute;(s)</td>",
                    "</tr>",
                 "</thead>")

pied <- paste0 ("<tfoot>",
                    "<tr>
                        <td class='piedtable' colspan=6>",imageVillage,"</td>
                    </tr>",
                "</tfoot>")

#####

corps_tab_mel_serv <- ""
for (i in 1:(nrow(Req_00j_notif_mel_serv)+1)){
    if (i==1){corps_tab_mel_serv <- paste0("<body>",corps_tab_mel_serv)}
    for (j in 1:7){
        if (j==1){corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"<tr>")}
        if (i==nrow(Req_00j_notif_mel_serv)+1){
            
            if (j==1){
                corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"<td class='ligneTot' colspan=2>","Ensemble","</td>")
            } else {
                if (j==2 | j==4 | j==5 | j==6 | j==7){
                    corps_tab_mel_serv <- corps_tab_mel_serv
                } else {
                    if (j==3){
                        corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"<td class='ligneTot'  colspan=5>",nrow(Req_00j_notif_mel_serv),"</td>")
                    }
                }
            }
            
        } else {
            corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"<td>",Req_00j_notif_mel_serv[i,j],"</td>")
        }
        
        if (j==ncol(Req_00j_notif_mel_serv)){corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"</tr>")}
    }
    if (i==nrow(Req_00j_notif_mel_serv)){corps_tab_mel_serv <- paste0(corps_tab_mel_serv,"</body>")} 
}

#####

corps_tab_sms_serv <- ""
for (i in 1:(nrow(Req_00j_notif_sms_serv)+1)){
    if (i==1){corps_tab_sms_serv <- paste0("<body>",corps_tab_sms_serv)}
    for (j in 1:7){
        if (j==1){corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"<tr>")}
        if (i==nrow(Req_00j_notif_sms_serv)+1){
            
            if (j==1){
                corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"<td class='ligneTot' colspan=2>","Ensemble","</td>")
            } else {
                if (j==2 | j==4 | j==5 | j==6 | j==7){
                    corps_tab_sms_serv <- corps_tab_sms_serv
                } else {
                    if (j==3){
                        corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"<td class='ligneTot'  colspan=5>",nrow(Req_00j_notif_sms_serv),"</td>")
                    }
                }
            }
            
        } else {
            corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"<td>",Req_00j_notif_sms_serv[i,j],"</td>")
        }
        
        if (j==ncol(Req_00j_notif_sms_serv)){corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"</tr>")}
    }
    if (i==nrow(Req_00j_notif_sms_serv)){corps_tab_sms_serv <- paste0(corps_tab_sms_serv,"</body>")} 
}


#####

corps_tab_mel_hote <- ""
for (i in 1:(nrow(Req_00j_notif_mel_hote)+1)){
    if (i==1){corps_tab_mel_hote <- paste0("<body>",corps_tab_mel_hote)}
    for (j in 1:7){
        if (j==1){corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"<tr>")}
        if (i==nrow(Req_00j_notif_mel_hote)+1){
            
            if (j==1){
                corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"<td class='ligneTot' colspan=2>","Ensemble","</td>")
            } else {
                if (j==2 | j==4 | j==5 | j==6 | j==7){
                    corps_tab_mel_hote <- corps_tab_mel_hote
                } else {
                    if (j==3){
                        corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"<td class='ligneTot'  colspan=5>",nrow(Req_00j_notif_mel_hote),"</td>")
                    }
                }
            }
            
        } else {
            corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"<td>",Req_00j_notif_mel_hote[i,j],"</td>")
        }
        
        if (j==ncol(Req_00j_notif_mel_hote)){corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"</tr>")}
    }
    if (i==nrow(Req_00j_notif_mel_hote)){corps_tab_mel_hote <- paste0(corps_tab_mel_hote,"</body>")} 
}


#####


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
tab <- paste0("<!DOCTYPE html>",
              "<html>",
                    head,
                    "<body>",
              
                        # titre1,titre2,imageGargamel,
              
                        "<table class='tabletitre'>",
                            "<tr>",
                                "<td class='tabletitre'>", # titre1,
                                                          titre2,imageSchtroumpf,"</td>",
                                # "<td class='tabletitre'>",imageGargamel,"</td>",
                            "</tr>",
                        "</table>",
              
                "<p style='font-size:20px'>-- Notifications MEL sur Services --</p>",
                        "<table>",
                            entete,
                            corps_tab_mel_serv,
                        "</table>",
              
                "<p style='font-size:20px'>-- Notifications SMS sur Services --</p>",
                        "<table>",
                            entete,
                            corps_tab_sms_serv,
                        "</table>",
              
              "<p style='font-size:20px'>-- Notifications MEL sur Hotes --</p>",
                        "<table>",
                            entete,
                            corps_tab_mel_hote,
                            # pied,
                        "</table>",
              
                    "</body>",
              "</html>")


# reconstitution du corps du mel par reunion des differentes parties de codes HTML
# SANS LES IMAGES
tab_ss_img <- paste0("<!DOCTYPE html>",
              "<html>",
              head,
              "<body>",
              
              # titre1,titre2,imageGargamel,
              
              "<table class='tabletitre'>",
                "<tr>",
                    "<td class='tabletitre'>", # titre1,
                                              titre2,"</td>",
                "</tr>",
              "</table>",
              
              "<p style='font-size:20px'>-- Notifications MEL sur Services --</p>",
                  "<table>",
                      entete,
                      corps_tab_mel_serv,
                  "</table>",
              
              "<p style='font-size:20px'>-- Notifications SMS sur Services --</p>",
                  "<table>",
                      entete,
                      corps_tab_sms_serv,
                  "</table>",
              
              "<p style='font-size:20px'>-- Notifications MEL sur Hotes --</p>",
                  "<table>",
                      entete,
                      corps_tab_mel_hote,
                  "</table>",
              
              "</body>",
              "</html>")



######################################################################
######################################################################
######################################################################
######################################################################

folder <- paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/Fichiers_date_",format(Sys.time(),'%Y%m%d'))

# Creation dun fichier HTML dans le repertoire nouvellement cree par injection du contenu de la variable tab
# setwd(paste0("C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/Fichiers_date_",format(Sys.time(),'%Y%m%d')))
setwd(folder)

# if (file.exists(file)) {
#     cat("The file already exists")
# } else {
#     file.create(file, showWarnings = TRUE)
# }
file <- "CorpsMel_Notifications.html"
file.create(file, showWarnings = TRUE)
cat(tab,file="CorpsMel_Notifications.html",append=TRUE)


###

#second endroit generalise du fichier (niveau général)
fileGen <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/Centreon_SyntheseNotifCentreon/General"
if (file.exists(fileGen)) {
    cat("The folder already exists")
} else {
    dir.create(fileGen, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}
setwd(fileGen)

file.create("CorpsMel_Notifications.html", showWarnings = TRUE)
cat(tab_ss_img,file="CorpsMel_Notifications.html",append=TRUE)

# insertion dans un .zip
#zip("Acquittements_Listing.zip",paste0("CorpsMel_Notifications.html"))

#  Selection des fichiers a mettre en PJ du mel
file.copy(from=paste0(folder,"/CorpsMel_Notifications.html"),
          to=paste0(fileGen,"/Liste_Notifications.html"))

file.copy(from=paste0(folder,"/ResumNotif_24dernheures.ods"),
          to=paste0(fileGen,"/ResumNotif_24dernheures.ods"))


######################################################################
######################################################################
######################################################################
######################################################################

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Centreon_SyntheseNotifCentreon\\code\\EnvoiMail_ViaPowershell.ps1"')


# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM
