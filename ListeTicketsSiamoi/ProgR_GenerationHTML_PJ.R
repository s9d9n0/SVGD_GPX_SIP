

############################################################
# Programme de generation du fichier HTML et d'envoi par mel

# PARTIE GENERATION DES IMAGES EN ENCODAGE 64 : 5 images

# library(base64enc)
# setwd(paste0(chemin,"img/"))
#
# imgencode <- base64encode("Logo_Insee.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_Logo_Insee.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("house1-transparent.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_House1.csv"), row.names = FALSE)
#
# imgencode <- base64encode("house2-transparent.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_House2.csv"), row.names = FALSE)
#
# imgencode <- base64encode("NTM.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_NTM.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("TitreGeneral.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_TitreGeneral.csv"), row.names = FALSE)
#
# imgencode <- base64encode("Schtroumpf_a_lunettes_xs.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_Schtroumpf_a_lunettes.csv"), row.names = FALSE)
#
# imgencode <- base64encode("Schtroumpf_a_lunettes_xs2.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_Schtroumpf_a_lunettes_2.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("Schtroumpfs_qui_dansent_xs.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_Schtroumpfs_qui_dansent.csv"), row.names = FALSE)
# 
# imgencode <- base64encode("Ancienne Ancienne-Gare_xs.png")
# write.csv2(imgencode,paste0(chemin,"img/","Img_encode64_AncienneGare.csv"), row.names = FALSE)
#
# setwd(chemin)
 
## 

# recuperation des images encodees dans des variables

img64_Logo_Insee <- read.csv2(paste0(chemin,"img/","Img_encode64_Logo_Insee.csv"))
img64_Logo_Insee <- img64_Logo_Insee$x[1]

img64_House <- read.csv2(paste0(chemin,"img/","Img_encode64_House1.csv"))
img64_House <- img64_House$x[1]

img64_House2 <- read.csv2(paste0(chemin,"img/","Img_encode64_House2.csv"))
img64_House2 <- img64_House2$x[1]

# img64_NTM <- read.csv2(paste0(chemin,"img/","Img_encode64_NTM.csv"))
# img64_NTM <- img64_NTM$x[1]
img64_NTM <- read.csv2(paste0(chemin,"img/","Img_encode64_TitreGeneral.csv"))
img64_NTM <- img64_NTM$x[1]

img64_Schtroumpf_a_lunettes <- read.csv2(paste0(chemin,"img/","Img_encode64_Schtroumpf_a_lunettes_2.csv"))
img64_Schtroumpf_a_lunettes <- img64_Schtroumpf_a_lunettes$x[1]

img64_Schtroumpfs_qui_dansent <- read.csv2(paste0(chemin,"img/","Img_encode64_Schtroumpfs_qui_dansent.csv"))
img64_Schtroumpfs_qui_dansent <- img64_Schtroumpfs_qui_dansent$x[1]

img64_AncienneGare <- read.csv2(paste0(chemin,"img/","Img_encode64_AncienneGare.csv"))
img64_AncienneGare <- img64_AncienneGare$x[1]


# PARTIE : CREATION DU HTML

entete_tickOUVR <- paste0("<thead>",
                            "<tr>",
                              "<td class='ligneTot' style='width:100px;'>jour_OUVR</td>",
                              "<td class='ligneTot' style='width:50px;'></td>",
                              "<td class='ligneTot'>r&eacute;f&eacute;rences ticket</td>",
                            "</tr>",
                          "</thead>")

table_tickOUVR <- ""
if (nrow(ListTickOUVR_2)>0){
    for (i in 1:nrow(ListTickOUVR_2)){
        for (j in 1:3){
            if (j==1){table_tickOUVR <- paste0(table_tickOUVR,"<tr>")}
            table_tickOUVR <- paste0(table_tickOUVR,"<td>",ListTickOUVR_2[i,j],"</td>")
            if (j==3){table_tickOUVR <- paste0(table_tickOUVR,"</tr>")}
        }
    }
} else {
    table_tickOUVR <- paste0(table_tickOUVR,"<tr><td colspan=3>absence de tickets ouverts</td></tr>")
}

##

entete_tickFERM <- paste0("<thead>",
                            "<tr>",
                              "<td class='ligneTot'>jour_OUVR</td>",
                              "<td class='ligneTot'></td>",
                              "<td class='ligneTot'>jour_FERM</td>",
                              "<td class='ligneTot'></td>",
                              "<td class='ligneTot'>dur&eacute;e de vie <br /> du ticket</td>",
                              "<td class='ligneTot'>r&eacute;f&eacute;rences ticket</td>",
                            "</tr>",
                          "</thead>")

table_tickFERM <- ""
if (nrow(ListTickFERM_2)>0){
    for (i in 1:nrow(ListTickFERM_2)){
        for (j in 1:6){
            if (j==1){table_tickFERM <- paste0(table_tickFERM,"<tr>")}
            table_tickFERM <- paste0(table_tickFERM,"<td>",ListTickFERM_2[i,j],"</td>")
            if (j==6){table_tickFERM <- paste0(table_tickFERM,"</tr>")}
        }
    }
} else {
    table_tickFERM <- paste0(table_tickFERM,"<tr><td colspan=5>absence de tickets ferm&eacute;s</td></tr>")
}


scriptJS <- "<script>
                const niv2 = document.querySelectorAll('details.niv2');
                console.log(niv2);
                niv2.forEach((item)=>{
                  item.setAttribute('open','');
                })
            </script>"



tab <- paste0(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
        "<meta charset='utf-8'/>",
        "<meta http-equiv='refresh' content='3600'/>",
        "<title>Listing Tickets GEPEX</title>",
        "<style>",
            " body {margin:  20px;
                    padding: 0px;
                    box-sizing:border-box}",
    
            " table {width:  100%;}",
    
            ".tabletitre {border:         none;
                          vertical-align: top;}",
    
            " h1 {text-align: center;}",
    
            ".SNSSIM {text-align: right;
                      font-style: italic;
                      font-size:  1.5rem;}",
    
            ".date, .eff, .infotri {text-align: right;}",
    
            ".titleHouse {position:relative;
                          width: 100%;
                          #justify-content: space-between;
                          }",
     
            "img.house2 {left: 0px;
                         top: 0px;
                         transform: scale(0.8);}",
    
            "img.titre {position: absolute;
                        left: 150px;
                        top: 30px;
                        z-index: 2;
                        width: 80%;
                        height:60%;
                        transition: 0.5s ease-in-out;}",
    
            "img.titre:hover {transform: scale(1.2) translateY(-30px);}",
    
            "img.house {position: absolute;
                        right: 0px;
                        bottom: 30px;
                        transform: scale(0.8);}",
    
            "tr, td {border:         0px solid black;
                    padding:         5px;
                    border-collapse: collapse;
                    text-align:      left;}",
    
            ".piedtable {border:      none;
                         padding-top: 3px;
                         text-align:  left;}",
    
            "sup {font-size: 15px;}",
    
            ".caseRed {color:            rgb(200,0,0);
                       font-weight:      bold;
                       background-color: rgb(255,228,196);}",
    
            ".caseGray {background-color: Gainsboro;}",
    
            ".ligneTot {font-weight:      bold;
                        background-color: rgb(239,228,176);}",
      
     
            "details summary { list-style: none; }",
    

    
            "details.nivOuvert table {animation: fadeNiv1 1s ease-in-out;}",
    
            "details.nivFerme table {animation: fadeNiv1 1s ease-in-out;}",
    
            "@keyframes fadeNiv1 {
                0%  {opacity:0;
                     transform: translateY(-100px);}
                50%  {opacity:0.5;
                      transform: translateY(20px);}
                100% {opacity:1;}
              };",


            "details.niv2 {margin: 1.3rem 0;
                           border-bottom: 1px solid #aaa;
                           padding: 0.5rem;
                           height: auto;
                           max-height: 1.5rem; /* set to line height */
                           max-width : 80%;
                           #transition: all 1s ease;
                           }
    
             details.niv2[open] {max-height: 99rem;
                                 border: 1px solid #aaa;
                                 #transition: all 1s ease;
                                 }
    
             details.niv2 summary {font-weight: bold;
                                   #cursor: pointer;
                                   margin-bottom: 0.5em;
                                   margin: -0.5em -0.5em 0;
                                   padding: 0.5em;
                                  }
              
             details.niv2 div {display:none;}  
             
             details.niv2:hover div {display:block;} 
             
             details.niv2 div {background:linear-gradient(135deg, rgba(255,255,255,0) 70%, rgba(195,0,0,1) 95% );
                               animation: fade 0.8S ease-in-out;}
                                  
             details.niv2[open] summary {border-bottom: 1px solid #aaa;
                                         margin-bottom: 0.8em;
                                        }",
    
            "@keyframes fade {
              0%  {opacity: 0.25;
                   background:linear-gradient(0deg, rgba(255,255,255,0) 10%, rgba(195,0,0,1) 95% );
                   transform-origin: center top;
                   transform: scale(1.5) translateY(-30px) rotate(-10deg) translateX(100px);
                   }
              25% {opacity: 0.5;
                   background:linear-gradient(20deg, rgba(255,255,255,0) 20%, rgba(195,0,0,1) 95% );
                   }
              50% {opacity: 0.75;
                   transform: translateY(-15px) rotate(5deg) translateX(-20px);
                   background:linear-gradient(45deg, rgba(255,255,255,0) 30%, rgba(195,0,0,1) 95% );
                   }
              75% {opacity: 1;
                   transform: translateY(5px) rotate(-2deg) translateX(7px);
                   background:linear-gradient(60deg, rgba(255,255,255,0) 40%, rgba(195,0,0,1) 95% );
                   }
              100% {opacity: 1;
                    background:linear-gradient(90deg, rgba(255,255,255,0) 50%, rgba(195,0,0,1) 95% );
                    }     
             };",
  
    
            #"details.[open] summary ~ * {animation: sweepon 2s ease-in-out;}",
    
            # "@keyframes sweepon {
            #     0%     {opacity: 0;
            #             /*transform: rotate(-30deg) scale(3);*/
            #             margin-left: -100px; margin-top: -100px;}
            #     100%   {opacity: 1;
            #             margin-left: 0px; margin-top: 0px;}
            #  }",
        "</style>",
    "</head>",
    "<body>",
    
        # "<table class='tabletitre'>",
        #     "<tr>",
        #         "<td >", #class='tabletitre'
                    # "<p><img src='C:\\Users\\R_ycg8l6\\ProgrammesR_Gepex\\Liste_TicketsSiamoi_Gepex\\Logo_Insee.png'
                    #          alt='Logo Insee'/></p>",

        #             "<p>",
        #                "<img src='data:image/png;base64,",img64_Logo_Insee,"'
        #                      alt='Logo Insee'/>",
        #             "</p>",
        #         "</td>",
        #         "<td >", #class='tabletitre'
        #             "<h1>Direction r&eacute;gionale Grand Est</h1>",
        #             "<p class='SNSSIM'><strong>GEPEX</strong> - <strong>S</strong>ervice <strong>N</strong>ational des <strong>S</strong>upports et <strong>S</strong>ervices <strong>I</strong>nformatiques de <strong>Metz</strong></h3>",
        #         "</td>",
        #     "</tr>",
        # "</table>",
    
        # "<hr>",
    
        # "<p><img class='titre' src='C:\\Users\\R_ycg8l6\\ProgrammesR_Gepex\\Liste_TicketsSiamoi_Gepex\\NTM.png'
        #      alt='Titre document'/></p>",

        "<div class='titleHouse'>",
          "<img class='house2' src='data:image/png;base64,",img64_House2,"'
                               alt='House2'/>",
          "<img class='titre' src='data:image/png;base64,",img64_NTM,"'
                              alt='Titre document'/>",
          "<img class='house' src='data:image/png;base64,",img64_House,"'
                              alt='House'/>",
        "</div>",
    
        "<hr>",
    
        "<table class='tabletitre'>",
            "<tr>",
                "<td class='tabletitre'>",
                    "<i>Diffusion restreinte</i>",
                "</td>",
                "<td class='tabletitre date'>",
                    "<p class='date'>&Agrave; Metz, le ",date_pres,"</p>",
                "</td>",
            "</tr>",
        "</table>",
    
    # PARTIE 1 : LISTE DES TICKETS OUVERTS
    
    # "<h2><img src='C:\\Users\\R_ycg8l6\\ProgrammesR_Gepex\\Liste_TicketsSiamoi_Gepex\\Schtroumpf_a_lunettes_xs.png'
    #          alt='Schtroumf a lunette'/>
    #     <span style='color: red'>Liste des tickets OUVERTS en supervision</span></h2>",
    
    "<details class='nivOuvert'>
        <summary>",
    "<h2><img src='data:image/png;base64,",img64_Schtroumpf_a_lunettes,"'
             alt='Schtroumf a lunette'/>
        <span style='color: red'>Liste des tickets OUVERTS en supervision</span></h2>",
    "<h2 class='eff'>Effectif : <span style='color: red'>",nrow(ListTickOUVR)," tickets</span></h2>",   
    "   </summary>",
    "<p class='infotri'>liste tri&eacute;e par ordre chronologique d&eacute;croissant, ",
    "de l'incident d&eacute;clar&eacute;e le plus r&eacute;cent au plus ancien</p>",   
    
    "<table>",
        entete_tickOUVR, table_tickOUVR,
    "</table>",
    "<p>source : Base de donn&eacute;es `Si@moi`</p>",
    "</details>",
    
    "<br/><br/>",
    
    # PARTIE 2 : LISTE DES TICKETS FERMES DEPUIS MOINS DE 7 JOURS
    
    # "<h2><img src='C:\\Users\\R_ycg8l6\\ProgrammesR_Gepex\\Liste_TicketsSiamoi_Gepex\\Schtroumpfs_qui_dansent_xs.png'
    #          alt='Schtroumfs qui dansent'/>
    #     <span style='color: blue'>Liste des tickets r&eacute;cemments FERMES en supervision</span></h2>",
    
    "<details class='nivFerme'>
        <summary>",
    "<h2><img src='data:image/png;base64,",img64_Schtroumpfs_qui_dansent,"'
             alt='Schtroumfs qui dansent'/>
        <span style='color: blue'>Liste des tickets r&eacute;cemment FERMES en supervision</span></h2>",
    "<h2 class='eff'>Effectif : <span style='color: blue'>",nrow(ListTickFERM)," tickets</span></h2>",
    "   </summary>",
    "<p class='infotri'>liste tri&eacute;e par ordre chronologique d&eacute;croissant ",
    "de fermeture de l'incident, du plus r&eacute;cent au plus ancien</p>",
    
    "<table>",
        entete_tickFERM, table_tickFERM,
    "</table>",
    "<p>source : Base de donn&eacute;es `Si@moi`</p>",
    "</details>",
    
    "<br>",
    "<p>&Agrave; noter : cette note de retranscription permet de visualiser d'une part l'int&eacute;gralit&eacute; des tickets actuellement
        en cours et de reprendre d'autre part les tickets r&eacute;solus et/ou clos sur ces 4 derni&egrave;res semaines.<br>",
    "Le p&eacute;rim&egrave;tre de supervision pris en compte et les pr&eacute;c&eacute;dentes notes de ces derniers mois sont disponibles ",
    "dans l'intranet du Gepex dans la rubrique \"Documents > -01- Notes de supervision\"
        (lien <a href='https://intranet.insee.fr/jcms/c_2265288/-espace-metier-dsi-gepex-supervision-si-insee'>ici</a>)</p>",
    
    # "<p><img src='C:\\Users\\R_ycg8l6\\ProgrammesR_Gepex\\Liste_TicketsSiamoi_Gepex\\Ancienne Ancienne-Gare_xs.png'
    #          alt='Ancienne-Gare'/><br>",
    # "l'ancienne Ancienne-Gare !</p>",

    # "<p><img src='data:image/png;base64,",img64_AncienneGare,"'
    #          alt='Ancienne-Gare'/><br>",
    # "l'ancienne Ancienne-Gare !</p>",
        
      scriptJS,
    
    "</body>",
    "</html>"
)

outputHTML <- paste0("Notes_Tickets_Monitoring_",date,".html")
file.create(outputHTML, showWarnings = TRUE)
cat(tab,file=outputHTML,append=TRUE)



#envoi sur le site peps-si...
file.copy(from = paste0(chemin,outputHTML),
          to = paste0("C:/inetpub/wwwroot/peps-si/A_MonitoringGepex/",outputHTML),
          overwrite = TRUE)
file.rename(from = paste0("C:/inetpub/wwwroot/peps-si/A_MonitoringGepex/",outputHTML),
            to = paste0("C:/inetpub/wwwroot/peps-si/A_MonitoringGepex/","listing.html"))



# insertion dans un .zip
zip(paste0("Retranscription_Monitoring_",date,".zip"),outputHTML)


###

#second endroit generalise du fichier (niveau general)

file.copy(from=paste0(chemin,"Retranscription_Monitoring_",date,".zip"),
          to=paste0(cheminDuJourGen,"/","Retranscription_Monitoring.zip"))

# TESTS
# cheminIN <- "C:/Users/R_ycg8l6/ProgrammesR_Gepex/Liste_TicketsSiamoi_Gepex/"
# cheminOUT <- "C:/inetpub/wwwroot/peps-si/"
# 
# file.copy(from=paste0(cheminIN,"index.html"),
#           to=paste0(cheminOUT,"index.html"))


######################################################################
######################################################################
######################################################################
######################################################################

# Creation dun repertoire avec la date du jour
# plus besoin ici a present, le zip est dans folder...
# folder <- paste0("C:/Users/R_ycg8l6/ProgrammesR_Gepex/Liste_TicketsSiamoi_Gepex/Fichiers_date_",date)
# setwd(folder)

######################################################################
######################################################################
######################################################################
######################################################################

# PARTIE : ENVOI DU MEL

# envoi finalement avec Powershell
system('powershell -file "C:\\Users\\SIAR_ycg8l6\\Docs\\ProgrammesR\\Liste_TicketsSiamoi_Gepex\\EnvoiMail_ViaPowershell.ps1"')

# puis suppression du fichier .zip envoye...
file.remove(paste0(cheminDuJourGen,"/","Retranscription_Monitoring.zip"))



# Package MailR ne marche plus depuis passage de NTMLv1 vers NTLM

# library(mailR) ne fonctionne pas sans lib.loc
#library(mailR, lib.loc="C:/Users/R_ycg8l6/ProgrammesR_Gepex/X_PackagesMail")

#locmdp <- "E:/X_PackagesMailR/"
#mdp_cpt <- read.table(paste0(locmdp,"ident_tls.txt")) %>% as.character()

#  Envoi du mel final en SMTP authentifie
# send.mail(from = "Centreon-SIP@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("Resume sur les tickets du Gepex au",format(Sys.time(),'%A %d %B %Y')),
#           body = "Note r&eacute;capitulative des tickets du Gepex",
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 587, 
#                       user.name = "PD0-SUPERVISION-SVC@ad.insee.intra", 
#                       passwd = mdp_cpt ),
#           authenticate = TRUE,
#           send = TRUE,
#           attach.files = c(paste0(chemin,"/","Retranscription_Monitoring_",date,".zip"))
# )



# Envoi du mel final
# send.mail(from = "antonio.sedeno@insee.fr",
#           to = c("antonio.sedeno@insee.fr"),
#           subject = paste("Résumé sur les tickets du Gepex au",format(Sys.time(),'%A %d %B %Y')),
#           body = "Note r&eacute;capitulative des tickets du Gepex",
#           html = TRUE,
#           inline = TRUE,
#           smtp = list(host.name = "smtp.appli.insee.fr", port = 25),
#           authenticate = FALSE,
#           send = TRUE,
#           attach.files = c(paste0(chemin,"/","Retranscription_Monitoring_",date,".zip"))
# )


