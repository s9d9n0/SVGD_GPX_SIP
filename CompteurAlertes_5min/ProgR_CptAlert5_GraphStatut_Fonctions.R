

# ggplot(df_status_Ens_BIS, 
#        aes(x=moment,y=eff,
#            fill=factor(name, levels=c("Unknown","Critical","Warning"))
#            )
#        ) + 
#     geom_col(position="stack", width = 1) +
#     scale_fill_manual(name="legende des statuts :", 
#                       values = c("Critical" = "red",
#                                  "Warning" = "DarkOrange",
#                                  "Unknown" = "grey"),
#                       labels = c("Critical" = "critique",
#                                  "Warning" = "warning",
#                                  "Unknown" = "inconnu")) +
#     scale_x_discrete(labels = char) +
#     scale_y_continuous(breaks=seq(0,100000,by=20)) +
#     theme_light() +
#     theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
#           axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=80, face="italic"),
#           
#           axis.title.y = element_text(color = "black", size=10, face="italic"),
#           axis.text.y = element_text(color="black", size=8),
#           
#           panel.border = element_blank(),
#           legend.position="bottom") +
#     labs(x="moment de la journée", y="effectif alertes") +
#     geom_text(aes(label = eff_label, fontface=4), size=3, vjust=0, hjust=1, angle=80, position="stack")



# Fonction 1
fct_modif_label_x <- function(df){
    ########################################
    # DEBUT Creation du vecteur moment label
    moment_vect <- df %>% select(moment,moment_label) %>%
        mutate(ligne=row_number()) %>% 
        mutate(mult=ligne%%3) %>% filter(mult==1) %>%
        mutate(ligne=row_number()) %>% 
        select(ligne,moment,moment_label)
    
    virgule <- c(',')
    for (i in 1:(nrow(moment_vect)-2)){
        virgule <- append(virgule,',')
    }
    virgule <- virgule %>% as.data.frame() %>%
        rename(moment_label = ".") %>% 
        mutate(ligne=row_number(),moment="") %>% select(ligne,moment,moment_label) 
    
    moment_vect <- rbind(moment_vect,virgule) %>% arrange(ligne)
    
    char <- c()
    for (i in 1:nrow(moment_vect)){
        char <- append(char,moment_vect$moment_label[i])
        cat(moment_vect$moment_label[i],"\n")
    }
    names(char) <- moment_vect$moment
    
    return (char)
    # structure attendue du char...
    # char <- c('23-06-28_00h00' = '23-06-28_00h00',
    #           ',',
    #           '23-06-28_00h05' = '_',
    #           ',',
    #           '23-06-28_00h10' = '23-06-28_00h10',
    #           ',',
    #           '23-06-28_00h15' = '_')
    
    # FIN  Creation du vecteur moment label
    ########################################    
}


# Fonction 2
fct_Crea_BarChart <- function(df){
    outgraph <- ggplot(df, 
                       aes(x=moment,y=eff,
                           fill=factor(name, levels=c("Unknown","Critical","Warning"))
                          )
                      ) + 
                    geom_col(position="stack", width = 1) +
                    scale_fill_manual(name="legende des statuts :", 
                                      values = c("Critical" = "red",
                                                 "Warning" = "DarkOrange",
                                                  "Unknown" = "grey"),
                                      labels = c("Critical" = "critique",
                                                 "Warning" = "warning",
                                                 "Unknown" = "inconnu")) +
                    scale_x_discrete(labels = char) +
                    scale_y_continuous(breaks=seq(0,100000,by=20)) +
                    theme_light() +
                    theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
                    axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=88, face="italic"),
          
                    axis.title.y = element_text(color = "black", size=10, face="italic"),
                    axis.text.y = element_text(color="black", size=8),
          
                    panel.border = element_blank(),
                    legend.position="bottom") +
                    labs(x="moment de la journee", y="effectif alertes") +
                    geom_text(aes(label = eff_label, fontface=4), size=3, vjust=0, hjust=1, angle=80, position="stack")
    return (outgraph)
}


# Fonction 3
fct_Crea_BarChart_ZoneOsny <- function(df){
    outgraph <- ggplot(df, 
                       aes(x=moment,y=eff,
                           fill=factor(zone, 
                                       levels=c("02_DMZ_Osny","04_Part_Osny","06_ZoneInt_Osny","08_ZoneSIA_Osny"))
                       )
    ) + 
        geom_col(position="stack", width = 1) +
        scale_fill_manual(name="legende des zones :", 
                          values = c("02_DMZ_Osny" = "PapayaWhip",
                                     "04_Part_Osny" = "Yellow",
                                     "06_ZoneInt_Osny" = "Khaki",
                                     "08_ZoneSIA_Osny" = "DarkKhaki"),
                          labels = c("02_DMZ_Osny" = "Osny DMZ",
                                     "04_Part_Osny" = "Osny Partenaire",
                                     "06_ZoneInt_Osny" = "Osny Interne",
                                     "08_ZoneSIA_Osny" = "Osny SIA")) +
        scale_x_discrete(labels = char) +
        scale_y_continuous(breaks=seq(0,100000,by=20)) +
        theme_light() +
        theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
              axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=88, face="italic"),
              
              axis.title.y = element_text(color = "black", size=10, face="italic"),
              axis.text.y = element_text(color="black", size=8),
              
              panel.border = element_blank(),
              legend.position="bottom") +
        labs(x="moment de la journee", y="effectif alertes") +
        geom_text(aes(label = eff_label, fontface=4), size=3, vjust=0, hjust=1, angle=80, position="stack")
    return (outgraph)
}


# Fonction 4
fct_Crea_BarChart_ZoneAuzeville <- function(df){
    outgraph <- ggplot(df, 
                       aes(x=moment,y=eff,
                           fill=factor(zone, 
                                       levels=c("01_DMZ_Auze","03_Part_Auze","05_ZoneInt_Auze","07_ZoneSIA_Auze"))
                       )
    ) + 
        geom_col(position="stack", width = 1) +
        scale_fill_manual(name="legende des zones :", 
                          values = c("01_DMZ_Auze" = "PapayaWhip",
                                     "03_Part_Auze" = "Yellow",
                                     "05_ZoneInt_Auze" = "Khaki",
                                     "07_ZoneSIA_Auze" = "DarkKhaki"),
                          labels = c("01_DMZ_Auze" = "Auzeville DMZ",
                                     "03_Part_Auze" = "Auzeville Partenaire",
                                     "05_ZoneInt_Auze" = "Auzeville Interne",
                                     "07_ZoneSIA_Auze" = "Auzeville SIA")) +
        scale_x_discrete(labels = char) +
        scale_y_continuous(breaks=seq(0,100000,by=20)) +
        theme_light() +
        theme(axis.title.x = element_text(color = "black", size=10, face="italic"),
              axis.text.x = element_text(color="black", size=8, vjust=1, hjust=1, angle=88, face="italic"),
              
              axis.title.y = element_text(color = "black", size=10, face="italic"),
              axis.text.y = element_text(color="black", size=8),
              
              panel.border = element_blank(),
              legend.position="bottom") +
        labs(x="moment de la journee", y="effectif alertes") +
        geom_text(aes(label = eff_label, fontface=4), size=3, vjust=0, hjust=1, angle=80, position="stack")
    return (outgraph)
}





