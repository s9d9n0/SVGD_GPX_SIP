
ListTickFERM <- recodActeurs(ListTickFERM,"FERM")

ListTickFERM_2 <- ListTickFERM %>% 
# traitement date_inc
    mutate(joursem_num=wday(date_inc),
           joursem=case_when(
               joursem_num==1 ~ "dim.", joursem_num==2 ~ "lun.", joursem_num==3 ~ "mar.", joursem_num==4 ~ "mer.",
               joursem_num==5 ~ "jeu.", joursem_num==6 ~ "ven.", joursem_num==7 ~ "sam.",
               TRUE ~ ""),
           jour=str_sub(date_inc,9,10),
           mois_num=str_sub(date_inc,6,7),
           mois=case_when(
               mois_num=="01" ~ "janv.", mois_num=="02" ~ "fevr.", mois_num=="03" ~ "mars",  mois_num=="04" ~ "avr.",
               mois_num=="05" ~ "mai",   mois_num=="06" ~ "juin",  mois_num=="07" ~ "juil.", mois_num=="08" ~ "ao&ucirc;t",
               mois_num=="09" ~ "sept.", mois_num=="10" ~ "oct.",  mois_num=="11" ~ "nov.",  mois_num=="12" ~ "d&eacute;c.",
               TRUE ~ ""), 
           an=str_sub(date_inc,1,4),
           heure_inc=paste0(str_sub(date_inc,12,13),"<sup>h",str_sub(date_inc,15,16),"</sup>")) %>% 
    mutate(jour2=paste0(joursem," ",jour," ",mois,"  ",an)) %>% 
    select(-jour) %>% rename(jour_inc=jour2) %>% relocate(jour_inc,.before=heure_inc) %>% 
# traitement date_realisation
    mutate(joursem_num=wday(date_realisation),
           joursem=case_when(
               joursem_num==1 ~ "dim.", joursem_num==2 ~ "lun.", joursem_num==3 ~ "mar.", joursem_num==4 ~ "mer.",
               joursem_num==5 ~ "jeu.", joursem_num==6 ~ "ven.", joursem_num==7 ~ "sam.",
               TRUE ~ ""),
           jour=str_sub(date_realisation,9,10),
           mois_num=str_sub(date_realisation,6,7),
           mois=case_when(
               mois_num=="01" ~ "janv.", mois_num=="02" ~ "fevr.", mois_num=="03" ~ "mars",  mois_num=="04" ~ "avr.",
               mois_num=="05" ~ "mai",   mois_num=="06" ~ "juin",  mois_num=="07" ~ "juil.", mois_num=="08" ~ "ao&ucirc;t",
               mois_num=="09" ~ "sept.", mois_num=="10" ~ "oct.",  mois_num=="11" ~ "nov.",  mois_num=="12" ~ "d&eacute;c.",
               TRUE ~ ""), 
           an=str_sub(date_realisation,1,4),
           heure_ferm=paste0(str_sub(date_realisation,12,13),"<sup>h",str_sub(date_realisation,15,16),"</sup>")) %>%
    mutate(jour2=paste0(joursem," ",jour," ",mois,"  ",an)) %>% 
    select(-jour) %>% rename(jour_ferm=jour2) %>% relocate(jour_ferm,.before=heure_ferm) %>% 
    select(-joursem_num,-joursem,-mois_num,-mois,-an) %>%
# recodification du statut
    mutate(statut=case_when(
               statut=="Résolu"              ~ "r&eacute;solu",
               statut=="Clôturé"             ~ "clos",
               statut=="Clôture automatique" ~ "clos automatiquement",
               TRUE ~ "")) %>% 
# creation de la variable info_tick
    mutate(info_tick=case_when(
        statut=="r&eacute;solu"     ~ paste0(" &nbsp;&nbsp;&nbsp;&nbsp;<span style='background-color: cyan'>&rarr; ticket n&deg;&nbsp;
                                          <a href='",lien,"' style='color:blue;' target='_blank'>",num_tick,"</a> ",
                                          str_to_lower(statut)," par ","<strong>",acteur,"</strong>"),
        str_sub(statut,1,4)=="clos" ~ paste0(" &nbsp;&nbsp;&nbsp;&nbsp;<span style='background-color: cyan'>&rarr; ticket n&deg;&nbsp;
                                          <a href='",lien,"' style='color:blue;' target='_blank'>",num_tick,"</a> ",
                                          "r&eacute;solu et <strong>",str_to_lower(statut),"</strong>"),
               TRUE ~ "")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"à","&agrave;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"é","&eacute;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"ê","&ecirc;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"è","&egrave;")) %>%     
    mutate(info_tick=str_replace_all(info_tick,"ô","&ocirc;")) %>% 
    select(info_tick,jour_inc,heure_inc,jour_ferm,heure_ferm,titre,date_inc,date_realisation,duree_str)


part01 <- ListTickFERM_2 %>%
    select(jour_inc,heure_inc,jour_ferm,heure_ferm,duree_str,titre) %>% 
    mutate(titre=str_replace_all(titre,"_"," ")) %>%
    mutate(titre=str_replace_all(titre,"à","&agrave;")) %>% 
    mutate(titre=str_replace_all(titre,"é","&eacute;")) %>% 
    mutate(titre=str_replace_all(titre,"ê","&ecirc;")) %>% 
    mutate(titre=str_replace_all(titre,"è","&egrave;")) %>%     
    mutate(titre=str_replace_all(titre,"ô","&ocirc;")) %>% 
    mutate(jour_inc=str_sub(jour_inc,1,str_length(jour_inc)-6),
           jour_ferm=str_sub(jour_ferm,1,str_length(jour_ferm)-6),
           titre=paste0("<span style='background-color: cyan'>",titre,"</span>")) %>%
    mutate(lg=row_number())

part02 <- ListTickFERM_2 %>% 
    select(jour_inc,jour_ferm,info_tick) %>% 
    rename(titre=info_tick) %>% mutate(jour2_inc=str_sub(jour_inc,-4,str_length(jour_inc)),
                                       jour2_ferm=str_sub(jour_ferm,-4,str_length(jour_ferm)),
                                       heure_inc="", heure_ferm="", duree_str="", lg=row_number()) %>% 
    select(-jour_inc,-jour_ferm) %>% rename(jour_inc=jour2_inc,jour_ferm=jour2_ferm) %>% 
    relocate(jour_inc) %>% relocate(heure_inc,.after=jour_inc) %>% 
    relocate(jour_ferm,.after=heure_inc) %>% relocate(heure_ferm,.after=jour_ferm) %>% 
    relocate(duree_str,.before=titre)


part03 <- data.frame(jour_inc=c(""),heure_inc=c(""),
                     jour_ferm=c(""),heure_ferm=c(""),
                     duree_str=c(""),titre=c(""),lg=c(""))
for (i in 1:(nrow(part01)-2)){
    part03 <- rbind(part03,data.frame(jour_inc=c(""),heure_inc=c(""),
                                      jour_ferm=c(""),heure_ferm=c(""),
                                      duree_str=c(""),titre=c(""),lg=c("")))
}

rm(i)

part03$lg <- part01$lg[1:(nrow(part01)-1)]
part03$jour_inc <- "_ _ _ _ _"
part03$heure_inc <- "_ _ _"
part03$jour_ferm <- "_ _ _ _ _"
part03$heure_ferm <- "_ _ _"
part03$duree_str <- "_ _ _ _ _ _ _"
part03$titre <- "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"


ListTickFERM_2 <- rbind(part01,part02,part03) %>% arrange(lg) %>% select(-lg) %>% 
    mutate(jour_inc=ifelse(jour_inc=="_ _ _ _ _" & lead(jour_inc)==lag(jour_inc,2),"",jour_inc)) %>%
    mutate(heure_inc=ifelse(jour_inc=="","",heure_inc)) %>% rename(description=titre)


