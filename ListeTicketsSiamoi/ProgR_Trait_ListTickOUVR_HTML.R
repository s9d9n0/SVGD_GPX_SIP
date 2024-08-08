
ListTickOUVR <- recodActeurs(ListTickOUVR,"OUVR")

ListTickOUVR_2 <- ListTickOUVR %>% 
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
           heure=paste0(str_sub(date_inc,12,13),"<sup>h",str_sub(date_inc,15,16),"</sup>"),
           info_tick = paste0(" &nbsp;&nbsp;&nbsp;&nbsp;<span style='background-color: pink'>&rarr; ticket n&deg;&nbsp;
                                <a href='",lien,"' style='color:blue;' target='_blank'>",num_tick,"</a> ",
                              str_to_lower(statut)," et orient&eacute; vers ","<strong>",acteur,"</strong></span>")) %>%
    mutate(info_tick=str_replace_all(info_tick,"à","&agrave;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"é","&eacute;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"ê","&ecirc;")) %>% 
    mutate(info_tick=str_replace_all(info_tick,"è","&egrave;")) %>%     
    mutate(info_tick=str_replace_all(info_tick,"ô","&ocirc;")) %>% 
    mutate(jour2=paste0(joursem," ",jour," ",mois,"  ",an)) %>% 
    select(-jour) %>% rename(jour=jour2) %>% 
    mutate(jour=ifelse(row_number()==1 | lag(jour)!=jour,jour,"")) %>% 
    select(info_tick,jour,heure,titre,description)


part01 <- ListTickOUVR_2 %>%
    select(jour,heure,titre) %>% 
    mutate(titre=str_replace_all(titre,"_"," ")) %>% 
    mutate(titre=str_replace_all(titre,'"',"'")) %>% 
    mutate(titre=str_replace_all(titre,"\'","'")) %>% 
    mutate(titre=str_replace_all(titre,"à","&agrave;")) %>% 
    mutate(titre=str_replace_all(titre,"é","&eacute;")) %>% 
    mutate(titre=str_replace_all(titre,"ê","&ecirc;")) %>%
    mutate(titre=str_replace_all(titre,"è","&egrave;")) %>%    
    mutate(titre=str_replace_all(titre,"ô","&ocirc;")) %>% 
    mutate(jour=str_sub(jour,1,str_length(jour)-6),
           titre=paste0("<span style='background-color: pink'>",titre,"</span>")) %>%
    mutate(lg=row_number())

part02 <- ListTickOUVR_2 %>% 
    select(jour,info_tick) %>% 
    rename(titre=info_tick) %>% mutate(jour2=str_sub(jour,-4,str_length(jour)),heure="", lg=row_number()) %>% 
    select(-jour) %>% rename(jour=jour2) %>% 
    relocate(jour) %>% relocate(heure,.after=jour)

part03 <- ListTickOUVR_2 %>% 
  select(jour,heure,description) %>%
  mutate(description=str_replace_all(description,"_"," ")) %>% 
  mutate(description=str_replace_all(description,'"',"'")) %>% 
  mutate(description=str_replace_all(description,"\'","'")) %>% 
  mutate(description=str_replace_all(description,"«","'")) %>% 
  mutate(description=str_replace_all(description,"»","'")) %>%
  mutate(description=str_replace_all(description,"'","'")) %>%
  mutate(description=str_replace_all(description,"à","&agrave;")) %>% 
  mutate(description=str_replace_all(description,"é","&eacute;")) %>% 
  mutate(description=str_replace_all(description,"ê","&ecirc;")) %>%
  mutate(description=str_replace_all(description,"è","&egrave;")) %>%    
  mutate(description=str_replace_all(description,"ô","&ocirc;")) %>% 
  mutate(description=str_replace_all(description,"Bonjour,<br />|Rebonjour,<br />|Bonjour,|
                                                  <p>Bonjour,</p>|<p>Bonjour, </p>|
                                                  </p><p>|</p>  <p>","")) %>%
  rename(titre=description) %>% mutate(jour="",heure="", lg=row_number()) %>% 
  mutate(titre=paste0("<details class='niv2'><summary>description du constat initial</summary><div>",titre,"</div></details>"))

part04 <- data.frame(jour=c(""),heure=c(""),titre=c(""),lg=c(""))
for (i in 1:(nrow(part01)-2)){
    part04 <- rbind(part04,data.frame(jour=c(""),heure=c(""),titre=c(""),lg=c("")))
}

rm(i)

part04$lg <- part01$lg[1:(nrow(part01)-1)]
part04$jour <- "_ _ _ _ _"
part04$heure <- "_ _ _"
part04$titre <- "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _"


ListTickOUVR_2 <- rbind(part01,part02,part03,part04) %>% arrange(lg) %>% select(-lg) %>% 
    mutate(jour=ifelse(jour=="_ _ _ _ _" & lead(jour)=="","",jour)) %>% rename(description=titre)
