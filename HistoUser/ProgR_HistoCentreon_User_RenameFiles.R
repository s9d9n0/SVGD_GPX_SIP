
library(dplyr)
library(stringr)

repertoire <- "C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/HistoCentreon_User/Fichiers_date_20231229"

listing <- list.files(repertoire) %>% as.data.frame() %>% 
  rename(liste_fich=".") %>% arrange(liste_fich)

essai <- listing %>% mutate(finnom = str_sub(liste_fich,20,38))

setwd(repertoire)

for (i in 1:nrow(listing)){
  cat(paste0("numero ",i," ",listing$liste_fich[i],"\n"))
  file.rename(listing$liste_fich[i],paste0("df_CentreonSIP_User",str_sub(listing$liste_fich[i],20,38)))
}





