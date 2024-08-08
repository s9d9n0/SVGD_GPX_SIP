
## Chargement des differents packages utilises dans le programme ----
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)

#####
# CPU
#####

cheCPU <- "E:/InfoCom_entre_CentreonSiamoi/Siamoi_vers_Centreon/fichier_extrait_rvtools/2024-07-09/"

date_deb <- str_locate(cheCPU,"2024")[1]
date <- str_sub(cheCPU,date_deb,date_deb+9)

df_dmz_d1_cpu <- read.csv2(paste0(cheCPU,"pdvcsa6d1ast91/RVTools_tabvCPU.csv"))
df_dmz_d2_cpu <- read.csv2(paste0(cheCPU,"pdvcsa6d2ast91/RVTools_tabvCPU.csv"))
df_int_d1_cpu <- read.csv2(paste0(cheCPU,"pdvcsa0d1ast91/RVTools_tabvCPU.csv"))
df_int_d2_cpu <- read.csv2(paste0(cheCPU,"pdvcsa0d2ast91/RVTools_tabvCPU.csv"))

filter_CPU <- function(df){
  df_out <- df %>% 
    filter(Powerstate=="poweredOn") %>% 
    select(Datacenter,Cluster,Host,cohesity,Edifice.vlan,VM,CPUs,
           os_version,
           OS.according.to.the.configuration.file,
           OS.according.to.the.VMware.Tools) %>% 
    rename(Vlan=Edifice.vlan, CPU=CPUs,
           OS_vers=os_version,
           OS_configfile=OS.according.to.the.configuration.file,
           OS_VMwareTool= OS.according.to.the.VMware.Tools) %>% 
    mutate(OS_vers=str_replace_all(OS_vers,"debian_","deb_"),
           OS_configfile=str_replace(OS_configfile," GNU/Linux"," "),
           OS_VMwareTool=str_replace(OS_VMwareTool," GNU/Linux"," ")) %>%
    mutate(OS_configfile=str_replace(OS_configfile,"Microsoft Windows Server","Windows Serv "),
           OS_VMwareTool=str_replace(OS_VMwareTool,"Microsoft Windows Server","Windows Serv ")) %>%
    mutate(OS_configfile=str_replace(OS_configfile,"(64-bit)",""),
           OS_VMwareTool=str_replace(OS_VMwareTool,"(64-bit)","")) %>% 
    mutate(OS_configfile=str_replace(OS_configfile,"\\(\\)",""),
           OS_VMwareTool=str_replace(OS_VMwareTool,"\\(\\)",""))    
  return (df_out)
}

df_dmz_d1_cpu <- filter_CPU(df_dmz_d1_cpu)
df_dmz_d2_cpu <- filter_CPU(df_dmz_d2_cpu)
df_int_d1_cpu <- filter_CPU(df_int_d1_cpu)
df_int_d2_cpu <- filter_CPU(df_int_d2_cpu)


########
# MEMORY
########

cheMEM <- "E:/InfoCom_entre_CentreonSiamoi/Siamoi_vers_Centreon/fichier_extrait_rvtools/2024-07-09/"

df_dmz_d1_mem <- read.csv2(paste0(cheMEM,"pdvcsa6d1ast91/RVTools_tabvMemory.csv"))
df_dmz_d2_mem <- read.csv2(paste0(cheMEM,"pdvcsa6d2ast91/RVTools_tabvMemory.csv"))
df_int_d1_mem <- read.csv2(paste0(cheMEM,"pdvcsa0d1ast91/RVTools_tabvMemory.csv"))
df_int_d2_mem <- read.csv2(paste0(cheMEM,"pdvcsa0d2ast91/RVTools_tabvMemory.csv"))


filter_MEM <- function(df){
  df_out <- df %>% 
    filter(Powerstate=="poweredOn") %>% select(VM,Size.MiB) %>% 
    rename(Memory=Size.MiB) %>% mutate(Memory=Memory/1024)   
  return (df_out)
}

df_dmz_d1_mem <- filter_MEM(df_dmz_d1_mem)
df_dmz_d2_mem <- filter_MEM(df_dmz_d2_mem)
df_int_d1_mem <- filter_MEM(df_int_d1_mem)
df_int_d2_mem <- filter_MEM(df_int_d2_mem)


########
# Fusion CPU, Memory
########

df_dmz_d1 <- df_dmz_d1_cpu %>% left_join(df_dmz_d1_mem,by=c("VM")) %>% relocate(Memory,.after="CPU")
df_dmz_d2 <- df_dmz_d2_cpu %>% left_join(df_dmz_d2_mem,by=c("VM")) %>% relocate(Memory,.after="CPU")
df_int_d1 <- df_int_d1_cpu %>% left_join(df_int_d1_mem,by=c("VM")) %>% relocate(Memory,.after="CPU")
df_int_d2 <- df_int_d2_cpu %>% left_join(df_int_d2_mem,by=c("VM")) %>% relocate(Memory,.after="CPU")

rm(df_dmz_d1_cpu,df_dmz_d1_mem,   df_dmz_d2_cpu,df_dmz_d2_mem,
   df_int_d1_cpu,df_int_d1_mem,   df_int_d2_cpu,df_int_d2_mem)

######

sortie <- "C:/Users/SIAR_YCG8L6/Docs/ProgrammesR/_Analyse_TdB_VCenter/"

write.csv2(df_dmz_d1,file=paste0(sortie,"df_dmz_d1_",date,".csv"),row.names = FALSE)
write.csv2(df_dmz_d2,file=paste0(sortie,"df_dmz_d2_",date,".csv"),row.names = FALSE)
write.csv2(df_int_d1,file=paste0(sortie,"df_int_d1_",date,".csv"),row.names = FALSE)
write.csv2(df_int_d2,file=paste0(sortie,"df_int_d2_",date,".csv"),row.names = FALSE)





