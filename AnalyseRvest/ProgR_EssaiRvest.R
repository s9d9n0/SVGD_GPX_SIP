
# install.packages("RSelenium")
# remotes::install_github("ropensci/RSelenium")
# library(RSelenium)


library(readr)



library(tidyverse)

install.packages("purrr")
library(purrr)

########################

library(xml2)
library(methods)
library(rvest)
library(dplyr)
library(stringr)

html <- read_html("https://www.insee.fr")


strtitle <- html %>% 
  html_node("title") %>% 
  html_text()

strhead <- html %>% 
  html_node("head") %>% 
  html_text()
strhead2 <- as.data.frame(unlist(strsplit(strhead,"\n|\t")))
colnames(strhead2) <- "code"
strhead2 <- strhead2 %>% 
  mutate(vide=ifelse(code=="","O","N")) %>% filter(vide=="N")



strbody <- html %>% 
  html_node("body") %>% 
  html_text()
strbody2 <- as.data.frame(unlist(strsplit(strbody,"\n|\t|\r")))
colnames(strbody2) <- "code"
strbody2 <- strbody2 %>% 
  mutate(code=str_replace_all(code,"  ","")) %>% 
  mutate(vide=ifelse(code %in% c(""," "),"O","N")) %>% filter(vide=="N")

strbody2[2,"code"]


str <- html %>% 
  html_nodes("body div a") %>%
  html_attr("href") %>% as.data.frame()

str <- html %>% 
  html_nodes("*") %>%
  unlist()

# strtot <- html %>% 
#   html_node("body") %>% 
#   html_text() %>% 
#   str_length() %>% as.integer()





plist <- html %>% html_nodes("p") %>% html_text()



html %>% 
  html_node("p") %>% 
  html_text()

body <- html %>% html_node("head")

install.packages("xml")



body_ls <- as.data.frame(as_list(body))
body_ls <- xml_text(body)

body_ls <- xml_contents(body)


body <- unlist(body) %>%  list  %>%  str()
  
txt_body <- list(body)
    


# class(html)
# 
# html <- minimal_html(html)
  


# %>% unlist()
#  html_text()

simple

parse_number(simple)


data(package = "ggplot2")
