
## Chargement des packages utilises dans le programme ----

library(RMariaDB)
library(dplyr)
library(stringr)

##################################################
##                                              ##
## 1/ PARTIE REQUETE DE LA BDD CENTREON         ##
##                                              ##
################################################## ----

##
## Connexion a la base Centreon ----
##
baseMdb <- dbConnect(MariaDB(), user="Gepex_lecture",
                     password="Reporting",
                     dbname="centreon_storage", 
                     host="xx.xx.xx.xx",
                     port="3306")
##################################################################

### Requete

Req <- dbGetQuery(baseMdb, 
                  paste("SELECT h.host_id, h.name as name_host, h.alias, h.address, h.enabled as activ_host,
                                i.id as index_id, i.service_id as index_service_id, i.service_description as index_description,
                                s.service_id, s.description, s.enabled as activ_service, s.last_check, s.command_line, s.output
                         FROM            hosts h
                              INNER JOIN index_data i on i.host_id = h.host_id
                              INNER JOIN services s on s.host_id = i.host_id and s.service_id = i.service_id
                         WHERE (s.description LIKE ('%HTTP-Healthcheck%') AND 
                                i.service_description = s.description )
                        "))


# pour connaitre les colonnes d'une table particuliere
Req_zer <- dbGetQuery(baseMdb,
                      paste("SELECT *
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE table_schema LIKE ('%CENTREON_STORAGE%') AND
                                table_name = 'data_bin'
                        "))

Req_uno <- dbGetQuery(baseMdb,
                      paste("SELECT *
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE table_schema LIKE ('%CENTREON_STORAGE%') AND
                                table_name = 'metrics'
                        "))

Req_bis <- dbGetQuery(baseMdb,
                   paste("SELECT *
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE table_schema LIKE ('%CENTREON_STORAGE%') AND
                                table_name = 'services'
                        "))

Req_ter <- dbGetQuery(baseMdb,
                      paste("SELECT *
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE table_schema LIKE ('%CENTREON_STORAGE%') AND
                                table_name = 'hosts'
                        "))

Req_qua <- dbGetQuery(baseMdb,
                      paste("SELECT *
                          FROM INFORMATION_SCHEMA.COLUMNS
                          WHERE table_schema LIKE ('%CENTREON_STORAGE%') AND
                                table_name = 'index_data'
                        "))

Req_VisuMoment <- dbGetQuery(baseMdb,
                paste("SELECT v.id_metric AS v_val_id, v.ctime, v.value,
                              m.metric_id AS m_val_id, m.index_id AS m_id, m.metric_name,
                              m.unit_name, m.warn, m.crit,
                              i.id AS i_id, i.host_id, i.host_name, i.service_id, i.service_description
                       FROM data_bin v
                            inner join metrics m on v.id_metric = m.metric_id
                            inner join index_data i on m.index_id = i.id
                       WHERE i.host_name LIKE ('%pdcol%') AND
                             i.service_description LIKE ('%HTTP-Healthcheck%') AND
                             ctime >= UNIX_TIMESTAMP('","2024-06-04","') AND
                             ctime <= UNIX_TIMESTAMP('","2024-06-05","')
                      "))

Req_VisuMoment2 <- dbGetQuery(baseMdb, 
                  paste("SELECT h.host_id, h.name as name_host, h.alias, h.address, h.enabled as activ_host,
                                i.id as index_id, i.service_id as index_service_id, i.service_description as index_description,
                                s.service_id, s.description, s.enabled as activ_service, s.last_check, s.command_line, s.output#,
                                #m.metric_id,
                                #v.*
                         FROM            hosts h
                              INNER JOIN index_data i on i.host_id = h.host_id
                              INNER JOIN services s on s.host_id = i.host_id and s.service_id = i.service_id
                              # INNER JOIN metrics m on i.index_id = m.metric_id and m.metric_id = v.id_metric
                              # INNER JOIN data_bin v
                         WHERE (s.description LIKE ('%HTTP-Healthcheck%') AND 
                                i.service_description = s.description ) #AND
                                #ctime >= UNIX_TIMESTAMP('","2024-06-04","') AND
                                #ctime <= UNIX_TIMESTAMP('","2024-06-05","')
                        "))


Req_TEST <- dbGetQuery(baseMdb,
                        paste("SELECT v.id_metric AS v_val_id, v.ctime, v.value,
                               m.metric_id AS m_val_id, m.index_id AS m_id, m.metric_name,
                               m.unit_name, m.warn, m.crit,
                               i.id AS i_id, i.host_id, i.host_name, i.service_id, i.service_description,
                               s.display_name, s.output, s.last_hard_state,
                               h.host_id, h.name as name_host
                        FROM data_bin v
                             inner join metrics m on v.id_metric = m.metric_id
                             inner join index_data i on m.index_id = i.id
                             inner join services s on s.service_id = i.service_id
                             inner join hosts h on i.host_id = h.host_id
                        WHERE i.host_name LIKE ('%pdcoltport%') AND
                              s.display_name LIKE ('%HTTP-Healthcheck%') AND
                              ctime >= UNIX_TIMESTAMP('","2024-06-04","') AND
                              ctime <= UNIX_TIMESTAMP('","2024-06-07","') #AND
                              #s.last_hard_state
                        ORDER BY output, i.host_name, ctime
                              
                       "))

Req_TEST <- dbGetQuery(baseMdb,
                       paste("SELECT s.description, s.output,
                                     h.host_id, h.name
                        FROM services s
                             inner join hosts h on s.host_id = h.host_id
                        WHERE h.name LIKE ('%pd%') AND
                              s.description LIKE ('%HTTP-Healthcheck%') AND
                              s.output LIKE ('%500%')
                        #AND
                              #ctime >= UNIX_TIMESTAMP('","2024-06-04","') AND
                              #ctime <= UNIX_TIMESTAMP('","2024-06-07","') #AND
                              #s.last_hard_state
                        #ORDER BY output, ctime
                              
                       "))

##################################################################
##
## Deconnexion de la base Centreon ----
##
dbDisconnect(baseMdb)
dbUnloadDriver(MariaDB())
##################################################################

Req_2 <- Req %>% arrange(name_host) %>% 
    select(name_host, alias, command_line) %>% 
    mutate(extract_type = str_sub(alias,str_length(alias)-4,str_length(alias)-2),
           extract_env = str_sub(alias,1,2)) %>% 
    mutate(type_serveur = case_when(
                            extract_type=="lht" ~ "Apache",
                            extract_type=="las" ~ "Tomcat",
                            TRUE ~ "XX")) %>% 
    mutate(env_serveur = case_when(
                            extract_env=="pd" ~ "Prod",
                            extract_env=="pp" ~ "PreProd",
                            extract_env=="fo" ~ "Formation",
                            extract_env=="bt" ~ "Beta",
                            TRUE ~ "XX")) %>%
    select(-extract_type,-extract_env) %>% 
    relocate(env_serveur, type_serveur) %>% 
    arrange(alias)
    
write.csv2(Req_2,"C:/Users/SIAR_ycg8l6/Docs/ProgrammesR/VisuService_HTTP_Healthcheck/Liste_HTTP_Healthcheck.csv",row.names=FALSE)

###############################################
###############################################
###############################################












