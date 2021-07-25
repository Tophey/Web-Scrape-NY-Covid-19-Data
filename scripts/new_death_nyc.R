# add new deaths of nyc counties

# create a separate table to contain nyc county deaths (5 counties, but 2 duplicate)
table_nycd<- as.data.frame(matrix(data = NA, nrow = 7, ncol = 2))
rownames(table_nycd)<- c("Bronx", "Brooklin","Kings", "Manhattan","Queens", "Richmond", "Staten Island")
colnames(table_nycd)<- c("total_death", "new_death")
table_nycd$total_death<- as.numeric(table_nycd$total_death)
table_nycd$new_death<- as.numeric(table_nycd$new_death)

# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)
"%notin%" <- Negate("%in%")
list_03_death <- foreach(his_date = 1:31, combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_430[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_430$table[his_date] == "Yes") { ## Level 1 if 
             
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

           # if there is death table
                if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_death<- table_2
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                   
                        return(nyc_death)
                }else {return(table_nycd)} # return empty table when there is no death table in the link
                
        }else {return(table_nycd)}
}
table_03_death<- list.cbind(list_03_death[20:length(list_03_death)]) 

# 04 nyc county deaths
list_04_death <- foreach(his_date = 32:length(url_430), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_430[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_430$table[his_date] == "Yes") { ## Level 1 if 
                
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                
                # if there is death table
                if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_death<- table_2
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                        
                        return(nyc_death)
                }else {return(table_nycd)} # return an empty table when there is no death table in the link
                
        }else {return(table_nycd)}
}
table_04_death<- list.cbind(list_04_death) 

# 05 nyc county deaths
list_05_death <- foreach(his_date = 1:length(url_531), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_531[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_531$table[his_date] == "Yes") { ## Level 1 if 
                
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                
                # if there is death table
                if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_death<- table_2
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                        
                        return(nyc_death)
                }else {return(table_nycd)} # return an empty table when there is no death table in the link
                
        }else {return(table_nycd)}
}
table_05_death<- list.cbind(list_05_death) 

# 06, 07 nyc county deaths
list_067_death <- foreach(his_date = 1:length(url_731), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_731[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_731$table[his_date] == "Yes") { ## Level 1 if 
                
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                
                # if there is death table
                if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_death<- table_2
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                        
                        return(nyc_death)
                }else {return(table_nycd)} # return an empty table when there is no death table in the link
                
        }else {return(table_nycd)}
}
table_067_death<- list.cbind(list_067_death) 

# 08, 09 nyc ocounty death
list_089_death <- foreach(his_date = 1:length(url_930), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_930[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_930$table[his_date] == "Yes") { ## Level 1 if 
                
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                
                # if there is death table
                if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_death<- table_2
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                        
                        return(nyc_death)
                }else {return(table_nycd)} # return an empty table when there is no death table in the link
                
        }else {return(table_nycd)}
}
table_089_death<- list.cbind(list_089_death)

# 10 nyc county deaths. Attention: the xpath does not work right on 10/05
list_10_death <- foreach(his_date = c(1:length(url_1031)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_1031[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_1031$table[his_date] == "Yes") { ## Level 1 if 
                if (his_date != 5){
                table_death<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[last()]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else{table_death<- rvest::html_nodes(x = htmls,
                                                     xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()}
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        # build a temp for NYC counties' death
                        nyc_death<- table_nycd
                        nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                                table_death[rownames(table_death)%in% rownames(nyc_death),2]
                        
                        return(nyc_death)

        }else {return(table_nycd)}
}
table_10_death<- list.cbind(list_10_death)

# 11 nyc county deaths
list_11_death <- foreach(his_date = c(1:length(url_1130)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_1130[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_1130$table[his_date] == "Yes") { ## Level 1 if 
                if (his_date != 11){
                        table_death<- rvest::html_nodes(x = htmls,
                                                        xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else{table_death<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()}
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_11_death<- list.cbind(list_11_death)

# 12/2020,01/2021 nyc county deaths 
list_01_death <- foreach(his_date = c(1:length(url_0131)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0131[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_0131$table[his_date] == "Yes") { ## Level 1 if 
        
                        table_death<- rvest::html_nodes(x = htmls,
                                                        xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
           
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_01_death<- list.cbind(list_01_death)

#### NYC new deaths of 12/28, 01/18, 01/20 need a different xpath
## add nyc new deaths of 12/28
table_death<- rvest::html_nodes(x = read_html(url_0131[28]),
                                     xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# build a temp for NYC counties' death
nyc_death<- table_nycd
nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
        table_death[rownames(table_death)%in% rownames(nyc_death),2]
table_01_death[, 55:56]<- nyc_death
rm(table_death)
rm(nyc_death)

## add data of 01/18
table_death<- rvest::html_nodes(x = read_html(url_0131[49]),
                                xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# build a temp for NYC counties' death
nyc_death<- table_nycd
nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
        table_death[rownames(table_death)%in% rownames(nyc_death),2]
table_01_death[, 97:98]<- nyc_death
rm(table_death)
rm(nyc_death)

## add data of 01/20
table_death<- rvest::html_nodes(x = read_html(url_0131[51]),
                                xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# build a temp for NYC counties' death
nyc_death<- table_nycd
nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
        table_death[rownames(table_death)%in% rownames(nyc_death),2]
table_01_death[, 101:102]<- nyc_death
rm(table_death)
rm(nyc_death)

# 02/2021 NYC counties' new death
list_02_death <- foreach(his_date = 1:length(url_0228), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0228[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_0228$table[his_date] == "Yes") { ## Level 1 if 
                if (his_date != 11){
                        table_death<- rvest::html_nodes(x = htmls,
                                                        xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else{table_death<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()}
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_02_death<- list.cbind(list_02_death)
#### NYC new deaths of 02/14/2021, 02/17/2021 need a different xpath
## add nyc new deaths of 02/14
table_death<- rvest::html_nodes(x = read_html(url_0228[14]),
                                xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# build a temp for NYC counties' death
nyc_death<- table_nycd
nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
        table_death[rownames(table_death)%in% rownames(nyc_death),2]
table_02_death[, 27:28]<- nyc_death
rm(table_death)
rm(nyc_death)

## add nyc new deaths of 02/17
table_death<- rvest::html_nodes(x = read_html(url_0228[17]),
                                xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# build a temp for NYC counties' death
nyc_death<- table_nycd
nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
        table_death[rownames(table_death)%in% rownames(nyc_death),2]
table_02_death[, 33:34]<- nyc_death
rm(table_death)
rm(nyc_death)

## 03/01/2021 to 03/31/2021
# 03/2021 NYC counties' new death
list_03_21death <- foreach(his_date = 1:length(url_0331_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0331_21[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_0331_21$table[his_date] == "Yes") { ## Level 1 if 
                if (his_date != 11){
                        table_death<- rvest::html_nodes(x = htmls,
                                                        xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else{table_death<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()}
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_03_21death<- list.cbind(list_03_21death)

## 04/01/2021 to 04/30/2021
# 04/2021 NYC counties' new death
list_04_21death <- foreach(his_date = 1:length(url_0430_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0430_21[his_date] 
        if (!is.na(urls) & press_0331_21$table[his_date] == "Yes") { ## Level 1 if 
                htmls <- read_html(urls, encoding = "ISO-8859-1") 
                if (his_date != 11){
                        table_death<- rvest::html_nodes(x = htmls,
                                                        xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else{table_death<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()}
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_04_21death<- list.cbind(list_04_21death)

## 05/01/2021 to 05/31/2021
# 05/2021 NYC counties' new death
"%notin%" <- Negate("%in%")
list_05_21death <- foreach(his_date = 1:length(url_0531_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0531_21[his_date] 
        if (!is.na(urls) & press_0331_21$table[his_date] == "Yes") { ## Level 1 if 
                htmls <- read_html(urls, encoding = "ISO-8859-1") 
                # Change in the source: a table for vaccination numbers 
                #is added to the end (Starting on 05/30)
                table_last <- rvest::html_nodes(x = htmls,
                                                xpath = '//table[last()]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                if (table_last[2,1] == "Region"){
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()-1]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else {
                        table_death <- table_last
                        rm(table_last)
                }
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_05_21death<- list.cbind(list_05_21death)

## 06/01/2021 to 06/30/2021
# 06/2021 NYC counties' new death
"%notin%" <- Negate("%in%")
list_06_21death <- foreach(his_date = 1:length(url_0630_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0630_21[his_date] 
        if (!is.na(urls) & press_0630_21$table[his_date] == "Yes") { ## Level 1 if 
                htmls <- read_html(urls, encoding = "ISO-8859-1") 
                # Change in the source: a table for vaccination numbers 
                #is added to the end (Starting on 05/30)
                table_last <- rvest::html_nodes(x = htmls,
                                                xpath = '//table[last()]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                if (table_last[2,1] == "Region"){
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-2]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()-1]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else if(table_last[1,1] == "Region"){  
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-3]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()-2]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                        
                }else {
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-1]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_06_21death<- list.cbind(list_06_21death)

## 07/01/2021 to 07/31/2021
# 07/2021 NYC counties' new death
"%notin%" <- Negate("%in%")
list_07_21death <- foreach(his_date = 1:length(url_0731_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0731_21[his_date] 
        if (!is.na(urls) & press_0731_21$table[his_date] == "Yes") { ## Level 1 if 
                htmls <- read_html(urls, encoding = "ISO-8859-1") 
                # Change in the source: a table for vaccination numbers 
                #is added to the end (Starting on 05/30)
                table_last <- rvest::html_nodes(x = htmls,
                                                xpath = '//table[last()]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                if (table_last[2,1] == "Region"){
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-2]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()-1]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }else if(table_last[1,1] == "Region"){  
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-3]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()-2]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                        
                }else {
                        table_pos<- rvest::html_nodes(x = htmls,
                                                      xpath = '//table[last()-1]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[last()]') %>% 
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                }
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                # build a temp for NYC counties' death
                nyc_death<- table_nycd
                nyc_death$new_death[rownames(nyc_death)%in% rownames(table_death)]<- 
                        table_death[rownames(table_death)%in% rownames(nyc_death),2]
                
                return(nyc_death)
                
        }else {return(table_nycd)}
}
table_07_21death<- list.cbind(list_07_21death)


# cbind all the tables of nyc county deaths
nyc_death_all<-table_03_death %>% 
        cbind(table_04_death) %>% 
        cbind(table_05_death) %>% 
        cbind(table_067_death) %>% 
        cbind(table_089_death) %>% 
        cbind(table_10_death) %>% 
        cbind(table_11_death) %>% 
        cbind(table_01_death) %>% 
        cbind(table_02_death) %>% 
        cbind(table_03_21death) %>% 
        cbind(table_04_21death) %>% 
        cbind(table_05_21death) %>% 
        cbind(table_06_21death) %>% 
        cbind(table_07_21death)

# combine duplicate counties: brooklyn and kings, richmond and staten island.
# Change Manhattan into New York County
nyc_death_all["Kings",] <- ifelse(is.na(nyc_death_all["Kings",]), 
                                  nyc_death_all["Brooklyn",], nyc_death_all["Kings",])
nyc_death_all["Richmond",]<- ifelse(is.na(nyc_death_all["Richmond",]),
                                    nyc_death_all["Staten Island",], nyc_death_all["Richmond",])
nyc_death_all<- nyc_death_all[c("Bronx", "Kings", "Manhattan", "Queens", "Richmond"), ]
rownames(nyc_death_all)[3]<- "New York County"
colnames(nyc_death_all)<- rep(c("total_death", "new_death"), ncol(nyc_death_all)/2)
nyc_death_insert<- as.data.frame(matrix(data = NA, nrow = nrow(nyc_death_all), ncol = 2*ncol(nyc_death_all)))
rownames(nyc_death_insert)<- rownames(nyc_death_all)
colnames(nyc_death_insert)<- rep(c("total_positive", "new_positive", "total_death", "new_death"), ncol(nyc_death_all)/2)

nyc_death_insert[,seq(4, ncol(nyc_death_insert), 4)]<- nyc_death_all[, seq(2, ncol(nyc_death_all), 2)]

# Now get back to main_script.R and replace NYC counties' deaths

