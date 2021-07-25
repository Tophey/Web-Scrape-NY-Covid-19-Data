###### ATTENTION: 12/01 to 01/02
# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)

"%notin%" <- Negate("%in%")
list_0131re <- foreach(his_date = c(1:5, 7:27, 29:48, 50, 52:length(url_0131)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0131[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_0131$table[his_date] == "Yes") { ## Level 1 if there are table(s)
                table_pos<- rvest::html_nodes(x = htmls,
                                              xpath = '//table[last()-1]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
                table_death <- rvest::html_nodes(x = htmls,
                                                 xpath = '//table[last()]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                
                #recode row and column names
                rownames(table_pos) <- table_pos[,1]
                table_pos <- table_pos[2:nrow(table_pos),2:3]
                colnames(table_pos) <- c("total_positive", "new_positive")
                # delete the comma and make the numbers "numeric"
                for (i in 1:ncol(table_pos)){
                        table_pos[,i] <- as.numeric(gsub(",","", table_pos[,i]))
                        
                }
                
                #match the reported counties' positive
                table_pos_death <- table_empty
                table_pos_death[rownames(table_pos_death) %in% rownames(table_pos),1:2] <- 
                        table_pos[,1:2]
                #recode row and column names
                rownames(table_death) <- table_death[,1]
                table_death <- table_death[3:nrow(table_death),]
                colnames(table_death) <- c("county", "new_death")
                table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                
                
                # Not including NYC boroughs' new deaths
                
                table_pos_death$new_death[rownames(table_pos_death) %in% rownames(table_death)] <- 
                        table_death[rownames(table_death) %notin% 
                                            c("Bronx","Brooklyn", "Manhattan", "Staten Island","Richmond", "Queens", "Kings"),2]
                
                # add the numbers of NYC new deaths
                table_pos_death[rownames(table_pos_death)=="NYC","new_death"] <- 
                        sum(table_death[rownames(table_death) %in% 
                                                c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings"),2])
                # add new deaths of "Non-NYs" (found in the source of 08/18/2020)
                table_pos_death["Non-NYs",3] <- table_death["Non-NYs", 2]
                # now table_pos_death has all the positive, new positive, and new deaths
                return(table_pos_death)
                
                
        }else {return(table_empty)}
}
#parallel::stopCluster(cl)
table_0131_orig <- list.cbind(list_0131re)

##################################################
# 12/06, 12/28, 01/18, 01/20 need different xpaths
table_0131<- as.data.frame(matrix(data = NA, nrow = nrow(table_0131_orig), ncol = ncol(table_0131_orig)+12))
# 12/06 -- columns 16 - 18
table_0131[, c(1:15, 19:81, 85:144, 148:150, 154:ncol(table_0131))]<- table_0131_orig
rownames(table_0131)<- rownames(table_0131_orig)
colnames(table_0131)<- rep(c("total_positive", "new_positive", "new_death"), ncol(table_0131)/3)

## add data of 12/06
table_pos<- rvest::html_nodes(x = read_html(url_0131[6], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0131[6], encoding = "ISO-8859-1"),
                                 xpath = '//table[contains(@id, "docx4j_tbl_6")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_pos) <- table_pos[,1]
table_pos <- table_pos[2:nrow(table_pos),2:3]
colnames(table_pos) <- c("total_positive", "new_positive")
# delete the comma and make the numbers "numeric"
for (i in 1:ncol(table_pos)){
        table_pos[,i] <- as.numeric(gsub(",","", table_pos[,i]))
        
}

#match the reported counties' positive
table_pos_death <- table_empty
table_pos_death[rownames(table_pos_death) %in% rownames(table_pos),1:2] <- 
        table_pos[,1:2]
#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# Not including NYC boroughs' new deaths

table_pos_death$new_death[rownames(table_pos_death) %in% rownames(table_death)] <- 
        table_death[rownames(table_death) %notin% 
                            c("Bronx","Brooklyn", "Manhattan", "Staten Island","Richmond", "Queens", "Kings"),2]

# add the numbers of NYC new deaths
table_pos_death[rownames(table_pos_death)=="NYC","new_death"] <- 
        sum(table_death[rownames(table_death) %in% 
                                c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings"),2])
# add new deaths of "Non-NYs" (found in the source of 08/18/2020)
table_pos_death["Non-NYs",3] <- table_death["Non-NYs", 2]
table_add1206<- table_pos_death
table_0131[,16:18]<-table_add1206 

##### Add the data of 12/28
table_pos<- rvest::html_nodes(x = read_html(url_0131[28], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0131[28], encoding = "ISO-8859-1"),
                                 xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_pos) <- table_pos[,1]
table_pos <- table_pos[2:nrow(table_pos),2:3]
colnames(table_pos) <- c("total_positive", "new_positive")
# delete the comma and make the numbers "numeric"
for (i in 1:ncol(table_pos)){
        table_pos[,i] <- as.numeric(gsub(",","", table_pos[,i]))
        
}

#match the reported counties' positive
table_pos_death <- table_empty
table_pos_death[rownames(table_pos_death) %in% rownames(table_pos),1:2] <- 
        table_pos[,1:2]
#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# Not including NYC boroughs' new deaths

table_pos_death$new_death[rownames(table_pos_death) %in% rownames(table_death)] <- 
        table_death[rownames(table_death) %notin% 
                            c("Bronx","Brooklyn", "Manhattan", "Staten Island","Richmond", "Queens", "Kings"),2]

# add the numbers of NYC new deaths
table_pos_death[rownames(table_pos_death)=="NYC","new_death"] <- 
        sum(table_death[rownames(table_death) %in% 
                                c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings"),2])
# add new deaths of "Non-NYs" (found in the source of 08/18/2020)
table_pos_death["Non-NYs",3] <- table_death["Non-NYs", 2]
table_add1228<- table_pos_death
table_0131[,82:84]<-table_add1228 

#####################
# add data of 01/18, columns 145 - 147
table_pos<- rvest::html_nodes(x = read_html(url_0131[49], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0131[49], encoding = "ISO-8859-1"),
                                 xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_pos) <- table_pos[,1]
table_pos <- table_pos[2:nrow(table_pos),2:3]
colnames(table_pos) <- c("total_positive", "new_positive")
# delete the comma and make the numbers "numeric"
for (i in 1:ncol(table_pos)){
        table_pos[,i] <- as.numeric(gsub(",","", table_pos[,i]))
        
}

#match the reported counties' positive
table_pos_death <- table_empty
table_pos_death[rownames(table_pos_death) %in% rownames(table_pos),1:2] <- 
        table_pos[,1:2]
#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# Not including NYC boroughs' new deaths

table_pos_death$new_death[rownames(table_pos_death) %in% rownames(table_death)] <- 
        table_death[rownames(table_death) %notin% 
                            c("Bronx","Brooklyn", "Manhattan", "Staten Island","Richmond", "Queens", "Kings"),2]

# add the numbers of NYC new deaths
table_pos_death[rownames(table_pos_death)=="NYC","new_death"] <- 
        sum(table_death[rownames(table_death) %in% 
                                c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings"),2])
# add new deaths of "Non-NYs" (found in the source of 08/18/2020)
table_pos_death["Non-NYs",3] <- table_death["Non-NYs", 2]
table_add0118<- table_pos_death
table_0131[,145:147]<-table_add0118 

##### Add the data of 12/28
table_pos<- rvest::html_nodes(x = read_html(url_0131[51], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0131[51], encoding = "ISO-8859-1"),
                                 xpath = '//table[contains(@id, "docx4j_tbl_5")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()

#recode row and column names
rownames(table_pos) <- table_pos[,1]
table_pos <- table_pos[2:nrow(table_pos),2:3]
colnames(table_pos) <- c("total_positive", "new_positive")
# delete the comma and make the numbers "numeric"
for (i in 1:ncol(table_pos)){
        table_pos[,i] <- as.numeric(gsub(",","", table_pos[,i]))
        
}

#match the reported counties' positive
table_pos_death <- table_empty
table_pos_death[rownames(table_pos_death) %in% rownames(table_pos),1:2] <- 
        table_pos[,1:2]
#recode row and column names
rownames(table_death) <- table_death[,1]
table_death <- table_death[3:nrow(table_death),]
colnames(table_death) <- c("county", "new_death")
table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))

# Not including NYC boroughs' new deaths

table_pos_death$new_death[rownames(table_pos_death) %in% rownames(table_death)] <- 
        table_death[rownames(table_death) %notin% 
                            c("Bronx","Brooklyn", "Manhattan", "Staten Island","Richmond", "Queens", "Kings"),2]

# add the numbers of NYC new deaths
table_pos_death[rownames(table_pos_death)=="NYC","new_death"] <- 
        sum(table_death[rownames(table_death) %in% 
                                c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings"),2])
# add new deaths of "Non-NYs" (found in the source of 08/18/2020)
table_pos_death["Non-NYs",3] <- table_death["Non-NYs", 2]
table_add0120<- table_pos_death
table_0131[, 151:153]<-table_add0120









######################################################

# export press releases 
# add the date row. press_release_ is just for exporting
press_release_01_2021 <- data.frame(matrix(data = NA, 
                                      nrow = nrow(table_0131)+2, ncol = ncol(table_0131)+1))
press_release_01_2021[3:nrow(press_release_01_2021), 2:ncol(press_release_01_2021)] <- table_0131
colnames(press_release_01_2021) <- c("county", colnames(table_0131))
press_release_01_2021[,1] <- c("","county",rownames(table_0131))
press_release_01_2021[2, 2:ncol(press_release_01_2021)] <- colnames(table_0131)
press_release_01_2021[1,seq(2,ncol(press_release_01_2021), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2020-12-01"):as.Date("2021-01-31")),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#
write_xlsx(press_release_01_2021, path = "./output/press_12_01.xlsx")

# remove unnecessary objects
rm(list_0131re, table_0131_orig)

