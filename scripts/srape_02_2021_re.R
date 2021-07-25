######## 02/01/2021 - 02/02/2021
# NY Press Release Historical Data

# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)

"%notin%" <- Negate("%in%")
list_0228re <- foreach(his_date = c(1:13, 15:length(url_0228)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0228[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_0228$table[his_date] == "Yes") { ## Level 1 if there are table(s)
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
table_0228_orig <- list.cbind(list_0228re)
table_0228<- as.data.frame(matrix(data = NA, nrow= nrow(table_0228_orig), ncol = ncol(table_0228_orig)+3))
table_0228[, c(1:(13*3),(14*3+1):ncol(table_0228))]<- table_0228_orig
rownames(table_0228)<- rownames(table_0228_orig)
colnames(table_0228)<- rep(c("total_positive", "new_positive", "new_death"), ncol(table_0228)/3)

# 02/14/2021, 02/17/2021, 02/20/2021, 02/23/2021 require differenct xpaths
## add data of 02/14/2021
table_pos<- rvest::html_nodes(x = read_html(url_0228[14], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0228[14], encoding = "ISO-8859-1"),
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
table_add0228<- table_pos_death
table_0228[,(14*3-2):(14*3)]<-table_add0228 
rm(table_add0228)

## add data of 02/17/2021
table_pos<- rvest::html_nodes(x = read_html(url_0228[17], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0228[17], encoding = "ISO-8859-1"),
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
table_add0217<- table_pos_death
table_0228[,(17*3-2):(17*3)]<-table_add0217
rm(table_add0217)

## add data of 02/20/2020
table_pos<- rvest::html_nodes(x = read_html(url_0228[20], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0228[20], encoding = "ISO-8859-1"),
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
table_add0220<- table_pos_death
table_0228[,(20*3-2):(20*3)]<-table_add0220 
rm(table_add0220)

## replace data of 02/23/2021
table_pos<- rvest::html_nodes(x = read_html(url_0228[23], encoding = "ISO-8859-1"),
                              xpath = '//table[last()-2]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_0228[23], encoding = "ISO-8859-1"),
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
table_add<- table_pos_death
table_0228[,(23*3-2):(23*3)]<-table_add
rm(table_add)

# export press releases 
# add the date row. press_release_ is just for exporting
press_release_02_2021 <- data.frame(matrix(data = NA, 
                                      nrow = nrow(table_0228)+2, ncol = ncol(table_0228)+1))
press_release_02_2021[3:nrow(press_release_02_2021), 2:ncol(press_release_02_2021)] <- table_0228
colnames(press_release_02_2021) <- c("county", colnames(table_0228))
press_release_02_2021[,1] <- c("","county",rownames(table_0228))
press_release_02_2021[2, 2:ncol(press_release_02_2021)] <- colnames(table_0228)
press_release_02_2021[1,seq(2,ncol(press_release_02_2021), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2021-02-01"):as.Date(press_0228$date[nrow(press_0228)])),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#
write_xlsx(press_release_02_2021, path = "./output/press_02_2021.xlsx")

# remove unnecessary objects
rm(list_0228re, table_0228, table_0228_orig)


