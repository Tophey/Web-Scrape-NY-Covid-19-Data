#11/01-11/17
# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)

"%notin%" <- Negate("%in%")
list_1130re <- foreach(his_date = c(1:10, 12:19, 22:length(url_1130)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_1130[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_1130$table[his_date] == "Yes") { ## Level 1 if there are table(s)
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
table_1130_orig <- list.cbind(list_1130re)

#########################################################add irregular dates#####
# the xpath does not work on 11/11, 11/20, 11/21
table_1130<- as.data.frame(matrix(data = NA, nrow = nrow(table_1130_orig), ncol = ncol(table_1130_orig)+9))
# 11/11 -- column 31:33, 11/20 -- column 58:60
table_1130[, c(1:30, 34:57, 64:ncol(table_1130))]<- table_1130_orig
rownames(table_1130)<- rownames(table_1130_orig)
colnames(table_1130)<- rep(c("total_positive", "new_positive", "new_death"), ncol(table_1130)/3)

##(1) add data of 11/11
table_pos<- rvest::html_nodes(x = read_html(url_1130[11], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_3")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_1130[11], encoding = "ISO-8859-1"),
                                 xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
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
table_add1111<- table_pos_death
table_1130[,31:33]<-table_add1111 

##(2) add data of 11/20
table_pos<- rvest::html_nodes(x = read_html(url_1130[20], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_4")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_1130[20], encoding = "ISO-8859-1"),
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
table_add1120<- table_pos_death
table_1130[,58:60]<-table_add1120

## add data of 11/21
table_pos<- rvest::html_nodes(x = read_html(url_1130[21], encoding = "ISO-8859-1"),
                              xpath = '//table')[5] %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_1130[21], encoding = "ISO-8859-1"),
                                 xpath = '//table')[6] %>% 
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
table_add1121<- table_pos_death
table_1130[,61:63]<-table_add1121

####################################adding irregular dates finished##################################

# export press releases in Nov
# add the date row. press_release_ is just for exporting
press_release_11 <- data.frame(matrix(data = NA, 
                                      nrow = nrow(table_1130)+2, ncol = ncol(table_1130)+1))
press_release_11[3:nrow(press_release_11), 2:ncol(press_release_11)] <- table_1130
colnames(press_release_11) <- c("county", colnames(table_1130))
press_release_11[,1] <- c("","county",rownames(table_1130))
press_release_11[2, 2:ncol(press_release_11)] <- colnames(table_1130)
press_release_11[1,seq(2,ncol(press_release_11), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2020-11-01"):as.Date("2020-11-30")),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#

write_xlsx(press_release_11, path = "./output/press_11.xlsx")

# remove unnecessary objects
rm(list_1130re, table_1130_orig)


