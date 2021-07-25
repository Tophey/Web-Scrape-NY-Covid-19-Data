#10/01-10/31
# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)

"%notin%" <- Negate("%in%")
list_1031 <- foreach(his_date = c(1:4, 6:length(url_1031)), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_1031[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_1031$table[his_date] == "Yes") { ## Level 1 if there are table(s)
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

# the xpath does not work right on 12/05 somehow. Mannually add it
list_1031re <- list_1031
list_1031re[6:31] <- list_1031re[5:30]

table_1031 <- list.cbind(list_1031re)
# add data of 10/05##########################
table_pos<- rvest::html_nodes(x = read_html(url_1031[5], encoding = "ISO-8859-1"),
                              xpath = '//table[contains(@id, "docx4j_tbl_3")]') %>% 
        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()   
table_death <- rvest::html_nodes(x = read_html(url_1031[5], encoding = "ISO-8859-1"),
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
table_add1005<- table_pos_death
table_1031[,13:15]<-table_add1005




# export press releases in May
# add the date row. press_release_0x is just for exporting
press_release_10 <- data.frame(matrix(data = NA, 
                                      nrow = nrow(table_1031)+2, ncol = ncol(table_1031)+1))
press_release_10[3:nrow(press_release_10), 2:ncol(press_release_10)] <- table_1031
colnames(press_release_10) <- c("county", colnames(table_1031))
press_release_10[,1] <- c("","county",rownames(table_1031))
press_release_10[2, 2:ncol(press_release_10)] <- colnames(table_1031)
press_release_10[1,seq(2,ncol(press_release_10), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2020-10-01"):as.Date("2020-10-31")),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#

write_xlsx(press_release_10, path = "./output/press_10.xlsx")

# remove unnecessary objects
rm(list_1031re, list_1031, table_1031)


