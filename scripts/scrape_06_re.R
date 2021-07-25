# scrape 06/03-06/30
"%notin%" <- Negate("%in%")
# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)

list_630re <- foreach(his_date = c(1:30), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_731[his_date] 
        htmls <- read_html(urls, encoding = "ISO-8859-1") 
        if (press_731$table[his_date] == "Yes") { ## Level 1 if 
                #get the positive table
                table_0 <- rvest::html_nodes(x = htmls,
                                             xpath = '//table[contains(@id, "docx4j_tbl_0")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_1 <-rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_1")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                table_2<- rvest::html_nodes(x = htmls,
                                            xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                # special dates like 05/02
                table_s5 <- rvest::html_nodes(x = htmls,
                                              xpath = '//table[contains(@style, "width: 324px")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()  
                # special date like 07/11
                table_s7 <- rvest::html_nodes(x = htmls,
                                              xpath = '//table[contains(@style, "width: 348px")]') %>% 
                        html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                ############## if special dates like 5/02
                if (nrow(table_s5)>0 & nrow(table_1)==0) 
                { table_pos <- rvest::html_nodes(x = htmls,
                                                 xpath = '//table[contains(@style, "width: 324px")]') %>% 
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
                table_pos_death$new_death <- NA # no death table means 0 death
                return(table_pos_death) 
                
                ############# if special dates like 07/11, 07/12       
                }else if (nrow(table_s7)>0& nrow(table_1)==0){
                        table_pos <- rvest::html_nodes(x = htmls,
                                                       xpath = '//table[contains(@style, "width: 348px")]') %>% 
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
                        table_pos_death$new_death <- NA # no death table means 0 death
                        
                        return(table_pos_death)
                        ###############if only table 0 pos
                }else if(nrow(table_0)>0 & nrow(table_1)==0 & nrow(table_2)==0){
                        table_pos <- table_0
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
                        table_pos_death$new_death <- NA # no death table means 0 death
                        
                        return(table_pos_death)
                        ################if there is table 1 but no table 2. Regardless of table 0
                }else if(nrow(table_1)>0 & nrow(table_2)==0) {
                        table_pos<- table_1
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
                        table_pos_death$new_death <- NA # no death table means 0 death
                        
                        return(table_pos_death)
                        ################if there is table 1 (pos) and 2 (death)
                }else if (nrow(table_1)>0& nrow(table_2)>0) {
                        table_pos <- table_1
                        table_death<- table_2
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
                        # the death table
                        table_death <- rvest::html_nodes(x = htmls,
                                                         xpath = '//table[contains(@id, "docx4j_tbl_2")]') %>%
                                html_table(fill = TRUE) %>% .[1] %>% as.data.frame()
                        #recode row and column names
                        rownames(table_death) <- table_death[,1]
                        table_death <- table_death[3:nrow(table_death),]
                        colnames(table_death) <- c("county", "new_death")
                        table_death[,2] <- as.numeric(gsub(",", "", table_death[, 2]))
                        
                        
                        # Not including NYC boroughs' new deaths
                        # create a "not in" function
                        
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
                }
                
        }else {return(table_empty)}
}
table_630 <- list.cbind(list_630re)

press_release_06 <- data.frame(matrix(data = NA, 
                                      nrow = nrow(table_630)+2, ncol = ncol(table_630)+1))
press_release_06[3:nrow(press_release_06), 2:ncol(press_release_06)] <- table_630
colnames(press_release_06) <- c("county", colnames(table_630))
press_release_06[,1] <- c("","county",rownames(table_630))
press_release_06[2, 2:ncol(press_release_06)] <- colnames(table_630)
press_release_06[1,seq(2,ncol(press_release_06), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2020-06-01"):as.Date("2020-06-30")),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#

write_xlsx(press_release_06, path = "./output/press_06.xlsx")

# remove unnecessary objects
rm(list_630re, table_630)


