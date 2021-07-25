# Scrape NY Press Release
# 04/01/2021 - 04/30/2021

# define the function used below
"%notin%" <- Negate("%in%")

# determing the number of cpu cores used for foreach command below
numcores<- detectCores()
#cl<-makeCluster(numcores-2) 
registerDoParallel(numcores-2)
list_0430_21 <- foreach(his_date = 1:length(url_0430_21), combine = cbind, .verbose = T) %dopar% {#test
        urls<- url_0430_21[his_date] 
        if (!is.na(urls) & press_0430_21$table[his_date] == "Yes") { ## Level 1 if there are table(s)
                htmls <- read_html(urls, encoding = "ISO-8859-1")
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
table_0430_21 <- list.cbind(list_0430_21)

# export press releases 
# add the date row. press_release_ is just for exporting
press_release_04_2021 <- data.frame(matrix(data = NA, 
                                           nrow = nrow(table_0430_21)+2, ncol = ncol(table_0430_21)+1))
press_release_04_2021[3:nrow(press_release_04_2021), 2:ncol(press_release_04_2021)] <- table_0430_21
colnames(press_release_04_2021) <- c("county", colnames(table_0430_21))
press_release_04_2021[,1] <- c("","county",rownames(table_0430_21))
press_release_04_2021[2, 2:ncol(press_release_04_2021)] <- colnames(table_0430_21)
press_release_04_2021[1,seq(2,ncol(press_release_04_2021), 3)] <- 
        as.character(as.Date(
                as.numeric(as.Date("2021-04-01"):as.Date(press_0430_21$date[nrow(press_0430_21)])),format = "%Y-%m-%d",
                origin = as.Date("2020-03-20")-18341))
#write the data for reference
write_xlsx(press_release_04_2021, path = "./output/press_04_2021.xlsx")

# remove unnecessary objects
rm(list_0430_21, table_0430_21)


