# Editing for press release data Version 2 (V2)
# 11/20/2020
# Haoyu (Tophey) Zhou.

# the blue highlighted counties:
# Albany, Clinton, Essex, Franklyn, Warren.
# Total deaths on 11/03 as reference.
library(readxl)
ny_v2<- read_excel("./input/NY_press_releases_0320_to_1117_v2input.xlsx")
# the only difference of "NY_press_releases_0320_to_1117_v2input.xlsx" compared to
# the original V1 is the total deaths of the 5 NYC counties. 
# The total death 11/13 is replaced with 11/12 in the data entry sheet by James.

# now add the recently updated data from V1 (changed made on 12/04)
ny_v2[, (ncol(ny_v2)+1):ncol(ny_final_output)]<- ny_final_output[, (ncol(ny_v2)+1):ncol(ny_final_output)]

ny_v2<- as.data.frame(ny_v2)
rownames(ny_v2)<- rownames(ny_final)

ny_v2[1,2:ncol(ny_v2)]<- ny_final[1,]
# some corrections added to V1. Add them to v2input as well
match("2020-11-13", ny_v2[1,]) # =954

# ny_v2_output used later
ny_v2_output<- ny_v2

# ny_v2_pros for processing
ny_v2_pros<- ny_v2[c( "Albany", "Clinton", "Essex", "Franklin", "Warren"),2:ncol(ny_v2)]


#now change the total deaths accordingly
#"2020-11-13" is the 239th day in the dataset
for (i in 1:ncol(ny_v2_pros)){
        ny_v2_pros[,i] <- as.numeric(ny_v2_pros[,i])
}
# NA + or - renders NA. Thus,
ny_v2_pros[is.na(ny_v2_pros)]<- 0
for (i in 1:(ncol(ny_v2_pros)/4)){
        if (i < 238){ # < 2020-11-12
        ny_v2_pros[, 4*i-1]<- ny_v2_pros[,953+2]- 
                rowSums(ny_v2_pros[,seq(4*i+4, 953+3, 4)])
        }else if (i == 238){ #==2020-11-12
                ny_v2_pros[,4*i-1]<- ny_v2_pros[, 953+2]-ny_v2_pros[,953+3]
        }else if (i == 239){ #==2020-11-13
                ny_v2_pros[,4*i-1]<- ny_v2_pros[, 953+2]
        
        }else{
                ny_v2_pros[, 4*i-1]<- ny_v2_pros[,953+2]+ 
                        rowSums(ny_v2_pros[,seq(953+3,4*i, 4)])
        }
        
}
ny_v2_pros[ny_v2_pros==0]<- NA
# replace the rows of V1
ny_v2_output["Albany",2:ncol(ny_v2_output)]<- ny_v2_pros["Albany",]
ny_v2_output["Clinton",2:ncol(ny_v2_output)]<- ny_v2_pros["Clinton",]
ny_v2_output["Essex",2:ncol(ny_v2_output)]<- ny_v2_pros["Essex",]
ny_v2_output["Franklin",2:ncol(ny_v2_output)]<- ny_v2_pros["Franklin",]
ny_v2_output["Warren",2:ncol(ny_v2_output)]<- ny_v2_pros["Warren",]

# correct the last zeros mistakenly introduced to Non-NYs in th V1
ny_v2_output[nrow(ny_v2_output), (ncol(ny_v2_output)-15):ncol(ny_v2_output)]<- NA

## Change made on 02/15/2021. 
# Purpose: to reset the total deaths of Non-NYC counties' on 02/13/20221 
#by using the total death data of 02/12/2021 from the data entry file as a reference.
#(Same thing as when the total death column was created. But only forward, no backward this time)
ny_death2<- as.data.frame(read_excel("./input/ny_death_#entry_reset.xlsx", col_names = T))
# the syntax below is to be improved. Needs better automation
## add new dates for resetting
colnames(ny_death2)[2:ncol(ny_death2)] <- 
        c("2021-02-13", "2021-03-19", "2021-04-03", "2021-04-18", 
          "2021-04-30", "2021-05-15", "2021-05-29", "2021-06-11", "2021-06-26",
          "2021-07-09")
ny_death2[3:(nrow(ny_death2)+2),] <- ny_death2[, ]
ny_death2[1:2,]<- ""
ny_death2<- ny_death2[ny_death2[,1] %notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"), ]
ny_death2[2,2:ncol(ny_death2)]<- "total_death"
# replace the Non-NYC counties' deaths directly
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-02-13", ny_v2_output[1,]))+2] <-
                ny_death2[, match("2021-02-13", colnames(ny_death2))]

# 02/12/2021 reset the total deaths of Non_NYC counties
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
match("2021-02-13", ny_v2_output[1,]) # 1322 in ny_v2_output, 1321 in ny_v2_temp
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-02-13", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # for 2021-02-14, because rowSums does not support adding up only two columns
        if (i == (match("2021-02-13", ny_v2_output[1,])+2+4)/4){
                ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),2])+ny_v2_temp[,i*4]
        }
        else{
                ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),2])+
                        rowSums(ny_v2_temp[seq(match("2021-02-13", ny_v2_output[1,])+2+4, 
                                                 i*4,4)])
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
        c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
                ((match("2021-02-13", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
                        ny_v2_temp[,((match("2021-02-13", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## Note date: 03/21/2021. Reset total deaths of non-NYC counties', starting from 03/19/2021
# use data of 03/18/2021 in the data entry sheet as a reference for 03/19/2021 in ny_v2
# replace the Non-NYC counties' deaths directly
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-03-19", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-03-19", colnames(ny_death2))]

# 03/19/2021 reset the total deaths of Non_NYC counties after 03/19/2021
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-03-19", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # for 2021-03-20
        if (i == (match("2021-03-19", ny_v2_output[1,])+2+4)/4){
                ny_v2_temp[,i*4-1]<- 
                        as.numeric(ny_death2[3:nrow(ny_death2),match("2021-03-19", colnames(ny_death2))])+ny_v2_temp[,i*4]
        }
        else{
                ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),match("2021-03-19", colnames(ny_death2))])+
                        rowSums(ny_v2_temp[seq(match("2021-03-19", ny_v2_output[1,])+2+4, 
                                               i*4,4)])
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-03-19", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-03-19", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)


## 04/03/2021. Reset the total deaths of Non_NYC counties after 04/03/2021
# using data of data entry sheet on 04/02/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-04-03", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-04-03", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-04-03", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # for 2021-04-03
        if (i == (match("2021-04-03", ny_v2_output[1,])+2+4)/4){
                ny_v2_temp[,i*4-1]<- 
                        as.numeric(ny_death2[3:nrow(ny_death2),match("2021-04-03", colnames(ny_death2))])+ny_v2_temp[,i*4]
        }
        else{
                ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),match("2021-04-03", colnames(ny_death2))])+
                        rowSums(ny_v2_temp[seq(match("2021-04-03", ny_v2_output[1,])+2+4, 
                                               i*4,4)])
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-04-03", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-04-03", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 04/18/2021. Reset the total deaths of Non_NYC counties after 04/18/2021
# using data of data entry sheet on 04/17/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-04-18", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-04-18", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-04-18", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-04-19:
        if (match("2021-04-18", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-04-19, because rowSums() does not work for combining two elements
                if (i == (match("2021-04-18", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-04-18", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-04-19
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-04-18", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-04-18", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-04-18", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-04-18", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 05/02/2021. Reset the total deaths of Non_NYC counties after 04/30/2021
# using data of data entry sheet on 04/30/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-04-30", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-04-30", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-04-30", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # for 2021-04-31
        if (i == (match("2021-04-30", ny_v2_output[1,])+2+4)/4){
                ny_v2_temp[,i*4-1]<- 
                        as.numeric(ny_death2[3:nrow(ny_death2),match("2021-04-30", colnames(ny_death2))])+ny_v2_temp[,i*4]
        }
        else{
                ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),match("2021-04-30", colnames(ny_death2))])+
                        rowSums(ny_v2_temp[seq(match("2021-04-30", ny_v2_output[1,])+2+4, 
                                               i*4,4)])
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-04-30", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-04-30", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 05/16/2021. Reset the total deaths of Non_NYC counties after 05//2021
# using data of data entry sheet on 05/15/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-05-15", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-05-15", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-05-15", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-04-19:
        if (match("2021-05-15", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-05-16, because rowSums() does not work for combining two elements
                if (i == (match("2021-05-15", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-05-15", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-04-19
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-05-15", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-05-15", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-05-15", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-05-15", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)
#########
ny_v2_copy <- ny_v2_output

## 05/29/2021. Reset the total deaths of Non_NYC counties after (including)  05/29/2021
# using data of data entry sheet on 05/28/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-05-29", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-05-29", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-05-29", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-05-30:
        if (match("2021-05-29", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-05-30, because rowSums() does not work for combining two elements
                if (i == (match("2021-05-29", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-05-29", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-05-30
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-05-29", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-05-29", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-05-29", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-05-29", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 06/11/2021. Reset the total deaths of Non_NYC counties after (including)  06/11/2021
# using data of data entry sheet on 06/10/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-06-11", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-06-11", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-06-11", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-05-30:
        if (match("2021-06-11", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-06-11, because rowSums() does not work for combining two elements
                if (i == (match("2021-06-11", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-06-11", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-05-30
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-06-11", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-06-11", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-06-11", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-06-11", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 06/26/2021. Reset the total deaths of Non_NYC counties after (including) 06/26/2021
# using data of data entry sheet on 06/25/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-06-26", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-06-26", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-06-26", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-05-30:
        if (match("2021-06-26", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-06-27, because rowSums() does not work for combining two elements
                if (i == (match("2021-06-26", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-06-26", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-05-30
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-06-26", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-06-26", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-06-26", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-06-26", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

## 07/09/2021. Reset the total deaths of Non_NYC counties after (including) 07/09/2021
# using data of data entry sheet on 07/08/2021 as a reference
ny_v2_output[rownames(ny_v2_output)%notin% c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             (match("2021-07-09", ny_v2_output[1,]))+2] <-
        ny_death2[, match("2021-07-09", colnames(ny_death2))]

# prepare a temp for loops
ny_v2_temp<- ny_v2_output[3:nrow(ny_v2_output), 2:ncol(ny_v2_output)]
ny_v2_temp[is.na(ny_v2_temp)]<- 0
ny_v2_temp<-ny_v2_temp[rownames(ny_v2_temp) %notin% 
                               c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),]
# iterate over the total deaths
#ny_v2_temp<- as.data.frame(lapply(ny_v2_temp, as.numeric))
for (i in 1:ncol(ny_v2_temp)){
        ny_v2_temp[,i]<- as.numeric(ny_v2_temp[,i])
}
for (i in ((match("2021-07-09", ny_v2_output[1,])+2+4)/4):(ncol(ny_v2_temp)/4)){
        # (To make the code more robust). If ny_v2_temp already has data of 2021-05-30:
        if (match("2021-07-09", ny_v2_output[1,])+2+4 <= ncol(ny_v2_temp)) {
                # for 2021-06-27, because rowSums() does not work for combining two elements
                if (i == (match("2021-07-09", ny_v2_output[1,])+2+4)/4){
                        ny_v2_temp[,i*4-1]<- 
                                as.numeric(ny_death2[3:nrow(ny_death2), 
                                                     match("2021-07-09", colnames(ny_death2))])+ny_v2_temp[,i*4]
                        
                        # for data after (not including) 2021-05-30
                } else {
                        ny_v2_temp[,i*4-1]<- as.numeric(ny_death2[3:nrow(ny_death2),
                                                                  match("2021-07-09", colnames(ny_death2))])+
                                rowSums(ny_v2_temp[seq(match("2021-07-09", ny_v2_output[1,])+2+4, i*4,4)])
                }
        }
}
ny_v2_temp[ny_v2_temp==0]<- NA

# now replace the part in ny_v2_output with ny_v2_temp
ny_v2_temp[3:(nrow(ny_v2_temp)+2),]<- ny_v2_temp
ny_v2_temp[1:2,]<- ny_v2_output[1:2,2:ncol(ny_v2_output)]
ny_v2_output[rownames(ny_v2_output)%notin% 
                     c("Bronx", "Kings", "New York County", "NYC", "Queens", "Richmond"),
             ((match("2021-07-09", ny_v2_output[1,]))+2):ncol(ny_v2_output)] <-
        ny_v2_temp[,((match("2021-07-09", ny_v2_temp[1,]))+2):ncol(ny_v2_temp)]
rm(ny_v2_temp)

rm(ny_death2)# to make sure ny_death2 is always re-entered when you add a new date for resetting

# export data
write_xlsx(ny_v2_output, "./output/NY_press_releases_0320_to_present_V2.xlsx")

