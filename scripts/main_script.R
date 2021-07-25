#The main file.
#NY Press Release of COVID-19 data.
#Web scraping and data processing.
# Created by Haoyu (Tophey) Zhou. 
# Starting on 07/09/2021, main maintainer: Katrina Paleologos
# For questions, email Tophey at hz1954@nyu.edu

# setting up folders
if (!dir.exists("./output")) {
        dir.create("./output")
}
if (!dir.exists("./input")) {
        dir.create("./input")
}

# load packages
library(tidyverse)
library(rvest)
library(xml2)
library(doParallel)
library(rlist)
library("writexl")
library(readxl)

# We do each month's work for easy maintenance. 
## Here we get real.##############################################################################
# read the url link list
ny_press <- read_excel("./input/New York Press Releases.xlsx")

# delete unnecessary columns, and rename column names
ny_press <- ny_press[,1:3]
colnames(ny_press) <- c("date", "link", "table")
# drop rows wthout links
#ny_press <- ny_press[!is.na(ny_press$link),]
# format the date column
ny_press$date<- as.character(ny_press$date)
## separate links to each month
press_430<- ny_press[ny_press$date <= "2020-04-30",]
press_531<- ny_press[ny_press$date > "2020-04-30" & ny_press$date <= "2020-05-31",]
press_731 <- ny_press[ny_press$date > "2020-05-31" & ny_press$date <= "2020-07-31",]
press_930<- ny_press[ny_press$date > "2020-07-31" & ny_press$date <= "2020-09-30",] 
press_1031 <- ny_press[ny_press$date > "2020-09-30" & ny_press$date <= "2020-10-31",] 
press_1130 <- ny_press[ny_press$date > "2020-10-31" & ny_press$date <= "2020-11-30",] 
press_0131<- ny_press[ny_press$date > "2020-11-30" & ny_press$date <= "2021-01-31",] 
press_0228<- ny_press[ny_press$date > "2021-01-31" & ny_press$date <= "2021-02-28",] 
press_0331_21<- ny_press[ny_press$date > "2021-02-28" & ny_press$date <= "2021-03-31",] 
press_0430_21<- ny_press[ny_press$date > "2021-03-31" & ny_press$date <= "2021-04-30",] 
press_0531_21<- ny_press[ny_press$date > "2021-04-30" & ny_press$date <= "2021-05-31",] 
press_0630_21<- ny_press[ny_press$date > "2021-05-31" & ny_press$date <= "2021-06-30",] 
press_0731_21<- ny_press[ny_press$date > "2021-06-30" & ny_press$date <= "2021-07-31",] 

# Test whether the numbers of rows add up.
nrow(press_430) + nrow(press_531) +nrow(press_731)+nrow(press_930) + 
        nrow(press_1031) + nrow(press_1130) + nrow(press_0131) +nrow(press_0228) + 
        nrow(press_0331_21) + nrow(press_0430_21) + nrow(press_0531_21) + 
        nrow(press_0630_21) + nrow(press_0731_21)== nrow(ny_press)
# save only the links
url_430 <- press_430$link
url_531 <- press_531$link
url_731 <- press_731$link
url_930 <- press_930$link
url_1031 <- press_1031$link
url_1130 <- press_1130$link
url_0131 <- press_0131$link
url_0228<- press_0228$link
url_0331_21 <- press_0331_21$link
url_0430_21 <- press_0430_21$link
url_0531_21 <- press_0531_21$link
url_0630_21 <- press_0630_21$link
url_0731_21 <- press_0731_21$link


#create an empty table for the NAs (dates without data tables)
table_empty<- data.frame(matrix(NA, nrow = 59, ncol = 3))
rownames(table_empty) <- 
        c("Albany", "Allegany", "Broome", "Cattaraugus", "Cayuga", 
          "Chautauqua", "Chemung", "Chenango", "Clinton", "Columbia", 
          "Cortland", "Delaware", "Dutchess", "Erie", "Essex", 
          "Franklin", "Fulton", "Genesee", "Greene", "Hamilton", 
          "Herkimer", "Jefferson", "Lewis", "Livingston", "Madison",     
          "Monroe", "Montgomery", "Nassau", "Niagara", "NYC",     
          "Oneida", "Onondaga", "Ontario", "Orange", "Orleans",      
          "Oswego", "Otsego", "Putnam", "Rensselaer", "Rockland",    
          "Saratoga", "Schenectady",  "Schoharie", "Schuyler", "Seneca",      
          "St. Lawrence", "Steuben", "Suffolk", "Sullivan", "Tioga",       
          "Tompkins", "Ulster", "Warren", "Washington", "Wayne",       
          "Westchester", "Wyoming", "Yates", "Non-NYs")
colnames(table_empty) <- c("total_positive", "new_positive", "new_death")


# Due to the unfixed code in scrape_05_re.R, we now load the data and
# rebuild release_03, release_05 instead. (the data were saved previously)
press_release_03 <- as.data.frame(read_excel("./output/press_03.xlsx"))
press_release_05 <- as.data.frame(read_excel("./output/press_05.xlsx"))

### Switch point 1. Switch to the script for the month you are working on.
## Back here after individual months' data is scraped and edited.
## Data Combining. Combine all the tables and export it
# "release" is the name of the tables for combining data
release_03 <- press_release_03[, 2:ncol(press_release_03)]
colnames(release_03) <- colnames(press_release_03)[2:length(press_release_03)]
rownames(release_03) <- c("1",press_release_03[2:nrow(press_release_03),1])
release_03[3:nrow(release_03),3] <-NA 

release_04 <- press_release_04[, 2:ncol(press_release_04)]
colnames(release_04) <- colnames(press_release_04)[2:length(press_release_04)]
rownames(release_04) <- press_release_04[,1]

release_05 <- press_release_05[, 2:ncol(press_release_05)]
colnames(release_05) <- colnames(press_release_05)[2:length(press_release_05)]
rownames(release_05) <- c("1",press_release_05[2:nrow(press_release_05),1])

release_06 <- press_release_06[, 2:ncol(press_release_06)]
colnames(release_06) <- colnames(press_release_06)[2:length(press_release_06)]
rownames(release_06) <- press_release_06[,1]

release_07 <- press_release_07[, 2:ncol(press_release_07)]
colnames(release_07) <- colnames(press_release_07)[2:length(press_release_07)]
rownames(release_07) <- press_release_07[,1]

release_08 <- press_release_08[, 2:ncol(press_release_08)]
colnames(release_08) <- colnames(press_release_08)[2:length(press_release_08)]
rownames(release_08) <- press_release_08[,1]

release_09 <- press_release_09[, 2:ncol(press_release_09)]
colnames(release_09) <- colnames(press_release_09)[2:length(press_release_09)]
rownames(release_09) <- press_release_09[,1]

release_10 <- press_release_10[, 2:ncol(press_release_10)]
colnames(release_10) <- colnames(press_release_10)[2:length(press_release_10)]
rownames(release_10) <- press_release_10[,1]

release_11 <- press_release_11[, 2:ncol(press_release_11)]
colnames(release_11) <- colnames(press_release_11)[2:length(press_release_11)]
rownames(release_11) <- press_release_11[,1]

#release_01_2021 includes 12/2020 and 01/2021
release_01_2021 <- press_release_01_2021[, 2:ncol(press_release_01_2021)]
colnames(release_01_2021) <- colnames(press_release_01_2021)[2:length(press_release_01_2021)]
rownames(release_01_2021) <- press_release_01_2021[,1]

#release_02_2021 includes 12/2020 and 01/2021
release_02_2021 <- press_release_02_2021[, 2:ncol(press_release_02_2021)]
colnames(release_02_2021) <- colnames(press_release_02_2021)[2:length(press_release_02_2021)]
rownames(release_02_2021) <- press_release_02_2021[,1]

#release_03_2021 includes data 03/01/2021 to 03/31/2021
release_03_2021 <- press_release_03_2021[, 2:ncol(press_release_03_2021)]
colnames(release_03_2021) <- colnames(press_release_03_2021)[2:length(press_release_03_2021)]
rownames(release_03_2021) <- press_release_03_2021[,1]

#release_04_2021 includes data 04/01/2021 to 04/30/2021
release_04_2021 <- press_release_04_2021[, 2:ncol(press_release_04_2021)]
colnames(release_04_2021) <- colnames(press_release_04_2021)[2:length(press_release_04_2021)]
rownames(release_04_2021) <- press_release_04_2021[,1]

#release_05_2021 includes data 05/01/2021 to 05/31/2021
release_05_2021 <- press_release_05_2021[, 2:ncol(press_release_05_2021)]
colnames(release_05_2021) <- colnames(press_release_05_2021)[2:length(press_release_05_2021)]
rownames(release_05_2021) <- press_release_05_2021[,1]

#release_06_2021 includes data 05/01/2021 to 05/31/2021
release_06_2021 <- press_release_06_2021[, 2:ncol(press_release_06_2021)]
colnames(release_06_2021) <- colnames(press_release_06_2021)[2:length(press_release_06_2021)]
rownames(release_06_2021) <- press_release_06_2021[,1]

#release_07_2021 includes data 07/01/2021 to 07/31/2021
release_07_2021 <- press_release_07_2021[, 2:ncol(press_release_07_2021)]
colnames(release_07_2021) <- colnames(press_release_07_2021)[2:length(press_release_07_2021)]
rownames(release_07_2021) <- press_release_07_2021[,1]

#combine data all the months
release_all <- cbind(release_03,release_04) %>% 
        cbind(release_05) %>%
        cbind(release_06) %>% 
        cbind(release_07) %>% 
        cbind(release_08) %>% 
        cbind(release_09) %>% 
        cbind(release_10) %>% 
        cbind(release_11) %>% 
        cbind(release_01_2021) %>% 
        cbind(release_02_2021) %>% 
        cbind(release_03_2021) %>% 
        cbind(release_04_2021) %>% 
        cbind(release_05_2021) %>% 
        cbind(release_06_2021) %>% 
        cbind(release_07_2021)

 #re-add the column names in the 1st column, for excel file does not show rownames
qa_03_12 <- data.frame(matrix(data = NA, nrow = nrow(release_all), ncol = ncol(release_all)+1))
qa_03_12[,2:ncol(qa_03_12)]<- release_all
qa_03_12[,1] <- rownames(release_all)

write_xlsx(qa_03_12, path = "./output/backupdata/NY_press_0320_to_present_nodeath.xlsx", col_names = F)

######
# adding the total death column
# death_ref is the confirmed mortality on 09/15, from the data entry sheet
death_ref<- read_excel("./input/ny_death_#entry.xlsx")
colnames(death_ref) <- (c("county", "death"))
death_ref<- as.data.frame(death_ref)
rownames(death_ref) <- substr(death_ref[,1], start = 1, stop = nchar(death_ref[,1])-17)
rownames(death_ref)[nrow(death_ref)]<- "unknown"
rownames(death_ref)[32]<- "NYC"
death_ref<- death_ref[rownames(death_ref)%notin% c("Bronx","Brooklyn", "Manhattan", "Staten Island", "Richmond", "Queens", "Kings", "New York"),]

#check county names
rownames(death_ref)==rownames(release_all)[3:nrow(release_all)]
rownames(death_ref)[41:59]
rownames(release_all)[43:61]
rownames(release_all[rownames(release_all)%notin%rownames(death_ref),])
# the only difference is the location of "St. Lawrence"
# change the order of release_all, not death_ref
stlawrance<- release_all[rownames(release_all)=="St. Lawrence",]

match("St. Lawrence", rownames(release_all))
release_all[(match("St. Lawrence", rownames(release_all))-4):match("St. Lawrence", rownames(release_all)),]<-
        release_all[(match("St. Lawrence", rownames(release_all))-5):(match("St. Lawrence", rownames(release_all))-1),]
release_all[match("St. Lawrence", rownames(release_all))-5,]<- stlawrance
rownames(release_all)[(match("St. Lawrence", rownames(release_all))-5):
                              match("St. Lawrence", rownames(release_all))]<-
        c("St. Lawrence","Saratoga","Schenectady","Schoharie","Schuyler","Seneca")

rownames(release_all)[31:32]
rownames(death_ref)[29:30]

nyc<- release_all[32,]
release_all[32,]<- release_all[31,]
release_all[31,]<- nyc
rownames(release_all)[31:32]<- c("NYC", "Niagara")
#recheck rownames
sum(rownames(release_all)[3:nrow(release_all)]==rownames(death_ref)) # all good

# add a total_death colomun for each day. cal it day_all
day_all<- as.data.frame(matrix(NA, nrow = nrow(release_all), ncol = ncol(release_all)+1/3*ncol(release_all)))
rownames(day_all)<- rownames(release_all)
day_all[1, seq(1, ncol(day_all), 4)]<- as.character(as.Date(
        as.numeric(as.Date("2020-03-20"):as.Date(ny_press$date[nrow(ny_press)])),format = "%Y-%m-%d",
        origin = as.Date("2020-03-20")-18341))



day_all[2,] <- rep(c("total_positive", "new_positive", "total_death", "new_death"),ncol(day_all)/4)
# add data in releas_all to day_all
day_all[, 1:ncol(day_all)%notin% seq(3, ncol(day_all), 4)] <- release_all

# the location of "2020-09-15" in dat_all
match("2020-09-15", day_all[1,])
day_all[3:nrow(day_all),717+2]<- death_ref[,2]# 09/15 as the reference date
# add the total deaths for the columns before and after 2020-09-15
death_other<- as.data.frame(matrix(data = NA, nrow = nrow(day_all), ncol = ncol(day_all)/4))
rownames(death_other) <- rownames(day_all)
death_other[1,]<- as.character(as.Date(
        as.numeric(as.Date("2020-03-20"):as.Date(ny_press$date[nrow(ny_press)])),format = "%Y-%m-%d",
        origin = as.Date("2020-03-20")-18341))
match("2020-09-15", death_other[1,])# =180

# change all NA in day_all in death into 0 so thatt subtraction works and sum work
day_all_copy<- as.data.frame(sapply(day_all[3:nrow(day_all),], as.numeric))
day_all_copy[is.na(day_all_copy)] <- 0
# day_all_copy does not have the first 2 rows: dates and pos,death..

for (i in 1:ncol(death_other)){
        if (i<179){
                death_other[3:nrow(death_other), i]<- 
                        day_all_copy[, 717+2]- 
                        rowSums(day_all_copy[, seq(4*i+4, 717+3, 4) ])
        }else if(i == 179){
                death_other[3:nrow(death_other), i]<- 
                        day_all_copy[, 717+2]-
                        day_all_copy[, 717+3]
        }else if (i>181){
                death_other[3:nrow(death_other), i]<- 
                        day_all_copy[, 717+2]+ 
                        rowSums(day_all_copy[, seq(717+3+4, 4*i, 4) ]) 
        }else if(i==181) {
                death_other[3:nrow(death_other), i] <- 
                        rowSums(day_all_copy[, c(717+2,181*4)])
        }else{
                death_other[3:nrow(death_other),i]<- day_all_copy[, i*4-1]
        }
        
}
# replace the 0s with NA
death_other[death_other==0] <- NA

# now insert the death_other columns into day_all
for (i in 1:ncol(death_other)){
        day_all[3:(nrow(day_all)-1), 4*i-1]<- death_other[3:(nrow(death_other)-1),i]
}

# day_all is complete with the total deaths. N
#now add the empty rows in the data entry sheet
rm(day_all_copy)
ny_final<- as.data.frame(matrix(data = NA, nrow = 66, ncol = ncol(day_all)))
rownames(ny_final)[1:4]<- rownames(day_all)[1:4]
# bronx, kings, new york county, nassau, queens, 
match("Jefferson", rownames(day_all))
match("Nassau", rownames(day_all))
match("Putnam", rownames(day_all))
match("Rensselaer", rownames(day_all))
rownames(ny_final)[c(5, 26, 33, 44, 46)]<- c("Bronx", "Kings", "New York County", "Queens", "Richmond")
rownames(ny_final)[6:25]<- rownames(day_all)[5:24]
rownames(ny_final)[27:32]<- rownames(day_all)[25:30]
rownames(ny_final)[34:43]<- rownames(day_all)[31:40]
rownames(ny_final)[45]<- rownames(day_all)[41]
rownames(ny_final)[47:65]<- rownames(day_all)[42:60]
rownames(ny_final)[66]<- "Non-NYs"

ny_final[1:4,]<- day_all[1:4,]
ny_final[6:25,]<- day_all[5:24,]
ny_final[27:32,]<- day_all[25:30,]
ny_final[34:43,]<- day_all[31:40,]
ny_final[45,]<- day_all[41,]
ny_final[47:66,]<- day_all[42:61,]
####################################################################
### Switch point 2. Switch to new_death_nyc.R.
# Finish scraping the death numbers of NYC counties
# Then come back here and run the code below.
# replace the empty rows of nyc counties with nyc_death_insert
ny_final["Bronx", ]<- nyc_death_insert["Bronx",]
ny_final["Kings", ]<- nyc_death_insert["Kings",]
ny_final["New York County", ]<- nyc_death_insert["New York County",]
ny_final["Queens", ]<- nyc_death_insert["Queens",]
ny_final["Richmond", ]<- nyc_death_insert["Richmond",]

## add total deaths of nyc counties after 11/12
# 11/12 data from the data entry sheet as reference
ref_nyc_death <- c(3410, 5160, 2132, 5154, 766)
match("2020-11-13", death_other[1,])# 239
ny_final["Bronx", (239*4-1)]<- ref_nyc_death[1]
ny_final["Kings", (239*4-1)]<- ref_nyc_death[2]
ny_final["New York County", (239*4-1)]<- ref_nyc_death[3]
ny_final["Queens", (239*4-1)]<- ref_nyc_death[4]
ny_final["Richmond", (239*4-1)]<- ref_nyc_death[5]

# because 1+NA = NA, but we want it to be 1
ny_final_copy<- ny_final[3:nrow(ny_final),]
ny_final_copy[is.na(ny_final_copy)]<- 0
for (i in 1:ncol(ny_final_copy)){
        ny_final_copy[,i]<- as.numeric(ny_final_copy[,i])
}

for (i in 240:(ncol(ny_final)/4)){
        if (i==240){
                ny_final[3:nrow(ny_final), 4*i-1]<- 
                        as.numeric(ny_final_copy[,(239*4-1)]) + 
                        as.numeric(ny_final_copy[, 240*4])
        }else{
                ny_final[3:nrow(ny_final), 4*i-1]<- 
                        rowSums(ny_final_copy[, c((239*4-1),seq(240*4, 4*i, 4))])
                
        }
}
rm(ny_final_copy)

# some zeros mistakenly introduced into ny_final due to the NA and 0 conversion
# in the row of Non-NYs
ny_final[nrow(ny_final), match("2020-11-14", ny_final[1,]):ncol(ny_final)]<- 
        ifelse(0, NA, ny_final[nrow(ny_final), match("2020-11-14", ny_final[1,]):ncol(ny_final)])
ny_final_output<- as.data.frame(matrix(data = NA, nrow = nrow(ny_final), ncol = ncol(ny_final)+1))
ny_final_output[, 2:ncol(ny_final_output)]<- ny_final
ny_final_output[,1]<- rownames(ny_final)
write_xlsx(ny_final_output, "./output/backupdata/NY_press_0320_to_present_V1.xlsx")

# V1 was used for a while. More updates were added to the methodology later 
# and V2 came to use instead.











