library(tidyr)
library(readxl)
library(dplyr)
busi <- read_xlsx("./Data/business.xlsx", skip = 2)
busi <- busi[,c(2,3,6)] %>% filter(동 != "합계") %>% filter(동 != "소계")

code <- read.csv("./Data/code.csv")

busi$paste <- paste(busi$자치구, busi$동, sep = " ")
busi[9,4] <- "종로구 종로5.6가동"
code$paste <- paste(code$자치구, code$행정동, sep = " ")

busi1 <- merge(busi, code, by = "paste")
busi1 <- busi1[,c(1,4,10)]
busi1 <- busi1 %>% separate(paste, c("구","동"), sep = " ")
busi1 <- busi1[,c(4,1,2,3)]
names(busi1) <- c("code","구","동","종사자")

#####################
#Process
library(dplyr)
library(tidyr)

store <- read.csv("./Data/store.csv")
code <- read.csv("./Data/code.csv")



store1 <- store
store1$paste <- paste(store$시군구명, store$행정동명, sep = " ")
code1 <- code
code1$paste <- paste(code$자치구, code$행정동, sep = " ")

store2 <- merge(store1, code1, by = "paste")
store2 <- store2[,-c(8,9,10,11,15,18,19,21)]
store2$code <- factor(store2$code)
write.csv(store2, "./Data/store2.csv")
############################################
source("./classify.csv",encoding = "utf-8")

classify("생활서비스","인력/고용/용역알선")
classify("생활서비스","이/미용/건강")
classify("생활서비스","세탁/가사서비스")
classify("생활서비스","물품기기대여")
classify("부동산","부동산중개")

부동산생활 <- 부동산
부동산생활$물품기기대여 <- 물품기기대여$물품기기대여
#
부동산생활$세탁가사서비스 <- 세탁가사서비스$TB
#
부동산생활$미용건강 <- 미용건강$TB
#
부동산생활$고용알선 <- 고용알선$TB

#
부동산생활큼직 <- 부동산
부동산생활큼직$생활 <- 물품기기대여$물품기기대여 + 세탁가사서비스$TB + 미용건강$TB + 고용알선$TB

###############################3
#노인복지
silver <- read_xlsx("./Data/동별노인복지.xlsx")
silver <- silver[,c(2,3,5)] %>% filter(동 != "합계") %>% filter(동 != "소계")
silver$시설수[silver$시설수 == "-"] <- 0

code <- read.csv("./Data/code.csv")

silver$paste <- paste(silver$자치구, silver$동, sep = " ")
silver[9,4] <- "종로구 종로5.6가동"
code$paste <- paste(code$자치구, code$행정동, sep = " ")

silver1 <- merge(silver, code, by = "paste")
silver1 <- silver1[,c(1,4,10)]
silver1 <- silver1 %>% separate(paste, c("구","동"), sep = " ")
silver1 <- silver1[,c(4,1,2,3)]
names(silver1) <- c("code","구","동","노인복지시설")
##############################################################
library(dplyr)
library(tidyr)
university <- read.csv("./Data/university.csv", stringsAsFactors = FALSE)
code <- read.csv("./Data/code.csv")
code$paste <- paste(code$자치구, code$행정동, sep = " ")


university <- university[,c(4,14,15)]

university[4,3] <- "면목3.8동"
university$paste <- paste(university$행정구, university$행정동, sep = " ")
university1 <- university %>% count(paste)

df <- merge(code, university1, by = "paste",all = TRUE)
df$n[is.na(df$n)] <- 0
df <- df[,c(1,7,9)] %>% separate(paste, c("구","동"), sep = " ")
df <- df[,c(3,1,2,4)]
names(df) <- c("code","구","동","대학교")
###############################################################
#청년인구비율 구하기
library(readxl)
pop <- read_xlsx("./Data/pop.xlsx")
pop$청년남자비율 <- pop$청년남자/pop$계
pop$청년여자비율 <- pop$청년여자/pop$계

head(pop)
pop <- edit(pop)

code <- read.csv("./Data/code.csv")

library(dplyr)
library(tidyr)
pop$paste <- paste(pop$자치구, pop$동, sep = " ")
code$paste <- paste(code$자치구, code$행정동, sep = " ")
pop1 <- merge(pop, code, by = "paste", all = TRUE)
pop2 <- pop1[,c(1,8,9,15)]
pop2 <- pop2 %>% separate(paste, c("gu","동"), sep = " ")
pop2 <- pop2[,c(2,1,5,3,4)]
pop2 <- pop2[,c(3,4,5)]

table <- read.csv("./Data/first_regression2.csv")
table <- merge(table, pop2, by = "code")
##집세까지
pay <- read.csv("./Data/집세까지.csv")
pay <- pay[,c(6,8)]

table <- merge(table, pay, by = "code")
###############################################
#범죄율까지
crime <- read_xlsx("./Data/crime.xlsx")
popul <- read_xlsx("./Data/동별1인가구수.xlsx")
popul <- popul[-1,c(2,3,4)]
popul <- popul %>% filter(행정동 != "소계")

crime1 <- merge(crime, popul, by = "자치구")
crime1$rate <- crime1$발생 / crime1$전체세대수
crime1$paste <- paste(crime1$자치구, crime1$행정동, sep = " ")
crime2 <- crime1[,c(6,7)]
crime3 <- merge(code, crime2, by = "paste")
crime4 <- crime3[,c(7,9)]
table2 <- merge(table, crime4, by = "code")