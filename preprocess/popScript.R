#서울 인구 데이터 정제하기
library(readxl)
library(dplyr)
df <- read_xlsx("./Data/pop.xlsx")
df <- df[,c(2,3,5,6,7)] %>% filter(행정동 != "합계") %>% filter(행정동 != "소계")

code <- read.csv("./Data/code.csv")

df$paste <- paste(df$자치구, df$행정동, sep = " ")
df[9,6] <- "종로구 종로5.6가동"
code$paste <- paste(code$자치구, code$행정동, sep = " ")

df1 <- merge(df, code, by = "paste")
df2 <- df1[,c(1,4,5,6)]
df3 <- merge(code, df2, by = "paste")
library(tidyr)
df3 <- df3 %>% separate(paste, c("구","동"), sep = " ")
df4 <- df3[,c(1,2,8,10,11,12)]

write.csv(df4, "./Data/pop.csv")
