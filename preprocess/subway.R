#Subway
library(readxl)
subway <- read_xlsx("./Data/subway.xlsx")
code <- read.csv("./Data/code.csv")
store <- read.csv("./Data/store.csv")
library(dplyr)
library(lubridate)
subway <- subway[,c(5,8,9,10)]
subway1 <- subway[!duplicated(subway$지하철역명),]
names(subway1) <- c("지하철역명","시도","구군","법정동명")
subway1 <- subway1[subway1$시도 == "서울특별시",]

store <- store[,c(7,8,9)]
store$paste <- paste(store$시군구명, store$행정동명, sep = " ")
store <- store[,c(3,4)]
store1 <- store[!duplicated(store),]

subway2 <- merge(x = subway1, y = store1, by = "법정동명", all.x = TRUE)
TB <- table(subway2$paste)
paste <- names(table(subway2$paste))
DF <- data_frame(paste = paste, x = TB)

code <- code[,c(2,3,6)]
code$paste <- paste(code$자치구, code$행정동)

subway3 <- merge(x = code, y = DF, by = "paste", all.x = TRUE)
subway3$x[is.na(subway3$x)] <- 0
library(tidyr)
subway3 <- subway3 %>% separate(paste, c("구","동"), sep = " ")
subway3 <- subway3[,c(5,1,2,6)]
names(subway3) <- c("code","구","동","인접지하철")
write.csv(subway3, "subway.csv")
