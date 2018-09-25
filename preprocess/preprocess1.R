############################################
#큰 카테고리 만들기
table4 <- read.csv("./Data/first_regression4.csv")
a <- (table4$police - min(table4$police)) / (max(table4$police) - min(table4$police)) * 100
b <- (table4$소방서 - min(table4$소방서)) / (max(table4$소방서) - min(table4$소방서)) * 100
c <- (table4$의료 - min(table4$의료)) / (max(table4$의료) - min(table4$의료)) * 100
table4$편의시설 <- (a + b + c)/100
table5 <- table4[,c(2,4,5,26,24,25,27,28,29,14,12,16,15)]
names(table5) <- c("code","동","구","집세","청년남자비율","청년여자비율","범죄안전","교통안전",
                  "편의시설","쇼핑시설","외식시설","대중교통","체육시설")
write.csv(table5, "./Data/first_regression5.csv", row.names = FALSE)
############################################
#0~100 2%아웃라이어 정규화
table <- read.csv("./Data/first_regression5.csv")

myfun <- function(vec){
    a <- sort(vec,decreasing = TRUE)[length(vec) * 0.02]
    vec[vec > a] <- 100
    vec[vec <= a] <- (vec[which(vec <= a)] - min(vec[which(vec <= a)])) / 
        (max(vec[which(vec <= a)]) - min(vec[which(vec <= a)])) * 100
    vec
}
table1 <- table
table1$집세 <- myfun(table1$집세)
table1$청년남자비율 <- myfun(table1$청년남자비율)
table1$청년여자비율 <- myfun(table1$청년여자비율)
table1$범죄안전 <- myfun(table1$범죄안전)
table1$교통안전 <- myfun(table1$교통안전)
table1$편의시설 <- myfun(table1$편의시설)
table1$쇼핑시설 <- myfun(table1$쇼핑시설)
table1$외식시설 <- myfun(table1$외식시설)
table1$대중교통 <- myfun(table1$대중교통)
table1$체육시설 <- myfun(table1$체육시설)

write.csv(table1, "./Data/first_regression_2rm.csv", row.names = FALSE)
##############################################
#0~100 4%아웃라이어 정규화
table <- read.csv("./Data/first_regression5.csv")

myfun1 <- function(vec){
    a <- sort(vec,decreasing = TRUE)[length(vec) * 0.04]
    vec[vec > a] <- 100
    vec[vec <= a] <- (vec[which(vec <= a)] - min(vec[which(vec <= a)])) / 
        (max(vec[which(vec <= a)]) - min(vec[which(vec <= a)])) * 100
    vec
}
table1 <- table
table1$집세 <- myfun1(table1$집세)
table1$청년남자비율 <- myfun1(table1$청년남자비율)
table1$청년여자비율 <- myfun1(table1$청년여자비율)
table1$범죄안전 <- myfun1(table1$범죄안전)
table1$교통안전 <- myfun1(table1$교통안전)
table1$편의시설 <- myfun1(table1$편의시설)
table1$쇼핑시설 <- myfun1(table1$쇼핑시설)
table1$외식시설 <- myfun1(table1$외식시설)
table1$대중교통 <- myfun1(table1$대중교통)
table1$체육시설 <- myfun1(table1$체육시설)

write.csv(table1, "./Data/first_regression_4rm.csv", row.names = FALSE)
####################################################
#-3,3 늘려서 정규화
table <- read.csv("./Data/first_regression5.csv")

table1 <- table
table1$집세 <- as.vector(scale(table1$집세) * 50/3 + 50)
table1$청년남자비율 <- as.vector(scale(table1$청년남자비율) * 50/3 + 50)
table1$청년여자비율 <- as.vector(scale(table1$청년여자비율) * 50/3 + 50)
table1$범죄안전 <- as.vector(scale(table1$범죄안전) * 50/3 + 50)
table1$교통안전 <- as.vector(scale(table1$교통안전) * 50/3 + 50)
table1$편의시설 <- as.vector(scale(table1$편의시설) * 50/3 + 50)
table1$쇼핑시설 <- as.vector(scale(table1$쇼핑시설) * 50/3 + 50)
table1$외식시설 <- as.vector(scale(table1$외식시설) * 50/3 + 50)
table1$대중교통 <- as.vector(scale(table1$대중교통) * 50/3 + 50)
table1$체육시설 <- as.vector(scale(table1$체육시설) * 50/3 + 50)

write.csv(table1, "./Data/first_regression_scale.csv", row.names = FALSE)
