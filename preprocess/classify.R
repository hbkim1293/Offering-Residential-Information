##################################################################
#구글 드라이브 data폴더의 store2 데이터와 code 데이터를 먼저 다운받아주세요.
library(dplyr)
library(tidyr)

store2 <- read.csv("./Data/store2.csv", stringsAsFactors = TRUE) #디렉토리를 설정해서 로드하기
code <- read.csv("./Data/code.csv") #디렉토리를 설정해서 로드하기

#함수만들기-스크립트 맨밑의 예를 보고 활용.
classify <- function(class1 = "X", class2 = "X", class3 = "X", class4 = "X"){
    store2$code <- factor(store2$code)
    code$행정동 <- paste(code$자치구, code$행정동, sep = " ")
    code <- code[,c(2,6)]
    if(class2 == "X"){
        df <- store2 %>% filter(상권업종대분류명 == class1)
        TB <- table(df$paste)
        DF <- data_frame(TB)
        DF$행정동 <- names(TB)
        DF <- merge(DF, code, by = "행정동")
        DF <- DF[,c(3,1,2)]
    }else if(class3 == "X"){
        df <- store2 %>% filter(상권업종대분류명 == class1 &
                                            상권업종중분류명 == class2)
        TB <- table(df$paste)
        DF <- data_frame(TB)
        DF$행정동 <- names(TB)
        DF <- merge(DF, code, by = "행정동")
        DF <- DF[,c(3,1,2)]
    }else if(class4 == "X"){
        df <- store2 %>% filter(상권업종대분류명 == class1 &
                                            상권업종중분류명 == class2 &
                                            상권업종소분류명 == class3)
        TB <- table(df$paste)
        DF <- data_frame(TB)
        DF$행정동 <- names(TB)
        DF <- merge(DF, code, by = "행정동")
        DF <- DF[,c(3,1,2)]
    }else{
        df <- store2 %>% filter(상권업종대분류명 == class1 &
                                            상권업종중분류명 == class2 &
                                            상권업종소분류명 == class3 &
                                            표준산업분류명 == class4)
        TB <- table(df$paste)
        DF <- data_frame(TB)
        DF$행정동 <- names(TB)
        DF <- merge(DF, code, by = "행정동")
        DF <- DF[,c(3,1,2)]
    }
    DF <<- DF %>% separate(행정동, c("구","동"), sep = " ")
}
#예) classify("대분류","중분류","소분류","표준분류")
#예) classify("음식","커피점/카페")
#예) classify("생활서비스")
#결과로 DF라는 데이터프레임이 생성되요.