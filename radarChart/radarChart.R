##레이더 차트 만들기

library(proxy)
library(ggplot2)
library(htmlwidgets)
library(ggiraph)
library(moonBook2)
library(extrafont)
theme_set(theme_gray(base_family='HUNemogulim2280')) 

dd <- read.csv("./data/first_regression_scale.csv", fileEncoding = 'euc-kr')

#극단값 최고수치 100으로 조정
dd[dd$집세 > 100, 4] <- 100; dd$집세 <- 100 - dd$집세 
dd[dd$청년남자비율 > 100, 5] <- 100
dd[dd$청년여자비율 > 100, 6] <- 100
dd[dd$범죄안전 > 100, 7] <- 100; dd$범죄안전 <- 100 - dd$범죄안전
dd[dd$교통안전 > 100, 8] <- 100; dd$교통안전 <- 100 - dd$교통안전
dd[dd$편의시설 > 100, 9] <- 100
dd[dd$쇼핑시설 > 100, 10] <- 100
dd[dd$외식시설 > 100, 11] <- 100
dd[dd$대중교통 > 100, 12] <- 100
dd[dd$체육시설 > 100, 13] <- 100
colnames(dd)[which(colnames(dd) %in% c('집세', '청년남자비율', '청년여자비율', 
                                       '범죄안전', '편의시설', '외식시설', '대중교통', 
                                       '체육시설'))]<-c('착한집세', '남자비율', '여자비율', 
                                        '범죄안전', '편의시설', '외식시설', '대중교통', '여가시설')


## 2. TF x IDF 반환 함수
data2 <- dd[4:13]

TF_IDF <- function(data2){
    rowsum <- rowSums(data2)
    TF <- data2
    for(i in 1:nrow(data2))
    {
        TF[i, ] <- data2[i, ] / rowsum[i]
    }
    #head(TF) #TF 나타내는 데이터프레임
    
    
    #colsum <- colSums(data2)
    #IDF <- colsum / sum(data2) - 이렇게 하면 idf가 거의 0.1로 다 비슷
    #IDF # 각 열(특성)에 대한 IDF 값 벡터 -- 50 넘는거만 colsum!
    IDF <- c()
    for (i in 1:ncol(data2))
    {
        colsum <- colSums(data2[data2[i] >= 50, ])
        IDF[i] <- colsum / sum(data2)
    }
    
    # TF x IDF 데이터프레임
    TFxIDF <- TF
    for (i in 1:ncol(TF))
    {
        TFxIDF[ ,i] <- TF[ ,i] * IDF[i]
    }
    return(TFxIDF)
}


## 3. 코사인 거리 구하기 
cosine_dist_mat <- as.matrix(dist(TF_IDF(data2), method = "cosine"))

## 4. 유사도 높은 top(k) 두개씩 비교할 수 있는 함수
# 동5개 반환 함수
# input; 동이름
simdong_list <- function(input, k = 3){
    i <- as.numeric(rownames(dd[dd$동 == input, ]))    # 동에 해당하는 동번호 추출
    a <- cosine_dist_mat[ ,i]                          #그 동번호와 다른 동과의 코사인 거리 행렬 추출
    b <- names(a[order(a)[1:(k+1)]])                   # 비슷한 top(k)동들의 번호 추출 (자기자신포함)
    dong <- dd[2:3]                                    #dd[2]; 동, dd[3]; 구
    simdong <- dong[b, ]$동                            #top(k)에 해당하는 동이름 추출
    return(simdong[2:(k+1)])                           #자기자신 제외한 k개 추출
} 


# 그래프 반환 함수
simdong_graph <- function(input, compare = 'top1', graph = 'altogether'){  
    if(compare == 'top1'){
        compare <- simdong_list(input)[1]
    }else if(compare == 'top2'){
        compare <- simdong_list(input)[2]
    }else{
        compare <- simdong_list(input)[3]
    }
    x <- dd[(dd$동 == input)|(dd$동 == compare), ]
    
    if (graph == 'separate')
    {
        g <- ggRadar(data=x[-1], groupvar = '동') + facet_wrap(~동) + ylim(0,100) + 
            theme(legend.position = 'top', legend.title = element_text(face="bold"),
                  legend.text = element_text(size=20, face="bold"),
                  legend.box.margin = margin(6, 6, 6, 6))
    }else{
        g <- ggRadar(data=x[-1], groupvar = '동', ylim = c(0,100)) + 
            theme(legend.position = 'top', legend.title = element_text(face="bold"),
                  legend.text = element_text(size=20, face="bold"),
                  legend.box.margin = margin(6, 6, 6, 6))
    }
    return(g)
}


###부족한 변수 하나를 강화한 유사동을 찾고 싶다면?
# 그 변수 제외하고 나머지에 대해 유사도 구한다음, 그 제외했던 변수가 가장 높은 것 추천
# data2; 동코드 제외 데이터, imp_var; 강화할 변수, input; 비교할 동(원래 동)
improved_dong_list <- function(imp_var, input){   
    if (imp_var == '착한집세'){
        Xdata <- data2[-1]
        selectedCol <- data2[1]
    }else if(imp_var == '남자비율'){
        Xdata <- data2[-2]
        selectedCol <- data2[2]
    }else if(imp_var == '여자비율'){
        Xdata <- data2[-3]
        selectedCol <- data2[3]
    }else if(imp_var == '범죄안전'){
        Xdata <- data2[-4]
        selectedCol <- data2[4]
    }else if(imp_var == '교통안전'){
        Xdata <- data2[-5]
        selectedCol <- data2[5]
    }else if(imp_var == '편의시설'){
        Xdata <- data2[-6]
        selectedCol <- data2[6]
    }else if(imp_var == '쇼핑시설'){
        Xdata <- data2[-7]
        selectedCol <- data2[7]
    }else if(imp_var == '외식시설'){
        Xdata <- data2[-8]
        selectedCol <- data2[8]
    }else if(imp_var == '대중교통'){
        Xdata <- data2[-9]
        selectedCol <- data2[9]
    }else{ #체육시설
        Xdata <- data2[-10]
        selectedCol <- data2[10]
    }
    X_TFxIDF <- TF_IDF(Xdata)
    cosine_dist_mat <- as.matrix(dist(X_TFxIDF, method = "cosine"))
    
    dong20 <- simdong_list(input, 200) # top200 뽑아서 그 중 중요변수 제일 큰 순으로 배열
    rownum <- c()
    for (i in 1:200)
    {
        rownum[i] <- as.numeric(rownames(dd[dd$동 == dong20[i], ]))
    }
    var20 <- selectedCol[rownum, ]
    rownum20 <- cbind(rownum, var20)
    top3rownum <- rownum20[order(var20, decreasing = TRUE)][1:3]
    top3dong <- dd[2:3][top3rownum, ]$동
    return(top3dong)
    
}

# 보완된 top3 지역 그래프
improved_dong_graph <- function(imp_var, input, compare = 'top1', graph = 'altogether'){
    if(compare == 'top1'){
        compare <- improved_dong_list(imp_var, input)[1]
    }else if(compare == 'top2'){
        compare <- improved_dong_list(imp_var, input)[2] 
    }else{
        compare <- improved_dong_list(imp_var, input)[3] 
    }
    
    x <- dd[(dd$동 == input)|(dd$동 == compare), ]
    
    if (graph == 'separate')
    {
        g <- ggRadar(data=x[-1], groupvar = '동') + facet_wrap(~동) + ylim(0,100) + 
            theme(legend.position = 'top', legend.title = element_text(face="bold"),
                  legend.text = element_text(size=20, face="bold"),
                  legend.box.margin = margin(6, 6, 6, 6))
    }else{
        g <- ggRadar(data=x[-1], groupvar = '동', ylim = c(0,100)) + 
            theme(legend.position = 'top', legend.title = element_text(face="bold"),
                  legend.text = element_text(size=20, face="bold"),
                  legend.box.margin = margin(6, 6, 6, 6))
    }
    return(g)
}

##예) improved_dong_graph('착한집세', '청량리동', 'top3')