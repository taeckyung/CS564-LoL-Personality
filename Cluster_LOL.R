rm(list=ls())

#get data
clu_df <- read.csv(file = 'data/lolchampion1116.csv')
clu_df <- clu_df[,c(2,3,17,33:41)]
key <- clu_df[,c(2)]
Top_df <- data.frame();Jg_df <- data.frame();Mid_df <- data.frame();Bot_df <- data.frame();Sup_df <- data.frame();

#put data into 5 data frame by LOL lane
for (row in 1:nrow(clu_df)){
  l1 <- clu_df[row, "lane"]
  l2 <- clu_df[row, "Second.lane"]
  l3 <- clu_df[row, "X.1"]

  if (l1 == "Top" | l2 =="Top" | l3 == "Top"){
    Top_df <- rbind(Top_df, clu_df[row,])
  }
  if (l1 == "Jg" | l2 == "Jg" | l3 == "Jg"){
    Jg_df <- rbind(Jg_df, clu_df[row,])
  }
  if (l1 == "Mid"| l2 == "Mid" | l3 == "Mid"){
    Mid_df <- rbind(Mid_df, clu_df[row,])
  }
  if (l1 == "Bot" | l2 == "Bot" | l3 == "Bot"){
    Bot_df <- rbind(Bot_df, clu_df[row,])
  }
  if (l1 == "Sup" | l2 == "Sup" | l3 == "Sup"){
    Sup_df <- rbind(Sup_df, clu_df[row,])
  }
}

Top_key <- Top_df[,c(2)];Jg_key <- Jg_df[,c(2)];Mid_key <- Mid_df[,c(2)];Bot_key <- Bot_df[,c(2)];Sup_key <- Sup_df[,c(2)]
#scaleing
Top_df <- Top_df[,c(1,3,7:12)]
Jg_df <- Jg_df[,c(1,3,7:12)]
Mid_df <- Mid_df[,c(1,3,7:12)]
Bot_df <- Bot_df[,c(1,3,7:12)]
Sup_df <- Sup_df[,c(1,3,7:12)]

Top_df[,c(2:8)] <- scale(Top_df[,c(2:8)])
Jg_df[,c(2:8)] <- scale(Jg_df[,c(2:8)])
Mid_df[,c(2:8)] <- scale(Mid_df[,c(2:8)])
Bot_df[,c(2:8)] <- scale(Bot_df[,c(2:8)])
Sup_df[,c(2:8)] <- scale(Sup_df[,c(2:8)])

#h clustering
library(cluster);library(NbClust);

############################# MODIFY HERE #########################################
# "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
NbClust_distance = "euclidean"

# "complete", "centroid", "median", "mcquitty", "ward.D", "ward.D2", "single", "average"
hclust_method = "complete"
###################################################################################

#top
nb <- NbClust(Top_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- nb$Best.nc[1]
ds <- dist(Top_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Top_df[,1], cex=0.8,main='Top clustering(hclust)',tip.color=c(1:n)[cutree(hcst,n)],hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Top <- cbind(Top_df,cluster=cutree(hcst,n))

#jungle
nb <- NbClust(Jg_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- nb$Best.nc[1]
ds <- dist(Jg_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Jg_df[,1], cex=0.8,main='Jungle clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Jg <- cbind(Jg_df,cluster=cutree(hcst,n))

#mid
nb <- NbClust(Mid_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- nb$Best.nc[1]
ds <- dist(Mid_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Mid_df[,1], cex=0.8,main='Mid clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Mid <- cbind(Mid_df,cluster=cutree(hcst,n))

# bottom
nb <- NbClust(Bot_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- nb$Best.nc[1]
ds <- dist(Bot_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Bot_df[,1], cex=0.8,main='Bottom clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Bot <- cbind(Bot_df,cluster=cutree(hcst,n))

# support
nb <- NbClust(Sup_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- nb$Best.nc[1]
ds <- dist(Sup_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Sup_df[,1], cex=0.8,main='Support clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Sup <- cbind(Sup_df,cluster=cutree(hcst,n))


#K-mean
nb <- NbClust(Top_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Top_df[,c(2:8)], centers = n)
kcluster_Top<- cbind(Top_df,kc$cluster)

nb <- NbClust(Jg_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Jg_df[,c(2:8)], centers = n)
kcluster_Jg<- cbind(Jg_df,kc$cluster)

nb <- NbClust(Mid_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Mid_df[,c(2:8)], centers = n)
kcluster_Mid<- cbind(Mid_df,kc$cluster)

nb <- NbClust(Bot_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Bot_df[,c(2:8)], centers = n)
kcluster_Bot<- cbind(Bot_df,kc$cluster)

nb <- NbClust(Sup_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Sup_df[,c(2:8)], centers = n)
kcluster_Sup<- cbind(Sup_df,kc$cluster)

#get key value pair champion key and kcluster, hcluster
kv_df_Top <- list();kv_df_Jg <- list();kv_df_Mid <- list();kv_df_Bot <- list();kv_df_Sup <- list();
kv_df_Top[Top_key] <- kcluster_Top$`kc$cluster`
kv_df_Jg[Jg_key] <- kcluster_Jg$`kc$cluster`
kv_df_Mid[Mid_key] <- kcluster_Mid$`kc$cluster`
kv_df_Bot[Bot_key] <- kcluster_Bot$`kc$cluster`
kv_df_Sup[Sup_key] <- kcluster_Sup$`kc$cluster`

#get key value pair champion key and hcluster
kv_df_TopH <- list();kv_df_JgH <- list();kv_df_MidH <- list();kv_df_BotH <- list();kv_df_SupH <- list();
kv_df_TopH[Top_key] <- hcluster_Top$cluster
kv_df_JgH[Jg_key] <- hcluster_Jg$cluster
kv_df_MidH[Mid_key] <- hcluster_Mid$cluster
kv_df_BotH[Bot_key] <- hcluster_Bot$cluster
kv_df_SupH[Sup_key] <- hcluster_Sup$cluster


#######################put 10 in other cluster#######################
i = 1
for (e in kv_df_Top) {
  if (is.null(e)) {
    kv_df_Top[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_Jg) {
  if (is.null(e)) {
    kv_df_Jg[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_Mid) {
  if (is.null(e)) {
    kv_df_Mid[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_Bot) {
  if (is.null(e)) {
    kv_df_Bot[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_Sup) {
  if (is.null(e)) {
    kv_df_Sup[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_TopH) {
  if (is.null(e)) {
    kv_df_TopH[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_JgH) {
  if (is.null(e)) {
    kv_df_JgH[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_MidH) {
  if (is.null(e)) {
    kv_df_MidH[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_BotH) {
  if (is.null(e)) {
    kv_df_BotH[i] = 10
  }
  i = i+1
}
i = 1
for (e in kv_df_SupH) {
  if (is.null(e)) {
    kv_df_SupH[i] = 10
  }
  i = i+1
}


##########################all champion cluster##########################
clu_df <- clu_df[,c(1,3,7:12)]
clu_df[,c(2:8)] <- scale(clu_df[,c(2:8)])

ds <- dist(clu_df, method="euclidean")
hcst <- hclust(ds, method="complete")
# plot(hcst, labels=clu_df[,1], cex=0.8)

nb <- NbClust(clu_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(clu_df[,c(2:8)], centers = n)
kcluster_All<- cbind(clu_df,kc$cluster)

#get key value pair champion key and kcluster
kv_df_All <- list()
kv_df_All[key] <- kcluster_All$`kc$cluster`

library(ggplot2);library(dplyr);
df <- read.csv(file = 'data/Roll_Data.csv')
names(df) <- c("champ1", "champ2", "champ3", "lane", "gender", "born_year", "game_started_year", "MBTI")


#######################preprocess data frame#######################
ChampLaneMBTI_df <- df[,c(1, 2, 3, 4, 8)]
ChampLaneMBTI <- data.frame()
for (row in 1:nrow(ChampLaneMBTI_df)){
  lane <- ChampLaneMBTI_df[row, "lane"]
  mbti <- ChampLaneMBTI_df[row, "MBTI"]
  M1 <- ChampLaneMBTI_df[row, "champ1"]
  M2 <- ChampLaneMBTI_df[row, "champ2"]
  M3 <- ChampLaneMBTI_df[row, "champ3"]
  mbti1 <- substring(ChampLaneMBTI_df[row, "MBTI"],1,1)
  mbti2 <- substring(ChampLaneMBTI_df[row, "MBTI"],2,2)
  mbti3 <- substring(ChampLaneMBTI_df[row, "MBTI"],3,3)
  mbti4 <- substring(ChampLaneMBTI_df[row, "MBTI"],4,4)
  if (lane != "None") {
    ChampLaneMBTI <- rbind(ChampLaneMBTI, c(lane, M1, M2, M3, mbti1, mbti2, mbti3, mbti4))
  }
}
names(ChampLaneMBTI) <- c("lane", "champ1","champ2","champ3", "M", "B", "T", "I")
#######################LANE & MBTI#######################
LaneMBTI <- ChampLaneMBTI[,c(1,5,6,7,8)]
#table lane, M, B, T, I
tb_lm <- table(LaneMBTI$lane, LaneMBTI$M);tb_lb <- table(LaneMBTI$lane, LaneMBTI$B);tb_lt <- table(LaneMBTI$lane, LaneMBTI$T);tb_li <- table(LaneMBTI$lane, LaneMBTI$I);
#chi-square test
chisq.test(tb_lm)
chisq.test(tb_lb)
chisq.test(tb_lt)
chisq.test(tb_li)


#######################MOST CHAMPION & MBTI#######################
ChampMBTI <- data.frame()

for (row in 1:nrow(ChampLaneMBTI[,c(1,2,5,6,7,8)])){
  champ1 <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "lane"]
  mbti1 <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "M"]
  mbti2 <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "B"]
  mbti3 <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "T"]
  mbti4 <- ChampLaneMBTI[,c(1,2,5,6,7,8)][row, "I"]
  if (mbti != "" & lane != "None"){
    ChampMBTI <- rbind(ChampMBTI, c(champ1, mbti1, mbti2, mbti3, mbti4, kv_df_All[keyChamp1], lane))
  }
}
names(ChampMBTI) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
#table cluster, MBTI
tb_cm <- table(ChampMBTI$cluster, ChampMBTI$M);tb_cb <- table(ChampMBTI$cluster, ChampMBTI$B);tb_ct <- table(ChampMBTI$cluster, ChampMBTI$T);tb_ci <- table(ChampMBTI$cluster, ChampMBTI$I);
#chi-square test
chisq.test(tb_cm)
chisq.test(tb_cb)
chisq.test(tb_ct)
chisq.test(tb_ci)

#######################Champ&line&MBTI by hclustering#######################
ChampMBTI_TopH<-data.frame();ChampMBTI_JgH<-data.frame();ChampMBTI_MidH<-data.frame();ChampMBTI_BotH<-data.frame();ChampMBTI_SupH<-data.frame();


for (row in 1:nrow(ChampMBTI)){
  champ1 <- ChampMBTI[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI[row, "lane"]
  M <- ChampMBTI[row, "M"]
  B <- ChampMBTI[row, "B"]
  T <- ChampMBTI[row, "T"]
  I <- ChampMBTI[row, "I"]
  
  if (lane == "TOP"){
    ChampMBTI_TopH <- rbind(ChampMBTI_TopH, c(champ1, M, B, T, I, kv_df_TopH[keyChamp1], lane))
  }
  if (lane == "JUNGLE"){
    ChampMBTI_JgH <- rbind(ChampMBTI_JgH, c(champ1, M, B, T, I, kv_df_JgH[keyChamp1], lane))
  }
  if (lane == "MID"){
    ChampMBTI_MidH <- rbind(ChampMBTI_MidH, c(champ1, M, B, T, I, kv_df_MidH[keyChamp1], lane))
  }
  if (lane == "ADC"){
    ChampMBTI_BotH <- rbind(ChampMBTI_BotH, c(champ1, M, B, T, I, kv_df_BotH[keyChamp1], lane))
  }
  if (lane == "SUPPORT"){
    ChampMBTI_SupH <- rbind(ChampMBTI_SupH, c(champ1, M, B, T, I, kv_df_SupH[keyChamp1], lane))
  }
}
names(ChampMBTI_TopH) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_JgH) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_MidH) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_BotH) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_SupH) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");
#table cluster, M,B,T,I
tb_cTm <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$M);tb_cTb <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$B);tb_cTt <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$T);tb_cTi <- table(ChampMBTI_TopH$cluster, ChampMBTI_TopH$I);
tb_cJm <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$M);tb_cJb <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$B);tb_cJt <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$T);tb_cJi <- table(ChampMBTI_JgH$cluster, ChampMBTI_JgH$I);
tb_cMm <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$M);tb_cMb <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$B);tb_cMt <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$T);tb_cMi <- table(ChampMBTI_MidH$cluster, ChampMBTI_MidH$I);
tb_cBm <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$M);tb_cBb <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$B);tb_cBt <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$T);tb_cBi <- table(ChampMBTI_BotH$cluster, ChampMBTI_BotH$I);
tb_cSm <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$M);tb_cSb <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$B);tb_cSt <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$T);tb_cSi <- table(ChampMBTI_SupH$cluster, ChampMBTI_SupH$I);

table_list = list(
  tb_cTm, tb_cTb, tb_cTt, tb_cTi,
  tb_cJm, tb_cJb, tb_cJt, tb_cJi,
  tb_cMm, tb_cMb, tb_cMt, tb_cMi,
  tb_cBm, tb_cBb, tb_cBt, tb_cBi,
  tb_cSm, tb_cSb, tb_cSt, tb_cSi
)

count_01 = 0
count_005 = 0
for (tb in table_list) {
  p_value = chisq.test(tb)$p.value
  if (p_value <= 0.05) {
    count_005 = count_005 + 1
  }
  if (p_value <= 0.1) {
    count_01 = count_01 + 1
  }
}
count_005
count_01