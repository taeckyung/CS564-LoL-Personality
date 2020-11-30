rm(list=ls())

#get data
clu_df <- read.csv(file = 'lolchampion1116.csv')
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
#top
nb <- NbClust(Top_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
ds <- dist(Top_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Top_df[,1], cex=0.8,main='Top clustering(hclust)',tip.color=c(1:n)[cutree(hcst,n)],hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Top <- cbind(Top_df,cluster=cutree(hcst,n))

#jungle
nb <- NbClust(Jg_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
ds <- dist(Jg_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Jg_df[,1], cex=0.8,main='Jungle clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Jg <- cbind(Jg_df,cluster=cutree(hcst,n))

#mid
nb <- NbClust(Mid_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
ds <- dist(Mid_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Mid_df[,1], cex=0.8,main='Mid clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Mid <- cbind(Mid_df,cluster=cutree(hcst,n))

# bottom
nb <- NbClust(Bot_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
ds <- dist(Bot_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Bot_df[,1], cex=0.8,main='Bottom clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Bot <- cbind(Bot_df,cluster=cutree(hcst,n))

# support
nb <- NbClust(Sup_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
ds <- dist(Sup_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Sup_df[,1], cex=0.8,main='Support clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Sup <- cbind(Sup_df,cluster=cutree(hcst,n))

#get key value pair champion key and hcluster
kv_df_TopH <- list();kv_df_JgH <- list();kv_df_MidH <- list();kv_df_BotH <- list();kv_df_SupH <- list();
kv_df_TopH[Top_key] <- hcluster_Top$cluster
kv_df_JgH[Jg_key] <- hcluster_Jg$cluster
kv_df_MidH[Mid_key] <- hcluster_Mid$cluster
kv_df_BotH[Bot_key] <- hcluster_Bot$cluster
kv_df_SupH[Sup_key] <- hcluster_Sup$cluster

#######################put 10 in other cluster#######################
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
plot(hcst, labels=clu_df[,1], cex=0.8)

nb <- NbClust(clu_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(clu_df[,c(2:8)], centers = n)
kcluster_All<- cbind(clu_df,kc$cluster)

#get key value pair champion key and kcluster
kv_df_All <- list()
kv_df_All[key] <- kcluster_All$`kc$cluster`

