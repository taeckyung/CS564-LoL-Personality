
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
ds <- dist(Top_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Top_df[,1], cex=0.8)

ds <- dist(Jg_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Jg_df[,1], cex=0.8)

ds <- dist(Mid_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Mid_df[,1], cex=0.8)

ds <- dist(Bot_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Bot_df[,1], cex=0.8)

ds <- dist(Sup_df, method="euclidean")
hcst <- hclust(ds, method="complete")
plot(hcst, labels=Sup_df[,1], cex=0.8)

#K-mean
nb <- NbClust(Top_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Top_df[,c(2:8)], centers = n)
kcluster_Top<- cbind(Top_df,kc$cluster)

nb <- NbClust(Jg_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Jg_df[,c(2:8)], centers = n)
kcluster_Jg<- cbind(Jg_df,kc$cluster)

nb <- NbClust(Mid_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Mid_df[,c(2:8)], centers = n)
kcluster_Mid<- cbind(Mid_df,kc$cluster)

nb <- NbClust(Bot_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Bot_df[,c(2:8)], centers = n)
kcluster_Bot<- cbind(Bot_df,kc$cluster)

nb <- NbClust(Sup_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="complete", index="all")
n <- nb$Best.nc[1]
kc <- kmeans(Sup_df[,c(2:8)], centers = n)
kcluster_Sup<- cbind(Sup_df,kc$cluster)

#get key value pair champion key and kcluster
kv_df_Top <- list();kv_df_Jg <- list();kv_df_Mid <- list();kv_df_Bot <- list();kv_df_Sup <- list();
kv_df_Top[Top_key] <- kcluster_Top$`kc$cluster`
kv_df_Jg[Jg_key] <- kcluster_Jg$`kc$cluster`
kv_df_Mid[Mid_key] <- kcluster_Mid$`kc$cluster`
kv_df_Bot[Bot_key] <- kcluster_Bot$`kc$cluster`
kv_df_Sup[Sup_key] <- kcluster_Sup$`kc$cluster`

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
