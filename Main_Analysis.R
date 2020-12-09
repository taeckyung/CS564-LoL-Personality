rm(list=ls())

library(hash)

#get data
clu_df <- read.csv(file = 'data/lolchampion1201.csv')
colnames(clu_df)
clu_df <- clu_df[,c(2,3,17,33:41)]
key <- clu_df[,c(2)]
Top_df <- data.frame();Jg_df <- data.frame();Mid_df <- data.frame();Bot_df <- data.frame();Sup_df <- data.frame();

#put data into 5 data frame by LOL lane
for (row in 1:nrow(clu_df)){
  l1 <- clu_df[row, "lane"]
  l2 <- clu_df[row, "Second.lane"]
  l3 <- clu_df[row, "X.1"]

  if (l1 == "TOP" | l2 =="TOP" | l3 == "TOP"){
    Top_df <- rbind(Top_df, clu_df[row,])
  }
  if (l1 == "JUNGLE" | l2 == "JUNGLE" | l3 == "JUNGLE"){
    Jg_df <- rbind(Jg_df, clu_df[row,])
  }
  if (l1 == "MID"| l2 == "MID" | l3 == "MID"){
    Mid_df <- rbind(Mid_df, clu_df[row,])
  }
  if (l1 == "ADC" | l2 == "ADC" | l3 == "ADC"){
    Bot_df <- rbind(Bot_df, clu_df[row,])
  }
  if (l1 == "SUPPORT" | l2 == "SUPPORT" | l3 == "SUPPORT"){
    Sup_df <- rbind(Sup_df, clu_df[row,])
  }
}

#scaling
Top_df = Top_df[Top_df$id != 'Gnar',]
Bot_df = Bot_df[Bot_df$id != 'Yasuo',]
Bot_df = Bot_df[Bot_df$id != 'Senna',]
Bot_df = Bot_df[Bot_df$id != 'Swain',]

Top_key <- Top_df[,c(2)];Jg_key <- Jg_df[,c(2)];Mid_key <- Mid_df[,c(2)];Bot_key <- Bot_df[,c(2)];Sup_key <- Sup_df[,c(2)]

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
hclust_method = "ward.D2"
###################################################################################

#top
nb <- NbClust(Top_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- max(nb$Best.partition)
ds <- dist(Top_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Top_df[,1], cex=0.8,main='Top clustering(hclust)',tip.color=c(1:n)[cutree(hcst,n)],hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Top <- cbind(Top_df,cluster=cutree(hcst,n))

#jungle
nb <- NbClust(Jg_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- max(nb$Best.partition)
ds <- dist(Jg_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Jg_df[,1], cex=0.8,main='Jungle clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Jg <- cbind(Jg_df,cluster=cutree(hcst,n))

#mid
nb <- NbClust(Mid_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- max(nb$Best.partition)
ds <- dist(Mid_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Mid_df[,1], cex=0.8,main='Mid clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Mid <- cbind(Mid_df,cluster=cutree(hcst,n))

# bottom
nb <- NbClust(Bot_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=4, method=hclust_method, index="all")
n <- max(nb$Best.partition)
ds <- dist(Bot_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Bot_df[,1], cex=0.8,main='Bottom clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, 2)
hcluster_Bot <- cbind(Bot_df,cluster=cutree(hcst,2))

# support
nb <- NbClust(Sup_df[,c(2:8)], distance=NbClust_distance, min.nc=2, max.nc=10, method=hclust_method, index="all")
n <- max(nb$Best.partition)
ds <- dist(Sup_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=Sup_df[,1], cex=0.8,main='Support clustering(hclust)',hang=-1,xlab='Champion')
rect.hclust(hcst, n)
hcluster_Sup <- cbind(Sup_df,cluster=cutree(hcst,n))


#K-mean
nb <- NbClust(Top_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
kc <- kmeans(Top_df[,c(2:8)], centers = n)
kcluster_Top<- cbind(Top_df,kc$cluster)

nb <- NbClust(Jg_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
kc <- kmeans(Jg_df[,c(2:8)], centers = n)
kcluster_Jg<- cbind(Jg_df,kc$cluster)

nb <- NbClust(Mid_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
kc <- kmeans(Mid_df[,c(2:8)], centers = n)
kcluster_Mid<- cbind(Mid_df,kc$cluster)

Bot_df
nb <- NbClust(Bot_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
kc <- kmeans(Bot_df[,c(2:8)], centers = n)
kcluster_Bot<- cbind(Bot_df,kc$cluster)

nb <- NbClust(Sup_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
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


#### Put 10 for champions not in any clusters ####
add_dummy = function(df) {
  i = 1
  for (e in df) {
    if (is.null(e)) {
      df[i] = 10
    }
    i = i + 1
  }
  df
}

kv_df_Top = add_dummy(kv_df_Top)
kv_df_Jg = add_dummy(kv_df_Jg)
kv_df_Mid = add_dummy(kv_df_Mid)
kv_df_Bot = add_dummy(kv_df_Bot)
kv_df_Sup = add_dummy(kv_df_Sup)

kv_df_TopH = add_dummy(kv_df_TopH)
kv_df_JgH = add_dummy(kv_df_JgH)
kv_df_MidH = add_dummy(kv_df_MidH)
kv_df_BotH = add_dummy(kv_df_BotH)
kv_df_SupH = add_dummy(kv_df_SupH)


##########################all champion cluster##########################
clu_df <- clu_df[,c(1,3,7:12)]
clu_df[,c(2:8)] <- scale(clu_df[,c(2:8)])

ds <- dist(clu_df, method=NbClust_distance)
hcst <- hclust(ds, method=hclust_method)
plot(hcst, labels=clu_df[,1], cex=0.8)
rect.hclust(hcst, n)

nb <- NbClust(clu_df[,c(2:8)], distance="euclidean", min.nc=2, max.nc=10, method="kmeans", index="all")
n <- max(nb$Best.partition)
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

#### Generate data.frame of champion-MBTI-cluster for each lane ####
key_value_hash = hash()
key_value_hash[["TOP"]] = kv_df_TopH
key_value_hash[["JUNGLE"]] = kv_df_JgH
key_value_hash[["MID"]] = kv_df_MidH
key_value_hash[["BOTTOM"]] = kv_df_BotH
key_value_hash[["SUPPORT"]] = kv_df_SupH

ChampMBTI_Cluster = hash()
for (lane in c("TOP", "JUNGLE", "MID", "BOTTOM", "SUPPORT")) {
  ChampMBTI_Cluster[[lane]] = data.frame()
}

cluster_agree_2 = 0
cluster_agree_3 = 0

for (row in 1:nrow(ChampLaneMBTI)){
  champs = list(ChampLaneMBTI[row, "champ1"],
                ChampLaneMBTI[row, "champ2"],
                ChampLaneMBTI[row, "champ3"])
  
  lane <- ChampLaneMBTI[row, "lane"]
  M <- ChampLaneMBTI[row, "M"]
  B <- ChampLaneMBTI[row, "B"]
  T <- ChampLaneMBTI[row, "T"]
  I <- ChampLaneMBTI[row, "I"]
  
  clusters = list(key_value_hash[[lane]][[as.integer(champs[1])]],
                  key_value_hash[[lane]][[as.integer(champs[2])]],
                  key_value_hash[[lane]][[as.integer(champs[3])]])
  # real_clusters = list()
  
  for (i in 1:3) {
    if (!is.null(clusters[[i]])) {
      if (clusters[[i]] != 10) {
        ChampMBTI_Cluster[[lane]] = rbind(ChampMBTI_Cluster[[lane]], c(champs[i], M, B, T, I, clusters[i], lane))
        # real_clusters = rbind(real_clusters, clusters[i])
      }
    }
  }
  if (length(unique(clusters)) == 1) {
    cluster_agree_2 = cluster_agree_2 + 1
    cluster_agree_3 = cluster_agree_3 + 1
  }
  else if (length(unique(clusters)) == 2) {
    cluster_agree_2 = cluster_agree_2 + 1
  }
}

# How does most champions agree with each other? (i.e. do most champions lie in same cluster?)
nrow(ChampLaneMBTI)
cluster_agree_2
cluster_agree_3

#### Generate tables for each clusters in each lane ####

per_line_tables = hash()

for (lane in c("TOP", "JUNGLE", "MID", "BOTTOM", "SUPPORT")) {
  names(ChampMBTI_Cluster[[lane]]) <- c("champ", "M", "B", "T", "I", "cluster", "lane")
  per_line_tables[[lane]] = hash()
  
  for (mbti_type in c("M", "B", "T", "I")) {
    temp_tb = table(ChampMBTI_Cluster[[lane]]$cluster, ChampMBTI_Cluster[[lane]][[mbti_type]])
    rownames(temp_tb) = lapply(rownames(temp_tb), function(x) paste(lane, x, sep="_"))
    per_line_tables[[lane]][[mbti_type]] = temp_tb
    
    print(paste(lane, mbti_type, sep="_"))
    print(chisq.test(temp_tb))
  }
}


#### Merge clusters in same lane ####

merged_tables = hash()
for (mbti_type in c("M", "B", "T", "I")) {
  merged_tables[[mbti_type]] = rbind(per_line_tables[["TOP"]][[mbti_type]],
                                per_line_tables[["JUNGLE"]][[mbti_type]],
                                per_line_tables[["MID"]][[mbti_type]],
                                per_line_tables[["BOTTOM"]][[mbti_type]],
                                per_line_tables[["SUPPORT"]][[mbti_type]]
                                )
}


#### Get statistics for each merged table in MBTI types ####
library(grid); library(vcd)

merged_tables[["M"]]
chisq.test(merged_tables[["M"]])
mosaic(merged_tables[["M"]], shade = TRUE, legend = TRUE,
       direction = "v",
       gp_varnames = gpar(fontsize = 14, fontface = 1),
       gp_labels = gpar(fontsize = 6),
       gp_args = list(interpolate = c(0, 1, 2)))


merged_tables[["B"]]
chisq.test(merged_tables[["B"]])
mosaic(merged_tables[["B"]], shade = TRUE, legend = TRUE,
       direction = "v",
       gp_varnames = gpar(fontsize = 14, fontface = 1),
       gp_labels = gpar(fontsize = 6),
       gp_args = list(interpolate = c(0, 1, 2)))


merged_tables[["T"]]
chisq.test(merged_tables[["T"]])
mosaic(merged_tables[["T"]], shade = TRUE, legend = TRUE,
       direction = "v",
       gp_varnames = gpar(fontsize = 14, fontface = 1),
       gp_labels = gpar(fontsize = 6),
       gp_args = list(interpolate = c(0, 1, 2)))


merged_tables[["I"]]
chisq.test(merged_tables[["I"]])
mosaic(merged_tables[["I"]], shade = TRUE, legend = TRUE,
       direction = "v",
       gp_varnames = gpar(fontsize = 14, fontface = 1),
       gp_labels = gpar(fontsize = 6),
       gp_args = list(interpolate = c(0, 1, 2)))

