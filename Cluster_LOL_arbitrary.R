rm(list=ls())

library(hash)

#get data
clu_df <- read.csv(file = 'data/lolchampion1201.csv')
clu_df <- clu_df[,c(2,3,17,33:44)]
clu_df
key <- clu_df[,c(2)]

total_df = hash()
total_df[["TOP"]] = list()
total_df[["JUNGLE"]] = list()
total_df[["MID"]] = list()
total_df[["ADC"]] = list()
total_df[["SUPPORT"]] = list()

#put data into 5 data frame by LOL lane
for (row in 1:nrow(clu_df)){
  champ_id = as.character(clu_df[row, "key"])
  
  l1 <- clu_df[row, "lane"]
  l2 <- clu_df[row, "Second.lane"]
  l3 <- clu_df[row, "X.1"]
  
  c1 <- clu_df[row, "cluster1"]
  c2 <- clu_df[row, "cluster2"]
  c3 <- clu_df[row, "cluster3"]
  
  for (lane in names(total_df)) {
    if (l1 == lane) {
      total_df[[lane]][champ_id] = c1
    }
    if (l2 == lane) {
      total_df[[lane]][champ_id] = c2
    }
    if (l3 == lane) {
      total_df[[lane]][champ_id] = c3
    }
  }
}

total_df

library(ggplot2);library(dplyr);
df <- read.csv(file = 'data/Roll_Data.csv')
names(df) <- c("champ1", "champ2", "champ3", "lane", "gender", "born_year", "game_started_year", "MBTI")


#######################preprocess data frame#######################
ChampLaneMBTI_df <- df[,c(1, 2, 3, 4, 8)]
ChampLaneMBTI <- data.frame()

cluster_agree_2 = 0
cluster_agree_3 = 0

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
  
  clusters = list(total_df[[lane]][[as.character(M1)]],
                  total_df[[lane]][[as.character(M2)]],
                  total_df[[lane]][[as.character(M3)]])
  
  if (length(unique(clusters)) == 1) {
    cluster_agree_2 = cluster_agree_2 + 1
    cluster_agree_3 = cluster_agree_3 + 1
  }
  else if (length(unique(clusters)) == 2) {
    cluster_agree_2 = cluster_agree_2 + 1
  }
  
  
  for (cluster in clusters) {
    if ((lane != "None") & (!is.null(cluster))) {
      ChampLaneMBTI <- rbind(ChampLaneMBTI, c(lane, M1, M2, M3, mbti1, mbti2, mbti3, mbti4, cluster))
    }
    else {
      print(ChampLaneMBTI_df[row,])
    }
  }
}
names(ChampLaneMBTI) <- c("lane", "champ1","champ2","champ3", "M", "B", "T", "I", "Cluster")
ChampLaneMBTI
cluster_agree_2
cluster_agree_3

final_m = table(lane_df$M)
final_b = table(lane_df$B)
final_t = table(lane_df$T)
final_i = table(lane_df$I)

for (lane in c("TOP", "JUNGLE", "MID", "ADC", "SUPPORT")) {
  print(lane)
  lane_df = ChampLaneMBTI[ChampLaneMBTI$lane == lane,]
  
  table_m = table(lane_df$Cluster, lane_df$M)
  rownames(table_m) = lapply(rownames(table_m), function(x) paste(lane, x, sep="_"))
  print(chisq.test(table_m))
  final_m = rbind(final_m, table_m)
  
  table_b = table(lane_df$Cluster, lane_df$B)
  rownames(table_b) = lapply(rownames(table_b), function(x) paste(lane, x, sep="_"))
  print(chisq.test(table_b))
  final_b = rbind(final_b, table_b)
  
  table_t = table(lane_df$Cluster, lane_df$T)
  rownames(table_t) = lapply(rownames(table_t), function(x) paste(lane, x, sep="_"))
  print(chisq.test(table_t))
  final_t = rbind(final_t, table_t)
  
  table_i = table(lane_df$Cluster, lane_df$I)
  rownames(table_i) = lapply(rownames(table_i), function(x) paste(lane, x, sep="_"))
  print(chisq.test(table_i))
  final_i = rbind(final_i, table_i)
}

final_m = final_m[2:nrow(final_m),]
final_b = final_b[2:nrow(final_b),]
final_t = final_t[2:nrow(final_t),]
final_i = final_i[2:nrow(final_i),]

final_m
final_b
final_t
final_i

chisq.test(final_m)
chisq.test(final_b)
chisq.test(final_t)
chisq.test(final_i)


ChampLaneMBTI_Encoded = ChampLaneMBTI
for (row in 1:nrow(ChampLaneMBTI_Encoded)) {
  ChampLaneMBTI_Encoded[row, "M"] = as.integer(ChampLaneMBTI_Encoded[row, "M"] == "I")
  ChampLaneMBTI_Encoded[row, "B"] = as.integer(ChampLaneMBTI_Encoded[row, "B"] == "S")
  ChampLaneMBTI_Encoded[row, "T"] = as.integer(ChampLaneMBTI_Encoded[row, "T"] == "T")
  ChampLaneMBTI_Encoded[row, "I"] = as.integer(ChampLaneMBTI_Encoded[row, "I"] == "J")
}
ChampLaneMBTI_Encoded

print("------------------------------------------------------")
for (lane in c("TOP", "JUNGLE", "MID", "ADC", "SUPPORT")) {
  target = ChampLaneMBTI_Encoded[ChampLaneMBTI_Encoded$lane == lane,]
  print(unique(target$Cluster))
  for (cluster in unique(target$Cluster)) {
    lm_target = target
    print(paste(lane, cluster, sum(lm_target$Cluster == cluster), sum(lm_target$Cluster != cluster)))
    lm_target$Cluster = as.integer(lm_target$Cluster == cluster)
    result = glm(formula = Cluster ~ M + B + T + I, family=binomial, data=lm_target)
    print(summary(result))
  }
}

