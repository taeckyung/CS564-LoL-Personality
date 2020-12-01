library(ggplot2);library(dplyr);
df <- read.csv(file = 'data/Roll_Data.csv')
names(df) <- c("champ1", "champ2", "champ3", "lane", "gender", "born_year", "game_started_year", "MBTI")
#######################SURVEY STATISTICS#######################
#lane, piegraph
dflane <- as.data.frame(table(df$lane))

ggplot(dflane, aes(x="", y=dflane$Freq, fill=dflane$Var1)) +
         geom_bar(stat="identity", width=1, color="white") +
         coord_polar("y", start=0)+
  labs(title = "Lane Piechart")+
  labs(fill = "Lane")+
  xlab("") +
  ylab("")
#gender
table(df$gender)
#born_year, bargraph
dfborn <- as.data.frame(table(df$born_year))
ggplot(dfborn, aes(x=dfborn$Var1, y=dfborn$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "Born Year Bargraph")+
  xlab("Born year") +
  ylab("Freq")
#game_started_year, bargraph
dfgamestart <- as.data.frame(table(df$game_started_year))
ggplot(dfgamestart, aes(x=dfgamestart$Var1, y=dfgamestart$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "Game Started Year Bargraph")+
  xlab("Game Started Year") +
  ylab("Freq")
#MBTI, bargraph
dfMBTI <- as.data.frame(table(df$MBTI))
ggplot(dfMBTI, aes(x=dfMBTI$Var1, y=dfMBTI$Freq))+
  geom_bar(stat = "identity", fill="steelblue")+
  labs(title = "MBTI Bargraph")+
  xlab("MBTI") +
  ylab("Freq")
#MBTI, bargraph by 4
LaneMBTI_df<-df[,c(11,15)]
LaneMBTI <- data.frame()
df_m <- as.data.frame(table(LaneMBTI$M))
df_b <- as.data.frame(table(LaneMBTI$B))
df_t <- as.data.frame(table(LaneMBTI$T))
df_i <- as.data.frame(table(LaneMBTI$I))
df_mbti <- rbind(df_m, df_b, df_t, df_i)
mycolor <- c("#00798c","#00798c","#d1495b","#d1495b","#edae49","#edae49","#66a182","#66a182")
ggplot(df_mbti, aes(x=df_mbti$Var1, y=df_mbti$Freq))+
  geom_bar(stat = "identity", fill = mycolor)+
  labs(title = "MBTI Bargraph")+
  xlab("MBTI") +
  ylab("Freq")

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

count = 0
for (tb in table_list) {
  p_value = chisq.test(tb)$p.value
  if (p_value <= 0.05) {
    count = count + 1
  }
}
count

#chi-square test
chisq.test(tb_cTm)
chisq.test(tb_cTb)
chisq.test(tb_cTt)
chisq.test(tb_cTi)

chisq.test(tb_cJm)
chisq.test(tb_cJb)
chisq.test(tb_cJt)
chisq.test(tb_cJi)

chisq.test(tb_cMm)
chisq.test(tb_cMb)
chisq.test(tb_cMt)
chisq.test(tb_cMi)

chisq.test(tb_cBm)
chisq.test(tb_cBb)
chisq.test(tb_cBt)
chisq.test(tb_cBi)

chisq.test(tb_cSm)
chisq.test(tb_cSb)
chisq.test(tb_cSt)
chisq.test(tb_cSi)


#######################Champ&line&MBTI by kclustering#######################
ChampMBTI_Top<-data.frame();ChampMBTI_Jg<-data.frame();ChampMBTI_Mid<-data.frame();ChampMBTI_Bot<-data.frame();ChampMBTI_Sup<-data.frame();

for (row in 1:nrow(ChampMBTI)){
  champ1 <- ChampMBTI[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI[row, "lane"]
  M <- ChampMBTI[row, "M"]
  B <- ChampMBTI[row, "B"]
  T <- ChampMBTI[row, "T"]
  I <- ChampMBTI[row, "I"]
  if (lane == "TOP"){
    ChampMBTI_Top <- rbind(ChampMBTI_Top, c(champ1, M, B, T, I, kv_df_Top[keyChamp1], lane))
  }
  if (lane == "JUNGLE"){
    ChampMBTI_Jg <- rbind(ChampMBTI_Jg, c(champ1, M, B, T, I, kv_df_Jg[keyChamp1], lane))
  }
  if (lane == "MID"){
    ChampMBTI_Mid <- rbind(ChampMBTI_Mid, c(champ1, M, B, T, I, kv_df_Mid[keyChamp1], lane))
  }
  if (lane == "ADC"){
    ChampMBTI_Bot <- rbind(ChampMBTI_Bot, c(champ1, M, B, T, I, kv_df_Bot[keyChamp1], lane))
  }
  if (lane == "SUPPORT"){
    ChampMBTI_Sup <- rbind(ChampMBTI_Sup, c(champ1, M, B, T, I, kv_df_Sup[keyChamp1], lane))
  }
}
names(ChampMBTI_Top) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_Jg) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_Mid) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_Bot) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");names(ChampMBTI_Sup) <- c("champ1", "M", "B", "T", "I", "cluster", "lane");
#table Top cluster, M,B,T,I
tb_cTm <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$M);tb_cTb <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$B);tb_cTt <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$T);tb_cTi <- table(ChampMBTI_Top$cluster, ChampMBTI_Top$I);
#table Jg cluster, M,B,T,I
tb_cJm <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$M);tb_cJb <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$B);tb_cJt <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$T);tb_cJi <- table(ChampMBTI_Jg$cluster, ChampMBTI_Jg$I);
#table Mid cluster, M,B,T,I
tb_cMm <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$M);tb_cMb <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$B);tb_cMt <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$T);tb_cMi <- table(ChampMBTI_Mid$cluster, ChampMBTI_Mid$I);
#table Bot cluster, M,B,T,I
tb_cBm <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$M);tb_cBb <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$B);tb_cBt <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$T);tb_cBi <- table(ChampMBTI_Bot$cluster, ChampMBTI_Bot$I);
#table Sup cluster, M,B,T,I
tb_cSm <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$M);tb_cSb <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$B);tb_cSt <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$T);tb_cSi <- table(ChampMBTI_Sup$cluster, ChampMBTI_Sup$I);
#chi-square test
chisq.test(tb_cTm)
chisq.test(tb_cTb)
chisq.test(tb_cTt)
chisq.test(tb_cTi)

chisq.test(tb_cJm)
chisq.test(tb_cJb)
chisq.test(tb_cJt)
chisq.test(tb_cJi)

chisq.test(tb_cMm)
chisq.test(tb_cMb)
chisq.test(tb_cMt)
chisq.test(tb_cMi)

chisq.test(tb_cBm)
chisq.test(tb_cBb)
chisq.test(tb_cBt)
chisq.test(tb_cBi)

chisq.test(tb_cSm)
chisq.test(tb_cSb)
chisq.test(tb_cSt)
chisq.test(tb_cSi)

