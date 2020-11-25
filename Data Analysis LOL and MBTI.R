library(ggplot2);library(dplyr);
df <- read.csv(file = 'RollData.csv')
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

#######################LANE & MBTI#######################
LaneMBTI_df<-df[,c(11,15)]
LaneMBTI <- data.frame()
#delete mbti is NULL, lane is Null
for (row in 1:nrow(LaneMBTI_df)){
  lane <- LaneMBTI_df[row, "lane"]
  mbti <- LaneMBTI_df[row, "MBTI"]
  mbti1 <- substring(LaneMBTI_df[row, "MBTI"],1,1)
  mbti2 <- substring(LaneMBTI_df[row, "MBTI"],2,2)
  mbti3 <- substring(LaneMBTI_df[row, "MBTI"],3,3)
  mbti4 <- substring(LaneMBTI_df[row, "MBTI"],4,4)
  if (mbti != "" & lane != "None"){
    LaneMBTI <- rbind(LaneMBTI, c(lane, mbti1, mbti2, mbti3, mbti4))
  }
}
names(LaneMBTI) <- c("lane", "M", "B", "T", "I")
#table lane, M
tb_lm <- table(LaneMBTI$lane, LaneMBTI$M)
#table lane, B
tb_lb <- table(LaneMBTI$lane, LaneMBTI$B)
#table lane, T
tb_lt <- table(LaneMBTI$lane, LaneMBTI$T)
#table lane, I
tb_li <- table(LaneMBTI$lane, LaneMBTI$I)
#chi-square test
chisq.test(tb_lm)
chisq.test(tb_lb)
chisq.test(tb_lt)
chisq.test(tb_li)


#######################MOST CHAMPION & MBTI#######################
ChampMBTI_df <- df[,c(8,11,15)]

ChampMBTI <- data.frame()
for (row in 1:nrow(ChampMBTI_df)){
  champ1 <- ChampMBTI_df[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI_df[row, "lane"]
  mbti <- ChampMBTI_df[row, "MBTI"]
  mbti1 <- substring(ChampMBTI_df[row, "MBTI"],1,1)
  mbti2 <- substring(ChampMBTI_df[row, "MBTI"],2,2)
  mbti3 <- substring(ChampMBTI_df[row, "MBTI"],3,3)
  mbti4 <- substring(ChampMBTI_df[row, "MBTI"],4,4)
  if (mbti != "" & lane != "None"){
    ChampMBTI <- rbind(ChampMBTI, c(champ1, mbti1, mbti2, mbti3, mbti4, kv_df_All[keyChamp1], lane))
  }
}
names(ChampMBTI) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")

#table cluster, M
tb_cm <- table(ChampMBTI$cluster, ChampMBTI$M)
#table cluster, B
tb_cb <- table(ChampMBTI$cluster, ChampMBTI$B)
#table cluster, T
tb_ct <- table(ChampMBTI$cluster, ChampMBTI$T)
#table cluster, I
tb_ci <- table(ChampMBTI$cluster, ChampMBTI$I)
#chi-square test
chisq.test(tb_cm)
chisq.test(tb_cb)
chisq.test(tb_ct)
chisq.test(tb_ci)

#######################Champ&line&MBTI#######################
ChampMBTI_Top<-data.frame();ChampMBTI_Jg<-data.frame();ChampMBTI_Mid<-data.frame();ChampMBTI_Bot<-data.frame();ChampMBTI_Sup<-data.frame();

for (row in 1:nrow(ChampMBTI)){
  champ1 <- ChampMBTI[row, "champ1"]
  keyChamp1 <- as.integer(champ1)
  lane <- ChampMBTI[row, "lane"]
  M <- ChampMBTI[row, "M"]
  B <- ChampMBTI[row, "B"]
  T <- ChampMBTI[row, "T"]
  I <- ChampMBTI[row, "I"]
  print(lane)
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
names(ChampMBTI_Top) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
names(ChampMBTI_Jg) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
names(ChampMBTI_Mid) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
names(ChampMBTI_Bot) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
names(ChampMBTI_Sup) <- c("champ1", "M", "B", "T", "I", "cluster", "lane")
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


#######################CORRELATION WITH MOST 1,2,3#######################
Most123 <-  df[,c(8,9,10)]
M123_cluster <- data.frame()
for (row in 1:nrow(Most123)){
  M1 <- Most123[row, "champ1"]
  M2 <- Most123[row, "champ2"]
  M3 <- Most123[row, "champ3"]
  keyM1 <- as.integer(M1)
  keyM2 <- as.integer(M2)
  keyM3 <- as.integer(M3)

  if (M1 != "None" & M2 != "None" & M3 != "None"){
    M123_cluster <- rbind(M123_cluster, c(kv_df_All[keyM1],kv_df_All[keyM2],kv_df_All[keyM3]))
  }
}
names(M123_cluster) <- c("m1", "m2", "m3")
three = 0
two = 0
for (row in 1:nrow(M123_cluster)){
  c1 <- M123_cluster[row, "m1"]
  c2 <- M123_cluster[row, "m2"]
  c3 <- M123_cluster[row, "m3"]
  if (c1 == c2 & c2 == c3){
    three = three + 1
  }
  else if (c1 == c2 | c2 == c3 | c1 == c3){
    two = two +1
  }
}
#19/326 , 143/326