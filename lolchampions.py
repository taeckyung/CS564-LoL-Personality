
import requests
import pandas as pd
import seaborn as sns

sns.set()


api_key = 'Key'
r = requests.get('https://ddragon.leagueoflegends.com/api/versions.json')
current_version = r.json()[0]
print(current_version)


#최신버전 받아오기
r = requests.get('http://ddragon.leagueoflegends.com/cdn/10.22.1/data/en_US/champion.json'.format(current_version))
parsed_data = r.json() # 파싱
info_df = pd.DataFrame(parsed_data)
print(info_df.head())

# champ_info_df의 data 값들을 데이터프레임으로 변환

champ_dic = {}
for i, champ in enumerate(info_df.data):
    champ_dic[i] = pd.Series(champ)


champ_df = pd.DataFrame(champ_dic).T


# champ_df의 info, stats의 데이터 변수로 추가
champ_info_df = pd.DataFrame(dict(champ_df['info'])).T
champ_stats_df = pd.DataFrame(dict(champ_df['stats'])).T

# 데이터 합치기
champ_df = pd.concat([champ_df, champ_info_df], axis=1)
champ_df = pd.concat([champ_df, champ_stats_df], axis=1)

#필요없는 데이터 제거
champ_df = champ_df.drop(['version', 'image', 'info', 'stats'], axis=1)
champ_df.info()
print(champ_df)

champ_df.to_csv("lolchampion_original.csv",mode='w')
