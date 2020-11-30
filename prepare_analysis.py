import pandas as pd
import numpy as np

survey_df = pd.read_csv("./survey.csv", encoding="cp949")
match_df = pd.read_csv("./preprocessed_matches.csv", encoding="utf-8-sig")
match_df = match_df[match_df['new_lane'] != 'None']

match_df = match_df.groupby(['name']).filter(lambda x: x.shape[0] >= 50)

print(match_df['name'].unique())
print(match_df['name'].nunique())

target_summoners = match_df['name'].unique()
print(match_df)


def get_mbti(x: pd.Series) -> str:
	mbti_0 = 'E' if (x['EI'] == 1) else 'I'
	mbti_1 = 'S' if (x['SN'] == 1) else 'N'
	mbti_2 = 'T' if (x['TF'] == 1) else 'F'
	mbti_3 = 'J' if (x['JP'] == 1) else 'P'
	return mbti_0 + mbti_1 + mbti_2 + mbti_3


def parse_year(x):
	if isinstance(x, int) or isinstance(x, float):
		return None
	elif isinstance(x, str):
		try:
			return int(x[0:4])
		except ValueError:
			return None
	return None


def parse_gender(x):
	if x == '남자':
		return 'male'
	else:
		return 'female'


print(survey_df.head())
result_df = []

match_df['name'] = match_df['name'].str.replace(' ', '').str.lower()
survey_df['name'] = survey_df['name'].str.replace(' ', '').str.lower()

print(match_df['name'].unique())

for summoner in target_summoners:
	summoner = summoner.lower().replace(' ', '')
	target_match_df = match_df[match_df['name'] == summoner]
	target_survey_df = survey_df[survey_df['name'] == summoner]
	target_survey_df = target_survey_df.iloc[0]

	most_lane = target_match_df['new_lane'].value_counts(sort=True).index[0]
	most_champs = target_match_df['champion'].value_counts(sort=True).index[0:3]

	result_df.append(list(most_champs) + [most_lane,
	                                      parse_gender(target_survey_df['gender']),
	                                      parse_year(target_survey_df['born_year']),
	                                      parse_year(target_survey_df['game_started_year']),
	                                      get_mbti(target_survey_df[['EI', 'SN', 'TF', 'JP']])])

result_df = pd.DataFrame(result_df, columns=['champ1', 'champ2', 'champ3', 'lane', 'gender', 'born_year', 'game_started_year', 'MBTI'])
result_df.to_csv('Roll_Data.csv', encoding="utf-8-sig", index=False)
