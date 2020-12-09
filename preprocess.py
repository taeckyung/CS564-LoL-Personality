import argparse
import numpy as np
import pandas as pd


parser = argparse.ArgumentParser()

parser.add_argument('-m', '--match', required=True,
                    help="File path of all matches")

parser.add_argument('-s', '--summoner', required=True,
                    help="File path of all summoner data")

parser.add_argument('-p', '--preprocess', required=True,
                    help="File path to store the preprocessed data")

args = parser.parse_args()

match_df = pd.read_csv(args.match, index_col=0)
summoner_df = pd.read_csv(args.summoner, index_col=0)

match_df = match_df[match_df['queue'] == 420]

summoner_id_list = summoner_df['accountId'].to_numpy()
summoner_name_list = summoner_df['name'].to_numpy()
size = summoner_name_list.size

summoner_info = np.hstack([summoner_id_list.reshape(size, 1), summoner_name_list.reshape(size, 1)])
summoner_dict = {}
for i in range(len(summoner_info)):
    summoner_dict[summoner_info[i][0]] = summoner_info[i][1]


def substitute_id(row):
    return summoner_dict[row['accountId']]


match_df['name'] = match_df.apply(lambda r: substitute_id(r), axis=1)

print(match_df['lane'].unique())
print(match_df['role'].unique())


def get_real_lane(row):
    if row['lane'] in ['TOP', 'JUNGLE', 'MID']:
        return row['lane']
    else:
        if row['role'] == 'DUO_SUPPORT':
            return 'SUPPORT'
        elif row['role'] == 'DUO_CARRY':
            return 'BOTTOM'
        else:
            return 'None'


match_df['new_lane'] = match_df.apply(lambda r: get_real_lane(r), axis=1)

print(len(match_df[match_df['new_lane'] == 'None']))
print(len(match_df[match_df['new_lane'] != 'None']))

match_df.to_csv(args.preprocess, encoding="utf-8-sig")