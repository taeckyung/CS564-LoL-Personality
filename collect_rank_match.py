from LoLMaster.api import summoner, match, config
import pandas as pd
import schedule
from datetime import datetime, timedelta
import argparse
import time
import os

summoner_dir = './'
match_dir = './'
match_by_summoner_dir = 'match_by_summoner'

api_key = ''
api_region = 'kr'

begin_datetime = datetime(2020, 1, 10, 0, 0, 0, 0) 
end_datetime = datetime(2020, 11, 10, 23, 59, 59, 99999) # 2020 11 10

def stringify_timestamp(dt):
	return datetime.fromtimestamp(dt/1000).strftime("%Y-%m-%d %H:%M:%S")

def authenticate(key, region):
	config.set_key(key)
	config.set_region(region)
	print(f"Key: {key}\nRegion: {region}")

def job():
	start = time.time()
	authenticate(api_key, api_region)
	base_summoner = os.path.join(summoner_dir, "data")
	base_match = os.path.join(match_dir, "data")
	name_series = pd.read_csv("responses.csv", encoding="utf-8-sig").iloc[:, 1]
	skip_summoner = True
	skip_matchlist = False
	
	# Collect summoner
	if skip_summoner == False:
		if not os.path.exists(base_summoner):
			os.mkdir(base_summoner)
		
		all_tiers_users = summoner.get_summoner_by_summoner_name(name_series)
		all_tiers_users.reset_index(drop=True, inplace=True)
		all_tiers_users.to_csv(os.path.join(base_summoner, 'all_tiers.csv'), encoding="utf-8-sig")
	else:
		all_tiers_users = pd.read_csv(os.path.join(base_summoner, 'all_tiers.csv'))

	# Collect matches
	if skip_matchlist == False:
		if not os.path.exists(base_match):
			os.mkdir(base_match)
		begin_dt_old = begin_datetime
		end_dt = end_datetime
		match_ids_total = pd.DataFrame()
		while begin_dt_old < end_dt:
			begin_dt_new = begin_dt_old + timedelta(days=7)
			begin_ts_old = int(begin_dt_old.timestamp()*1000)
			begin_ts_new = int(begin_dt_new.timestamp()*1000)
			print(f"Collecting data from {stringify_timestamp(begin_ts_old)} to {stringify_timestamp(begin_ts_new)}")
			match_ids = match.get_list_by_account(all_tiers_users['accountId'], 
				begin_time=begin_ts_old, end_time=begin_ts_new, queue_ids=[420])
			match_ids.reset_index(drop=True, inplace=True)
			match_ids_total = match_ids_total.append(match_ids)
			begin_dt_old = begin_dt_new
		match_ids_total.reset_index(drop=True, inplace=True)
		match_ids_total.timestamp = match_ids_total.timestamp.map(stringify_timestamp)
		match_ids_total.to_csv(os.path.join(base_match, 'all_matches.csv'), encoding="utf-8-sig")
	else:
		match_ids_total = pd.read_csv(os.path.join(base_match, 'all_matches.csv'))



	print(F"Data collection complete! (time: {time.time() - start})")
	return

def parse_args():
	parser = argparse.ArgumentParser()

	parser.add_argument('-s', '--summoner_dir', required=True,
	                    help="Base directory to store summoner data.")

	parser.add_argument('-m', '--match_dir', required=True,
	                    help="Base directory to store match data.")

	parser.add_argument('-t', '--time', default='00:00',
	                    help='Time to collect user data every day. Default is 00:00')

	parser.add_argument('-n', '--now', action='store_true',
	                    help="execute only once without scheduling")

	parser.add_argument('--skip_summoner', action='store_true',
                        help="skip collecting summoner data")

	return parser.parse_args()


args = parse_args()
summoner_dir = args.summoner_dir
match_dir = args.match_dir
skip_summoner = args.skip_summoner

if args.now:
	job()
else:
	schedule.every().day.at(args.time).do(job)

	while True:
		schedule.run_pending()
		time.sleep(1)
