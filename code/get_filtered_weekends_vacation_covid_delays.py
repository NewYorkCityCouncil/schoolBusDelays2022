import pandas as pd
import numpy as np
import re
import requests

if __name__ == "__main__":
    print("Running get_filtered_weekends_vacation_covid_delays.py")

    data = pd.read_csv('../data/input/modified/DOE School Calendar Dates_edited.csv').drop(columns = ['Unnamed: 3'])
    data = data.replace('–', '-') 

    start_dates, end_dates = [], []
    for i in data['Dates']:
        if ('-' not in i):
            start_dates.append(pd.to_datetime(i))
            end_dates.append(pd.to_datetime(i))
        else:
            x = i.split(' ')
            start_dates.append(pd.to_datetime(' '.join([k.split('-')[0] if ('-' in k) else k for k in x])))
            end_dates.append(pd.to_datetime(' '.join([k.split('-')[1] if ('-' in k) else k for k in x])))
            
    data['Start Date'] = start_dates
    data['End Date'] = end_dates 

    def get_year(school_year, month):
        if int(month) < 9:
            return int(school_year[:4]) + 1
        else:
            return int(school_year[:4]) 
        
    data['End Date'] = pd.to_datetime(data['End Date'].dt.month.astype(str) + "/" + data['End Date'].dt.day.astype(str) + "/" + data.apply( (lambda x: get_year(x['School Year'], x['End Date'].month)), axis = 1).astype(str))

    data['Start Date'] = pd.to_datetime(data['Start Date'].dt.month.astype(str) + "/" + data['Start Date'].dt.day.astype(str) + "/" + data.apply( (lambda x: get_year(x['School Year'], x['Start Date'].month)), axis = 1).astype(str))

    covid = {'School Year': '2021-2022', 
            'Dates': '3/16/2020 - 9/13/2021', 
            'Reason': 'COVID', 
            'Start Date': pd.to_datetime('3/16/2020'), 
            'End Date': pd.to_datetime('9/13/2021')
            }

    covid_df = pd.DataFrame([covid])
    data = pd.concat([data, covid_df], ignore_index=True)

    data['Closure'] = data['Reason'].apply( lambda x: 1 if  ( ("closed" in x) | 
                                        ("no students" in x) | 
                                        ("9-12 schools only" in x) | 
                                        ("remote" in x) | 
                                        ("hours early" in x) | 
                                        ("do not attend" in x) | 
                                        ("CLOSED" in x) | 
                                        ("Clerical Day" in x) | 
                                        ("staff development" in x) | 
                                        ("Students not in attendance" in x) | 
                                        ("Students will not" in x) | 
                                        ("will not be in attendance" in x) | 
                                        ("COVID" in x) | 
                                        ("Summer Recess" in x) ) else 0 )

    url = "https://data.cityofnewyork.us/resource/ez4e-fazm.json?$limit=99999999&$where=school_year%20not%20in(%272015-2016%27,%20%272016-2017%27)"
    response = requests.get(url, verify=True)
    data_orig = pd.read_json(response.text)

    data_orig['occurred_on'] = pd.to_datetime(data_orig['occurred_on'])

    # using this to filter to dates of interest
    # data_orig = data_orig[data_orig['occurred_on'] < '2024-07-01']

    def clean_delay_times(x):
        a = x
        x = str(x).lower().rstrip().replace('1 1/2', '1 2').replace('11/2', '1 2').\
        replace('1/2', '0 1').replace('half', '0 1').replace('to', '-')
        units = re.findall(r'[a-z ]+', x)
        time = re.split(r'[- /,]', ''.join(re.findall(r'[0-9 .,/-]+', x)) )
        time = [i for i in time if not((re.search('[.]+',i) !=None)|(re.search('[ ]+',i) !=None)|(i==''))]
        if time:
            time = np.nanmean([float(i) for i in time])
        else:
            time = 0
        
        if any(i in ''.join(units) for i in ['h', 'r']) and ( time <= 5 ):
            return time*60
        elif any(i in ''.join(units) for i in ['m', 'n']) | (x == ''):
            return time
        else:
            return 0
        
    data_orig['delay_time'] = data_orig.how_long_delayed.astype(str).apply(clean_delay_times)

    days_closed = data[data['Closure'] == 1]

    ###########Faster Version####################
    data_orig['closed'] = 1

    for row, val in days_closed.iterrows():
        a = val['Start Date']
        b = val['End Date'] + pd.DateOffset(1)
        data_orig.loc[:,('closed')] = data_orig.loc[:,('closed')]*(~data_orig.loc[:,('occurred_on')].apply(
            lambda x: (x > a) & (x < b)))

    data_orig = data_orig[data_orig['closed'] == 1]
    #############################################
    # Eliminate weekends

    data_orig = data_orig[data_orig['occurred_on'].dt.dayofweek <= 4]

    data_orig.to_feather("../data/output/filtered_weekends_vacation_covid_delays.feather")
