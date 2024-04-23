import xarray as xr
from tqdm import tqdm
import pandas as pd
import os



"""
steps:

convert_netcdf_to_csv():
    - loop through netcdf files in gldas/
        - open the file w/ xarray
        - convert to dataframe
        - save to csv in parsed/ directory
            - format: gldas_{year}_{month}.csv


    - open outbreak csv
    - extract longitude, latitude, and location_period_id
    - remove duplicate rows

get_soil_moisture_and_temperature():
    - loop through each year
        - loop through each month
            - open the corresponding gldas csv
            - find the corresponding precipitation value from the csv
                - round the lat & lon values to match the gldas data
            - append the precipitation value to the outbreak csv
    - save file
"""

def convert_netcdf_to_csv():
    print("Converting netcdf files to csv...")
    files = os.listdir("./gldas/")
    files.sort()
    for file_name in tqdm(files):
        date = file_name.split("%2F")[5].split(".")[1][1:]
        year = date[:4]
        month = date[4:]
        dataset = xr.open_dataset(f"./gldas/{file_name}")
        df = dataset.to_dataframe().reset_index().drop_duplicates()
        # keep lon, lat, SoilMoi0_10cm_inst, and SoilTMP0_10cm_inst
        df = df[['lon', 'lat', 'SoilMoi0_10cm_inst', 'SoilTMP0_10cm_inst']]
        df.to_csv(f"./parsed/gldas_{year}_{month}.csv")

def round_lat(lat):
    # range of latitude is from -34.875 to 49.875, with steps of 0.25
    # round the latitude to the nearest 0.25
    rounded_lat = round((lat - 0.125) / 0.25) * 0.25 + 0.125

    # ensure the result is within the range -34.875 to 49.875
    return max(min(rounded_lat, 49.875), -34.875)

def round_lon(lon):
    # range of longitude is from -19.875 to 59.875, with steps of 0.25
    # round the longitude to the nearest 0.25
    rounded_lon = round((lon - 0.125) / 0.25) * 0.25 + 0.125

    # ensure the result is within the range -19.875 to 59.875
    return max(min(rounded_lon, 59.875), -19.875)

def round_coordinates(row):
    row['lat_rounded'] = round_lat(row['latitude'])
    row['lon_rounded'] = round_lon(row['longitude'])
    return row

def get_info(row, date, info):
    lat = row['lat_rounded']
    lon = row['lon_rounded']
    row[f"soilMois-{date}"] = info.loc[(info['lat'] == lat) & (info['lon'] == lon), 'SoilMoi0_10cm_inst'].values[0]
    row[f"soilTemp-{date}"] = info.loc[(info['lat'] == lat) & (info['lon'] == lon), 'SoilTMP0_10cm_inst'].values[0]
    return row

def get_soil_temperature_(row, date, info):
    lat = row['lat_rounded']
    lon = row['lon_rounded']
    row[f"soilTemp-{date}"] = info.loc[(info['lat'] == lat) & (info['lon'] == lon), 'SoilTMP0_10cm_inst'].values[0]
    return row

def get_soil_moisture_(row, date, info):
    lat = row['lat_rounded']
    lon = row['lon_rounded']
    row[f"soilMois-{date}"] = info.loc[(info['lat'] == lat) & (info['lon'] == lon), 'SoilMoi0_10cm_inst'].values[0]
    return row

def add_info(mois_df, temp_df):
    print("adding soil moisture and temperature data to new file...")
    for year in tqdm(range(2010, 2020), desc=f"total progress"):
        for month in tqdm(range(1, 13), desc=f"   {year} months"):
            date = f"{year}_{month:02}"
            info = pd.read_csv(f"./parsed/gldas_{year}_{month:02}.csv")
            mois_df = mois_df.apply(get_soil_moisture_, args=(date, info), axis=1)
            # temp_df = temp_df.apply(get_soil_temperature_, args=(date, info), axis=1)
    
    mois_df.to_csv("./outbreak_data_plus_moisture.csv", index=False)
    # temp_df.to_csv("./outbreak_data_plus_temperature.csv", index=False)

if __name__ == "__main__":
    # convert_netcdf_to_csv()
    outbreaks = pd.read_csv("./outbreak_data.csv")
    moisture = pd.DataFrame(outbreaks[['latitude', 'longitude', 'location_period_id']])
    moisture = moisture.drop_duplicates()   # soil moisture and soil temperature
    moisture = moisture.apply(round_coordinates, axis=1)
    temperature = moisture.copy(deep=True)
    add_info(moisture, temperature)



