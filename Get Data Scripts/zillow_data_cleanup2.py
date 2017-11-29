import pandas as pd

## Zillow data that contains ZHIV, Median Home Value, Median Listing Price and Median Sale Price by Zip Code
xl = pd.ExcelFile("Zillow-NYC.xlsx")
df_zip = xl.parse('Zip') ## In Zip tab
## Separate out relevant date columns and store as list
date_column = [column for column in df_zip.columns.tolist() if column not in ['Dataset', 'RegionName', 'City','State','Metro','CountyName','SizeRank']]
all_columns = df_zip.columns.tolist()

## Normalize zillow data by dividing by the max of each of the dataset
df_zip = df_zip[df_zip['Dataset']!='Median Sale Price per sqft']
df_zip_max = df_zip[['2016-08', 'Dataset']].groupby(['Dataset']).max().reset_index() ##Taking 8/2016 because data available across all dataset
df_zip_max.columns = ['Dataset','Max']
df_zip = pd.merge(df_zip, df_zip_max, how = 'inner', on = ['Dataset'])
df_zip[date_column] = df_zip[date_column].div(df_zip['Max'], axis=0)
df_zip = df_zip[all_columns]
df_zip_grouped = df_zip.groupby(['RegionName']).mean()

df_zip_grouped = df_zip_grouped.T[df_zip_grouped.T.index!='SizeRank'] ##remove unnecessary data
df_zip_grouped['Year'] = df_zip_grouped.index.map(lambda x: x[0:4]) ##Get year from date format YYYY-MM

df_zip_grouped_sum = df_zip_grouped.groupby(['Year']).agg(['sum']) ##Get total sum by year
df_zip_grouped_sum.columns = df_zip_grouped_sum.columns.get_level_values(0) ##Reset multi-index columns
df_zip_grouped_count = df_zip_grouped.groupby(['Year']).agg(['count']) ##Get total count by year
df_zip_grouped_count.columns = df_zip_grouped_count.columns.get_level_values(0) ##Reset multi-index columns
df_zip_grouped_year = df_zip_grouped_sum / df_zip_grouped_count ##element wise division

## Melt the data
df_zip_grouped_year = pd.melt(df_zip_grouped_year.reset_index(), id_vars=['Year'], value_vars=df_zip_grouped_year.columns, var_name='ZIP Code', value_name='Zillow_value' )
df_zip_grouped_year[['ZIP Code', 'Year']] = df_zip_grouped_year[['ZIP Code', 'Year']].apply(pd.to_numeric) ## Convert string to numeric

df_zip_grouped_year.to_csv('Zillow_by_Zip_year.csv', index=False)