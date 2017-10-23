import pandas as pd

## Zillow data that contains ZHIV, Median Home Value, Median Listing Price and Median Sale Price by Zip Code
xl = pd.ExcelFile("Zillow-NYC.xlsx")
df_zip = xl.parse('Zip') ## In Zip tab
## Separate out relevant date columns and store as list
date_column = [column for column in df_zip.columns.tolist() if column not in ['Dataset', 'RegionName', 'City','State','Metro','CountyName','SizeRank']]

df_zip = df_zip[df_zip['Dataset']!='Median Sale Price per sqft'] ## removed because too spotty
df_zip[date_column] = df_zip[date_column].div(df_zip['2016-08'], axis=0) ##Date that is avaialble across all datasets
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

df = pd.read_csv('Income_Home_Prices_ZIP.csv') ## Read the master data
df = pd.merge(df,df_zip_grouped_year,how='left', on=['ZIP Code', 'Year']) ##Merge
df.to_csv('Income_Home_Prices_ZIP.csv', index=False)