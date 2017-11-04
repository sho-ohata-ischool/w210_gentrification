import pandas as pd

df = pd.read_csv('Income_Home_Prices_ZIP_v2.csv')
column_names = df.columns.tolist() ##master column list

permits = pd.read_csv('NYC permit yearly count.csv')
crimes = pd.read_csv('NYC violent crime yearly count.csv')

## Need to pre-process permits data a bit
permits = permits.pivot_table(index=['Zip_Code', 'YEAR'], columns='Job_Type') ##un-melt data
permits.columns = permits.columns.droplevel().rename(None) ## drop nested columns
permits = permits.reset_index()
column_names.extend(['A1','A2','DM','NB'])
df2 = pd.merge(df, permits, how='left', left_on=['ZIP', 'Year'], right_on=['Zip_Code', 'YEAR']) ##merge with permit data
df2 = df2[column_names]

## Crime data is easy to merge
column_names.extend(['CRIME_COUNT'])
df2 = pd.merge(df2, crimes, how='left', left_on=['ZIP', 'Year'], right_on=['Zip_Code', 'YEAR'])
df2 = df2[column_names]

assert(df.shape[0]==df2.shape[0]) ##assertion just to double check rows weren't dropped
df2.to_csv('Income_Home_Prices_ZIP_v2.csv', index=False)