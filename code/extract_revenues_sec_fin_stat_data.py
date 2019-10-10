import pandas as pd

df_names = ["sub", "tag", "num", "pre"]
for df in df_names:
  globals()[df] = pd.read_csv('data/' + df + '.csv')

df_merged = sub[(sub.countryba == "US") & (sub.fp.str[0] == 'Q')]

df_merged = df_merged[['adsh', 'cik', 'name', 'sic', 'fp', 'period', 'fye']]
df_merged = df_merged[
  df_merged.groupby(['cik'])['period'].transform(max) == df_merged.period]

df_merged = pd.merge(df_merged, pre, on = ['adsh'], how = 'left')

df_merged = df_merged[(df_merged.stmt == "IS") & 
  ((df_merged.tag == "Revenues") | 
  (df_merged.tag == "RevenueFromContractWithCustomerExcludingAssessedTax") |
  (df_merged.tag == "RevenueFromContractWithCustomerIncludingAssessedTax"))]

df_merged = pd.merge(df_merged, num, on = ['adsh', 'tag', 'version'], how = 'left')

df_merged = df_merged[(df_merged.ddate == df_merged.period) & 
  (df_merged.uom == "USD") &
  (df_merged.qtrs == 1) & 
  df_merged.value.notnull() &
  df_merged.coreg.isna()]

df_merged = df_merged[
  df_merged.groupby(['adsh', 'cik'])['value'].transform(max) ==
  df_merged.value]
  
df_merged = df_merged.rename({'value': 'total_revenue'}, axis = 1)

df_merged = df_merged[['adsh', 'cik', 'name', 'sic', 'fp', 'fye', 
         'ddate', 'total_revenue']]

df_merged = df_merged.drop_duplicates(['cik', 'name', 'sic', 'fp', 
        'fye', 'ddate', 'total_revenue'])

print(len(df_merged.adsh))
