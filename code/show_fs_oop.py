import pandas as pd

df_names = ["sub", "tag", "num", "pre"]
for df in df_names:
  globals()[df] = pd.read_csv('data/' + df + '.csv')

sub = sub[['adsh', 'cik', 'name', 'sic']]
tag = tag[['tag', 'version', 'datatype', 'tlabel']]
num = num[['adsh', 'tag', 'version', 'ddate', 'qtrs', 'uom', 'value']]
pre = pre[['adsh', 'stmt', 'line', 'tag', 'version', "plabel"]]
sub.cik = sub.cik.apply(str)
sub.sic = sub.sic.astype('Int32').apply(str)
num.ddate = num.ddate.astype('Int32') 
num.qtrs = num.qtrs.astype('Int32') 

class Firm:
  def __init__(self, cik):
    df = sub[sub.cik == cik].drop_duplicates(['cik', 'name', 'sic'])
    if len(df) == 0: 
      raise Exception("Firm " + cik + " does not show up in SEC data")
    if len(df) > 1: 
      raise Exception("Firm " + cik + " has inconsistent double entries in SEC data")
    self.cik = cik
    self.name = df.name
    self.sic = df.sic
    print("Data for firm " + self.name + " (CIK: " + self.cik +") found")
    print("Firm is classified to SIC " + self.sic)

  def get_fin_statement(self, stmt):
    fs = pd.merge(sub[sub.cik == self.cik], pre[pre.stmt == stmt], 
      on = ['adsh'], how = 'left')
    fs = pd.merge(fs, num, on = ['adsh', 'tag', 'version'], how = 'left')
    fs = fs[fs.qtrs.isnull() | fs.qtrs <= 1]
    fs = fs[fs.groupby(['cik'])['ddate'].transform(max) == fs.ddate]
    fs = fs[['line', 'plabel', 'ddate', 'qtrs', 'uom', 'value']].sort_values(by = 'line')
    return(fs.reset_index(drop = True))

valid_stmts = ['BS', 'IS', 'CF', 'EQ', 'CI', 'UN', 'CP'] 
cik = input("CIK code of firm to search: ")
stmt = input("Financial statement that you want (BS = Balance Sheet, " 
  "IS = Income Statement, CF = Cash Flow, EQ = Equity, CI = Comprehensive Income, " 
  "UN = Unclassifiable Statement, CP = Cover Page): ")

if stmt not in valid_stmts:
  print("'" + stmt + "' is not a valid statement. Try again.")
else:
  print(Firm(cik).get_fin_statement(stmt).to_string(index = False))

