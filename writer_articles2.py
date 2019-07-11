import sqlite3
import re
from nltk.corpus import names
import csv


sqlite_file = '/Users/michaeltauberg/projects/NewsScraper/writers3.sqlite'
conn = sqlite3.connect(sqlite_file)
c = conn.cursor()
f = open("writers_all_to_july3.csv") 

# update writers_all with the latest data then update

# get writers 
writers = [line.rstrip('\n') for line in f]
# set up output csv
csvFile = "all_writers_gender_outlet3.csv"
outCsv = open(csvFile, 'w')
fieldnames = ['writer','num_articles','gender', 'last_url']
csv_writer = csv.DictWriter(outCsv, fieldnames=fieldnames)
csv_writer.writeheader()


for writer in writers:
    count = 0 
    try:
        writer = writer.encode('utf-8')
    except:
        continue
    print(writer)
    #c.execute('SELECT * FROM {tn} WHERE lower({cn}) like "%{sn}%"'.format(tn="Headlines", cn="authors", sn=writer))

    cmd = "SELECT * FROM Headlines WHERE authors=%s" % writer
    print(cmd)
    c.execute(cmd)
    writer_rows = c.fetchall()
    num_articles = len(writer_rows)
    if (num_articles == 0):
    	continue
   
    writer = re.sub("\"","", writer)
    gender = writer_rows[-1][3]
    last_url = writer_rows[-1][4]

    csv_row = {}
    csv_row['writer'] = writer
    csv_row['num_articles'] = num_articles
    csv_row['gender'] = gender
    csv_row['last_url'] = last_url
            
    print("%s, %s, %s %s" % (writer, num_articles, gender, last_url))
    csv_writer.writerow(csv_row)

    
