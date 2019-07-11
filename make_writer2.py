import sqlite3
import csv
from nltk.corpus import names
import re

sqlite_file= '/Users/michaeltauberg/projects/NewsScraper/newsdb_v6.sqlite'
con = sqlite3.connect(sqlite_file)
c = con.cursor()


c.execute("SELECT date,authors, url,headline,keywords, summary, content FROM Headlines")
all_rows = c.fetchall()
OUTFILE = "news_writer_data.csv"
out_file = open(OUTFILE, 'w') 
keys = ['date', 'author', 'gender', 'url', 'headline']
dict_writer = csv.DictWriter(out_file, keys)
dict_writer.writeheader()

sqlite_file_small = '/Users/michaeltauberg/projects/NewsScraper/writers3.sqlite'
con_small = sqlite3.connect(sqlite_file_small)
c_small = con_small.cursor()


def get_first_name(name):
    print "MT " + name + " MT"
    return name.split()[0]

try:
    c_small.executescript('''

    CREATE TABLE Headlines (
        id  INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT UNIQUE,
        date    TEXT, 
        authors TEXT,
        gender  TEXT,
        url     TEXT UNIQUE,
        headline TEXT
        );

    ''')
except:
    print("database already exists")
    #import sys
    #sys.exit()


m_file = open("male.txt", "r")
males = m_file.readlines()
males = [w.replace('\n', '') for w in males]
f_file = open("female.txt", "r")
females = f_file.readlines()
females = [w.replace('\n', '') for w in females]


#print males
#import sys
#sys.exit()


for row in all_rows:
    authors = row[1].strip().split(':')
    print authors
    for author in authors:
        if (author.isspace() or (author == '') or (author is None)):
            continue
        first_name = get_first_name(author)
        if (first_name in males):
            gender = "male" 
        elif (first_name in females):
            gender = "female"
        else:
            gender = "unknown"

        csv_row = {}
        try:
            csv_row['date'] = row[0].encode('utf-8')
            csv_row['author'] = author.encode('utf-8')
            csv_row['gender'] = gender
            csv_row['url'] = row[2].encode('utf-8')
            csv_row['headline'] = row[3].encode('utf-8')
            print(csv_row)
            dict_writer.writerow(csv_row)
        except:
            pass

        if re.match("Buzzfeed", author):
            print author
            print csv_row
            import sys
            sys.exit()

        c_small.execute('''INSERT OR REPLACE INTO Headlines
            (date, authors, gender, url, headline) 
            VALUES ( ?, ?, ?, ?, ?)''', 
            ( row[0], author, gender, row[2], row[3]) )
	con_small.commit()
       
