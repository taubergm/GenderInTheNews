import re
writers = []
with open('writers_july.csv', 'r') as f:
    for line in f:
        line = re.sub("\"","",line)
        authors = line.strip().split(':') 
        for author in authors:
            writers.append(author)


#writers = uniq(writers)
for writer in writers:
    m = re.match("(\w+) (\w+)$", writer)
    if m:
        #print(m.groups())
        print "\"%s\"" % writer


