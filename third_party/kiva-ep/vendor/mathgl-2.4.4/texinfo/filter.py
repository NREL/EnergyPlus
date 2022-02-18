#!/usr/bin/python

import os,string,re,glob

print 'Tuning resulting html files for Qt\n'

beginskip=0
for filename in glob.glob('./*ru.html/*html'):
    fpin=open(filename,'r')
    fpout=open(filename+'.tmp','w')
    for line in fpin.readlines():
        if re.search('<html>',line): 
            beginskip=1
            fpout.write(line)
            continue
        if re.search('<head>',line): beginskip=0
        if re.search('<meta name=\"keywords"\"',line): continue
        if re.search('<meta name=\"description\"',line): continue
        if re.search('<meta name=\"distribution\" content=\"global\">',line): continue
        if re.search('<meta name=\"Generator\" content=\"texi2html',line): continue
        if beginskip==0 : fpout.write(line)
    fpin.close()
    fpout.close()
    os.remove(filename)
    os.rename(filename+'.tmp', filename)
