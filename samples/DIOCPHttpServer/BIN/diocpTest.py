#!/usr/bin/env python
#coding=utf8
 
import httplib,urllib
import urllib2
import time,datetime
import random
import threading

def Inidbtest():
    for i in range(10):
        
        httpClient = None
 
        try:
            params="test=你好我是参数"
            httpClient = httplib.HTTPConnection('127.0.0.1','8081',3000)
            httpClient.request('GET','/diocp-v5')
            #response是HTTPResponse对象
            response = httpClient.getresponse()
            print response.reason.decode('utf-8').encode('gbk')
            print response.read().decode('utf-8').encode('gbk')
            #print 'start down..'
            #f = urllib2.urlopen('http://192.168.3.115:808/pad.db');
            #with open("e:\\"+time.strftime("%H%M%S")+".db", "wb") as code:
            #    code.write(f.read())
            #print 'down over..'
            #time.sleep(random.randrange(0, 6, 2))
        except Exception, e:
            print e
        finally:
            if httpClient:
                httpClient.close()

    
threads = []
for i in range(50):
    threads.append(threading.Thread(target=Inidbtest))

for t in threads:
    
    t.setDaemon(True)
    t.start()
    

    
    

