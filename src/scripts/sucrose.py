import xml.etree.ElementTree
import urllib2
import sys
import time
import random

volumes = [0,1,2,3,5,6,10,15,11,12,13,16,20,21,22,23,24,25,26,27]

for volume in volumes:
    res = urllib2.urlopen('http://judge.u-aizu.ac.jp/onlinejudge/webservice/problem_list?volume={}'.format(volume))
    if res.code != 200:
        sys.exit('Error: Invalid response')

    problem_tree = xml.etree.ElementTree.fromstring(unicode(res.read(), errors = 'ignore'))

    for problem in problem_tree.iterfind('problem'):
        print (problem.findtext('id').replace(u'\n', u'').encode('utf-8', errors = 'ignore')
          + u'\t' +  problem.findtext('name').replace(u'\n', u'').encode('utf-8', errors = 'ignore'))
    
    time.sleep(random.uniform(1, 2))
