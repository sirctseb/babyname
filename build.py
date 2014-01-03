#!/usr/bin/python

import httplib, urllib, sys

# Define the parameters for the POST request and encode them in
# a URL-safe format.
g = open('enamerator.js')

params = urllib.urlencode([
    # ('code_url', sys.argv[1]), # <--- This parameter has a new name!
    # ('code_url', 'enamerator.js'),
    ('js_code', g.read()),
    ('compilation_level', 'ADVANCED_OPTIMIZATIONS'),
    ('output_format', 'text'),
    ('output_info', 'compiled_code'),
  ])

# Always use the following value for the Content-type header.
headers = { "Content-type": "application/x-www-form-urlencoded" }
conn = httplib.HTTPConnection('closure-compiler.appspot.com')
conn.request('POST', '/compile', params, headers)
response = conn.getresponse()
data = response.read()
f = open('enamerator.min.js', 'w')
f.write(data)
# print data
conn.close()