#!/bin/bash python2

from string import Template

blog_file = open("writeup.template")
cmp_results_file = open("diffgraph.html")
x86_file = open("x86html")
arm_file = open("armhtml")

blog = Template(blog_file.read())
blog_text = blog.substitute(comparisonresults=cmp_results_file.read(), armresults=arm_file.read(), x86results=x86_file.read())
f = open('writeup.md','w')
f.write(blog_text)
f.close()

blog_file.close()
cmp_results_file.close()
x86_file.close()
arm_file.close()
