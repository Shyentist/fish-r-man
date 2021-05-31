---
title: 'fishRman: A Shiny R Dashboard improving Global Fishing Watch data availability'

tags:
  - R
  - fisheries
  - marine biology
  - global fishing watch
  - AIS data
  - dashboard
  - shiny
authors:
  - name: Pasquale Buonomo
    orcid: 0000-0002-1848-9313
date: 31 May 2021
bibliography: paper.bib
---

# Summary
One of the burdens of fisheries scientists is the scarcity or lack of consistent, 
extensive data on the subject. When such data do exist, they are often only available:

- To universities or other research institutions;
- Through bureaucratic ordeals;
- For a fee.

This issue has been tackled by Global Fishing Watch[@GFW], an independent, international 
non-profit organization promoting ocean sustainability through greater transparency, 
visualizing, tracking and sharing data about global fishing activity for free.

While the datasets are indeed publicly available, they are also rather large and quite 
difficult to manage, since they require proficiency in the programming language R [@R], 
in the SQL query language, or both.

# Statement of need
Overcoming the major barriers described in the summary, `fishRman` configures itself as 
a web-based, one-stop solution that provides an intuitive user interface for querying, 
downloading, analyzing, and visualizing Global Fishing Watch data on fishing effort.

The user-friendly interface allows users to easily interact with the SQL query 
constructor, building and running queries with ease. In a few clicks, users are able to 
analyze and visualize retrieved data in several different ways, such as subsetting the 
top n-th percentile for any percentage, calculating the fishing effort exerted by 
specific countries via certain geartypes, and producing time series of fishing effort 
with a daily, monthly, or yearly frequency [@dplyr; @sf; @ggplot].

Designed with ease of use in mind, `fishRman` is intended for a public of researchers,
students, managers, and stakeholders in the field of fisheries management with little
to no proficiency in programming, data analysis, or both. More experienced users may
also benefit from the software to avoid writing lines of code for what has already
been implemented in the dashboard, in order to focus on other aspects of their research,
or even customize the source code to better meet their specific needs.

# References