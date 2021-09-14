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
  - spatial analysis
authors:
  - name: Pasquale Buonomo
    orcid: 0000-0002-1848-9313
    affiliation: 1
affiliations:
 - name: Open-Source for Marine and Ocean Sciences (OSMOS)
   index: 1
date: 06 September 2021
bibliography: paper.bib
---

# Summary
One of the burdens of fisheries scientists is the scarcity or lack of consistent, 
extensive data on the subject. When such data do exist, they are often only available:

- To universities or other research institutions;
- Through bureaucratic ordeals;
- For a fee.

This issue has been tackled by Global Fishing Watch, an independent, international 
non-profit organization promoting ocean sustainability through greater transparency, 
visualizing, tracking and sharing data about global fishing activity for free[@GFW].

While the datasets are indeed publicly available, they are also rather large and quite 
difficult to manage, since they require proficiency in the programming language R [@R], 
in the SQL query language, or both.

# Statement of need
Life sciences will soon need a widespread integration of computational approaches to store, 
manage, analyze, and visualize datasets that are quickly growing in size and complexity [@carey]. 
This is rather concerning, given how, although the number of published papers reporting the 
use of the R statistical language increased fivefold from 2007 to 2018 in the field of ecology 
[@lai],  most life science majors do not offer basic programming courses [@mariano].

Designed with ease of use in mind, `fishRman` is intended for a public of researchers,
students, managers, and stakeholders in the fields of fisheries science, life sciences, 
and economics, with little to no proficiency in programming, data analysis, or both, who
intend to query, download, analyze, and visualize Global Fishing Watch data. 

Users who can program in R may also benefit from the software to avoid writing lines of code 
for  what has already been implemented in the dashboard, in order to focus on other aspects 
of their research, or even customize the source code to better meet their specific needs.

Users with a deeper understanding of statistics and fisheries science, and with prior knowledge 
of the datasets, only need to get acquainted with the software, while users that are new to the 
field can easily learn what they need to know via `fishRman`’s official instructions for use, the 
Handbook. Regardless of the prior knowledge of the user, reading the Handbook, which is available 
in the software itself and in the [GitHub repository](https://github.com/Shyentist/fish-r-man), is 
key to the correct usage of the software.

The user-friendly interface [@shiny; @shinyBS; @shinyjs; @shinyWidgets] allows users to 
easily interact with the SQL query constructor, seamlessly building [@glue; @stringi; @countrycode]
and running queries [@bigrquery; @DBI]. In a few clicks, users are able to analyze retrieved 
data in several different ways, such as:

- visualizing the top n-th percentile of the dataframe for any percentage [@viridis; @sf; @maps; @ggplot], 
a key passage in assessing how fishing effort overlaps fishing stocks, protected or restricted areas, or 
another country's jurisdiction. 
- calculating the fishing effort exerted by specific countries via certain geartypes, in precise areas. This
is vital in assessing who is fishing where, when, and how they are doing it, so that fisheries management plans can
address the right issues even at an international level with a clear understanding of each country's responsibilities.
- producing time series of fishing effort with a daily, monthly, or yearly frequency [@dplyr; @tidyverse], which
is indispensable when searching for patterns to compare to species' life-cycles, seafood prices over the years,
compliance to maritime and market laws, and overall consistency with data from third-parties.

# Acknowledgements
Part of this work was carried out within the financial support from [Open-Source for Marine and Ocean Sciences (OSMOS)](https://osmos.xyz/)


# References