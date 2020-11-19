#---
#  title: "Testing solr Queries"
#  author: "Christopher W. Beltz"
#  date created: "2020-09-04"
#  date updated: "2020-09-08"
#  packages updated: "2020-09-08"
#  R version: "3.6.3"
#  input: "..."
#  output: "metadata info"

#---


#load packages
library(dataone)
library(EML)
library(emld)
library(tidyverse)


#set up your nodes (in this case using the arcticdata node)
cn <- CNode("PROD")
adc <- getMNode(cn, "urn:node:ARCTIC") # the mn

#authentication for PROD arcticdata.io
options(dataone_token = "...")



#####################################################################################

#Data Team Reference for solr: https://nceas.github.io/datateam-training/reference/example-solr-queries.html
#List of solr query fields: https://arcticdata.io/metacat/d1/mn/v2/query/solr
#Apache Reference: https://lucene.apache.org/solr/guide/6_6/common-query-parameters.html#common-query-parameters


#test query for Bonsell parent dataset
dataone::query(adc, list(q='identifier:"doi:10.18739/A28S4JQ23"',
               rows="10"))


#query returned as a data frame using the "as" argument (outside the 'list()')
dataone::query(adc, list(q='identifier:"doi:10.18739/A28S4JQ23"',
                         fl="title",
                         rows="10"),
               as="data.frame")


#return multiple articles
dataone::query(adc, list(q='documents:*17760867-f5a6-499b-9c44-2a433f4247cf*',
                         fl="score, dateUploaded, datePublished, obsoletes, obsoletedBy, rightsHolder"))


#get all keywords
query(adc, list(q="documents:*",
                          fl="keywords",
                          rows="10"),
                as = "data.frame")


#most recent version of all Bonsell datasets
most_recent <- query(adc, list(q = "rightsHolder:*orcid.org/0000-0002-8564-0618* AND (*:* NOT obsoletedBy:*)",
                fl = "identifier, score",
                rows = "1000"),
      as="data.frame")


#get everything
result_current <- query(adc, list(q="documents:* AND (*:* NOT obsoletedBy:*)",
                          fl="identifier, dateUploaded, formatId",
                          rows="7000"),
                as = "data.frame")

colnames(result_current)[which(names(result_current) == "identifier")] <- "identifier.current"
colnames(result_current)[which(names(result_current) == "dateUploaded")] <- "dateUploaded.current"
colnames(result_current)[which(names(result_current) == "formatId")] <- "formatId.current"

result_old <- query(adc, list(q="documents:* AND (*:* NOT obsoletes:*)",
                                  fl="identifier, dateUploaded, formatId",
                                  rows="7000"),
                        as = "data.frame")


#get EML doc with all fields
full_doc <- dataone::query(adc, list(q='identifier:"doi:10.18739/A28S4JQ23"',
                         fl="*, dv_field_name", #from apache reference
                         rows="10"),
               as="data.frame")


