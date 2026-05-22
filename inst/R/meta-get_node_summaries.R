url <- "https://cn.dataone.org/cn/v2/query/solr/?q=formatType:METADATA&facet=true&facet.field=authoritativeMN&rows=0"

library(xml2)
library(dplyr)

doc <- xml2::read_xml(url)

facet_nodes <- xml_find_all(doc, "//lst[@name='authoritativeMN']/int")

nodes <- data.frame(
    node = xml_attr(facet_nodes, "name"),
    count = as.integer(xml_text(facet_nodes)),
    stringsAsFactors = FALSE) %>% 
    filter(count > 5)

node_list <- nodes$node
node_str <- gsub("urn:node:", "*", node_list,)

formats_url <- paste0("https://cn.dataone.org/cn/v2/query/solr/?q=formatType:METADATA%20AND%20authoritativeMN:", node_str,"&facet=true&facet.field=formatId&rows=0")

formats <- list()

for (i in seq_along(formats_url)){
    d <- read_xml(formats_url[i])
    fns <- xml_find_all(d, "//lst[@name='formatId']/int")
    formats[[i]] <- data.frame(
        format = xml_attr(fns, "name"),
        count = as.integer(xml_text(fns)),
        stringsAsFactors = FALSE
    ) %>% 
        filter(count > 5) %>% 
        mutate(node = node_list[i])
}

fmt_all <- do.call(rbind, formats) %>% 
    filter(!grepl("beta|fgdc|onedcx|portal|oai|mercury", format, ignore.case = TRUE)) %>% 
    mutate(format_clean = case_when(
        grepl("eml", format, ignore.case = TRUE) ~ "EML",
        grepl("gmd|isotc211", format)           ~ "ISO",
        grepl("schema\\.org", format)           ~ "schema.org",
    )) %>% 
    filter(!is.na(format_clean)) %>% 
    select(-format) %>% 
    group_by(node, format_clean) %>% 
    summarise(count = sum(count)) %>% 
    tidyr::pivot_wider(names_from = format_clean, values_from = count, values_fill = 0) %>% 
    filter(!grepl("Test", node))

df <- tibble::tribble(
    ~node,              ~number_run,
    "urn:node:ARCTIC",         56232,
    "urn:node:BCODMO",         22375,
    "urn:node:BONARES",        1376,
    "urn:node:BOREALIS",       12869,
    "urn:node:CANWIN",         31841,
    "urn:node:CA_OPC",         22,
    "urn:node:CERP_SFWMD",     41,
    "urn:node:CIB",            13,
    "urn:node:DRP",            1188,
    "urn:node:DRYAD",          27300,
    "urn:node:DVNO",           409,
    "urn:node:EDI",            2273,
    "urn:node:ESS_DIVE",       16860,
    "urn:node:FEMC",           39,
    "urn:node:FWC",            40,
    "urn:node:GEM",            14022,
    "urn:node:GEUS",           668,
    "urn:node:GLEON",          15,
    "urn:node:GRIIDC",         3583,
    "urn:node:HD",             16820,
    "urn:node:HYDROSHARE",     18564,
    "urn:node:IEDA_EARTHCHEM", 1678,
    "urn:node:IEDA_USAP",      1445,
    "urn:node:ISRIC",          293,
    "urn:node:KNB",            27278,
    "urn:node:LTER",           16502,
    "urn:node:NORDICANA_D",    8,
    "urn:node:NSIDC",          5885,
    "urn:node:OBIS",           4,
    "urn:node:OBIS_SEAMAP",    245,
    "urn:node:PANGAEA",        253468,
    "urn:node:PDC",            2582,
    "urn:node:PISCO",          90,
    "urn:node:RW",             871,
    "urn:node:SCTLD",          10,
    "urn:node:SDR",            92,
    "urn:node:SFWMD",          177,
    "urn:node:SI",             998,
    "urn:node:SNAP",           72,
    "urn:node:USANPN",         7
)

all <- full_join(df, fmt_all) %>% 
    group_by(node) %>% 
    mutate(total_docs = sum(EML, ISO, schema.org, na.rm = T)) %>% 
    select(node, number_run, total_docs, EML, ISO, schema.org) %>% 
    mutate(done = if_else(number_run/total_docs > 0.8, "done", NA))
