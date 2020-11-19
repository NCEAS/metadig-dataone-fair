#POTENTIALLY TO BE DELETED
#removing ~300 datasets from a bunch of glacier photo dataseets as a data recovery project
library(dataone)

#options(dataone_token =)
print("WARNING: DON'T FORGET TO ADD ARCTIC DATA TOKEN")

cn <- CNode("PROD")
adc <- getMNode(cn, "urn:node:ARCTIC")
# cd <- query(adc, list(q = 'formatType:METADATA AND title:photogrammetric',
#                       fl = 'id,dateUploaded,obsoletes',
#                       sort = 'dateUploaded+desc',
#                       rows='1000000'),
#             as = "data.frame")
# result <- cd %>% 
#   filter(is.na(obsoletes))
# 
# temp_index <- merge(result, checks_aggregate_ADC, by.x="id", by.y="pid")
# 
# NSF_data <- NSF_data %>% 
#   filter(!sequenceId %in% temp_index$sequenceId)



#create list of data team members
data_team_members <- c("CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org",
                       "BryceMecum",
                       "CN=Lauren Walker A10489,O=Google,C=US,DC=cilogon,DC=org",
                       "LaurenWalker",
                       "CN=Christopher Jones A2108,O=Google,C=US,DC=cilogon,DC=org",
                       "ChrisJones",
                       "CN=Lauren Walker A10489,O=Google,C=US,DC=cilogon,DC=org",
                       "CN=urn:node:ARCTIC,DC=dataone,DC=org",
                       "uid=mecum,ou=Account,dc=ecoinformatics,dc=org",
                       "uid=cjones,ou=Account,dc=ecoinformatics,dc=org",
                       "CN=Matt Jones A729,O=Google,C=US,DC=cilogon,DC=org",
                       "MattJones",
                       "http://orcid.org/0000-0001-8199-864X", # couture
                       "http://orcid.org/0000-0002-8121-2341", # c jones
                       "http://orcid.org/0000-0003-0077-4738", # m jones
                       "http://orcid.org/0000-0002-0381-3766", # mecum
                       "http://orcid.org/0000-0002-1006-9496", # goldstein
                       "http://orcid.org/0000-0002-9766-8044", # emery
                       "http://orcid.org/0000-0003-1180-9959", # john clark
                       "http://orcid.org/0000-0002-1586-0121", # heller
                       "http://orcid.org/0000-0002-8941-9259", # ng
                       "http://orcid.org/0000-0003-1758-9950", # leinfelder
                       "http://orcid.org/0000-0002-0480-2663", # tan
                       "http://orcid.org/0000-0003-2192-431X", # walker
                       "http://orcid.org/0000-0001-5966-0466", # prescott
                       "http://orcid.org/0000-0001-8775-4996", # raquel
                       "http://orcid.org/0000-0001-7489-5423", # su
                       "http://orcid.org/0000-0001-8778-5696", # meade
                       "http://orcid.org/0000-0003-4703-1974", # jeanette clark
                       "http://orcid.org/0000-0003-2077-855X", # oliver
                       "http://orcid.org/0000-0002-2192-403X", # slaughter
                       "http://orcid.org/0000-0002-4244-2865", # cornejo
                       "http://orcid.org/0000-0003-0047-8808", # louchouarn
                       "http://orcid.org/0000-0001-7847-0043", # mcgill
                       "http://orcid.org/0000-0003-0740-3649", # beck
                       "http://orcid.org/0000-0003-0003-2515", # bachtel
                       "http://orcid.org/0000-0002-1076-8342", # boyle
                       "http://orcid.org/0000-0001-9205-2948", # gordee
                       "http://orcid.org/0000-0001-7553-6692", # graybiel
                       "http://orcid.org/0000-0003-2245-3804", # halperin
                       "http://orcid.org/0000-0002-0979-560X", # kim
                       "http://orcid.org/0000-0001-6695-8383", # lee
                       "http://orcid.org/0000-0002-2561-5840", # mullen
                       "http://orcid.org/0000-0003-3283-554X", # ochs
                       "http://orcid.org/0000-0002-1681-9032", # sum
                       "http://orcid.org/0000-0002-3681-1337", # ou
                       "http://orcid.org/0000-0002-1209-5268", # jing tao
                       "http://orcid.org/0000-0003-3697-394X", # tran
                       "http://orcid.org/0000-0001-8594-506X", # sophia tao
                       "http://orcid.org/0000-0002-7822-2303", # sun
                       "http://orcid.org/0000-0002-4830-5378", # reeder
                       "http://orcid.org/0000-0002-6316-0058", # randazzo
                       "http://orcid.org/0000-0003-2057-2578", # nguyen
                       "http://orcid.org/0000-0002-7811-0504", # mohanti
                       "http://orcid.org/0000-0003-2878-3943", # foran
                       "http://orcid.org/0000-0002-2820-8714", # carlson
                       "http://orcid.org/0000-0001-7379-185X", # freund
                       "http://orcid.org/0000-0001-6955-0535", # maier
                       "http://orcid.org/0000-0002-6220-0134", # o'dean
                       "http://orcid.org/0000-0002-5511-9717", # steves
                       "http://orcid.org/0000-0003-0200-0787", # meyer
                       "http://orcid.org/0000-0002-3118-8697", # raymond
                       "http://orcid.org/0000-0002-7705-5670", # johnson
                       "http://orcid.org/0000-0003-1264-1166", # chong
                       "http://orcid.org/0000-0001-9663-2923", # mortensen
                       "http://orcid.org/0000-0003-0632-7576", # schildhauer
                       "http://orcid.org/0000-0002-0347-8574", # strong
                       "http://orcid.org/0000-0002-1615-3963", # thiessen-bock
                       "http://orcid.org/0000-0001-8874-7595", # kibele
                       "http://orcid.org/0000-0001-6885-9821", # reevesman
                       "http://orcid.org/0000-0003-1241-8351", # monper
                       "http://orcid.org/0000-0001-6602-1558", # pruett
                       "https://orcid.org/0000-0002-7822-2303", # sun
                       "https://orcid.org/0000-0003-3515-6710", # chen
                       "https://orcid.org/0000-0003-1070-9585", # clarin
                       "https://orcid.org/0000-0001-6618-3928", # erickson
                       "https://orcid.org/0000-0001-8888-547X", # lai
                       "https://orcid.org/0000-0001-8613-8956", # mclean
                       "https://orcid.org/0000-0002-6388-0901", # peach
                       "https://orcid.org/0000-0002-5248-9712", # samet
                       "CN=arctic-data-admins,DC=dataone,DC=org") 



#additional dataset removal. attempting remove all datasets initially uploaded by a datateam member
all_ADC <- query(adc, list(q = "formatType:METADATA AND dateUploaded:[2016-03-21T00:00:00Z TO NOW] AND (*:* NOT obsoletes:*)",
                           fl = "identifier, writePermission",
                           rows = "10000"),
                 as="data.frame")

#spaces in names removed
all_ADC$writePermission <- gsub("CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org", "BryceMecum", all_ADC$writePermission)
all_ADC$writePermission <- gsub("CN=Lauren Walker A10489,O=Google,C=US,DC=cilogon,DC=org", "LaurenWalker", all_ADC$writePermission)
all_ADC$writePermission <- gsub("CN=Christopher Jones A2108,O=Google,C=US,DC=cilogon,DC=org", "ChrisJones", all_ADC$writePermission)
all_ADC$writePermission <- gsub("CN=Matt Jones A729,O=Google,C=US,DC=cilogon,DC=org", "MattJones", all_ADC$writePermission)
all_ADC$writePermission <- gsub("CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org", "BryceMecum", all_ADC$writePermission)
all_ADC$writePermission <- gsub("CN=Bryce Mecum A27576,O=Google,C=US,DC=cilogon,DC=org", "BryceMecum", all_ADC$writePermission)

#convert double space to single space
all_ADC$writePermission <- gsub("  ", " ", all_ADC$writePermission)

#trim whitespace at beginning and end of names
all_ADC$writePermission <- str_trim(all_ADC$writePermission, side="both")


#replace all Data Team ORCIDs and admin codes with blank
all_ADC$writePermission2 <- textclean::mgsub(all_ADC$writePermission, data_team_members, "")


#convert double space to single space
all_ADC$writePermission2 <- gsub("  ", " ", all_ADC$writePermission2)

#trim whitespace at beginning and end of names
all_ADC$writePermission2 <- str_trim(all_ADC$writePermission2, side="both")


#count number of blanks, which are data team submissions
for (i in 1:nrow(all_ADC)) {
  all_ADC$length[i] <- nchar(all_ADC$writePermission2[i])
}

#count data team submissions
nrow(all_ADC[which(all_ADC$length==0),]) #1308

#mark data team submissions
all_ADC$DT_submission <- ifelse(all_ADC$length >0, "non-DT", "DataTeam")


#get sequenceIDs associated with the Data Team submissions
DataTeam_index <- merge(all_ADC, checks_aggregate_ADC, by.x="identifier", by.y="pid", all.x = TRUE)
DataTeam_index <- DataTeam_index[,c("identifier", "writePermission", "writePermission2", "length", "DT_submission", "sequenceId")]


#keep only entries that Data Team initiated the initial upload
DataTeam_index <- DataTeam_index[DataTeam_index$DT_submission=="DataTeam",]






