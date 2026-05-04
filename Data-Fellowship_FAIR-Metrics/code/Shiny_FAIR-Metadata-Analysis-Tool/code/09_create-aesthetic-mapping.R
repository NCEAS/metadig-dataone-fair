#create new asthetic mapping
aggScore_clean$aesMap <- aggScore_clean$dateSplit

aggScore_clean$aesMap <- factor(aggScore_clean$aesMap, levels=c("INITIAL", "INTERMEDIATE", "FINAL", "DOI"))

aggScore_clean$aesMap[aggScore_clean$dateSplit=="FINAL" & aggScore_clean$DOI_present=="DOI"] <- factor("DOI")


