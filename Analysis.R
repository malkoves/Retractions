library(ggthemes)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readxl)
library(plyr)
library(rcrossref)
library(gtsummary)
library(gtsummary)
library(labelled)
library(ggplot2)
library(xtable)
library(extrafont)
library(remotes)
library(ggpattern)  

yesfont_import(pattern = "lmroman*")
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

# load RW data  remove NA DOI
retraction_df <- read_xlsx("/Users/dmitrymalkov/Desktop/R/Introductory\ Data\ Science/Retraction\ Project/Retraction_df.xlsx") %>%
  as.data.frame() %>% 
  filter(!is.na(OriginalPaperDOI)) %>% 
  filter(OriginalPaperDOI != "unavailable") %>%
  filter(OriginalPaperDOI != "Unavailable") %>%
  filter(OriginalPaperDOI != "unavilable")

# numer of papers with DOI
length(unique(retraction_df$OriginalPaperDOI)) # 16095
# there are some papers with the same DOI, but different retraction dates. 
# It's just a handful, so not a big deal. But might be worth thinking of some solution
retraction_df <- retraction_df %>% filter(!duplicated(OriginalPaperDOI))

# collect CrossRef citations 
crossref_check <- function(x) { 
  print(x)
  count <- cr_citation_count(doi = x)
  return(count)
}

length(retraction_df$OriginalPaperDOI) # 16095

retr_citations_1 <- lapply(X=retraction_df$OriginalPaperDOI[1:5000], FUN=crossref_check)
retr_citations_2 <- lapply(X=retraction_df$OriginalPaperDOI[5001:10000], FUN=crossref_check)
retr_citations_3 <- lapply(X=retraction_df$OriginalPaperDOI[10001:11000], FUN=crossref_check)
retr_citations_4 <- lapply(X=retraction_df$OriginalPaperDOI[11001:12000], FUN=crossref_check)
retr_citations_5 <- lapply(X=retraction_df$OriginalPaperDOI[12001:13000], FUN=crossref_check)
retr_citations_6 <- lapply(X=retraction_df$OriginalPaperDOI[13001:14000], FUN=crossref_check)
retr_citations_7 <- lapply(X=retraction_df$OriginalPaperDOI[14001:15000], FUN=crossref_check)
retr_citations_8 <- lapply(X=retraction_df$OriginalPaperDOI[15001:16095], FUN=crossref_check)

retr_citations_full <- c(retr_citations_1,retr_citations_2,retr_citations_3,retr_citations_4,
                         retr_citations_5,retr_citations_6,retr_citations_7,retr_citations_8) 
retr_citations_binded <- bind_rows(retr_citations_full) %>% 
  dplyr::rename(citation_count_full = count) 

# write_csv(retr_citations_binded, "cross_ref_citations_RW")

retr_citations_binded <- read_csv("/Users/dmitrymalkov/Desktop/R/Introductory\ Data\ Science/Retraction\ Project/cross_ref_citations_RW")

retr_citations_binded$doi <- tolower(retr_citations_binded$doi)
retraction_df$OriginalPaperDOI <- tolower(retraction_df$OriginalPaperDOI)

retraction_df <- merge(retraction_df, retr_citations_binded, 
                       by.x = "OriginalPaperDOI", by.y = "doi", all.x = T) 

# Coding retraction reasons into types
# Errors no Academic Misconduct - Type 4
type_4 <- c("Concerns/Issues About Data","Concerns/Issues About Image",
            "Concerns/Issues About Results", "Contamination of Cell Lines/Tissues",
            "Contamination of Reagents", "Contamination of Materials \\(General\\)",
            "Error in Analyses", "Error in Cell Lines/Tissues","Error in Data",
            "Error in Image", "Error in Materials",
            "Error in Methods", "Error in Results and/or Conclusions", 
            "Results Not Reproducible", "Unreliable Data",
            "Unreliable Image", "Unreliable Results")

# No Errors no Academic Misconduct - Type 3
type_3 <- c("Author Unresponsive",
            "Breach of Policy by Author", "Breach of Policy by Third Party",
            "Cites Prior Retracted Work", "Civil Proceedings",
            "Complaints about Author", "Complaints about Company/Institution",
            "Complaints about Third Party", "Concerns/Issues About Authorship",
            "Concerns/Issues about Referencing/Attributions",
            "Concerns/Issues about Third Party Involvement",
            "Conflict of Interest", "Copyright Claims", "Criminal Proceedings",
            "Doing the Right Thing", "Error by Journal/Publisher", "Error by Third Party",
            "Error in Text", "Ethical Violations by Third Party",
            "Informed/Patient Consent - None/Withdrawn", "Lack Of Balance/Bias Issues",
            "Legal Reasons/Legal Threats", "Objections by Author\\(s\\)",
            "Objections by Company/Institution", "Objections by Third Party",
            "Publishing Ban", "Retract and Replace", "Temporary Removal", "Withdrawal",
            "Lack of Approval from Author", "Miscommunication by Author", 
            "Lack of Approval from Company/Institution", "Nonpayment of Fees/Refusal to Pay",
            "Not Presented at Conference", "Withdrawn to Publish in Different Journal")

# No Errors Academic Misconduct - Type 2
type_2 <- c("Duplication of Article", "Duplication of Data",
            "Duplication of Text", "Duplication of Image",
            "Euphemisms for Duplication", "Euphemisms for Misconduct",
            "Euphemisms for Plagiarism", "Forged Authorship",
            "Misconduct - Official Investigation/Finding",
            "Misconduct by Author","Misconduct by Company/Institution",
            "Misconduct by Third Party", "Plagiarism of Article",
            "Plagiarism of Text", "Salami Slicing", "Lack of Approval from Third Party",
            "Lack of IRB/IACUC Approval","Ethical Violations by Author")

# Erros and Academic Misconduct - Type 1
type_1 <- c("Falsification/Fabrication of Data",
            "Falsification/Fabrication of Image","Falsification/Fabrication of Results",
            "Hoax Paper", "Manipulation of Images",
            "Manipulation of Results", "Plagiarism of Data",
            "Plagiarism of Image", "Sabotage of Materials",
            "Sabotage of Methods", "Fake Peer Review")

# unknown
unknown <- c("Date of Retraction/Other Unknown","Notice - Lack of", "Notice - Limited or No Information",
             "Investigation by Company/Institution", "Investigation by Journal/Publisher", 
             "Notice - Unable to Access via current resources", "Notice - No/Limited Information",
             "Upgrade/Update of Prior Notice")

# special case
special <- c("Duplication of Article", "Error by Journal/Publisher")

# Classifying the whole RW data set into types in the eright sequence to avoid overlap
retraction_df$retraction_type_full_MY <-  NA
retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(type_3,collapse="|"),retraction_df$Reason),
                                                "Type 3",  retraction_df$retraction_type_full_MY)

retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(type_1,collapse="|"), retraction_df$Reason)  |
                                                  grepl(paste(type_2,collapse="|"), retraction_df$Reason) &
                                                  grepl(paste(type_4,collapse="|"), retraction_df$Reason),
                                                "Type 1",  
                                                retraction_df$retraction_type_full_MY) 

retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(type_2,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_1,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_4,collapse="|"), retraction_df$Reason),
                                                "Type 2", 
                                                retraction_df$retraction_type_full_MY)

retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(type_4,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_2,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_1,collapse="|"), retraction_df$Reason),
                                                "Type 4",
                                                retraction_df$retraction_type_full_MY)

retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(unknown,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_1,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_2,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_3,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_4,collapse="|"), retraction_df$Reason),
                                                "Unknown",
                                                retraction_df$retraction_type_full_MY)

# special case: duplication + error by journal -> type 3
retraction_df$retraction_type_full_MY <- ifelse(grepl(paste(special,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_1,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_2,collapse="|"), retraction_df$Reason) &
                                                  !grepl(paste(type_4,collapse="|"), retraction_df$Reason),
                                                "Type 3",
                                                retraction_df$retraction_type_full_MY)

unsorted_RW <- retraction_df %>% filter(is.na(retraction_df$retraction_type_full_MY))

sorted_RW <- retraction_df %>% filter(!is.na(retraction_df$retraction_type_full_MY)) %>%
  select(retraction_type_full_MY, Reason)

count(retraction_df$retraction_type_full_MY)

# retraction and papr years to date format
retraction_df$retraction_year_MY <- format(as.Date(retraction_df$RetractionDate, format = "%d/%m/%Y"), "%Y")
retraction_df$paper_year_MY <- format(as.Date(retraction_df$OriginalPaperDate, format = "%d/%m/%Y"), "%Y")

# time to retraction in full years 
retraction_df$time_to_retraction_MY <- as.numeric(format(round(as.duration(as.Date(retraction_df$paper_year_MY, format ="%Y")  %--% 
                                        as.Date(retraction_df$retraction_year_MY, format = "%Y")) / dyears(1), 0), nsmall =0)) 


###############################
###############################
###############################

# Loading coded policy sample
coded_df <- read_xlsx("/Users/dmitrymalkov/Desktop/Dissertation/Data/checked_dataset_09_December.xlsx") %>%
  as.data.frame()

coded_df <- coded_df %>%
  mutate(retraction_flag = case_when(
    retraction_flag =="yes" ~ "Acknowledged",
    retraction_flag =="no" ~ "Not Acknowledged"))

# descriptives
nrow(coded_df) # 1157
length(unique(coded_df$paper_doi)) # 481
length(unique(coded_df$policy_doc_id_MY)) # 998

count(coded_df$type_doc)  # 24 not policy documents
count(coded_df$language) # 222 other languages
count(coded_df$duplicate) #  199 duplicates
count(coded_df$not_found) # 71 not found

coded_df <- filter (coded_df, citation_type !="NA")

coded_df <-  coded_df %>%
  mutate(citation_type = case_when(
  citation_type == "Affirmative" ~ "Positive/Neutral",
  citation_type == "Assumptive" ~  "Positive/Neutral",
  citation_type == "Contrastive" ~ "Positive/Neutral",
  citation_type == "Perfunctory" ~ "Positive/Neutral",
  citation_type == "Bibliography" & retraction_flag ==  "no" ~ "Positive/Neutral",
  citation_type == "Negative" ~  "Negative/Exclusion",
  citation_type == "Exclusion" ~ "Negative/Exclusion",
  citation_type == "Bibliography" & retraction_flag == "yes" ~ "Negative/Exclusion"))
coded_df <-  coded_df %>%
  mutate(type_doc = case_when(
      type_doc == "Clinical recommendation" ~ "Recommendation/Guideline",
      type_doc == "Guideline" ~ "Recommendation/Guideline"))
coded_df$type_doc[is.na(coded_df$type_doc)] <- "Other"

nrow(coded_df) # 644
length(unique(coded_df$paper_doi)) # 367
length(unique(coded_df$policy_doc_id_MY)) # 563

count(coded_df$type_doc) # 161 recommendations and guidelines
count(coded_df$citation_type) # 178 neg / 466 pos
count(coded_df$retraction_flag) # 557 no / 87 yes
count(coded_df$correction) # 103 policy date / 9 paper date / 6 retraction date 

# date = rounded year
coded_df$paper_date_RW <- format(as.Date(coded_df$paper_date_RW, format = "%d/%m/%Y"), "%Y")
coded_df$policy_date <- format(as.Date(coded_df$policy_date, format = "%d/%m/%Y"), "%Y")
coded_df$retraction_date_RW <- format(as.Date(coded_df$retraction_date_RW, format = "%d/%m/%Y"), "%Y")

# time to retraction
coded_df$time_to_retraction_upd <- as.numeric(
  format(
    round(
      as.duration(
        as.Date(coded_df$paper_date_RW, format ="%Y")  %--% 
                           as.Date(coded_df$retraction_date_RW, format = "%Y")) / dyears(1), 0), nsmall =0)) 

# time from paper to policy
coded_df$paper_to_policy_upd <- as.numeric(
  format(
    round(
      as.duration(
        as.Date(coded_df$paper_date_RW, format ="%Y")  %--% 
          as.Date(coded_df$policy_date, format = "%Y")) / dyears(1), 0), nsmall =0)) 

# time from retraction to policy
coded_df$retraction_to_policy_upd <- as.numeric(
  format(
    round(
      as.duration(
        as.Date(coded_df$retraction_date_RW, format ="%Y")  %--% 
          as.Date(coded_df$policy_date, format = "%Y")) / dyears(1), 0), nsmall =0)) 


# Classifying policy sample into retraction types 
coded_df$paper_doi <- tolower(coded_df$paper_doi)

coded_df <- merge(coded_df, select(retraction_df, OriginalPaperDOI, retraction_type_full_MY),
              by.x = "paper_doi", by.y = "OriginalPaperDOI", all.x = T) %>%
  dplyr::rename(retraction_type_MY = retraction_type_full_MY)

# counting types for unique papers. Careful not to count paper policy combinations instead!!!
count(coded_df[which(!duplicated(coded_df$paper_doi)),]$retraction_type_MY)

# addding CrossRef citations count from RW data set
coded_df  <- merge(coded_df, retr_citations_binded, 
               by.x = "paper_doi", by.y = "doi", all.x = T) %>%
  dplyr::rename(citation_count_policy = citation_count_full)
  
###############################
###############################
###############################

# Comparisons and key visualisations

# Comparing citation distribution
summary(coded_df$citation_count_policy) 
count(is.na(retraction_df$citation_count_full))
summary(retraction_df$citation_count_full[!is.na(retraction_df$citation_count_full)])

# Relative perceentage: retractioon reasons 
type_count_full <- count(retraction_df$retraction_type_full_MY[!is.na(retraction_df$OriginalPaperDOI)]) %>%
  mutate(sample = "RW")

unique_retr_doi <- coded_df %>% distinct(paper_doi, .keep_all = T)
type_count_policy <- count(unique_retr_doi$retraction_type_MY) %>%
  mutate(sample = "Policy")

# type_count_policy <- count(coded_df$retraction_type_MY) %>%
  mutate(sample = "Policy")

comp_retr_type <- rbind(type_count_full,type_count_policy)

comp_retr_type$perc[comp_retr_type$sample == "RW"] <-
  (comp_retr_type$freq[comp_retr_type$sample == "RW"]/
     sum(comp_retr_type$freq[comp_retr_type$sample == "RW"]))*100

comp_retr_type$perc[comp_retr_type$sample == "Policy"] <-
  (comp_retr_type$freq[comp_retr_type$sample == "Policy"]/
     sum(comp_retr_type$freq[comp_retr_type$sample == "Policy"]))*100

comp_retr_type %>%
  ggplot(aes(x = x, y = perc, fill = sample)) +
  geom_bar(stat="identity",width = 0.8, position= position_dodge(width = 0.8)) +
  scale_fill_tableau() +
  #scale_fill_brewer(palette = "Paired") +
  theme_fivethirtyeight() +
  xlab("Type of retraction") +
  ylab("Percent") +
  labs(fill = "Sample") +
  ggtitle("Percentage distribution of retraction types in RW data and policy sample") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = 'white', color ="white"),
      plot.background = element_rect(fill = 'white', color ="white"),
      axis.ticks = element_line(colour = "black"))

# Analysis of citation types and retraction types over time
citation_type_summary <- coded_df %>%
  group_by(retraction_to_policy_upd, citation_type) %>%
  dplyr::summarise(count = n())

citation_type_summary$count[citation_type_summary$citation_type == "Negative/Exclusion"] <- 
  citation_type_summary$count[citation_type_summary$citation_type == "Negative/Exclusion"]*-1

citation_type_summary$alpha[citation_type_summary$citation_type == "Negative/Exclusion" &
                               citation_type_summary$retraction_to_policy_upd !=0 ] <- "1"

citation_type_summary$alpha[citation_type_summary$citation_type == "Positive/Neutral" &
                               citation_type_summary$retraction_to_policy_upd != 0] <- "1"

citation_type_summary$alpha[citation_type_summary$citation_type == "Positive/Neutral" &
                               citation_type_summary$retraction_to_policy_upd == 0] <- "2"

citation_type_summary$alpha[citation_type_summary$citation_type == "Negative/Exclusion" &
                               citation_type_summary$retraction_to_policy_upd == 0] <- "2"

# visualisation of citation type using full years 
#  you can add alpha  with mutate(name = (name == condition))
citation_type_summary %>% 
  ggplot(aes(x = retraction_to_policy_upd, y = count, alpha = alpha, pattern = citation_type)) +
  geom_bar_pattern(stat="identity",position="stack",
                   fill = "grey80",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_key_scale_factor = 0.6)+
  scale_y_continuous(breaks = pretty(citation_type_summary$count),
                     labels = abs(pretty(citation_type_summary$count))) +
  theme_fivethirtyeight() +
  scale_pattern_manual(values = c("stripe","none")) +
  #scale_fill_manual("grey80") +
  #scale_fill_tableau() +
  scale_alpha_discrete(range=c(3, 0.3)) +
  xlab("\nTime from retraction to policy") +
  ylab("\nNumber of policy citations") +
  labs(pattern = "Type of citation") +
  #ggtitle("Number and type of policy citations by time from retraction to policy") +
  geom_hline(yintercept=0, size = 0.2) + 
  geom_vline(xintercept=0, size = 0.2) +
  guides(alpha = FALSE) +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text(),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 12)) +         # Axis titles
  theme(legend.text = element_text(size = 12)) +        # Legend text
  theme(legend.title = element_text(size = 12))    +       # Legend title
  theme(plot.margin=grid::unit(c(1.5,0,0,0), "mm"))




# retraction flags

retraction_flag_summary <- coded_df %>%
  group_by(retraction_to_policy_upd, retraction_flag) %>%
  dplyr::summarise(count = n()) 

retraction_flag_summary$count[retraction_flag_summary$retraction_flag == "Acknowledged"] <- 
  retraction_flag_summary$count[retraction_flag_summary$retraction_flag == "Acknowledged"]*-1

retraction_flag_summary$alpha[retraction_flag_summary$retraction_flag == "Acknowledged" &
                                retraction_flag_summary$retraction_to_policy_upd !=0 ] <- "1"

retraction_flag_summary$alpha[retraction_flag_summary$retraction_flag == "Not Acknowledged" &
                                retraction_flag_summary$retraction_to_policy_upd != 0] <- "1"

retraction_flag_summary$alpha[retraction_flag_summary$retraction_flag == "Acknowledged" &
                                retraction_flag_summary$retraction_to_policy_upd == 0] <- "2"

retraction_flag_summary$alpha[retraction_flag_summary$retraction_flag == "Not Acknowledged" &
                                retraction_flag_summary$retraction_to_policy_upd == 0] <- "2"

retraction_flag_summary %>%
  ggplot(aes(x = retraction_to_policy_upd, y = count, alpha = alpha, pattern = retraction_flag)) +
  geom_bar_pattern(stat="identity",position="stack",
                   fill = "grey80",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02,
                   pattern_key_scale_factor = 0.6)+
 scale_y_continuous(breaks = seq(-15,80,15)) +
  theme_fivethirtyeight() +
  scale_pattern_manual(values = c("stripe","none")) +
  #scale_fill_brewer(palette = "Set1") +
  scale_alpha_discrete(range=c(3, 0.3)) +
  #scale_fill_tableau() +
  xlab("\nTime from retraction to policy") +
  ylab("\nNumber of policy citations") +
  labs(pattern = "Retraction acknowledgment") +
  #ggtitle("") +
  geom_hline(yintercept=0, size = 0.2) + 
  geom_vline(xintercept=0, size = 0.2) +
  guides(alpha = FALSE) +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text(),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 12)) +         # Axis titles
  theme(legend.text = element_text(size = 12)) +        # Legend text
  theme(legend.title = element_text(size = 12))    +       # Legend title
  theme(plot.margin=grid::unit(c(1.5,0,0,0), "mm"))






citation_type_summary <- coded_df %>%
  group_by(retraction_to_policy_upd, citation_type) %>%
  dplyr::summarise(count = n())

citation_type_summary$count[citation_type_summary$citation_type == "Negative/Exclusion"] <- 
  citation_type_summary$count[citation_type_summary$citation_type == "Negative/Exclusion"]*-1


# Summary by retraction types over time
retraction_type_summary <- coded_df %>%
  group_by(retraction_to_policy_upd, citation_type, retraction_type_MY) %>%
  dplyr::summarise(count = n())

retraction_type_summary$count[retraction_type_summary$citation_type == "Negative/Exclusion"] <- 
  retraction_type_summary$count[retraction_type_summary$citation_type == "Negative/Exclusion"]*-1

retraction_type_summary$alpha[retraction_type_summary$citation_type == "Negative/Exclusion" &
                                retraction_type_summary$retraction_to_policy_upd !=0 ] <- "1"

retraction_type_summary$alpha[retraction_type_summary$citation_type == "Positive/Neutral" &
                                retraction_type_summary$retraction_to_policy_upd != 0] <- "1"

retraction_type_summary$alpha[retraction_type_summary$citation_type == "Positive/Neutral" &
                                retraction_type_summary$retraction_to_policy_upd == 0] <- "2"

retraction_type_summary$alpha[retraction_type_summary$citation_type == "Negative/Exclusion" &
                                retraction_type_summary$retraction_to_policy_upd == 0] <- "2"

# visualisation of retraction types using full years 
retraction_type_summary %>% 
  filter(retraction_type_MY != "Unknown") %>%
  ggplot(aes(x = retraction_to_policy_upd, y = count, pattern = count > 0, alpha = alpha)) +
  geom_bar_pattern(stat="identity",position="stack",
                   fill = "grey80",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values = c("stripe","none"), labels = c("Negative/Exclusion", "Positive/Neutral"))  +
  scale_y_continuous(breaks = seq(-10,20,10)) +
  theme_fivethirtyeight() +
  # scale_fill_brewer(palette = "Set1") +
  # scale_fill_tableau() +
  scale_alpha_discrete(range=c(3, 0.3)) +
  guides(alpha = F) +
  xlab("\nTime from retraction to policy") +
  ylab("\nNumber of policy citations") +
  labs(pattern = "Type of citation") +
 # ggtitle("Number of policy citations and corresponding retraction types by time from retraction to policy") +
  geom_hline(yintercept=0, size = 0.2) +
  geom_vline(xintercept=0, size = 0.2) +
  facet_grid(rows = vars(retraction_type_MY)) +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text(),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 12)) +         # Axis titles
  theme(legend.text = element_text(size = 12)) +        # Legend text
  theme(legend.title = element_text(size = 12))    +       # Legend title
  theme(plot.margin=grid::unit(c(1.5,0,0,0), "mm"))



# Policy organisations analysis 
orgs_coded <- read_xlsx("/Users/dmitrymalkov/Desktop/Dissertation/Data/orgs_coded.xlsx") %>%
  as.data.frame() %>% select(-freq)
print(xtable(orgs_coded))


coded_df <- merge(coded_df, orgs_coded, by.x = "policy_organisation", by.y  = "x")
length(unique(coded_df$policy_organisation_upd))
count(coded_df$policy_organisation_type)

# visualisation of policy organisation types using full years 
org_count <- coded_df %>%
  group_by(retraction_to_policy_upd, citation_type, policy_organisation_type) %>%
  dplyr::summarise(count = n())

org_count$count[org_count$citation_type == "Negative/Exclusion"] <- 
  org_count$count[org_count$citation_type == "Negative/Exclusion"]*-1

org_count$alpha[org_count$citation_type == "Negative/Exclusion" &
                  org_count$retraction_to_policy_upd !=0 ] <- "1"

org_count$alpha[org_count$citation_type == "Positive/Neutral" &
                  org_count$retraction_to_policy_upd != 0] <- "1"

org_count$alpha[org_count$citation_type == "Positive/Neutral" &
                                org_count$retraction_to_policy_upd == 0] <- "2"

org_count$alpha[org_count$citation_type == "Negative/Exclusion" &
                  org_count$retraction_to_policy_upd == 0] <- "2"

org_count %>% 
  ggplot(aes(x = retraction_to_policy_upd, y = count, pattern = count > 0, alpha = alpha)) +
  geom_bar_pattern(stat="identity",position="stack",
                   fill = "grey80",
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.03,
                   pattern_key_scale_factor = 0.6)+
  scale_pattern_manual(values = c("stripe","none"), labels = c("Negative/Exclusion", "Positive/Neutral"))  +
  theme_fivethirtyeight() +
  #scale_fill_tableau(palette = "Superfishel Stone") +
  scale_alpha_discrete(range=c(3, 0.3)) +
  guides(alpha = F) +
  xlab("\nTime from retraction to policy") +
  ylab("\nNumber of policy citations") +
  labs(pattern = "Type of citation") +
  #ggtitle("Number of policy citations by time from retraction to policy") +
  geom_hline(yintercept=0, size = 0.2) +
  geom_vline(xintercept=0, size = 0.2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text()) +
  facet_grid(rows = vars(policy_organisation_type)) +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text(),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA)) +
  theme(axis.title = element_text(size = 12)) +         # Axis titles
  theme(legend.text = element_text(size = 12)) +        # Legend text
  theme(legend.title = element_text(size = 12))    +       # Legend title
  theme(plot.margin=grid::unit(c(1.5,0,0,0), "mm"))


# individual organisations analysis
coded_df$after_retraction_MY <- 0
coded_df$after_retraction_MY[coded_df$retraction_to_policy_upd > 0] <- 1
coded_df$after_retraction_MY[coded_df$retraction_to_policy_upd < 0] <- -1
count(coded_df$after_retraction_MY)

coded_df$after_retraction_MY[coded_df$after_retraction_MY == 
                           0][unlist(by(1:nrow(filter(coded_df,after_retraction_MY == 0)), 
                           coded_df$policy_organisation_upd[coded_df$after_retraction_MY == 0], 
                  function(x) sample(x, ceiling(length(x)/2), FALSE)))] <- -1

org_count_1 <- coded_df %>%
  group_by(policy_organisation_upd) %>%
  dplyr::summarise(times= n())

org_count_2 <- coded_df %>%
  group_by(policy_organisation_upd, citation_type, after_retraction_MY) %>%
  dplyr::summarise(count= n())

org_count_2 <- merge(org_count_1, org_count_2, by.x = "policy_organisation_upd", by.y = "policy_organisation_upd")

org_count_2 <- org_count_2 %>% filter(times > 3)
length(unique(org_count_2$policy_organisation_upd))

org_count_2$count[org_count_2$after_retraction_MY < 0] <- 
  org_count_2$count[org_count_2$after_retraction_MY  < 0]*-1

org_count_2 %>% 
  ggplot(aes(x = policy_organisation_upd, y = count, fill = citation_type)) +
  geom_bar(stat="identity",position="stack")+
  scale_y_continuous(breaks = pretty(org_count_2$count),
                     labels = abs(pretty(org_count_2$count))) +
  theme_fivethirtyeight() +
  scale_fill_brewer(palette ="Set1") +
  coord_flip() +
  geom_hline(yintercept=0, size = 0.2) +
  facet_grid(cols = vars(citation_type)) +
  xlab("Number of policy citations of retracted articles") +
  ylab("Policy organisation") +
  labs(fill = "Citation type") +
  ggtitle("Types of citations for policy organisations with the most number of documents") +
  geom_hline(yintercept=0) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())

# Journal analysis 
journals_policy <- count(coded_df[which(!duplicated(coded_df$paper_doi)),]$journal_RW)
journals_policy$sample <- "Policy"
journals_policy$total <- sum(journals_policy$freq)
journals_all <- count(retraction_df$Journal)
journals_all$sample <- "RW"
journals_all$total <- sum(journals_all$freq)
jrn_aggr <- rbind(journals_policy,journals_all)
jrn_aggr$perc <- (jrn_aggr$freq/jrn_aggr$total)*100
top_poli <- jrn_aggr[which(jrn_aggr$sample == "Policy"),] %>% filter(perc >= 1)
jrn_aggr <- subset(jrn_aggr, jrn_aggr$x %in% top_poli$x, value = TRUE)
#jrn_aggr <- jrn_aggr %>%
  mutate(x = case_when(
    x == "JAMA: Journal of the American Medical Association" ~ "JAMA",
    x == "PNAS: Proceedings of the National Academy of Sciences of the United States of America" ~ "PNAS",
    x == "Journal of Clinical Oncology : Official Journal of the American Society of Clinical Oncology" ~ "Journal of Clinical Oncology"))
jrn_aggr$x <- as.character(jrn_aggr$x)
jrn_aggr$x[jrn_aggr$x == "JAMA: Journal of the American Medical Association"] <- "JAMA"
jrn_aggr$x[jrn_aggr$x == "PNAS: Proceedings of the National Academy of Sciences of the United States of America"] <- "PNAS"
jrn_aggr$x[jrn_aggr$x == "Journal of Clinical Oncology : Official Journal of the American Society of Clinical Oncology"] <- "Journal of Clinical Oncology"

jrn_aggr %>% 
  ggplot(aes(x=perc,y=x,fill=sample)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_tableau() +
  scale_x_continuous(labels = scales::percent_format(scale=1)) +
  theme_fivethirtyeight() +
  xlab("Percentage of each group") +
  ylab("Journal") +
  labs(fill = "Sample") +
  ggtitle("Percentage of articles from top journals in Policy sample") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())
  
# retractions and paper over the years for the whole data set
retraction_year_count_full <- count(retraction_df$retraction_year_MY)
retraction_year_count_full$sample <-"RW"
retraction_year_count_full$total <- sum(retraction_year_count_full$freq)

paper_year_count_full <- count(retraction_df$paper_year_MY)         
paper_year_count_full$sample <- "RW"
paper_year_count_full$total <- sum(paper_year_count_full$freq)

retraction_year_count_full %>%
  filter(as.character(x) >1994) %>%
  ggplot(aes(x=x, y=freq)) +
  geom_bar(stat="identity",position="stack")

paper_year_count_full %>%
  filter(as.character(x) < 2020 & as.character(x) > 1994) %>%
  ggplot(aes(x=x, y=freq)) +
  geom_bar(stat="identity",position="stack")


# retractions and paper over the years for the policy data set
retraction_year_count_policy <- count(coded_df$retraction_date_RW[!duplicated(coded_df$paper_doi)])
retraction_year_count_policy$sample <- "Policy sample"
retraction_year_count_policy$total <- sum(retraction_year_count_policy$freq)

paper_year_count_policy <- count(coded_df$paper_date_RW[!duplicated(coded_df$paper_doi)])      
paper_year_count_policy$sample <- "Policy sample"
paper_year_count_policy$total <- sum(paper_year_count_policy$freq)

retraction_year_count_policy %>%
  #filter(x >1974) %>%
  ggplot(aes(x=x, y=freq)) +
  geom_bar(stat="identity",position="stack")

paper_year_count_policy %>%
  # filter(x < 2020) %>%
  ggplot(aes(x=x, y=freq)) +
  geom_bar(stat="identity",position="stack")






# relative percentages full and policy
papr_count_comb <- rbind(paper_year_count_full, paper_year_count_policy)
papr_count_comb$perc <- (papr_count_comb$freq/papr_count_comb$total)*100
pub_year <- papr_count_comb %>%
  filter(as.character(x) < 2020 & as.character(x) > 1998) %>%
  ggplot(aes(x=x, y=round(perc), group = sample, color =sample)) +
  #geom_bar(stat="identity", position ="dodge") +
  geom_line(aes(x=x, y=round(perc)), size = 1) +
  geom_vline(xintercept = c("2010","2014"), linetype="dashed", color = "black", size = 0.2) +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent_format(scale=1)) +
 # scale_fill_tableau() +
  scale_color_tableau() +
  xlab("\nYear of publication") +
  ylab("Number of publications") +
  labs(color = "") +
 # ggtitle("Number of publications over time: percentage of total") +
  theme(text = element_text(family="LM Roman 10"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text(),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white", color = NA)) 
ggsave(filename = "pub_year.png", pub_year,
       width = 10, height = 4, dpi = 300, units = "in", device='png')






retraction_count_comb <- rbind(retraction_year_count_full, retraction_year_count_policy)
retraction_count_comb$perc <- (retraction_count_comb$freq/retraction_count_comb$total)*100
papr_count_comb$perc <- (papr_count_comb$freq/papr_count_comb$total)*100
retraction_count_comb %>%
  filter(as.character(x) > 2003) %>%
  ggplot(aes(x=x, y=round(perc), fill=sample)) +
  geom_bar(stat="identity", position ="dodge") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent_format(scale=1)) +
  scale_fill_tableau() +
  xlab("Year of retraction") +
  ylab("Number of retractions") +
  labs(fill = "Sample") +
  ggtitle("Number of retractions over time: percentage of total")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())

# Distributions of time to retraction/policy
retraction_df$time_to_retraction_MY[!is.na(retraction_df$OriginalPaperDOI)]  <- 
  as.numeric(format(round(as.duration(as.Date(retraction_df$OriginalPaperDate[!is.na(retraction_df$OriginalPaperDOI)])  %--% 
            as.Date(retraction_df$RetractionDate[!is.na(retraction_df$OriginalPaperDOI)])) / dyears(1), 0), nsmall =0)) 
  
retraction_df$time_to_retraction_MY[retraction_df$time_to_retraction_MY ==  -1] <- 0

time_to_retr_full <- retraction_df %>% 
  filter(!is.na(time_to_retraction_MY)) %>%
  filter(time_to_retraction_MY >= 0) %>%
  select(time_to_retraction_MY) %>%
  dplyr::rename(`Time to retraction` =  time_to_retraction_MY)
time_to_retr_full$sample <- "full"

time_to_retr_policy <- coded_df[which(!duplicated(coded_df$paper_doi)),] %>%
  filter(!is.na(paper_to_retraction_upd)) %>%
  filter(paper_to_retraction_upd >= 0) %>%
  select(paper_to_retraction_upd) %>%
  dplyr::rename(`Time to retraction` =  paper_to_retraction_upd)
time_to_retr_policy$sample <- "policy"
  
time_to_retr <- rbind(time_to_retr_policy,time_to_retr_full)
time_to_retr %>% 
  ggplot(aes(x=`Time to retraction`, y=sample, fill=sample)) +
  geom_boxplot() +
  scale_x_continuous(breaks = seq(0,50,1)) +
  theme_fivethirtyeight() 

coded_df[which(!duplicated(coded_df$paper_doi)),]  %>%
  ggplot(aes(x=paper_to_retraction_upd, y=retraction_type_MY, fill=retraction_type_MY)) +
  geom_boxplot() +
  theme_fivethirtyeight() 

retraction_df[which(!is.na(retraction_df$OriginalPaperDOI)),] %>%
  filter(!is.na(time_to_retraction_MY)) %>%
  filter(time_to_retraction_MY >= 0) %>%
  ggplot(aes(x=time_to_retraction_MY, y=retraction_type_full_MY, fill=retraction_type_full_MY)) +
  geom_boxplot() +
  theme_fivethirtyeight() 
  
coded_df[which(!duplicated(coded_df$paper_doi)),]  %>%
  ggplot(aes(x=retraction_to_policy_upd, y=retraction_type_MY, fill=retraction_type_MY)) +
  geom_boxplot() +
  theme_fivethirtyeight() 

coded_df[which(!duplicated(coded_df$paper_doi)),]  %>%
  ggplot(aes(x=retraction_to_policy_upd, y=citation_type, fill=citation_type)) +
  geom_boxplot() +
  theme_fivethirtyeight() 

# Subject fields 
sub_field <- retraction_df[which(!is.na(retraction_df$OriginalPaperDOI)),] %>% 
  separate_rows(Subject, sep = ";") %>%
  filter(Subject != "")
sub_field <- count(sub_field$Subject)

coded_df_f <- merge(coded_df, select(retraction_df, OriginalPaperDOI, Subject),
                    by.x ="paper_doi", by.y="OriginalPaperDOI" )
sub_field_poli <- coded_df_f[which(!duplicated(coded_df_f$paper_doi)),] %>% 
  separate_rows(Subject, sep = ";") %>%
  filter(Subject != "")
sub_field_poli <- count(sub_field_poli$Subject)


# descriptive tables
coded_df <- coded_df %>%
  mutate(retraction_flag = case_when(
    retraction_flag =="yes" ~ "Acknowledged",
    retraction_flag =="no" ~ "Not Acknowledged"))

var_labels <- list(time_to_retraction_upd = "Time to retraction", 
                   retraction_to_policy_upd = "Time from retraction to policy",
                   paper_to_policy_upd = "Time from publication to policy",
                   citation_type = "Citation type",
                   retraction_type_MY = "Retraction type",
                   retraction_flag  = " Retraction acknowledged",
                   type_doc  = "Document type",
                   policy_organisation_upd = "Policy organisation",
                   policy_organisation_type = "Policy organisation type")

coded_df <- labelled::set_variable_labels(coded_df, .labels = var_labels)






# FINAL CITATION TYPE
coded_df %>% 
  mutate(`Citation period` =  case_when(
    retraction_to_policy_upd < 0 ~ "Before retraction",
    retraction_to_policy_upd == 0 ~ "Retraction year",
    retraction_to_policy_upd > 0 ~ "After retraction"
  )) %>% 
  mutate(`Citation period` = factor(`Citation period`, 
                                    levels = c("Before retraction", "Retraction year", "After retraction"))) %>%
  select(retraction_to_policy_upd, paper_to_policy_upd, `Citation period`, citation_type,
             retraction_flag, retraction_type_MY, policy_organisation_type) %>%
  tbl_summary(by = citation_type,
              statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
             # digits = list("time_to_retraction_upd" ~ c(0)),
              missing_text = "(Missing)") %>%
  add_p() %>%
  add_overall()  


# summary table RW and PL 
retraction_df_sh <- retraction_df %>%
  transmute(retraction_type = retraction_type_full_MY,
         retraction_year = as.factor(retraction_year_MY),
         time_to_retraction = time_to_retraction_MY,
         citation_count = citation_count_full,
         sample = "Retraction watch")

coded_df_sh <- coded_df[which(!duplicated(coded_df$paper_doi)),] %>%
  transmute(retraction_type = retraction_type_MY,
         retraction_year = as.factor(retraction_date_RW),
         time_to_retraction = time_to_retraction_upd,
         citation_count = citation_count_policy,
         sample = "Sample") 

coded_retr_merg <- rbind(retraction_df_sh, coded_df_sh)

var_labels <- list(time_to_retraction = "Time to retraction",
                   citation_count = "CrossRef citations",
                   retraction_type = "Retraction type")

coded_retr_merg <- labelled::set_variable_labels(coded_retr_merg, .labels = var_labels)

# FINAL RW ARTICLES 
coded_retr_merg %>% select(time_to_retraction,citation_count,retraction_type, sample) %>%
  tbl_summary(by = sample,
              statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list("time_to_retraction" ~ c(0),
                            "citation_count" ~ c(0)),
              missing = "always",
              missing_text = "(Missing)") %>%
  add_p() %>%
  as_kable(format = "latex")



# BY TYPE
coded_retr_merg %>% select(time_to_retraction,citation_count,retraction_type, sample) %>%
  tbl_summary(by = retraction_type,
              statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
              digits = list("time_to_retraction" ~ c(0),
                            "citation_count" ~ c(0)),
              missing = "always",
              missing_text = "(Missing)") %>%
  add_p() 

var_labels <- list(retraction_to_policy_upd = "Time from retraction to policy", 
                   paper_to_policy_upd = "Time from publication to policy",
                   citation_type = "Citation type",
                   retraction_flag = "Retraction acknowledgement",
                   retraction_type_MY = "Retraction type")

coded_df <- labelled::set_variable_labels(coded_df, .labels = var_labels)

# FINAL BEFORE AFTER 
coded_df %>% select(retraction_to_policy_upd, paper_to_policy_upd, citation_type,
                    retraction_flag, retraction_type_MY, policy_organisation_type) %>%
  mutate(before_after =  case_when(
     retraction_to_policy_upd < 0 ~ 1,
     retraction_to_policy_upd == 0 ~ 2,
     retraction_to_policy_upd > 0 ~ 3
  ))  %>%
  tbl_summary(by = before_after,
              statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
              missing_text = "(Missing)") %>%
  add_p() %>%
  add_overall() %>%
  modify_header(update = list(
        label ~ "**Characteristic**",
        stat_1 ~ "**Before retraction** N = 261 ",#261
        stat_2 ~ "**Retraction year** N = 91 ", #91
        stat_3 ~ "**After retraction** N = 292 ", #292
        p.value ~ '**p-value**'))

# by type
coded_df %>% select(retraction_to_policy_upd, paper_to_policy_upd, citation_type,
                    retraction_flag, retraction_type_MY, policy_organisation_type) %>%
  mutate(before_after =  case_when(
    retraction_to_policy_upd < 0 ~ 1,
    retraction_to_policy_upd == 0 ~ 2,
    retraction_to_policy_upd > 0 ~ 3
  ))  %>%
  tbl_summary(by = retraction_type_MY,
              statistic = list(all_continuous() ~ "{median} [{p25}, {p75}]",
                               all_categorical() ~ "{n} ({p}%)"),
              missing_text = "(Missing)") %>%
  add_p() %>%
  add_overall()



# Distributions of citations
count_pub <- coded_df  %>%
  group_by(paper_title_RW, paper_doi, citation_type)  %>%
  dplyr::summarise(count = n())

test <- coded_df  %>%
  group_by(paper_title_RW, paper_doi)  %>%
  dplyr::summarise(count = n()) %>%
  group_by(count)  %>%
  dplyr::summarise(frac = n()/367) 
test %>%
  ggplot(aes(x = count, y = frac)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,8,9,11,12,34,56)) +
  scale_y_continuous(limits = c(0,0.8), labels = scales::percent_format(scale=100)) + 
  geom_line() +
  geom_point()  +
  geom_point(data = test[12,], aes(x=count, y=frac), colour="red", size = 2) +
  geom_point(data = test[11,], aes(x=count, y=frac), colour="red",size = 2) +
  geom_label(
    label="Wakefield et al. (1998)", 
    x=53,
    y=0.04,
    label.size = 0.35,
    color = "red",
  ) +
  geom_label(
    label="Estruch et al. (2013)", 
    x=31,
    y=0.04,
    label.size = 0.35,
    color = "red",
  ) +
  theme_fivethirtyeight() +
  xlab("Number of policy citations") +
  ylab("Percent of publications") +
  ggtitle("Distribution of policy citations among retracted articles")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text()) 


# inrerview data
int_coded <- read_csv("/Users/dmitrymalkov/Desktop/Codes.csv") %>%
  select(1,4)
int_coded$Codes <- gsub("Codes\\", "", int_coded$Codes, fixed = T)
res_ret <- subset(int_coded, int_coded$Codes %in% grep("Reasons for spread of retracted research", int_coded$Codes, value = T))
res_ret$Codes <- gsub("Reasons for spread of retracted research", "", res_ret$Codes, fixed = T)
names(res_ret) <- c("Code","Number of interviewees")
res_ret$`Number of interviewees` <-
  as.integer(res_ret$`Number of interviewees`)
res_ret <- res_ret[-1,]
print(xtable(res_ret[order(res_ret$`Number of interviewees`, decreasing = T),]))

ggplot(res_ret, aes(x= reorder(Codes, `Number of items coded`), y=`Number of items coded`)) +
  geom_segment( aes(x=reorder(Codes, `Number of items coded`), 
                    xend=Codes, y=0, yend=`Number of items coded`), color="black", size =0.2) +
  geom_point( color="black", size=2) +
  theme_fivethirtyeight() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank() 
  ) +
  xlab("Topic") +
  ggtitle("Resons for spread of retracted research") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())

res_sel <- subset(int_coded, int_coded$Codes %in% grep("Research selection, appraisal and utilisation", int_coded$Codes, value = T))
res_sel$Codes <- gsub("Research selection, appraisal and utilisation", "", res_sel$Codes, fixed = T)
res_sel <- res_sel[-1,]
names(res_sel) <- c("Code","Number of interviewees")
res_sel$`Number of interviewees` <-
  as.integer(res_sel$`Number of interviewees`)
res_sel <- res_sel[-1,]
print(xtable(res_sel[order(res_sel$`Number of interviewees`, decreasing = T),]))

ggplot(res_sel, aes(x= reorder(Codes, `Number of items coded`), y=`Number of items coded`)) +
  geom_segment( aes(x=reorder(Codes, `Number of items coded`), 
                    xend=Codes, y=0, yend=`Number of items coded`), color="black", size =0.2) +
  geom_point( color="black", size=2) +
  theme_fivethirtyeight() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank() 
  ) +
  xlab("Topic") +
  ggtitle("Research selection, appraisal and utilisation") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())


ret_sol <- subset(int_coded, int_coded$Codes %in% grep("Solutions to prevent spread of retracted research", int_coded$Codes, value = T))
ret_sol$Codes <- gsub("Solutions to prevent spread of retracted research", "", ret_sol$Codes, fixed = T)
ret_sol <- ret_sol[-1,]
names(ret_sol) <- c("Code","Number of interviewees")
ret_sol$`Number of interviewees` <-
  as.integer(ret_sol$`Number of interviewees`)
ret_sol <- ret_sol[-1,]
print(xtable(ret_sol[order(ret_sol$`Number of interviewees`, decreasing = T),]))

ggplot(ret_sol, aes(x= reorder(Codes, `Number of items coded`), y=`Number of items coded`)) +
  geom_segment( aes(x=reorder(Codes, `Number of items coded`), 
                    xend=Codes, y=0, yend=`Number of items coded`), color="black", size =0.2) +
  geom_point( color="black", size=2) +
  theme_fivethirtyeight() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank() 
  ) +
  xlab("Topic") +
  ggtitle("Solutions to prevent spread of retracted research") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = 'white', color ="white"),
        plot.background = element_rect(fill = 'white', color ="white"),
        axis.ticks = element_line(colour = "black"), axis.title = element_text())
  

  
  
# Appendix tables 

annex_reasons <- data.frame(c(type_1,type_3, type_1,type_4))


type_4 <- c("Concerns/Issues About Data","Concerns/Issues About Image",
            "Concerns/Issues About Results", "Contamination of Cell Lines/Tissues",
            "Contamination of Reagents", "Contamination of Materials \\(General\\)",
            "Error in Analyses", "Error in Cell Lines/Tissues","Error in Data",
            "Error in Image", "Error in Materials",
            "Error in Methods", "Error in Results and/or Conclusions", 
            "Results Not Reproducible", "Unreliable Data",
            "Unreliable Image", "Unreliable Results")

# No Errors no Academic Misconduct - Type 3
type_3 <- c("Author Unresponsive",
            "Breach of Policy by Author", "Breach of Policy by Third Party",
            "Cites Prior Retracted Work", "Civil Proceedings",
            "Complaints about Author", "Complaints about Company/Institution",
            "Complaints about Third Party", "Concerns/Issues About Authorship",
            "Concerns/Issues about Referencing/Attributions",
            "Concerns/Issues about Third Party Involvement",
            "Conflict of Interest", "Copyright Claims", "Criminal Proceedings",
            "Doing the Right Thing", "Error by Journal/Publisher", "Error by Third Party",
            "Error in Text", "Ethical Violations by Third Party",
            "Informed/Patient Consent - None/Withdrawn", "Lack Of Balance/Bias Issues",
            "Legal Reasons/Legal Threats", "Objections by Author\\(s\\)",
            "Objections by Company/Institution", "Objections by Third Party",
            "Publishing Ban", "Retract and Replace", "Temporary Removal", "Withdrawal",
            "Lack of Approval from Author", "Miscommunication by Author", 
            "Lack of Approval from Company/Institution", "Nonpayment of Fees/Refusal to Pay",
            "Not Presented at Conference", "Withdrawn to Publish in Different Journal")

# No Errors Academic Misconduct - Type 2
type_2 <- c("Duplication of Article", "Duplication of Data",
            "Duplication of Text", "Duplication of Image",
            "Euphemisms for Duplication", "Euphemisms for Misconduct",
            "Euphemisms for Plagiarism", "Forged Authorship",
            "Misconduct - Official Investigation/Finding",
            "Misconduct by Author","Misconduct by Company/Institution",
            "Misconduct by Third Party", "Plagiarism of Article",
            "Plagiarism of Text", "Salami Slicing", "Lack of Approval from Third Party",
            "Lack of IRB/IACUC Approval","Ethical Violations by Author")

# Erros and Academic Misconduct - Type 1
type_1 <- c("Falsification/Fabrication of Data",
            "Falsification/Fabrication of Image","Falsification/Fabrication of Results",
            "Hoax Paper", "Manipulation of Images",
            "Manipulation of Results", "Plagiarism of Data",
            "Plagiarism of Image", "Sabotage of Materials",
            "Sabotage of Methods", "Fake Peer Review")

# unknown
unknown <- c("Date of Retraction/Other Unknown","Notice - Lack of", "Notice - Limited or No Information",
             "Investigation by Company/Institution", "Investigation by Journal/Publisher", 
             "Notice - Unable to Access via current resources", "Notice - No/Limited Information",
             "Upgrade/Update of Prior Notice")














  

install.packages("summarytools")
library(summarytools)



# try later with Sccopus from campus maybe
library(rscopus)

set_api_key("7a5f08300b8ac9a1a387d821e8179e41")
elsevier_authenticate(api_key = "7a5f08300b8ac9a1a387d821e8179e41", api_key_error = TRUE,
                      choice = NULL, verbose = TRUE, headers = NULL)

citation_retrieval(doi = "10.20964/2017.06.91",  verbose = FALSE)






                            