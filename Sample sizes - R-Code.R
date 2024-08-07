# INSTALL AND LOAD PACKAGES ################################

library(datasets)  # Load base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, dplyr,tidyr,xtable) 




# IMPORTING WITH RIO #######################################
#Here, input the folder you put the csv sheet in
# CSV
rio_csv <- import("folder-position/Datasheet-samplesizes.csv")
head(rio_csv)


#############################################################################

# Table 1 and 2: Number of studies included and excluded in the analysis, divided along Journal and year
study_counts <- rio_csv %>% group_by(Journal,Year,`Include study?`) %>% summarize(Count = as.numeric(n()))
included_studies <- study_counts %>% filter(`Include study?`== 1)
excluded_studies <- study_counts %>% filter(`Include study?`== 0)

# Reshape included studies into table and add 'Total' column
included_table <- included_studies %>%
  select(Journal, Year, Count) %>%
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(contains("202"))))

# Reshape excluded studies into table and add 'Total' column
excluded_table <- excluded_studies %>%
  select(Journal, Year, Count) %>%
  pivot_wider(names_from = Year, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(contains("202")))) 
 

#############################################################################

#Table 4: split up along design
study_design_counts <- rio_csv %>%
  group_by(Journal,Design) %>% 
  summarise(Number = as.integer(n())) %>%
  filter(Design > 0)

#create table
design_counts_table <- study_design_counts %>%
  pivot_wider(names_from = Journal, values_from = Number, values_fill = 0)

#get the total number collapsed on journals
design_counts_total <- rio_csv %>%
  group_by(Design) %>%
  summarise(Number = as.integer(n())) %>%
  filter(Design > 0) %>%
  ungroup()

#add the total count to the other table
design_counts_table$Total <- design_counts_total$Number

#cleanup: rename rows, remove unnecessary columns
design_counts_table <- data.frame(design_counts_table) %>%
  select(.,-Design)
rownames(design_counts_table) <- c("Experimental","Quasi-Experimental","Correlational","Undefined")


#create table for within/between
study_within_between_counts <- rio_csv %>%
  group_by(Journal,`Within or Between?`) %>% 
  summarise(Number = as.integer(n())) %>%
  filter(`Within or Between?`>0)
  ungroup()

study_within_between_table <- study_within_between_counts %>%
  pivot_wider(names_from = Journal, values_from = Number, values_fill = 0)

#Add the "Total" Column to the table
study_within_between_total <- rio_csv %>%
  group_by(`Within or Between?`) %>%
  summarise(Number = as.integer(n())) %>%
  filter(`Within or Between?` > 0) %>%
  ungroup() 

study_within_between_table$Total <- study_within_between_total$Number

#cleanup: remove unnecessary columns, rename rows
study_within_between_table <- data.frame(study_within_between_table) %>%
  select(.,-c(1))
rownames(study_within_between_table) <- c("Within","Mixed","Between")


#unite the two tables

part1 <- design_counts_table[1,]
part2 <- design_counts_table[2:nrow(design_counts_table),]

design_counts_final_table <- rbind(part1, study_within_between_table, part2)
#############################################################################

#clean up data sheet to leave only the analysable data
unusable <- rio_csv[,15] == 0 | is.na(rio_csv[,15])
clean_data <- rio_csv[!unusable,]


#############################################################################

#Table3: median samples along Journals

median_by_journal <- clean_data %>%
  group_by(Journal) %>%
  summarise(Median_value_j = median(as.integer(N),na.rm = TRUE)) %>%
  ungroup()


median_table_byjournal <- median_by_journal %>%
  pivot_wider(names_from = Journal, values_from = Median_value_j)

median_table_byjournal$Total <- median(as.numeric(clean_data$N))

#cleanup the table: rename row
rownames(median_table_byjournal) <- "Total (2022-2023)"

#############################################################################

#Quantile Table by Journal

#25. Quantile
quantile25_by_journal <- clean_data %>%
  group_by(Journal) %>%
  summarise(quantile_value_j25 = quantile(as.integer(N),probs = .25)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_j25)
quantile25_by_journal$Total <- quantile(as.integer(clean_data$N), probs = .25)

#75. Quantile
quantile75_by_journal <- clean_data %>%
  group_by(Journal) %>%
  summarise(quantile_value_j75 = quantile(as.integer(N),probs = .75)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_j75)
quantile75_by_journal$Total <- quantile(as.integer(clean_data$N), probs = .75)

#Combine into one table, rename rows
quantiles_by_journal <- rbind(quantile25_by_journal,quantile75_by_journal)
rownames(quantiles_by_journal) <- c("25. Quantile","75. Quantile")


#############################################################################

#Table 5: Median sample sizes per journal, analysed separately for different study design

#first, only do design without within or between

#compute the median along design, divided along journals
median_by_group <- clean_data %>%
  group_by(Design,Journal) %>%
  summarise(Median_value = median(as.integer(N),na.rm = TRUE)) %>%
  ungroup()

median_table_bygroup <- median_by_group %>%
  pivot_wider(names_from = Journal, values_from = Median_value)

#compute the median along design for all journals combined
total_median_by_group <- clean_data %>%
  group_by(Design) %>%
  summarise(total_median_value = median(as.integer(N), na.rm = TRUE)) %>%
  ungroup()

#add the total median to the data frame above
median_table_bygroup$Total <- total_median_by_group$total_median_value

#cleanup the table: rename, remove unnecessary column
median_table_bygroup <- data.frame(median_table_bygroup) %>%
  select(.,-Design)
rownames(median_table_bygroup) <- c("Experimental","Quasi-Experimental","Correlational")

#############################################################################

#now differentiate whether it's within or between
within_between_median <- clean_data %>%
  group_by(`Within or Between?`,Journal) %>%
    summarise(Median_valueWB = median(as.integer(N),na.rm = TRUE)) %>%
  ungroup()

within_between_table_bygroup <- within_between_median %>%
  pivot_wider(names_from = Journal, values_from = Median_valueWB)

#compute the median along design for all journals combined
within_between_total_median <- clean_data %>%
  group_by(`Within or Between?`) %>%
  summarise(total_median_valueWB = median(as.integer(N), na.rm = TRUE)) %>%
  ungroup()

#add the total median to the data frame above
within_between_table_bygroup$Total <- within_between_total_median$total_median_valueWB

#cleanup the table: rename, remove unnecessary column and row
within_between_table_bygroup <- data.frame(within_between_table_bygroup) %>%
  select(.,!1)
within_between_table_bygroup <- within_between_table_bygroup[-c(4),]
rownames(within_between_table_bygroup) <- c("Within","Mixed","Between")

#############################################################################

#unite the two tables into one
part1 <- median_table_bygroup[1,]
part2 <- median_table_bygroup[2:nrow(median_table_bygroup),]

final_table <- rbind(part1, within_between_table_bygroup, part2)

#############################################################################

#Interquartile ranges split for study design

#25. quantiles by design
quantile25_by_design <- clean_data %>%
  group_by(Journal,Design) %>%
  summarise(quantile_value_d25 = quantile(as.integer(N),probs = .25)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_d25) %>%
  select(.,-Design)
quantile25_by_design$Total <- clean_data %>%
  group_by(Design) %>%
  summarise(quantile_value_dt25 = quantile(as.integer(N),probs = .25)) %>%
  ungroup() %>%
  select(.,2)

#75. quantiles by design
quantile75_by_design <- clean_data %>%
  group_by(Journal,Design) %>%
  summarise(quantile_value_d75 = quantile(as.integer(N),probs = .75)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_d75) %>%
  select(.,-Design)
quantile75_by_design$Total <- clean_data %>%
  group_by(Design) %>%
  summarise(quantile_value_dt75 = quantile(as.integer(N),probs = .75)) %>%
  ungroup() %>%
  select(.,2)

#unite the two tables in an easy-to-read manner
quantiles_by_design <- rbind(quantile25_by_design[1,],quantile75_by_design[1,],quantile25_by_design[2,],quantile75_by_design[2,],quantile25_by_design[3,],quantile75_by_design[3,])

#cleanup the table: rename, remove unnecessary column
quantiles_by_design <- as.matrix(quantiles_by_design)
rownames(quantiles_by_design) <- c("Experimental-25","Experimental-75","Quasi-Experimental-25","Quasi-Experimental-75","Correlational-25","Correlational-75")

#############################################################################

#interquantile ranges split along within, mixed, between designs

#25. quantiles by within/between
quantile25_by_WMB <- clean_data %>%
  group_by(Journal,`Within or Between?`) %>%
  summarise(quantile_value_wmb25 = quantile(as.integer(N),probs = .25)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_wmb25) %>%
  select(.,-`Within or Between?`)
quantile25_by_WMB$Total <- clean_data %>%
  group_by(`Within or Between?`) %>%
  summarise(quantile_value_wmbt25 = quantile(as.integer(N),probs = .25)) %>%
  ungroup() %>%
  select(.,2)

#75. quantiles by within/between
quantile75_by_WMB <- clean_data %>%
  group_by(Journal,`Within or Between?`) %>%
  summarise(quantile_value_wmb75 = quantile(as.integer(N),probs = .75)) %>%
  ungroup() %>%
  pivot_wider(names_from = Journal, values_from =quantile_value_wmb75) %>%
  select(.,-`Within or Between?`)
quantile75_by_WMB$Total <- clean_data %>%
  group_by(`Within or Between?`) %>%
  summarise(quantile_value_wmbt75 = quantile(as.integer(N),probs = .75)) %>%
  ungroup() %>%
  select(.,2)

#unite the two tables in an easy-to-read manner
quantiles_by_WMB <- rbind(quantile25_by_WMB[1,],quantile75_by_WMB[1,],quantile25_by_WMB[2,],quantile75_by_WMB[2,],quantile25_by_WMB[3,],quantile75_by_WMB[3,])

#cleanup the table: rename, remove unnecessary column
quantiles_by_WMB <- as.matrix(quantiles_by_WMB)
rownames(quantiles_by_WMB) <- c("Within-25","Within-75","Mixed-25","Mixed-75","Between-25","Between-75")

#############################################################################

#Some Calculations taking into account if a priori power analyses were conducted 

#Calculate median sample size split along design and whether a power analysis was conducted
median_by_power <- clean_data %>%
  group_by(Design,`Power analysis conducted?`) %>%
  summarise(med_power = median(as.integer(N), na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = `Power analysis conducted?`, values_from = med_power) %>%
  select(.,-(1 | 4)) %>%
  as.matrix() 
rownames(median_by_power) = c("Experimental","Quasi-Experimental","Correlational")
colnames(median_by_power) = c("Not conducted","Conducted")

#Calculate number of studies split along design and whether a power analysis was conducted
power_counts <- clean_data %>%
  group_by(Design, `Power analysis conducted?`) %>%
  summarise(power_count = as.numeric(n())) %>%
  select(Design, `Power analysis conducted?`,power_count) %>%
  pivot_wider(names_from = Design, values_from = power_count) %>%
  select(.,-1) %>%
  slice(-3) %>%
  t()
rownames(power_counts) = c("Experimental","Quasi-Experimental","Correlational")
colnames(power_counts) = c("Not conducted","Conducted")

#############################################################################

#export to latex

table1 <- xtable(included_table) %>%
  print(.,file='included_table.tex',type = 'latex')
table2 <- xtable(excluded_table) %>%
  print(.,file='excluded_table.tex',type = 'latex')
table3 <- xtable(median_table_byjournal) %>%
  print(.,file='median_table_by_journal.tex', type='latex')
table4 <- xtable(design_counts_final_table) %>%
  print(.,file='counts_by_design.tex', type='latex')
table5 <- xtable(final_table) %>%
  print(.,file='median_table_by_design.tex', type='latex')

#interquantile ranges

table3q <- xtable(quantiles_by_journal) %>%
  print(.,file='quantiles_by_journal.latex', type = 'latex')
table5qDesign <- xtable(quantiles_by_design) %>%
  print(.,file='quantiles_by_design.latex', type = 'latex')
table5qWithinBetween <- xtable(quantiles_by_WMB) %>%
  print(.,file='quantiles_by_WMB.latex', type = 'latex')

#power 

table1p <- xtable(median_by_power) %>%
  print(.,file='median_by_power.latex',type = 'latex')
table2p <- xtable(power_counts) %>%
  print(.,file='power_counts.latex',type = 'latex')

############################################################



# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  

# Clear console
cat("\014")  

# Clear mind :)
