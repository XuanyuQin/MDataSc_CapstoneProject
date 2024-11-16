library(dplyr)

# Load data
covariates = read.csv("aggregate-spreadsheet-important-data-students1.csv")
covariates 

unique(covariates$QSM.dataset)
colnames(covariates)
column_types1 = sapply(covariates, class)
column_types1

# Check column subjid's format 
invalid_subjid = covariates %>%
  filter(!grepl("^sub-\\d{3}$", subjid) & dataset != "SEVENTEA" & dataset != "Sydney") 
invalid_subjid

update_subjid = covariates %>%
  mutate(
    subjid = ifelse(!grepl("^sub-\\d{3}$", subjid) & dataset != "SEVENTEA" & dataset != "Sydney",
                    ifelse(grepl("^sub-\\d{2}$", subjid), gsub("sub-", "sub-0", subjid),  
                           ifelse(grepl("^\\d$", subjid), paste0("sub-00", subjid),  
                                  ifelse(grepl("^sub_", subjid), gsub("sub_", "sub-", subjid), subjid))),
                    subjid)
  )
update_subjid

filter(update_subjid, subjid == "sub-036" & date.of.scan == "10/06/2024")

# Keep values which are not "" in the column "QSM.dataset", select columns that would be useful for the following process, modify the subjid to keep consistency
covariates_filtered = update_subjid %>%
  filter(QSM.dataset != "") %>%
  select(c("subjid", "MND.IMAGING.ID", "MND.IMAGING.session", "QSM.dataset", "date.of.scan", "sex.numerical", "height", "weight", "age.at.scan", "months.since.onset", "ALSFRS.R.tot", "disease.severity.delta.frs", "formal.diagnosis.numeric")) %>%
  # Update inconsistent info and update error info
  mutate(subjid = ifelse(subjid == 4, "sub-004", subjid)) %>%
  mutate(subjid = ifelse(subjid == 5, "sub-005", subjid)) %>%
  mutate(ALSFRS.R.tot = ifelse(subjid == "sub-036" & date.of.scan == "10/06/2024", 27, ALSFRS.R.tot)) 
covariates_filtered

covariates_filtered = mutate(covariates_filtered, disease.severity.delta.frs = ifelse(subjid == "sub-036" & date.of.scan == "10/06/2024", (48 - ALSFRS.R.tot)/months.since.onset, disease.severity.delta.frs))

write.csv(covariates_filtered, "covariates_filtered.csv", row.names = FALSE)

check_na_empty = sapply(covariates_filtered, function(x) sum(is.na(x) | x == ""))
check_na_empty

# Check value type of each column
column_types = sapply(covariates_filtered, class)
column_types

covariates_filtered = covariates_filtered %>%
  mutate(disease.severity.delta.frs = as.numeric(disease.severity.delta.frs))
covariates_filtered

column_types = sapply(covariates_filtered, class)
column_types

# Check subjects who had MRI test session more than once 
covariates_check_session = covariates_filtered %>%
  group_by(MND.IMAGING.ID) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  select(subjid, MND.IMAGING.ID, MND.IMAGING.session, QSM.dataset)
print(covariates_check_session)
# write.csv(covariates_check_session, "covariates_check_session.csv", row.names = FALSE)

# Filter subject' info who have more than 1 session
covariates_filtered = covariates_filtered %>%
  filter(!(MND.IMAGING.ID == "sub-002MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-003MND" & MND.IMAGING.session == "ses-05") &
           !(MND.IMAGING.ID == "sub-005MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-006MND" & MND.IMAGING.session == "ses-02") &
           !(MND.IMAGING.ID == "sub-007MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-013MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-014MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-016MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-017MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-020MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-023MND" & MND.IMAGING.session == "ses-02") &
           !(MND.IMAGING.ID == "sub-023MND" & MND.IMAGING.session == "ses-03") &
           !(MND.IMAGING.ID == "sub-024MND" & MND.IMAGING.session == "ses-01") &
           !(MND.IMAGING.ID == "sub-049MND" & MND.IMAGING.session == "ses-04")) %>%
  mutate(subjid = ifelse(QSM.dataset == "SEVENTEA", gsub("^7TEA-B-", "sub-", subjid), subjid)) 
# mutate(subjid = ifelse(QSM.dataset == "EATT", gsub("-(\\d+)", "-0\\1", subjid), subjid)) 
covariates_filtered


# Check subjects who had MRI test session more than once again after filtering
covariates_check_session2 = covariates_filtered %>%
  group_by(MND.IMAGING.ID) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  select(subjid, MND.IMAGING.ID, MND.IMAGING.session, QSM.dataset)
print(covariates_check_session2)

# Fill control subj info in the "ALSFRS.R.tot" "months.since.onset" column based on the "formal.diagnosis" == 0.
covariates_impute = covariates_filtered %>%
  mutate(ALSFRS.R.tot = ifelse(formal.diagnosis.numeric == 0, 48, ALSFRS.R.tot)) %>%
  mutate(disease.severity.delta.frs = ifelse(formal.diagnosis.numeric == 0 | formal.diagnosis.numeric == 9, -1, disease.severity.delta.frs)) %>%
  mutate(months.since.onset = ifelse(formal.diagnosis.numeric == 0 | formal.diagnosis.numeric == 9, -1, months.since.onset)) %>%
  mutate(ALSFRS.R.tot = round(ifelse(MND.IMAGING.ID == "sub-003MND", 48-disease.severity.delta.frs*months.since.onset, ALSFRS.R.tot))) %>%
  mutate(disease.severity.delta.frs = ifelse(MND.IMAGING.ID == "sub-033MND", 0.086134692, disease.severity.delta.frs) ) %>%
  # For 5 = Fronto temporal dementia/c9 FTD, 15 = Not MND and not otherwise classified, disease.severity.delta.frs is not applicable  
  mutate(disease.severity.delta.frs = ifelse(formal.diagnosis.numeric == 15 | formal.diagnosis.numeric == 5, -1, disease.severity.delta.frs))
covariates_impute

# write.csv(covariates_impute, "covariates_impute.csv", row.names = FALSE)

na_rows = covariates_impute[is.na(covariates_impute$disease.severity.delta.frs),]
na_mnd_id = na_rows$MND.IMAGING.ID
na_mnd_id # "sub-183MND"

# Impute missing value of sub-183MND based on months.since.onset and formal.diagnosis.numeric
sub_183_diagnosis = covariates_impute %>%
  filter(MND.IMAGING.ID == "sub-183MND") %>%
  select(months.since.onset, formal.diagnosis.numeric)
sub_183_diagnosis

sub_183_onset_value = sub_183_diagnosis$months.since.onset
sub_183_diagnosis_value = sub_183_diagnosis$formal.diagnosis.numeric

impute_filter = covariates_impute %>%
  filter(
    formal.diagnosis.numeric == sub_183_diagnosis_value &
      abs(months.since.onset - sub_183_onset_value) <= 5 &
      !is.na(disease.severity.delta.frs)
  ) %>%
  select(MND.IMAGING.ID, months.since.onset, disease.severity.delta.frs, formal.diagnosis.numeric)
impute_filter

impute_median = median(impute_filter$disease.severity.delta.frs)
impute_median
# impute_mean = mean(impute_filter$disease.severity.delta.frs)
# impute_mean

covariates_impute = covariates_impute %>%
  mutate(disease.severity.delta.frs = ifelse(MND.IMAGING.ID == "sub-183MND", impute_median, disease.severity.delta.frs))
covariates_impute
colnames(covariates_impute)
covariates_final = covariates_impute %>%
  select(subjid, MND.IMAGING.ID, QSM.dataset, sex.numerical,	height,	weight,	age.at.scan, disease.severity.delta.frs, formal.diagnosis.numeric)
covariates_final

check_na_final = sapply(covariates_final, function(x) sum(is.na(x)))
check_na_final

write.csv(covariates_final, "covariates_final.csv", row.names = FALSE)

cov_filter = rename(covariates_final, unique_id = MND.IMAGING.ID, sex = sex.numerical, age = age.at.scan, severity = disease.severity.delta.frs, condition = formal.diagnosis.numeric)
cov_filter

# Load MS data
ms_data = read.csv("MS-participants.csv")
ms_data

cleaned_ms_data = ms_data[-c(1, 2), -c(1, ncol(ms_data), ncol(ms_data)-1)]
colnames(cleaned_ms_data)
cleaned_ms_data = cleaned_ms_data %>%
  select(c("ID", "Sex", "Age..y.","Condition")) %>%
  rename(sub_id = ID, sex = Sex, age = Age..y., condition = Condition) %>%
  mutate(sex = ifelse(sex == "M", 0, 1)) %>%
  mutate(condition = ifelse(condition == "HC", 0, condition)) %>%
  mutate(condition = ifelse(condition == "MS", 16, condition))

cleaned_ms_data

# Add a neew column to the ms dataset 
cleaned_ms_data$QSM.dataset = "MS-Radiomics"
cleaned_ms_data

# Process MS dataset
cleaned_ms_data = cleaned_ms_data %>%
  mutate(sub_id = paste0("sub-", sub_id)) %>%
  mutate(sub_id = ifelse(QSM.dataset == "MS-Radiomics",
                         ifelse(nchar(gsub("sub-", "", sub_id)) == 1, gsub("-(\\d+)", "-00\\1", sub_id),
                                ifelse(nchar(gsub("sub-", "", sub_id)) == 2, gsub("-(\\d+)", "-0\\1", sub_id), sub_id)),
                         sub_id))

cleaned_ms_data

# Combine MS feature and QSM feature
sub_features = read.csv("sub_features.csv")
ms_sub_filter = left_join(cleaned_ms_data, sub_features, by = c("sub_id", "QSM.dataset"))
ms_sub_filter = filter(ms_sub_filter, !is.na(ses_id))
ms_sub_filter

ms_sub_filtered = select(ms_sub_filter, sub_id, QSM.dataset, sex, age, condition)
ms_sub_filtered

write.csv(ms_sub_filtered, "ms_sub_filtered.csv", row.names = FALSE)



# Load covariates file 
cov = read.csv("covariates_final.csv")
colnames(cov)
cov = rename(cov, unique_id = MND.IMAGING.ID, sex = sex.numerical, age = age.at.scan, condition = formal.diagnosis.numeric)
cov

colnames(cov)


ms_cov_data = read.csv("ms_sub_filtered.csv")
ms_cov_data
colnames(ms_cov_data)
ms_cov_data$unique_id = ms_cov_data$sub_id
ms_cov_data = rename(ms_cov_data, subjid = sub_id)
ms_cov_data = select(ms_cov_data, subjid, unique_id, everything())
ms_cov_data

colnames(ms_cov_data)

# Combine two dataframes
cov_selected = select(cov, -height, -weight, -disease.severity.delta.frs)
combined_data = rbind(cov_selected, ms_cov_data)


# Filter subjects' data 
sub_table_all = inner_join(sub_all, combined_data, by = c("subjid", "QSM.dataset"))
sub_table_all = select(sub_table_all, unique_id, everything())
sub_table_all
colnames(sub_table_all)

write.csv(sub_table_all, "all_subject_table.csv", row.names = FALSE)


# Select biology covariates 
bio_covariates = select(sub_table_all_load, unique_id, QSM.dataset, sex, age, condition)
bio_covariates

write.csv(bio_covariates, "bio_covariates.csv", row.names = FALSE)

batch_covariates = read.csv("Batch_covariates.csv")
colnames(batch_covariates)
batch_covariates_all = batch_covariates %>%
  select(-Manufacturer)
batch_covariates_all
write.csv(batch_covariates_all, "batch_covariates_all.csv", row.names = FALSE)

# Visualisation for biology_covariates.csv
# Check sex distribution
# library(ggplot2)
# 
# ggplot(bio_covariates, aes(x = factor(sex))) +
#   geom_bar(fill = "steelblue") +
#   labs(x = "Sex (0 = Male, 1 = Female)", y = "Count", title = "Sex Distribution")
# 
# # Age distribution
# ggplot(bio_covariates, aes(x = age)) +
#   geom_histogram(binwidth = 5, fill = "darkgreen", color = "black") +
#   labs(x = "Age at Scan", y = "Count", title = "Age Distribution")