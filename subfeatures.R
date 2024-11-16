library(dplyr)
library(tidyr)
library(purrr)

# Read all csv files and combine them together
process_csv = function(file_path){
  data = read.csv(file_path)
  db_name = basename(dirname(file_path))
  sub_id = sub("^.*sub-(\\d+).*\\.csv$", paste(db_name, "\\1", sep = "-"), basename(file_path)) 
  ses_id = sub("^.*(ses-\\d+).*\\.csv$", "\\1", basename(file_path))
  
  data_processed = data %>%
    select(roi, median) %>%
    # mutate(median = round(median, 4)) %>%
    pivot_wider(names_from = roi, values_from = median) %>%
    mutate(sub_id = sub_id, ses_id = ses_id) %>%
    select(sub_id, ses_id, everything())
  
  
  return(data_processed)
}

BeLong_3d_path = "./BeLong-3d"
csv_files = list.files(path = BeLong_3d_path, pattern = "*.csv", full.names = TRUE)
BeLong_3d = do.call(rbind, lapply(csv_files, process_csv))
BeLong_3d = mutate(BeLong_3d, ses_id = ifelse(ses_id == "ses-1", "ses-01", ses_id))
print(BeLong_3d)


# Convert data from a long format to a long format to facilitate the drawing of box plots with multiple features
sub_belong3d_long = BeLong_3d %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

# Plot boxplot
ggplot(sub_belong3d_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in sub_belong3d Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate the quartile and IQR for each feature
outlier_counts = sub_belong3d_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_belong3d_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
# Calculte total number of outliers
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)

BeLong_epi_path = "./BeLong-epi"
csv_files = list.files(path = BeLong_epi_path, pattern = "*.csv", full.names = TRUE)
BeLong_epi = do.call(rbind, lapply(csv_files, process_csv))
print(BeLong_epi)



sub_belongepi_long = BeLong_epi %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

# Draw box plot
ggplot(sub_belongepi_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in sub_belongepi Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate the quartile and IQR for each feature
outlier_counts = sub_belongepi_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_belongepi_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)


BeLong_gre4_path = "./BeLong-gre-4echoes"
csv_files = list.files(path = BeLong_gre4_path, pattern = "*.csv", full.names = TRUE)
BeLong_gre4 = do.call(rbind, lapply(csv_files, process_csv))
print(BeLong_gre4)


sub_BeLong_gre4_long = BeLong_gre4 %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

ggplot(sub_BeLong_gre4_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in sub_belong4echoes Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate the quartile and IQR for each feature
outlier_counts = sub_BeLong_gre4_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_BeLong_gre4_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)



BeLong_gre5_path = "./BeLong-gre-5echoes"
csv_files = list.files(path = BeLong_gre5_path, pattern = "*.csv", full.names = TRUE)
BeLong_gre5 = do.call(rbind, lapply(csv_files, process_csv))
print(BeLong_gre5)

sub_BeLong_gre5_long = BeLong_gre5 %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")


ggplot(sub_BeLong_gre5_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in sub_belong5echoes Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate the quartile and IQR for each feature
outlier_counts = sub_BeLong_gre5_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_BeLong_gre5_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)


# Check EATT dataset
EATT_path = "./EATT"
csv_files = list.files(path = EATT_path, pattern = "*.csv", full.names = TRUE)

# Collect name of each column
column_names_list = list()

for (file in csv_files) {
  processed_data = process_csv(file)
  column_names_list[[file]] = names(processed_data)
}

# Find unique name of each column
all_columns = unique(unlist(column_names_list))

# Check which column names are missing in which files
missing_columns_per_file = sapply(column_names_list, function(cols) {
  missing_cols = setdiff(all_columns, cols)
  if (length(missing_cols) > 0) {
    return(toString(missing_cols))
  } else {
    return("No missing columns")
  }
})

print(missing_columns_per_file)
# sub-5: "ctx-lh-paracentral", "ctx-rh-paracentral" are missing # Replace paracentral with precentral in the final csv
# sub-19: "ctx-lh-caudalmiddlefrontal, ctx-lh-paracentral, 
# ctx-lh-posteriorcingulate, ctx-rh-caudalmiddlefrontal, ctx-rh-paracentral, ctx-rh-posteriorcingulate" are missing # Deleted in the final csv
EATT_path = "./EATT"
csv_files = list.files(path = EATT_path, pattern = "*.csv", full.names = TRUE)
EATT = bind_rows(lapply(csv_files, process_csv))
EATT = mutate(EATT, ses_id = ifelse(ses_id == "ses-1", "ses-01", ses_id))
print(EATT)


sub_eatt_long = EATT %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

ggplot(sub_eatt_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in EATT Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the quartile and IQR for each feature
outlier_counts = sub_eatt_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_eatt_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)



MeDALS_path = "./MeDALS"
csv_files = list.files(path = MeDALS_path, pattern = "*.csv", full.names = TRUE)
MeDALS = do.call(rbind, lapply(csv_files, process_csv))
print(MeDALS)

sub_medals_long = MeDALS %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

ggplot(sub_medals_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in MeDALS Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the quartile and IQR for each feature
outlier_counts = sub_medals_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_medals_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)




# Check MS dataset
MS_path = "./MS-Radiomics"
csv_files = list.files(path = MS_path, pattern = "*.csv", full.names = TRUE)

# Collect name of each column
column_names_list = list()

for (file in csv_files) {
  processed_data = process_csv(file)
  column_names_list[[file]] = names(processed_data)
}

# Find unique name of each column
all_columns = unique(unlist(column_names_list))

# Check which column names are missing in which files
missing_columns_per_file = sapply(column_names_list, function(cols) {
  missing_cols = setdiff(all_columns, cols)
  if (length(missing_cols) > 0) {
    return(toString(missing_cols))
  } else {
    return("No missing columns")
  }
})

print(missing_columns_per_file)
# sub-108, sub-123, sub-133, sub-21, sub-57, sub-62, sub-68, sub-84 are missing a lot of features. 
MS_path = "./MS-Radiomics"

# Skip these csv files because a lot of features are missing
skip_subs = c("sub-108", "sub-123", "sub-133", "sub-21", "sub-57", "sub-62", "sub-68", "sub-84")

csv_files = list.files(path = MS_path, pattern = "*.csv", full.names = TRUE)
filtered_csv_files = csv_files[!sapply(csv_files, function(x) {
  any(sapply(skip_subs, function(sub) grepl(sub, x)))
})]
MS = do.call(rbind, lapply(filtered_csv_files, process_csv))
print(MS)

sub_ms_long = MS %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

ggplot(sub_ms_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in MS Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the quartile and IQR for each feature
outlier_counts = sub_ms_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_ms_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)


seventea_csv = function(file_path){
  data = read.csv(file_path)
  db_name = basename(dirname(file_path))
  sub_id = sub("^.*sub-([a-zA-Z0-9]+).*\\.csv$", paste(db_name, "\\1", sep = "-"), basename(file_path))
  ses_id = sub("^.*(ses-\\d+).*\\.csv$", "\\1", basename(file_path))
  
  data_processed = data %>%
    select(roi, median) %>%
    pivot_wider(names_from = roi, values_from = median) %>%
    mutate(sub_id = sub_id, ses_id = ses_id) %>%
    select(sub_id, ses_id, everything())
  
  return(data_processed)
}

SEVENTEA_path = "./SEVENTEA"
csv_files = list.files(path = SEVENTEA_path, pattern = "*.csv", full.names = TRUE)
SEVENTEA = do.call(rbind, lapply(csv_files, seventea_csv))
SEVENTEA = mutate(SEVENTEA, ses_id = ifelse(ses_id == "ses-1", "ses-01", ses_id))
print(SEVENTEA)

sub_7t_long = SEVENTEA %>%
  pivot_longer(cols = -c(sub_id, ses_id), names_to = "Feature", values_to = "Value")

ggplot(sub_7t_long, aes(x = Feature, y = Value)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  labs(title = "Boxplot of QSM Features in 7tea Dataset",
       x = "Feature",
       y = "Value") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the quartile and IQR for each feature
outlier_counts = sub_7t_long %>%
  group_by(Feature) %>%
  summarise(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    Upper = Q3 + 1.5 * IQR,
    Lower = Q1 - 1.5 * IQR
  ) %>%
  # Count the number of outliers
  left_join(sub_7t_long, by = "Feature") %>%
  mutate(Is_Outlier = Value < Lower | Value > Upper) %>%
  group_by(Feature) %>%
  summarise(Outlier_Count = sum(Is_Outlier))

print(outlier_counts)
outlier_counts
total_outliers = sum(outlier_counts$Outlier_Count)
print(total_outliers)



# Combine all csv files into a sub-features.csv file
sub_features = bind_rows(BeLong_3d, BeLong_epi, BeLong_gre4, BeLong_gre5, EATT, MeDALS, MS, SEVENTEA)
print(sub_features)

sub_features = sub_features %>%
  # Split sub_id into two parts
  separate(sub_id, into = c("QSM.dataset", "sub_num"), sep = "-(?!.*-)") %>%
  mutate(sub_id = paste0("sub-", sub_num)) %>%
  select(-sub_num) %>%
  select(sub_id, ses_id, QSM.dataset, everything()) 

print(sub_features)

sub_features= mutate(sub_features, sub_id = ifelse(QSM.dataset == "MS-Radiomics",
                                                   ifelse(nchar(gsub("sub-", "", sub_id)) == 1, gsub("-(\\d+)", "-00\\1", sub_id),
                                                          ifelse(nchar(gsub("sub-", "", sub_id)) == 2, gsub("-(\\d+)", "-0\\1", sub_id), sub_id)),
                                                   sub_id)) 
sub_features


write.csv(sub_features, "sub_features.csv", row.names = FALSE)

# Filter subject' info who have more than 1 session
sub_filtered = sub_features %>%
  filter(!(sub_id == "sub-014" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-015" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-018" & ses_id == "ses-02" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-019" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-025" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-026" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-028" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-029" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-032" & ses_id == "ses-01" & QSM.dataset == "BeLong-3d") &
           !(sub_id == "sub-035" & ses_id == "ses-02" & QSM.dataset == "BeLong-gre-4echoes") &
           !(sub_id == "sub-65" & ses_id == "ses-01" & QSM.dataset == "EATT") &
           !(sub_id == "sub-011" & ses_id == "ses-20240417" & QSM.dataset == "MeDALS") &
           !(sub_id == "sub-035" & ses_id == "ses-01" & QSM.dataset == "MeDALS"))
sub_filtered

sub_all = sub_filtered %>%
  rename(subjid = sub_id) %>%
  mutate(subjid = ifelse(QSM.dataset == "EATT",
                         ifelse(nchar(gsub("sub-", "", subjid)) == 1, gsub("-(\\d+)", "-00\\1", subjid),
                                ifelse(nchar(gsub("sub-", "", subjid)) == 2, gsub("-(\\d+)", "-0\\1", subjid), subjid)),
                         subjid)) %>%
  select(c(-ses_id))
sub_all

# Check if there is any missing value
check_sub_na = sapply(sub_all, function(x) sum(is.na(x)))
sum(check_sub_na)
total_na = sum(is.na(sub_all))
total_elements = prod(dim(sub_all))

# Calculate the proportion of missing values
na_ratio = total_na / total_elements
print(na_ratio)

# Impute NA by using median 
for (col in colnames(sub_all)) {
  if (any(is.na(sub_all[[col]]))) {
    sub_all[[col]][is.na(sub_all[[col]])] = median(sub_all[[col]], na.rm = TRUE)
  }
}

# Check if there is any missing value again
check_sub_na = sapply(sub_all, function(x) sum(is.na(x)))
check_sub_na

write.csv(sub_all, "sub_all.csv", row.names = FALSE)


# Extract qsm features from all_subject_table.csv
sub_table_all_load = read.csv("all_subject_table.csv")
qsm_features_all = select(sub_table_all_load, -subjid, -QSM.dataset, -sex, -age, -condition)
qsm_features_all
colnames(qsm_features_all)

write.csv(qsm_features_all, "qsm_features_all.csv", row.names = FALSE)

