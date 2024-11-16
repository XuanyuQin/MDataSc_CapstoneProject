library(dplyr)
library(ggplot2)
library(reshape2)
library(neuroCombat)
library(ggseg)

# Load qsm data
qsm_data = read.csv("qsm_features_all.csv")
qsm_data

# Standardisation 
standardise = function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# Apply the standardisation to all columns except the first one
qsm_data_standardised = qsm_data
qsm_data_standardised[, -1] = apply(qsm_data[, -1], 2, standardise)

# Check the result
head(qsm_data_standardised)
write.csv(qsm_data_standardised, "qsm_data_standardised.csv", row.names = FALSE)

# load biology data
biology_data = read.csv("bio_covariates.csv")
biology_data = rename(biology_data, Dataset = QSM.dataset)
biology_data

# Load batch data
batch_data = read.csv("batch_covariates_all.csv")
batch_data

covariates_table = left_join(biology_data, batch_data, by = "Dataset")
covariates_table
colnames(covariates_table)

covariates_table = select(covariates_table, -Magnetic.field.strength..T.)
covariates_table

covariates_table$MR.scanner = trimws(covariates_table$MR.scanner)
covariates_table$Scanning.Sequence = trimws(covariates_table$Scanning.Sequence)
covariates_table$batch_info = paste0(covariates_table$MR.scanner, "_", covariates_table$Scanning.Sequence)
covariates_table = select(covariates_table, -MR.scanner, -Scanning.Sequence)
covariates_table

# Create a mapping from the unique values of batch_info to numbers
batch_info_numeric = as.numeric(factor(covariates_table$batch_info))

# batch_info_mapping = data.frame(Numeric_Value = batch_info_numeric, Original_Batch_Info = covariates_table$batch_info)
# 
# unique(batch_info_mapping)
# write.csv(batch_info_mapping, "batch_info_mapping.csv", row.names = FALSE)

# Add the numeric version of Scanning.Sequence to dataframe
covariates_table$batch_info_numeric = batch_info_numeric

# Check the updated dataframe
covariates_table
covariates_table = select(covariates_table, -batch_info, -Dataset)
covariates_table
colnames(covariates_table)

write.csv(qsm_data_standardised, "qsm_dat.csv", row.names = FALSE)
write.csv(covariates_table, "covariates_table.csv", row.names = FALSE)

# Harmonisation
qsm_dat = as.matrix(read.csv("qsm_dat.csv", row.names = 1))
qsm_dat
# Transpose the data matrix
qsm_dat = t(qsm_dat)
qsm_dat

covariates_dat = read.csv("covariates_table.csv")

batch = covariates_dat$batch_info_numeric
age = covariates_dat$age
disease = as.factor(covariates_dat$condition)

# Create the model matrix for age and disease condition
mod = model.matrix(~ age + condition, data = covariates_dat)
mod


# Plot boxplot and density plot to see the data distribution of each feature before harmonisation
data_before_harmonise = melt(qsm_dat)
colnames(data_before_harmonise) = c("Feature", "Subject", "Value")
data_before_harmonise$Batch = rep(batch, times = nrow(qsm_dat))

ggplot(data_before_harmonise, aes(x = Value, fill = as.factor(Batch), color = as.factor(Batch))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Density Plot Before Harmonisation", x = "Feature Value", y = "Density", fill = "Batch", color = "Batch") +
  theme(legend.position = "bottom")



# Harmonise data
harmonise = neuroCombat(dat = qsm_dat, batch = batch, mod = mod, mean.only = TRUE)

# Get the harmonised data
harmonised_data = harmonise$dat.combat
harmonised_data
write.csv(harmonised_data, "harmonised_data.csv", row.names = TRUE)

# Plot data after hamonisation
data_after_harmonise = melt(t(harmonised_data))
colnames(data_after_harmonise) = c("Feature", "Subject", "Value")

data_after_harmonise$Batch = rep(batch, times = nrow(harmonised_data))

# Plot density plot
ggplot(data_after_harmonise, aes(x = Value, fill = as.factor(Batch), color = as.factor(Batch))) +
  geom_density(alpha = 0.3) +
  theme_minimal() +
  labs(title = "Density Plot After Harmonisation", x = "Feature Value", y = "Density", fill = "Batch", color = "Batch") +
  theme(legend.position = "bottom")



#  look at QSM values in the pre central and paracentral gyrus before and after harmonisation in controls (0) and ALS (1) and PLS (2) and PMA (4)
precentral_features = c("ctx.rh.precentral", "ctx.lh.precentral")
paracentral_features = c("ctx.rh.paracentral", "ctx.lh.paracentral")

precentral_data_before = qsm_dat[precentral_features, ]
paracentral_data_before = qsm_dat[paracentral_features, ]

precentral_data_after = harmonised_data[precentral_features, ]
paracentral_data_after = harmonised_data[paracentral_features, ]

# Extract condition
condition = covariates_table$condition

# Combine precentral and paracentral data
precentral_paracentral_before = rbind(precentral_data_before, paracentral_data_before)
precentral_paracentral_after = rbind(precentral_data_after, paracentral_data_after)

# Transform data into long format to make plot
data_before_long = melt(precentral_paracentral_before)
data_after_long = melt(precentral_paracentral_after)

# Rename colnames
colnames(data_before_long) = c("Feature", "Subject", "Value")
colnames(data_after_long) = c("Feature", "Subject", "Value")

# Add condition info (eg., Control, ALS, PLS, PMA)
data_before_long$Condition = rep(condition, each = nrow(precentral_paracentral_before))
data_after_long$Condition = rep(condition, each = nrow(precentral_paracentral_after))


# Filter data to include only conditions 0, 1, 2, and 4
data_before_long_filtered = data_before_long %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

data_after_long_filtered = data_after_long %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

summary_data_before_filtered = summary_data_before %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

summary_data_after_filtered = summary_data_after %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

# Boxplot for unharmonised data
ggplot(data_before_long_filtered, aes(x = as.factor(Condition), y = Value, fill = as.factor(Condition))) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  labs(title = "QSM Values Before Harmonisation in Precentral and Paracentral Gyrus",
       x = "Condition (0=Control, 1=ALS, 2=PLS, 4=PMA, 16=MS)", y = "QSM Value", fill = "Condition") +
  facet_wrap(~ Feature, scales = "fixed")

# Boxplot for harmonised data
ggplot(data_after_long_filtered, aes(x = as.factor(Condition), y = Value, fill = as.factor(Condition))) +
  geom_boxplot(outlier.shape = NA) +
  theme_minimal() +
  labs(title = "QSM Values After Harmonisation in Precentral and Paracentral Gyrus",
       x = "Condition (0=Control, 1=ALS, 2=PLS, 4=PMA, 16=MS)", y = "QSM Value", fill = "Condition") +
  facet_wrap(~ Feature, scales = "fixed")


summary_data_before_filtered$Condition_Label = factor(summary_data_before_filtered$Condition, 
                                                      levels = c(0, 1, 2, 4, 16),
                                                      labels = c("Control", "ALS", "PLS", "PMA", "MS"))

summary_data_after_filtered$Condition_Label = factor(summary_data_after_filtered$Condition, 
                                                     levels = c(0, 1, 2, 4, 16),
                                                     labels = c("Control", "ALS", "PLS", "PMA", "MS"))

# Before Harmonisation
ggplot(summary_data_before_filtered, aes(x = Condition_Label, y = mean_QSM, fill = Condition_Label)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_QSM - se_QSM, ymax = mean_QSM + se_QSM), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean QSM Values Before Harmonisation in Precentral and Paracentral Gyrus",
       x = "Condition", y = "Mean QSM Value", fill = "Condition") +
  facet_wrap(~ Feature, scales = "fixed") +
  theme(legend.position = "none")

# After Harmonisation
ggplot(summary_data_after_filtered, aes(x = Condition_Label, y = mean_QSM, fill = Condition_Label)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin = mean_QSM - se_QSM, ymax = mean_QSM + se_QSM), width = 0.2) +
  theme_minimal() +
  labs(title = "Mean QSM Values After Harmonisation in Precentral and Paracentral Gyrus",
       x = "Condition", y = "Mean QSM Value", fill = "Condition") +
  facet_wrap(~ Feature, scales = "fixed") +
  theme(legend.position = "none")




# All data
qsm_data_before = qsm_dat
condition = covariates_table$condition
data_before_all_long = melt(qsm_data_before)
colnames(data_before_all_long) = c("Feature", "Subject", "Value")
data_before_all_long$Condition = rep(condition, each = nrow(qsm_data_before))

data_before_all_long_filtered = data_before_all_long %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

write.csv(data_before_all_long_filtered, "data_before_all_long_filtered.csv", row.names = FALSE)


# Check if the distribution of data follow normal distribution (use Shapiro-Wilk)
# Shapiro-Wilk normality test before harmonisation
shapiro_results_before = data_before_all_long_filtered %>%
  group_by(Feature, Condition) %>%
  summarise(p_value = shapiro.test(Value)$p.value)

shapiro_results_before


# Draw a QQ plot
ggplot(data_before_all_long_filtered, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Condition) +
  labs(title = "QQ Plot to Check Normality before harmonisation", x = "Theoretical Quantiles", y = "Sample Quantiles")


# Create long format for data after harmonisation
qsm_data_after = harmonised_data 
condition = covariates_table$condition

data_after_all_long = melt(qsm_data_after)
colnames(data_after_all_long) = c("Feature", "Subject", "Value")
data_after_all_long$Condition = rep(condition, each = nrow(qsm_data_after))

# filter data
data_after_all_long_filtered = data_after_all_long %>%
  filter(Condition %in% c(0, 1, 2, 4, 16))

write.csv(data_after_all_long_filtered, "data_after_all_long_filtered.csv", row.names = FALSE)

# Shapiro-Wilk normality test after harmonisation
shapiro_results_after = data_after_all_long_filtered %>%
  group_by(Feature, Condition) %>%
  summarise(p_value = shapiro.test(Value)$p.value)

shapiro_results_after

# Add a significance column to indicate if the distribution is non-normal
shapiro_results_after = shapiro_results_after %>%
  mutate(non_normal = ifelse(p_value < 0.05, 1, 0))

# Calculate the proportion of non-normal features per condition
non_normal_proportion = shapiro_results_after %>%
  group_by(Condition) %>%
  summarise(non_normal_count = sum(non_normal),
            total_features = n(),
            proportion_non_normal = non_normal_count / total_features)

# Print the results
non_normal_proportion

# Draw a QQ plot
ggplot(data_after_all_long_filtered, aes(sample = Value)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Condition) +
  labs(title = "QQ Plot to Check Normality after harmonisation", x = "Theoretical Quantiles", y = "Sample Quantiles")


# they don't follow normal distribution


perform_welchs_test = function(data, condition1, condition2) {
  data_filtered = data %>% filter(Condition %in% c(condition1, condition2))
  
  # Perform Welch's t test
  test_result = t.test(Value ~ Condition, data = data_filtered, var.equal = FALSE)
  
  return(data.frame(
    t_statistic = test_result$statistic,
    df = test_result$parameter,
    p_value = test_result$p.value,
    mean_group1 = mean(data_filtered$Value[data_filtered$Condition == condition1], na.rm = TRUE),
    mean_group2 = mean(data_filtered$Value[data_filtered$Condition == condition2], na.rm = TRUE)
  ))
}
#  Welch's t-test
pairwise_results_before = data_before_all_long_filtered %>%
  group_by(Feature) %>%
  summarise(
    ALS_vs_Control = list(perform_welchs_test(., 1, 0)),
    ALS_vs_PMA = list(perform_welchs_test(., 1, 4)),
    ALS_vs_PLS = list(perform_welchs_test(., 1, 2)),
    ALS_vs_MS = list(perform_welchs_test(., 1, 16))
  )

pairwise_results_before_long = pairwise_results_before %>%
  pivot_longer(
    cols = c(ALS_vs_Control, ALS_vs_PMA, ALS_vs_PLS, ALS_vs_MS),
    names_to = "Comparison",
    values_to = "Test_Result"
  ) %>%
  unnest(Test_Result)

print(pairwise_results_before_long)
pairwise_results_before_long

# select motor cortex
# Filter the data for specific Features
filtered_pairwise_results_before = pairwise_results_before_long %>%
  filter(Feature %in% c("ctx.lh.precentral", "ctx.rh.precentral", "ctx.lh.paracentral", "ctx.rh.paracentral"))

print(filtered_pairwise_results_before)


# After harmonisation
pairwise_results_after = data_after_all_long_filtered %>%
  group_by(Feature) %>%
  summarise(
    ALS_vs_Control = list(perform_welchs_test(., 1, 0)),
    ALS_vs_PMA = list(perform_welchs_test(., 1, 4)),
    ALS_vs_PLS = list(perform_welchs_test(., 1, 2)),
    ALS_vs_MS = list(perform_welchs_test(., 1, 16))
  )

pairwise_results_after_long = pairwise_results_after %>%
  pivot_longer(
    cols = c(ALS_vs_Control, ALS_vs_PMA, ALS_vs_PLS, ALS_vs_MS),
    names_to = "Comparison",
    values_to = "Test_Result"
  ) %>%
  unnest(Test_Result)

print(pairwise_results_after_long)
filtered_pairwise_results_after = pairwise_results_after_long %>%
  filter(Feature %in% c("ctx.lh.precentral", "ctx.rh.precentral", 
                        "ctx.lh.paracentral", "ctx.rh.paracentral")) %>%
  mutate(region = case_when(
    Feature == "ctx.lh.precentral" ~ "precentral",  
    Feature == "ctx.rh.precentral" ~ "precentral",  
    Feature == "ctx.lh.paracentral" ~ "paracentral", 
    Feature == "ctx.rh.paracentral" ~ "paracentral"   
  ),
  groups = case_when(
    Comparison == "ALS_vs_Control" ~ "ALS_vs_Control",
    Comparison == "ALS_vs_PLS" ~ "ALS_vs_PLS",
    Comparison == "ALS_vs_PMA" ~ "ALS_vs_PMA",
    Comparison == "ALS_vs_MS" ~ "ALS_vs_MS"
  ))

# visualise P-value
filtered_pairwise_results_after %>%
  group_by(groups) %>%
  ggplot() +
  geom_brain(atlas = dk, 
             position = position_brain(hemi ~ side),
             aes(fill = p_value)) +
  facet_wrap(~groups) + 
  labs(title = "Precentral and Paracentral Regions p-value Visualisation",
       fill = "p-value")+
  scale_fill_gradient(
    breaks = c(0.05, 0.25, 0.50, 0.75),  
    labels = c("0.05", "0.25", "0.50", "0.75")  
  )


# Check p-value of ALS group
als_p_values = filtered_pairwise_results_after %>%
  filter(groups == "ALS_vs_PLS" | groups == "ALS_vs_Control" | groups == "ALS_vs_PMA" | groups == "ALS_vs_MS" ) %>%
  select(Feature, Comparison, p_value)

print(als_p_values)


# ALS study
# Before harmonisation (group1)
als_before_harmonisation = data_before_all_long_filtered %>%
  filter(Condition == 1) %>%
  mutate(groups = "Before Harmonisation")  

# After harmonisation (group2)
als_after_harmonisation = data_after_all_long_filtered %>%
  filter(Condition == 1) %>%
  mutate(groups = "After Harmonisation")  

# Combine data
combined_als_data = bind_rows(als_before_harmonisation, als_after_harmonisation)

write.csv(combined_als_data, "combined_als_data.csv", row.names = FALSE)

# Map Feature to the supported brain region names in ggseg
combined_als_data = combined_als_data %>%
  mutate(region = case_when(
    # Map brain regions of the left and right hemispheres to regions supported by the ggseg
    Feature == "ctx.lh.precentral" ~ "precentral",
    Feature == "ctx.rh.precentral" ~ "precentral",
    Feature == "ctx.lh.paracentral" ~ "paracentral",
    Feature == "ctx.rh.paracentral" ~ "paracentral",
    Feature == "ctx.lh.postcentral" ~ "postcentral",
    Feature == "ctx.rh.postcentral" ~ "postcentral",
    Feature == "ctx.lh.supramarginal" ~ "supramarginal",
    Feature == "ctx.rh.supramarginal" ~ "supramarginal",
    Feature == "ctx.lh.inferiorparietal" ~ "inferior parietal",
    Feature == "ctx.rh.inferiorparietal" ~ "inferior parietal",
    Feature == "ctx.lh.caudalmiddlefrontal" ~ "caudal middle frontal",
    Feature == "ctx.rh.caudalmiddlefrontal" ~ "caudal middle frontal",
    Feature == "ctx.lh.parsopercularis" ~ "pars opercularis",
    Feature == "ctx.rh.parsopercularis" ~ "pars opercularis",
    Feature == "ctx.lh.parsorbitalis" ~ "pars orbitalis",
    Feature == "ctx.rh.parsorbitalis" ~ "pars orbitalis",
    Feature == "ctx.lh.parstriangularis" ~ "pars triangularis",
    Feature == "ctx.rh.parstriangularis" ~ "pars triangularis",
    Feature == "ctx.lh.caudalanteriorcingulate" ~ "caudal anterior cingulate",
    Feature == "ctx.rh.caudalanteriorcingulate" ~ "caudal anterior cingulate",
    Feature == "ctx.lh.inferiortemporal" ~ "inferior temporal",
    Feature == "ctx.rh.inferiortemporal" ~ "inferior temporal",
    Feature == "ctx.lh.precuneus" ~ "precuneus",
    Feature == "ctx.rh.precuneus" ~ "precuneus",
    Feature == "ctx.lh.medialorbitofrontal" ~ "medial orbitofrontal",
    Feature == "ctx.rh.medialorbitofrontal" ~ "medial orbitofrontal",
    Feature == "ctx.lh.pericalcarine" ~ "pericalcarine",
    Feature == "ctx.rh.pericalcarine" ~ "pericalcarine",
    Feature == "ctx.lh.parahippocampal" ~ "parahippocampal",
    Feature == "ctx.rh.parahippocampal" ~ "parahippocampal",
    Feature == "ctx.lh.superiorfrontal" ~ "superior frontal",
    Feature == "ctx.rh.superiorfrontal" ~ "superior frontal",
    Feature == "ctx.lh.superiorparietal" ~ "superior parietal",
    Feature == "ctx.rh.superiorparietal" ~ "superior parietal",
    Feature == "ctx.lh.cuneus" ~ "cuneus",
    Feature == "ctx.rh.cuneus" ~ "cuneus",
    Feature == "ctx.lh.lingual" ~ "lingual",
    Feature == "ctx.rh.lingual" ~ "lingual",
    Feature == "ctx.lh.insula" ~ "insula",
    Feature == "ctx.rh.insula" ~ "insula",
    Feature == "ctx.lh.inferiorparietal" ~ "inferior parietal",
    Feature == "ctx.rh.inferiorparietal" ~ "inferior parietal",
    Feature == "ctx.lh.fusiform" ~ "fusiform",
    Feature == "ctx.rh.fusiform" ~ "fusiform",
    Feature == "ctx.lh.entorhinal" ~ "entorhinal",
    Feature == "ctx.rh.entorhinal" ~ "entorhinal",
    Feature == "ctx.lh.middletemporal" ~ "middle temporal",
    Feature == "ctx.rh.middletemporal" ~ "middle temporal",
    Feature == "ctx.lh.superiortemporal" ~ "superior temporal",
    Feature == "ctx.rh.superiortemporal" ~ "superior temporal",
    Feature == "ctx.lh.transversetemporal" ~ "transverse temporal",
    Feature == "ctx.rh.transversetemporal" ~ "transverse temporal",
    Feature == "ctx.lh.lateraloccipital" ~ "lateral occipital",
    Feature == "ctx.rh.lateraloccipital" ~ "lateral occipital",
    Feature == "ctx.lh.lateralorbitofrontal" ~ "lateral orbitofrontal",
    Feature == "ctx.rh.lateralorbitofrontal" ~ "lateral orbitofrontal",
    Feature == "ctx.lh.isthmuscingulate" ~ "isthmus cingulate",
    Feature == "ctx.rh.isthmuscingulate" ~ "isthmus cingulate",
    Feature == "ctx.lh.posteriorcingulate" ~ "posterior cingulate",
    Feature == "ctx.rh.posteriorcingulate" ~ "posterior cingulate",
    Feature == "ctx.lh.rostralanteriorcingulate" ~ "rostral anterior cingulate",
    Feature == "ctx.rh.rostralanteriorcingulate" ~ "rostral anterior cingulate",
    Feature == "ctx.lh.rostralmiddlefrontal" ~ "rostral middle frontal",
    Feature == "ctx.rh.rostralmiddlefrontal" ~ "rostral middle frontal",
    Feature == "Left.Accumbens.area" ~ "accumbens",
    Feature == "Right.Accumbens.area" ~ "accumbens",
    Feature == "Left.Amygdala" ~ "amygdala",
    Feature == "Right.Amygdala" ~ "amygdala",
    Feature == "Left.Caudate" ~ "caudate",
    Feature == "Right.Caudate" ~ "caudate",
    Feature == "Left.Pallidum" ~ "pallidum",
    Feature == "Right.Pallidum" ~ "pallidum",
    Feature == "Left.Putamen" ~ "putamen",
    Feature == "Right.Putamen" ~ "putamen",
    Feature == "Left.Hippocampus" ~ "hippocampus",
    Feature == "Right.Hippocampus" ~ "hippocampus",
    Feature == "Left.Thalamus.Proper" ~ "thalamus",
    Feature == "Right.Thalamus.Proper" ~ "thalamus",
    Feature == "Left.Cerebellum.Cortex" ~ "cerebellum cortex",
    Feature == "Right.Cerebellum.Cortex" ~ "cerebellum cortex",
    Feature == "Left.Cerebellum.White.Matter" ~ "cerebellum white matter",
    Feature == "Right.Cerebellum.White.Matter" ~ "cerebellum white matter",
    Feature == "Left.Cerebral.White.Matter" ~ "cerebral white matter",
    Feature == "Right.Cerebral.White.Matter" ~ "cerebral white matter",
    Feature == "X3rd.Ventricle" ~ NA_character_,  # Ventricles not in atlas
    Feature == "X4th.Ventricle" ~ NA_character_,  # Ventricles not in atlas
    Feature == "Brain.Stem" ~ NA_character_,  # Brain stem not in atlas
    Feature == "CSF" ~ NA_character_,  # CSF not in atlas
    Feature == "Left.Inf.Lat.Vent" ~ NA_character_,  # Lateral ventricles not in atlas
    Feature == "Right.Inf.Lat.Vent" ~ NA_character_,
    Feature == "Left.Lateral.Ventricle" ~ NA_character_,
    Feature == "Right.Lateral.Ventricle" ~ NA_character_,
    Feature == "Left.VentralDC" ~ NA_character_,  # Ventral DC not in atlas
    Feature == "Right.VentralDC" ~ NA_character_,
    Feature == "Left.choroid.plexus" ~ NA_character_,  # Choroid plexus not in atlas
    Feature == "Right.choroid.plexus" ~ NA_character_,
    Feature == "WM.hypointensities" ~ NA_character_,
    TRUE ~ NA_character_  
  ))

# Calculate the average QSM value of each Feature
combined_als_data_avg = combined_als_data %>%
  group_by(Feature, groups, region) %>%
  summarise(mean_value = mean(Value, na.rm = TRUE))  

combined_als_data_avg$groups = factor(combined_als_data_avg$groups, 
                                       levels = c("Before Harmonisation", "After Harmonisation"))

# Visualise ALS whole brain QSM values for group1 and group2 (before and after harmonisation)
combined_als_data_avg %>%
  group_by(groups) %>%
  ggplot() +
  geom_brain(atlas = dk,  
             aes(fill = mean_value),
             position = position_brain(hemi ~ side)) +
  facet_wrap(~groups) + 
  labs(title = "QSM Values of ALS Before and After Harmonisation",
       fill = "Mean QSM Value")

# Filter brain regions with positive QSM
positive_qsm_regions = combined_als_data_avg %>%
  filter(mean_value > 0)
print(positive_qsm_regions)
write.csv(positive_qsm_regions, "positive_qsm_regions.csv", row.names = FALSE)

# Filter brain regions with negative QSM
neg_qsm_regions = combined_als_data_avg %>%
  filter(mean_value < 0)
write.csv(neg_qsm_regions, "neg_qsm_regions.csv", row.names = FALSE)

