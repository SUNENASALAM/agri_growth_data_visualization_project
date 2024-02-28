print(Agri_resources_data)
cap_fish_prod_mean <- mean(Agri_resources_data$cap_fish_prod)
print(cap_fish_prod_mean) #gives the mean value of the feature "cap_fish_prod" for the whole countries

cer_prod_mean <- mean(Agri_resources_data$cer_prod)
print(cer_prod_mean) #gives the mean value of the feature "cer_prod" for the whole countries

Aqua_cult_prod_mean <- mean(Agri_resources_data$Aqua_cult_prod) 
print(Aqua_cult_prod_mean) #gives the mean value of the feature "Aqua_cult_prod" for the whole countries

cer_yld_mean <- mean(Agri_resources_data$cer_yld)
print(cer_yld_mean) #gives the mean value of the feature "cer_yld" for the whole countries

fert_cnsmtn_mean <- mean(Agri_resources_data$fert_cnsmtn)
print(fert_cnsmtn_mean) #gives the mean value of the feature "fert_cnsmtn" for the whole countries

crp_prod_indx_mean <- mean(Agri_resources_data$crp_prod_indx)
print(crp_prod_indx_mean) #gives the mean value of the feature "crp_prod_indx" for the whole countries

fd_prod_indx_mean <- mean(Agri_resources_data$fd_prod_indx)
print(fd_prod_indx_mean) #gives the mean value of the feature "fd_prod_indx" for the whole countries

lvstk_prod_indx_mean <- mean(Agri_resources_data$lvstk_prod_indx)
print(lvstk_prod_indx_mean) #gives the mean value of the feature "lvstk_prod_indx" for the whole countries

lnd_und_crl_prod <- mean(Agri_resources_data$lnd_und_crl_prod)
print(lnd_und_crl_prod) #gives the mean value of the feature "lnd_und_crl_prod" for the whole countries

tot_fish_prod_mean <- mean(Agri_resources_data$tot_fish_prod)
print(tot_fish_prod_mean) #gives the mean value of the feature "tot_fish_prod" for the whole countries

#to find out complete features mean in one, lets use the below codeline
#lets separate the three features which we want to compare, i.e., "cap_fish_prod", "cer_prod" and "Aqua_cult_prod"
Agri_resources_in_metric_tons <- Agri_resources_data[, c("cap_fish_prod", "cer_prod" , "Aqua_cult_prod")]
print(Agri_resources_in_metric_tons)
means_in_one_line <- colMeans(Agri_resources_in_metric_tons)
max_mean_feature <- names(means_in_one_line)[which.max(means_in_one_line)]
max_mean_value <- max(means_in_one_line)
# Print the results
print(paste("Feature with the largest mean:", max_mean_feature))
print(paste("Largest mean value:", max_mean_value))

#finding which one has the highest data, either developed or developing
developed_countries_data <- Agri_resources_data[Agri_resources_data$country_name %in% c("United Kingdom", "United States", "Saudi Arabia", "Australia", "Netherlands", "Germany"),]
print(developed_countries_data)
Agri_resources_in_metric_tons_dvlped <- developed_countries_data[, c("cap_fish_prod", "cer_prod" , "Aqua_cult_prod")]
print(Agri_resources_in_metric_tons_dvlped)
means_in_one_line_dvlped <- colMeans(Agri_resources_in_metric_tons_dvlped)
max_mean_feature_dvlped <- names(means_in_one_line_dvlped)[which.max(means_in_one_line_dvlped)]
max_mean_value_dvlped <- max(means_in_one_line_dvlped)
# Print the results
print(paste("Feature with the largest mean:", max_mean_feature_dvlped))
print(paste("Largest mean value:", max_mean_value_dvlped))

developing_countries_data <- Agri_resources_data[Agri_resources_data$country_name %in% c("India", "China", "Sri Lanka", "Indonesia", "Brazil", "Mexico"),]
print(developing_countries_data)
Agri_resources_in_metric_tons_dvlpng <- developing_countries_data[, c("cap_fish_prod", "cer_prod" , "Aqua_cult_prod")]
print(Agri_resources_in_metric_tons_dvlpng)
means_in_one_line_dvlpng <- colMeans(Agri_resources_in_metric_tons_dvlpng)
max_mean_feature_dvlpng <- names(means_in_one_line_dvlpng)[which.max(means_in_one_line_dvlpng)]
max_mean_value_dvlpng <- max(means_in_one_line_dvlpng)
# Print the results
print(paste("Feature with the largest mean:", max_mean_feature_dvlpng))
print(paste("Largest mean value:", max_mean_value_dvlpng))

if (max_mean_value_dvlped>max_mean_feature_dvlpng) {
  print(paste("The Largest Mean Feature is from developed countries data and that is", max_mean_value_dvlped))
  print(paste("Feature with the largest means is:", max_mean_feature_dvlped))
} else {
  print(paste("The Largest Mean Feature is from developing countriies data and that is", max_mean_value_dvlpng))
  print(paste("Feature with the largest means is:", max_mean_feature_dvlpng))
}

print(median(Agri_resources_data$cap_fish_prod)) #gives the median value of the feature "cap_fish_prod" for the whole countries
print(median(Agri_resources_data$cer_prod)) #gives the median value of the feature "cer_prod" for the whole countries
print(median(Agri_resources_data$Aqua_cult_prod)) #gives the median value of the feature "Aqua_cult_prod" for the whole countries
print(median(Agri_resources_data$cer_yld)) #gives the median value of the feature "cer_yld" for the whole countries
print(median(Agri_resources_data$fert_cnsmtn)) #gives the median value of the feature "fert_cnsmtn" for the whole countries
print(median(Agri_resources_data$crp_prod_indx)) #gives the median value of the feature "crp_prod_indx" for the whole countries
print(median(Agri_resources_data$fd_prod_indx)) #gives the median value of the feature "fd_prod_indx" for the whole countries
print(median(Agri_resources_data$lvstk_prod_indx)) #gives the median value of the feature "lvstk_prod_indx" for the whole countries
print(median(Agri_resources_data$lnd_und_crl_prod)) #gives the median value of the feature "lnd_und_crl_prod" for the whole countries
print(median(Agri_resources_data$tot_fish_prod)) #gives the median value of the feature "tot_fish_prod" for the whole countries


get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
print(get_mode(Agri_resources_data$cap_fish_prod)) #gives the mode value of the feature "cap_fish_prod" for the whole countries
print(get_mode(Agri_resources_data$cer_prod)) #gives the mode value of the feature "cer_prod" for the whole countries
print(get_mode(Agri_resources_data$Aqua_cult_prod)) #gives the mode value of the feature "Aqua_cult_prod" for the whole countries
print(get_mode(Agri_resources_data$cer_yld)) #gives the mode value of the feature "cer_yld" for the whole countries
print(get_mode(Agri_resources_data$fert_cnsmtn)) #gives the mode value of the feature "fert_cnsmtn" for the whole countries
print(get_mode(Agri_resources_data$crp_prod_indx)) #gives the mode value of the feature "crp_prod_indx" for the whole countries
print(get_mode(Agri_resources_data$fd_prod_indx)) #gives the mode value of the feature "fd_prod_indx" for the whole countries
print(get_mode(Agri_resources_data$lvstk_prod_indx)) #gives the mode value of the feature "lvstk_prod_indx" for the whole countries
print(get_mode(Agri_resources_data$lnd_und_crl_prod)) #gives the mode value of the feature "lnd_und_crl_prod" for the whole countries
print(get_mode(Agri_resources_data$tot_fish_prod)) #gives the mode value of the feature "tot_fish_prod" for the whole countries

print(sd(Agri_resources_data$cap_fish_prod)) #gives the standard deviation value of the feature "cap_fish_prod" for the whole countries
print(sd(Agri_resources_data$cer_prod)) #gives the standard deviation value of the feature "cer_prod" for the whole countries
print(sd(Agri_resources_data$Aqua_cult_prod)) #gives the standard deviation value of the feature "Aqua_cult_prod" for the whole countries
print(sd(Agri_resources_data$cer_yld)) #gives the standard deviation value of the feature "cer_yld" for the whole countries
print(sd(Agri_resources_data$fert_cnsmtn)) #gives the standard deviation value of the feature "fert_cnsmtn" for the whole countries
print(sd(Agri_resources_data$crp_prod_indx)) #gives the standard deviation value of the feature "crp_prod_indx" for the whole countries
print(sd(Agri_resources_data$fd_prod_indx)) #gives the standard deviation value of the feature "fd_prod_indx" for the whole countries
print(sd(Agri_resources_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the whole countries
print(sd(Agri_resources_data$lnd_und_crl_prod)) #gives the standard deviation value of the feature "lnd_und_crl_prod" for the whole countries
print(sd(Agri_resources_data$tot_fish_prod)) #gives the standard deviation value of the feature "tot_fish_prod" for the whole countries

#let's analyse how the feature "lvstk_prod_indx" standard deviation can relate to the countries
print(sd(Agri_resources_data$lvstk_prod_indx))
print(mean(Agri_resources_data$lvstk_prod_indx))
#since outliers can be significant in the analysis, lets have a boxplot
boxplot(developed_countries_data$lvstk_prod_indx, data=Agri_resources_data, xlab = "Developed Countries Boxplot",ylab = "Livestock Production Index")
outliers <- boxplot(Agri_resources_data$lvstk_prod_indx)$out
# Print the identified outliers
print(paste("Outliers:", outliers))
#just to see which country have the outliers
outlier <- 78.06 #lets take one outlier value
row_containing_outlier <- which(Agri_resources_data$lvstk_prod_indx == outlier)
if (length(row_containing_outlier) > 0) {
  data <- Agri_resources_data[row_containing_outlier, ]
  # Print the results
  print(paste("Outlier:", outlier))
  print(paste("Country Name having the outlier:", data$country_name))
} else {
  print("No match found.")
}

#Let's analyse the standard deviation over different countries and find out the stability/variancy of the feature "lvstk_prod_indx"
#for that, lets split the data into 12 as per the country level
UK_data <- Agri_resources_data[Agri_resources_data$country_name == "United Kingdom",]
print(UK_data) #printing data only having country name as United Kingdom
print(sd(UK_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country United Kingdom
boxplot(UK_data$lvstk_prod_indx, data=UK_data, xlab = "United Kingdom",ylab = "Livestock Production Index") #there is no outliers as well

Indi_data <- Agri_resources_data[Agri_resources_data$country_name == "India",]
print(Indi_data) #printing data only having country name as India
print(sd(Indi_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country India
boxplot(Indi_data$lvstk_prod_indx, data=Indi_data, xlab = "India",ylab = "Livestock Production Index") #there is no outliers as well

Indo_data <- Agri_resources_data[Agri_resources_data$country_name == "Indonesia",]
print(Indo_data) #printing data only having country name as Indonesia
print(sd(Indo_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Indonesia
boxplot(Indo_data$lvstk_prod_indx, data=Indo_data, xlab = "Indonesia",ylab = "Livestock Production Index") #there is no outliers as well

US_data <- Agri_resources_data[Agri_resources_data$country_name == "United States",]
print(US_data) #printing data only having country name as United States
print(sd(US_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country United States
boxplot(US_data$lvstk_prod_indx, data=US_data, xlab = "United States",ylab = "Livestock Production Index") #there is no outliers as well

Ger_data <- Agri_resources_data[Agri_resources_data$country_name == "Germany",]
print(Ger_data) #printing data only having country name as Germany
print(sd(Ger_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Germany
boxplot(Ger_data$lvstk_prod_indx, data=Ger_data, xlab = "Germany",ylab = "Livestock Production Index") #there is no outliers as well

SA_data <- Agri_resources_data[Agri_resources_data$country_name == "Saudi Arabia",]
print(SA_data) #printing data only having country name as Saudi Arabia
print(sd(SA_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Saudi Arabia
boxplot(SA_data$lvstk_prod_indx, data=SA_data, xlab = "Saudi Arabia",ylab = "Livestock Production Index") #there is no outliers as well

Net_data <- Agri_resources_data[Agri_resources_data$country_name == "Netherlands",]
print(Net_data) #printing data only having country name as Netherlands
print(sd(Net_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Netherlands
boxplot(Net_data$lvstk_prod_indx, data=Net_data, xlab = "Netherlands",ylab = "Livestock Production Index") #there is no outliers as well

Chi_data <- Agri_resources_data[Agri_resources_data$country_name == "China",]
print(Chi_data) #printing data only having country name as China
print(sd(Chi_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country China
boxplot(Chi_data$lvstk_prod_indx, data=Chi_data, xlab = "China",ylab = "Livestock Production Index") #there is one outlier and can be ignored

Mex_data <- Agri_resources_data[Agri_resources_data$country_name == "Mexico",]
print(Mex_data) #printing data only having country name as Mexico
print(sd(Mex_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Mexico
boxplot(Mex_data$lvstk_prod_indx, data=Mex_data, xlab = "Mexico",ylab = "Livestock Production Index") #there is no outliers as well

Bra_data <- Agri_resources_data[Agri_resources_data$country_name == "Brazil",]
print(Bra_data) #printing data only having country name as Brazil
print(sd(Bra_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Brazil
boxplot(Bra_data$lvstk_prod_indx, data=Bra_data, xlab = "Brazil",ylab = "Livestock Production Index") #there is no outliers as well

Aus_data <- Agri_resources_data[Agri_resources_data$country_name == "Australia",]
print(Aus_data) #printing data only having country name as Australia
print(sd(Aus_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Australia
boxplot(Aus_data$lvstk_prod_indx, data=Aus_data, xlab = "Australia",ylab = "Livestock Production Index") #there is no outliers as well

SL_data <- Agri_resources_data[Agri_resources_data$country_name == "Sri Lanka",]
print(SL_data) #printing data only having country name as Sri Lanka
print(sd(SL_data$lvstk_prod_indx)) #gives the standard deviation value of the feature "lvstk_prod_indx" for the country Sri Lanka
boxplot(SL_data$lvstk_prod_indx, data=SL_data, xlab = "Sri Lanka",ylab = "Livestock Production Index") #there is no outliers as well

install.packages("moments")
# Load the 'moments' package
install.packages("gridExtra")
library(moments)
library(ggplot2)
library(e1071)  # for skewness and kurtosis functions
library(gridExtra)

# Calculate kurtosis - feature - cap_fish_prod
Kurtosis_cap_fish_prod <- e1071::kurtosis(Agri_resources_data$cap_fish_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cap_fish_prod), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Capital Fish Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cap_fish_prod, 2)),
       x = "Capital Fish Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cap_fish_prod), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Capital Fish Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cap_fish_prod, 2)),
       x = "Capital Fish Production in metric tons",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - cer_prod
Kurtosis_cer_prod <- e1071::kurtosis(Agri_resources_data$cer_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cer_prod), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Cereal Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cer_prod, 2)),
       x = "Cereal Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cer_prod), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Cereal Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cer_prod, 2)),
       x = "Cereal Production in metric tons",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - Aqua_cult_prod
Kurtosis_Aqua_cult_prod <- e1071::kurtosis(Agri_resources_data$Aqua_cult_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$Aqua_cult_prod), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Aquaculture Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_Aqua_cult_prod, 2)),
       x = "Aquaculture Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$Aqua_cult_prod), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Aquaculture Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_Aqua_cult_prod, 2)),
       x = "Aquaculture Production in metric tons",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - cer_yld
Kurtosis_cer_yld <- e1071::kurtosis(Agri_resources_data$cer_yld)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cer_yld), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Cereal Yield with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cer_yld, 2)),
       x = "Cereal Yield in Kg per Hectare",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cer_yld), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Cereal Yield with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_cer_yld, 2)),
       x = "Cereal Yield in Kg per Hectare",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - fert_cnsmtn
Kurtosis_fert_cnsmtn <- e1071::kurtosis(Agri_resources_data$fert_cnsmtn)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$fert_cnsmtn), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Fertilizer Consumption with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_fert_cnsmtn, 2)),
       x = "Fertilizer Consumption in percentage",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$fert_cnsmtn), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Fertilizer Consumption with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_fert_cnsmtn, 2)),
       x = "Fertilizer Consumption in percentage",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - crp_prod_indx
Kurtosis_crp_prod_indx <- e1071::kurtosis(Agri_resources_data$crp_prod_indx)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$crp_prod_indx), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Crop Production Index with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_crp_prod_indx, 2)),
       x = "Crop Production",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$crp_prod_indx), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Crop Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_crp_prod_indx, 2)),
       x = "Crop Production",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - fd_prod_indx
Kurtosis_fd_prod_indx <- e1071::kurtosis(Agri_resources_data$fd_prod_indx)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$fd_prod_indx), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Food Production Index with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_fd_prod_indx, 2)),
       x = "Food Production",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$fd_prod_indx), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Food Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_fd_prod_indx, 2)),
       x = "Food Production",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - lvstk_prod_indx
Kurtosis_lvstk_prod_indx <- e1071::kurtosis(Agri_resources_data$lvstk_prod_indx)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$lvstk_prod_indx), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Livestock Production Index with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_lvstk_prod_indx, 2)),
       x = "Livestock Production",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$lvstk_prod_indx), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Livestock Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_lvstk_prod_indx, 2)),
       x = "Livestock Production",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - lnd_und_crl_prod
Kurtosis_lnd_und_crl_prod <- e1071::kurtosis(Agri_resources_data$lnd_und_crl_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$lnd_und_crl_prod), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Land Under Cereal Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_lnd_und_crl_prod, 2)),
       x = "Land Under Cereal Production in hectares",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$lnd_und_crl_prod), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Land Under Cereal Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_lnd_und_crl_prod, 2)),
       x = "Land Under Cereal Production in hectares",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate kurtosis - feature - tot_fish_prod
Kurtosis_tot_fish_prod <- e1071::kurtosis(Agri_resources_data$tot_fish_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$tot_fish_prod), aes(x)) +
  geom_histogram(fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Total Fish Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_tot_fish_prod, 2)),
       x = "Total Fish Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$tot_fish_prod), aes(x)) +
  geom_density(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Total Fish Production with Kurtosis",
       subtitle = paste("Kurtosis:", round(Kurtosis_tot_fish_prod, 2)),
       x = "Total Fish Production in metric tons",
       y = "Density") +
  theme_minimal()

# Arrange plots side by side
grid.arrange(hist_plot, density_plot, ncol = 2)

#-------------------------------------------------------------------------------
# Skewness
install.packages("car")
library(e1071)  # for skewness function
library(car)    # for qqPlot function
library(ggplot2)
library(gridExtra)

# Calculate skewness - feature - cap_fish_prod
skew_cap_fish_prod <- skewness(Agri_resources_data$cap_fish_prod)

# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cap_fish_prod), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Capital Fish production with Skewness",
       subtitle = paste("Skewness:", round(skew_cap_fish_prod, 2)),
       x = "Capital Fish Production in Metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cap_fish_prod), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Capital Fish production with Skewness",
       subtitle = paste("Skewness:", round(skew_cap_fish_prod, 2)),
       x = "Capital Fish Production in Metric tons",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)
#-----------------------------------------------------------------------------------------------
# Calculate skewness - feature - fert_cnsmtn
skew_fert_cnsmtn <- skewness(Agri_resources_data$fert_cnsmtn)
print(skew_fert_cnsmtn)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$fert_cnsmtn), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Fertilizer consumption with Skewness",
       subtitle = paste("Skewness:", round(skew_fert_cnsmtn, 2)),
       x = "Fertilizer Consumption in Percentage",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$fert_cnsmtn), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Fertilizer Consumption with Skewness",
       subtitle = paste("Skewness:", round(skew_fert_cnsmtn, 2)),
       x = "Fertilizer Consumption in Percentage",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-----------------------------------------------------------------------------------------------
# Calculate skewness - feature - cer_prod
skew_cer_prod <- skewness(Agri_resources_data$cer_prod)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cer_prod), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Cereal production with Skewness",
       subtitle = paste("Skewness:", round(skew_cer_prod, 2)),
       x = "Cereal Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cer_prod), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Cereal production with Skewness",
       subtitle = paste("Skewness:", round(skew_cer_prod, 2)),
       x = "Cereal production in metric tons",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - Aqua_cult_prod
skew_Aqua_cult_prod <- skewness(Agri_resources_data$Aqua_cult_prod)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$Aqua_cult_prod), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Aquaculture production with Skewness",
       subtitle = paste("Skewness:", round(skew_Aqua_cult_prod, 2)),
       x = "Aquaculture Production in metric tons",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$Aqua_cult_prod), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Aquaculture production with Skewness",
       subtitle = paste("Skewness:", round(skew_Aqua_cult_prod, 2)),
       x = "Aquaculture production in metric tons",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - cer_yld
skew_cer_yld <- skewness(Agri_resources_data$cer_yld)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$cer_yld), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Cereal yield with Skewness",
       subtitle = paste("Skewness:", round(skew_cer_yld, 2)),
       x = "Cereal Yield in Kg per hectares",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$cer_yld), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Cereal Yield with Skewness",
       subtitle = paste("Skewness:", round(skew_cer_yld, 2)),
       x = "Cereal Yield in Kg per hectares",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - crp_prod_indx
skew_crp_prod_indx <- skewness(Agri_resources_data$crp_prod_indx)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$crp_prod_indx), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Crop Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_crp_prod_indx, 2)),
       x = "Crop Production Index",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$crp_prod_indx), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Crop Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_crp_prod_indx, 2)),
       x = "Crop production Index",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - fd_prod_indx
skew_fd_prod_indx <- skewness(Agri_resources_data$fd_prod_indx)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$fd_prod_indx), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Food Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_fd_prod_indx, 2)),
       x = "Food Production Index",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$fd_prod_indx), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Food Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_fd_prod_indx, 2)),
       x = "Food production Index",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - lvstk_prod_indx
skew_lvstk_prod_indx <- skewness(Agri_resources_data$lvstk_prod_indx)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$lvstk_prod_indx), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Livestock Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_lvstk_prod_indx, 2)),
       x = "Livestock Production Index",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$lvstk_prod_indx), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Livestock Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_lvstk_prod_indx, 2)),
       x = "Livestock production Index",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - lnd_und_crl_prod
skew_lnd_und_crl_prod <- skewness(Agri_resources_data$lnd_und_crl_prod)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$lnd_und_crl_prod), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Land Under Cereal Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_lnd_und_crl_prod, 2)),
       x = "Land Under Cereal Production Index",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$lnd_und_crl_prod), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Land Under Cereal Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_lnd_und_crl_prod, 2)),
       x = "land Under Cereal production Index",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)

#-------------------------------------------------------------------------------
# Calculate skewness - feature - tot_fish_prod
skew_tot_fish_prod <- skewness(Agri_resources_data$tot_fish_prod)
# Create a histogram
hist_plot <- ggplot(data.frame(x = Agri_resources_data$tot_fish_prod), aes(x)) +
  geom_histogram(fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Total Fish Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_tot_fish_prod, 2)),
       x = "Total Fish Production Index",
       y = "Frequency") +
  theme_minimal()

# Create a density plot
density_plot <- ggplot(data.frame(x = Agri_resources_data$tot_fish_prod), aes(x)) +
  geom_density(fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Density Plot of Total Fish Production Index with Skewness",
       subtitle = paste("Skewness:", round(skew_tot_fish_prod, 2)),
       x = "Total Fish production Index",
       y = "Density") +
  theme_minimal()

#conversting hist_plot into hist_group since only 'grobs' allowed in "gList"
hist_grob <- ggplotGrob(hist_plot)
density_grob <- ggplotGrob(density_plot)

# Arrange plots side by side
grid.arrange(hist_grob, density_grob, ncol = 2)
