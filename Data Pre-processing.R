#First of all read the data set into the RStudio and giving it a new name called "Agri_resources_data"
Agri_resources_data <- read.csv("Agricultural_Development_Statistics_2012-2021.csv", header = TRUE)

#Exploring the data set "Agri_resources_data":
print(Agri_resources_data) #prints the data set "Agri_resources_data" upto the limit in Rstudio
#since this is not the format we need for our dataset. So as to modify it to the required format, we can use the following codelines.
print("After removal of the column 'Time Code' and 'Country Code' from the Agri_resources_data:")
Agri_resources_data$Time.Code <- NULL
Agri_resources_data$Country.Code <- NULL
print(Agri_resources_data)
#in order to get the format, we just have to swap the columns such that country comes first and year comes second, we just have to do:
Agri_resources_data <- Agri_resources_data[, c("Country.Name" , "Time", names(Agri_resources_data)[-c(which(names(Agri_resources_data) %in% c("Country.Name" , "Time")))])]
names(Agri_resources_data) #prints the column attribute values or features of the data set
print(Agri_resources_data)
#Hence the recommended Data Structure for the assignment is attained

#The feature names seems a bit large and includes codes of it as a default coming when we download the data from World Bank Repository. Hence, we need to short it first in order to make the analysis easier and more understandable.
feature_names_modified <- c("country_name", "year_of_value", "cap_fish_prod","cer_prod", "Aqua_cult_prod", "cer_yld", "fert_cnsmtn", "crp_prod_indx" , "fd_prod_indx", "lvstk_prod_indx", "lnd_und_crl_prod", "tot_fish_prod" )
colnames(Agri_resources_data) <- feature_names_modified
names(Agri_resources_data) #Hence changed the column feature names.
head(Agri_resources_data) #prints the first few rows of the data set.
tail(Agri_resources_data) #prints the last few rows of the data set.
#we can see here that from Row No:121 to Row No: 125, there is NULL data. Removing them, we get
Agri_resources_data <- Agri_resources_data[-c(121:125), ]
tail(Agri_resources_data) #Hence the last rows having null values have been removed from the dataset

str(Agri_resources_data) #prints the "feature head", "data type", "some first row values" of the data set accordingly
summary(Agri_resources_data) #it provides "length", "class" and "mode" for string datatype and for numerical values, it gives the Box-Plot values viz., "Min", "1st Quad", "Median", "Mean", "3rd Quad", "Max" 

#checking if there is any missing values present or not:
missing_values <- sum(is.na(Agri_resources_data))
print(paste("Number of missing values:", missing_values))

#checking if there is any missing values by column:
missing_values_per_column <- colSums(is.na(Agri_resources_data))
print("Missing values per column:")
print(missing_values_per_column)

#checking if there is any missing values by row:
rows_with_missing <- Agri_resources_data[apply(is.na(Agri_resources_data), 1, any), ]
print("Rows with missing values:")
print(rows_with_missing)
#Hence, there is no missing values in our data since the data is well cleaned and we don't have to do any pre-processing in order to remove the missing values from it.
#Now, the dataset is ready for our Analysis.

#-------------------------------------------------------------------------------
#Lets split the countries into two as developing and developed countries for the latest year 2021 comparison using graphs
countries_2021 <- Agri_resources_data[Agri_resources_data$year_of_value %in% "2021" ,]
print(countries_2021)
dvlped_countries_2021 <- countries_2021[countries_2021$country_name %in% c("United Kingdom", "United States", "Germany", "Australia", "Netherlands", "Saudi Arabia"), ]
print(dvlped_countries_2021)
dvlpng_countries_2021 <- countries_2021[countries_2021$country_name %in% c("India", "Indonesia", "Brazil", "Mexico", "Sri Lanka", "China"), ]
print(dvlpng_countries_2021)
#plotting a barplot for a better visualization of our results here,
#bar plot of country-wise yield in "cap_fish_prod" in 2021- Capture fisheries production measures the volume of fish catches landed by a country for all commercial, industrial, recreational and subsistence purposes.
barplot(countries_2021$lnd_und_crl_prod,names.arg=countries_2021$country_name,col=rainbow(8))
combined_data <- rbind(
  transform(dvlpng_countries_2021, dataset = "Developing Countries"),
  transform(dvlped_countries_2021, dataset = "Developed Countries")
)
# Load necessary libraries
library(ggplot2)

# Bar plot
ggplot(combined_data, aes(x = country_name, y = lnd_und_crl_prod, fill =dataset )) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.7) +
  labs(title = "Comparison of Common Feature in Developing and Developed Countries", x = "Country Name", y = "Land under cereal production (hectares)") +
  theme_minimal()


#To compare both developed and developing countries in one frame, you might consider using a line plot. These visualization techniques allow you to compare the distribution or summary statistics of a variable across different categories, such as developed and developing countries.
install.packages("ggplot2") #install required packages
library(ggplot2) #required library

data_india <- Agri_resources_data[Agri_resources_data$country_name == "India",]
print(data_india)
data_UK <- Agri_resources_data[Agri_resources_data$country_name == "United Kingdom", ]
print(data_UK)
ggplot() +
  geom_line(data = data_india, aes(x = data_india$year_of_value , y = data_india$cer_yld, color = "India"), group = 1) +
  geom_line(data = data_UK, aes(x = data_UK$year_of_value, y = data_UK$cer_yld, color = "United Kingdom"), group = 1) +
  labs(title = "Comparison of cer_yld between India (Developing) and United Kingdom (developed) ", x = "Year from 2012 to 2021", y = "Cereal production (metric tons)") +
  theme_minimal()

developed_countries_data <- Agri_resources_data[Agri_resources_data$country_name %in% c("United Kingdom", "United States", "Saudi Arabia", "Australia", "Netherlands", "Germany"),]
print(developed_countries_data)
developing_countries_data <- Agri_resources_data[Agri_resources_data$country_name %in% c("India", "China", "Sri Lanka", "Indonesia", "Brazil", "Mexico"),]
print(developing_countries_data)
ggplot() +
  geom_point(data = developed_countries_data, aes(x = developed_countries_data$country_name, y = developed_countries_data$cap_fish_prod), color = "blue", alpha = 0.6) +
  geom_point(data = developing_countries_data, aes(x = developing_countries_data$country_name, y = developing_countries_data$cap_fish_prod), color = "red", alpha = 0.6) +
  labs(title = "Scatter Plot between developed and developing countries", x = "Country Name (Incuded Both developed(blue) and Developing(Red)", y = "Capture fisheries production (metric tons)") +
  theme_minimal()

#usage of the boxplot to find out the outliers
boxplot(lnd_und_crl_prod~country_name, data=Agri_resources_data,col=rainbow(8), horizontal = TRUE)

#lnd_und_crl_prod:
boxplot(Agri_resources_data$lnd_und_crl_prod, data=Agri_resources_data)
outliers <- boxplot(Agri_resources_data$lnd_und_crl_prod)$out
# Print the identified outliers
print(paste("Outliers:", outliers))

boxplot(developing_countries_data$lnd_und_crl_prod, data=Agri_resources_data)
outliers <- boxplot(developing_countries_data$lnd_und_crl_prod)$out
# Print the identified outliers
print(paste("Outliers:", outliers))

boxplot(developed_countries_data$lnd_und_crl_prod, data=Agri_resources_data, xlab = "Developed Countries Boxplot",ylab = "Land Under Cereal Production")
outliers <- boxplot(developed_countries_data$lnd_und_crl_prod)$out
# Print the identified outliers
print(paste("Outliers:", outliers))

#just to see which country have the outliers
outlier <- 54744521 #lets take one outlier value
row_containing_outlier <- which(developed_countries_data$lnd_und_crl_prod == outlier)
if (length(row_containing_outlier) > 0) {
  data <- developed_countries_data[row_containing_outlier, ]
  # Print the results
  print(paste("Outlier:", outlier))
  print(paste("Country Name having the outlier:", data$country_name))
} else {
  print("No match found.")
}

#cap_fish_prod:
boxplot(Agri_resources_data$cap_fish_prod, data=Agri_resources_data)
outliers <- boxplot(Agri_resources_data$cap_fish_prod)$out
# Print the identified outliers
print(paste("Outliers:", outliers))