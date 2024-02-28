# Data visualisation using RStudio
Detailed statistical analysis of twelve (developed and developing) countries agricultural development between 2012 to 2021 portraying different data visualisation techniques such as  correlation analysis, regression analysis, hypothesis testing and time-series analysis as well.

## OVERALL – CODEBASE OVERVIEW
Basically, I have included all the codebase into a R Script since all the codebase I prepared are interconnected (since the dataset is the same, so in order to reduce the code lines, I have initiated the variable at the beginning and continued to use that whenever required.For a detailed view on the specific tasks, I have separately created the task-based R script files for reducing the confusion. Hereby providing the variable names and initializations which is there for the code for all the subtasks.

# 1.	Data preprocessing: 

### Agri_resources_data: 	
Variable saved for the data frame named “Agricultural_Development_Statistics_2012-2021.csv”
### feature_names_modified: 	
Modified features name containing variable
### missing_values: 	
For getting the missing values in the dataset
### missing_values_per_column: 	
For getting the missing values per column
### rows_with_missing: 	
For getting the missing values per row
### countries_2021: 	
Containing data of all the countries for the year 2021
### dvlped_countries_2021: 	
Containing data of all the countries for the year 2021
### dvlpng_countries_2021: 	
Containing data of all the countries for the year 2021
### data_(country_name): 	
Specifies the country data only for the period between 2012 to 2021
### developed_countries_data: 	
Specifies the data for developed countries only
### developing_countries_data:	
Specifies the data for developing countries only

## 2.	Statistical Analysis:

### (feature_name)_mean:	
Stores the mean value of the feature described along with
### Agri_resources_in_metric_tons:
Stored the subset of the dataset containing features whose measure are only in metric tons
### max_mean_feature:
Stored the feature name of which the maximum mean calculated
### max_mean_value:	
Stored the maximum mean value of the feature calculated
### developed_countries_data:	
Contains only the developed countries data
### Agri_resources_in_metric_tons_dvlped:	
Contains developed countries data of the features whose measured value is in metric tons
### means_in_one_line_dvlped:	
Store the means calculated in one line
### max_mean_feature_dvlped:	
Stored the maximum feature containing maximum mean in the developed countries
### max_mean_value_dvlped:	
Stored the maximum mean in the developed countries
### developing_countries_data:	
Contains only the developing countries data
### Agri_resources_in_metric_tons_dvlpng:	
Contains developing countries data of the features whose measured value is in metric tons
### means_in_one_line_dvlpng:	
Store the means calculated in one line
### max_mean_feature_dvlpng:	
Stored the maximum feature containing maximum mean in the developing countries
### max_mean_value_dvlpng:	
Stored the maximum mean in the developing countries
### outliers:	
Stored the outlier values found in the dataset for the condition given
### row_containing_outlier:	
Stores the rows which contains the outlier values
### (country_name) _data:	
Stored the specific country name data given
### Kurtosis_(feature_name):	
Stores the Kurtosis value of the feature name given
### hist_plot:
Histogram plot initialization
### density_plot:	
Density plot initialization
### skew_(feature_name):
Stores the Skewness value of the feature name given along with

## 3.	Correlation Analysis:

### agri_prod_exc_cntry_nd_yr:
Agricultural production data excluding the country_name and year_of_value column head
### correlation_spearman:
Spearman correlation initialization
### correlation_point_biserial:	
Point-biserial correlation initialization

## 4.	Hypothesis Testing:

### Agri_resources_data_upto_2020:
Stores the agricultural production data up to 2020
### agri_crl_yld_2021:
Stores the agricultural data of the feature crl_yld for the year 2021
### agri_prod_dvlpd_cntrs:
Stores the data for developed countries
### agri_prod_dvlpng_cntrs:
Stores the data for developing countries
### agri_prod_dvlpd_cntrs_2021:	
Stores the data for developed countries over the year 2021
### agri_prod_dvlpng_cntrs_2021:
Stores the data for developing countries over the year 2021
### agri_prod_dvlpd_cntrs_2012:
Stores the data for developed countries over the year 2012
### agri_prod_dvlpng_cntrs_2012:
Stores the data for developing countries over the year 2012

## 5.	Regression Analysis:
### Agri_resources_data_num:
Stores all the numerical feature data of the overall dataset
### regression_model:
Regression model initialization

## 6.	Time-Series Analysis:

### time_series_data_(feature_name):
Time series data initialization for the specific feature name given
### arima_model_(feature_name):
Arima model initialization for the specific feature name given
### forecast_(feature_name):
Forecast initialization for the specific feature name given

# How to Run the Project:

### 1. Open RStudio:
   - Launch RStudio on your computer.

### 2. Create a Project:
   - If you haven't already, create a new project by going to `File -> New Project` and choose the appropriate project type (e.g., New Directory, Existing Directory).

### 3. Set Working Directory:
   - Make sure your working directory is set to the directory where your project is located. You can check and set the working directory using the `Session -> Set Working Directory` menu.

### 4. Create R Scripts or Notebooks:
   - Write your R code in R scripts (files with a .R extension) or R Notebooks (files with a .Rmd extension).

### 5. Load Data:
   - load the dataset into R using functions like `read.csv()` or other appropriate functions.

### 6. Run Code:
   - Run your R code by selecting the code and using the `Run` button, or by using the keyboard shortcut `Ctrl + Enter` (Windows/Linux) or `Command + Enter` (Mac).

### 7. View Output:
   - Inspect the output in the Console or the Plots/Files/Viewer panes, depending on the type of output your code produces.
