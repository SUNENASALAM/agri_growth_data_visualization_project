#As a researcher, define at least two hypotheses testing related to the objectives and test them.

#in order to begin with the hypothesis testing, lets install the required packages and load the libraries
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
library(ggplot2)
library(datarium)
library(qqplotr)

#----------------------------------------------------------------------------------------------
#research condition 1:
#--> Mean of the feature "cer_yld" in the year 2021 for all the countries is greater than the combined mean of the feature "cer_yld" for the previous 9 years
#for this, lets find out the mean of the features of the previous 9 years from 2012 to 2020
Agri_resources_data_upto_2020 <- Agri_resources_data[Agri_resources_data$year_of_value %in% c("2012","2013","2014","2015","2016","2017","2018","2019","20120"), ]
print(Agri_resources_data_upto_2020)
sum(mean(Agri_resources_data_upto_2020$cer_yld))
#we are trying to test here that the mean value of our dataset containing only 2021 data have value which is greater than 5385.218


#in-order to carry out the hypothesis testing, we need to first find out whether the data is normally distributed or not.
#we can use two tests to double confirm the normality of the variables used in the correlation analysis:

Agri_resources_data <- Agri_resources_data[, !(names(Agri_resources_data) %in% c("cer_yld_log", "cube_root_transformed_data_2", "cube_root_transformed_data", "cap_fish_prod_sqaure_root", "cap_fish_prod_log", "cap_fish_prod10", "cap_fish_prod_squared"))]
print(Agri_resources_data)

agri_crl_yld_2021 <- Agri_resources_data[Agri_resources_data$year_of_value %in% c("2021"), ]
print(agri_crl_yld_2021)

#feature: "cer_yld"
#Test - 1 :- Q-Q Plot:- visual method
#If the points in the plot roughly fall along a straight diagonal line, then the data is assumed to be normally distributed.
ggplot(mapping = aes(sample = agri_crl_yld_2021$cer_yld)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical") + 
  ylab("Sample")
#Its clearly visible that the data is normally distributed here.

#Test - 2 :- Shapiro-Wilk Test (Statistical Test)
#If the p-value of the test is greater than α = 0.05, then the data is assumed to be normally distributed
shapiro.test(agri_crl_yld_2021$cer_yld)
#P value = 0.8574, which is clearly less than α = 0.05, hence confirmed the feature is normally distributed.

#null hypothesis here is:
#Mean is greater than 5385.218
#𝐻0: 𝜇 > 5385.218

#Alternate Hypothesis is:
#Mean is less than 5385.218
#𝐻1: 𝜇 <  5385.218

#for a better view, lets plot a histogram showing the distribution of the "cer_yld" feature for the year 2021
hist(agri_crl_yld_2021$cer_yld)
t.test(agri_crl_yld_2021$cer_yld, mu=5385.218, alternative="less")
#hence,p value of the test result, p = 0.5691 > 0.05 threshold value. Thus, null hypothesis cannot be ignores. Thus, the mean of the feature "cer_yld" in the year 2021 for all the countries is greater than the combined mean of the feature "cer_yld" for the previous 9 years.  
#----------------------------------------------------------------------------------------------
#research condition 2:
#--> We are going to use the Paired Sample T Test and doing two sub-researches
#--> In First research, we are trying to point out that the features "cer_yld", "crp_prod_indx" developement for the developed countries in the year 2021 is way better than the year 2012
#--> In Second research, we are trying to point out that the features "cer_yld", "crp_prod_indx" developement for the developing countries in the year 2021 is way better than the year 2012

#for that, we need to split the data into "agri_prod_dvlpd_cntrs" (Agricultural production for developed countries), "agri_prod_dvlpd_cntrs_2021" (Agricultural production for developed countries in 2021), "agri_prod_dvlpng_cntrs" (Agrocultural Production for developing countries) and "agri_prod_dvlpng_cntrs_2021" (Agricultural production for developing countries 2021)
agri_prod_dvlpd_cntrs <- Agri_resources_data[Agri_resources_data$country_name %in% c("Australia", "Germany", "Netherlands", "United Kingdom", "United States", "Saudi Arabia"), ]
print(agri_prod_dvlpd_cntrs)
agri_prod_dvlpng_cntrs <- Agri_resources_data[Agri_resources_data$country_name %in% c("Mexico", "India", "Indonesia", "Sri Lanka", "Brazil", "China"), ]
print(agri_prod_dvlpng_cntrs)

agri_prod_dvlpd_cntrs_2021 <- agri_prod_dvlpd_cntrs[agri_prod_dvlpd_cntrs$year_of_value %in% c("2021"), ]
print(agri_prod_dvlpd_cntrs_2021)
agri_prod_dvlpng_cntrs_2021 <- agri_prod_dvlpng_cntrs[agri_prod_dvlpng_cntrs$year_of_value %in% c("2021"), ]
print(agri_prod_dvlpng_cntrs_2021)

agri_prod_dvlpd_cntrs_2012 <- agri_prod_dvlpd_cntrs[agri_prod_dvlpd_cntrs$year_of_value %in% c("2012"), ]
print(agri_prod_dvlpd_cntrs_2012)
agri_prod_dvlpng_cntrs_2012 <- agri_prod_dvlpng_cntrs[agri_prod_dvlpng_cntrs$year_of_value %in% c("2012"), ]
print(agri_prod_dvlpng_cntrs_2012)

#in-order to carry out the hypothesis testing, we need to first find out whether the data is normally distributed or not.
#feature1: "cer_yld"
#Test - 1 :- Q-Q Plot:- visual method
#If the points in the plot roughly fall along a straight diagonal line, then the data is assumed to be normally distributed.

ggplot(mapping = aes(sample = agri_prod_dvlpd_cntrs_2021$cer_yld)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developed Countries 2021") + 
  ylab("Sample values for Developed Countries 2021")
#Its clearly visible that the data is normally distributed here.
ggplot(mapping = aes(sample = agri_prod_dvlpng_cntrs_2021$cer_yld)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developing Countries 2021") + 
  ylab("Sample values for Developing Countries 2021")

ggplot(mapping = aes(sample = agri_prod_dvlpd_cntrs_2012$cer_yld)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developed Countries 2012") + 
  ylab("Sample values for Developed Countries 2012")

#Its clearly visible that the data is normally distributed here.
ggplot(mapping = aes(sample = agri_prod_dvlpng_cntrs_2012$cer_yld)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developing Countries 2012") + 
  ylab("Sample values for Developing Countries 2012")

#Test - 2 :- Shapiro-Wilk Test (Statistical Test)
#If the p-value of the test is greater than α = 0.05, then the data is assumed to be normally distributed
print("Agricultural Production Data in Developed Countries 2021 for the feature Cereal Yiled ('cer_yld')")
shapiro.test(agri_prod_dvlpd_cntrs_2021$cer_yld)
shapiro.test(agri_prod_dvlpng_cntrs_2021$cer_yld)
#P value = 0.2403 for developed and 0.7904 for developing countries, which is clearly less than α = 0.05, hence confirmed the feature is normally distributed.

shapiro.test(agri_prod_dvlpd_cntrs_2012$cer_yld)
shapiro.test(agri_prod_dvlpng_cntrs_2012$cer_yld)
#P value = 0.7356 for developed and 0.7007 for developing countries, which is clearly less than α = 0.05, hence confirmed the feature is normally distributed.

#feature2: "crp_prod_indx"
#Test - 1 :- Q-Q Plot:- visual method
#If the points in the plot roughly fall along a straight diagonal line, then the data is assumed to be normally distributed.

ggplot(mapping = aes(sample = agri_prod_dvlpd_cntrs_2021$crp_prod_indx)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developed Countries 2021") + 
  ylab("Sample values for Developed Countries 2021")

ggplot(mapping = aes(sample = agri_prod_dvlpng_cntrs_2021$crp_prod_indx)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developing Countries 2021") + 
  ylab("Sample values for Developing Countries 2021")

ggplot(mapping = aes(sample = agri_prod_dvlpd_cntrs_2012$crp_prod_indx)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developed Countries 2012") + 
  ylab("Sample values for Developed Countries 2012")

ggplot(mapping = aes(sample = agri_prod_dvlpng_cntrs_2012$crp_prod_indx)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="maroon") +
  xlab("Theoretical values for Developing Countries 2012") + 
  ylab("Sample values for Developing Countries 2012")

#Its clearly visible that the data is normally distributed here.

#Test - 2 :- Shapiro-Wilk Test (Statistical Test)
#If the p-value of the test is greater than α = 0.05, then the data is assumed to be normally distributed
shapiro.test(agri_prod_dvlpd_cntrs_2021$crp_prod_indx)
shapiro.test(agri_prod_dvlpng_cntrs_2021$crp_prod_indx)
#P value = 0.05217 for developed and 0.6138 for developing countries, which is clearly less than α = 0.05, hence confirmed the feature is normally distributed.

shapiro.test(agri_prod_dvlpd_cntrs_2012$crp_prod_indx)
shapiro.test(agri_prod_dvlpng_cntrs_2012$crp_prod_indx)
#P value = 0.3109 for developed and 0.7632 for developing countries, which is clearly less than α = 0.05, hence confirmed the feature is normally distributed.

#--> First Research:
#--> Hypothesis Testing Using "Paired Sample T Test"
#--> Null Hypothesis: 2012 production in developed countries is lesser than 2021 in developed countries
#--> Alternate Hypothesis: 2012 production in developed countries is greater than 2021 in developed countries
#lets just plot a boxplot in order to view the data in more visuals
boxplot(agri_prod_dvlpd_cntrs_2012$crp_prod_indx, agri_prod_dvlpd_cntrs_2021$crp_prod_indx, names=c("crp_prod_indx", "crp_prod_indx"),xlab="crp_prod_indx_2012_and_2021", ylab="value_range",main="Crop Production Index of before 10 years and after 10 years")
t.test(agri_prod_dvlpd_cntrs_2012$crp_prod_indx, agri_prod_dvlpd_cntrs_2021$crp_prod_indx, paired=TRUE)
#here, the p value is 0.14, which is > threshold value of p = 0.05. hence the null hypothesis is correct. That is the 2012 production in developed countries is less than 2021 in developed countries

#--> Second Research:
#--> Hypothesis Testing Using "Paired Sample T Test"
#--> Null Hypothesis: 2012 production in developing countries is greater than 2021 in developed countries
#--> Alternate Hypothesis: 2012 production in developing countries is lesser than 2021 in developed countries
#lets just plot a boxplot in order to view the data in more visuals
boxplot(agri_prod_dvlpng_cntrs_2012$crp_prod_indx, agri_prod_dvlpng_cntrs_2021$crp_prod_indx, names=c("crp_prod_indx", "crp_prod_indx"),xlab="crp_prod_indx_2012_and_2021", ylab="value_range",main="Crop Production Index of before 10 years and after 10 years")
t.test(agri_prod_dvlpng_cntrs_2012$crp_prod_indx, agri_prod_dvlpng_cntrs_2021$crp_prod_indx, paired=TRUE)
#here, the p value is 0.0003328, which is < threshold value of p = 0.05. hence the null hypothesis can be ignores, That is the 2012 production in developing countries is less than 2021 in developing countries
