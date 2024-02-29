#Do regression analysis. Explain why the selected regression techniques are appropriate for the selected variables and defined objectives and show if you’ve found any similar research in the literature
install.packages("car")
install.packages("corrplot")
install.packages("caret")
library(car)
library(corrplot)
library(caret)

#SIMPLE LINEAR REGRESSSION ANALYSIS:

#check the variables and data types of our dataset
str(Agri_resources_data)
#reducing the dataset into a new dataframe which only consists of the numerical datatypes
Agri_resources_data_num <- Agri_resources_data[ ,c('cap_fish_prod', 'cer_prod','Aqua_cult_prod','cer_yld','fert_cnsmtn','crp_prod_indx','fd_prod_indx','lvstk_prod_indx', 'lnd_und_crl_prod')]
print(Agri_resources_data_num)
#applying correlation analysis:
cor(Agri_resources_data_num)
#plotting correlation matrix for a better visual analysis
corrplot(cor(Agri_resources_data_num))
#from the corrplot, we get to know the following analysis:
#case 1 : --> the correlation between Aqua_cult_prod and cap_fish_prod is high (0.917892512).
#case 2 : --> the correlation between lnd_und_crl_prod and cer_prod is high (0.9104541843).
#Lets consider both the cases, hence performing SLR in 

#case 1 using forward step wise method
regression_model <- lm(Aqua_cult_prod ~ cap_fish_prod, Agri_resources_data_num)
summary.lm(regression_model)
#To Visualize the regression mode:
plot(Aqua_cult_prod ~ cap_fish_prod, Agri_resources_data_num,col="Maroon",main = "SLR Plot for Aqua Culture Production VS Capital Fish Production in the Years between 2012 and 2021",xlab = "Total Production in Aquaculture",ylab = "Influence of it in Capital Fish Production")
#adding the regression line to the plot:
abline(regression_model, col="red")

#Now, finally, running the assumptions whether to check the regression model meets the criterions satisfying the SLR methodology
#Assumption1: Linearity
ggplot(regression_model, aes(x = Aqua_cult_prod, y = cap_fish_prod)) +
  geom_point() +
  labs(x = "Total Production in Aquaculture", y = "Capital Fish Production", title = "Scatterplot of influence on Aqua Culture production over the Capital Fish production in the years between 2012 and 2021")
#The results are linear

plot(regression_model, 1)
plot(regression_model, 2)
plot(regression_model, 3)


#case 2 using forward step wise method
regression_model_2 <- lm(cer_prod ~ lnd_und_crl_prod, Agri_resources_data_num)
summary.lm(regression_model_2)
#To Visualize the regression mode:
plot(cer_prod ~ lnd_und_crl_prod, Agri_resources_data_num,col="blue",main = "SLR Plot for Cereal Production VS Land Under Cereal Production in the Years between 2012 and 2021",xlab = "Total Production in Cereal Category",ylab = "Land Under Usage for Cereal Production")
#adding the regression line to the plot:
abline(regression_model_2, col="red")

#Now, finally, running the assumptions whether to check the regression model meets the criterions satisfying the SLR methodology
#Assumption1: Linearity
ggplot(regression_model_2, aes(x = cer_prod, y = lnd_und_crl_prod)) +
  geom_point() +
  labs(x = "Total Production in Cereal Category", y = "Land Under usage", title = "Scatterplot showing the land under cereal harvesting to the actual cereal prodcution for the years between 2012 to 2021")
#The results are linear

plot(regression_model_2, 1)
plot(regression_model_2, 2)
plot(regression_model_2, 3)
