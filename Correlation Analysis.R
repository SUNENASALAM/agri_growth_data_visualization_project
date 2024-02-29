#correlation analysis
agri_prod_exc_cntry_nd_yr <- Agri_resources_data %>% select(tot_fish_prod, lnd_und_crl_prod, lvstk_prod_indx, fd_prod_indx, crp_prod_indx, fert_cnsmtn, cer_yld, Aqua_cult_prod, cer_prod, cap_fish_prod) 
head(agri_prod_exc_cntry_nd_yr, 10)
#Spearman correlation matrix for a mix of continuous and discrete variables is:
round(cor(agri_prod_exc_cntry_nd_yr, method = "spearman"), digit=2)
#We can also plot a correlation analysis plot by using the below codeline:
#This version of the correlation matrix presents the correlation coefficients in a slightly more readable way, i.e., by coloring the coefficients based on their sign. Applied to our dataset, we have:
# improved correlation matrix
library(corrplot)

corrplot(cor(agri_prod_exc_cntry_nd_yr),
         method = "number",
         type = "upper" # show only upper side
)

#consider only two features:"Aqua_cult_prod" and "tot_fish_prod"
# Perform Spearman correlation
correlation_spearman <- cor.test(Agri_resources_data$Aqua_cult_prod, Agri_resources_data$tot_fish_prod, method = "spearman")

# Create a scatter plot with smoother line and confidence bands
plot(Agri_resources_data$Aqua_cult_prod, Agri_resources_data$tot_fish_prod, main = "Scatter Plot with Spearman Correlation", 
     xlab = "Aquaculture production", ylab = "Total Fish production")
scatter.smooth(x = Agri_resources_data$Aqua_cult_prod, y = Agri_resources_data$tot_fish_prod, add = TRUE, col = "red")

#-------------------------------------------------------------------------------

# Add correlation information to the plot
text(
  x = min(Agri_resources_data$Aqua_cult_prod),
  y = max(Agri_resources_data$tot_fish_prod),
  labels = paste("Spearman correlation:", round(correlation_spearman$estimate, 2)),
  pos = 4,
  col = "darkgreen"
)
#-------------------------------------------------------------------------------
#consider only two features:"cer_prod" and "lnd_und_crl_prod"
# Perform Spearman correlation
correlation_spearman <- cor.test(Agri_resources_data$cer_prod, Agri_resources_data$lnd_und_crl_prod, method = "spearman")

# Create a scatter plot with smoother line and confidence bands
plot(Agri_resources_data$cer_prod, Agri_resources_data$lnd_und_crl_prod, main = "Scatter Plot with Spearman Correlation", 
     xlab = "Cereal production", ylab = "Land Under cereal production")
scatter.smooth(x = Agri_resources_data$cer_prod, y = Agri_resources_data$lnd_und_crl_prod, add = TRUE, col = "red")

# Add correlation information to the plot
text(
  x = min(Agri_resources_data$cer_prod),
  y = max(Agri_resources_data$lnd_und_crl_prod),
  labels = paste("Spearman correlation:", round(correlation_spearman$estimate, 2)),
  pos = 4,
  col = "darkgreen"
)
#-------------------------------------------------------------------------------
#correlation analysis between numerical and categorical value: we have country_name as categorical value here, but for changing it in numerical type, then we can analyse the correlation between developing and developed countries.
#hence, changed all developing countries into 0 and all developed countries into 1
Agri_resources_data$devlpng_or_devlpd <- ifelse(Agri_resources_data$country_name %in% c("United Kingdom", "United States", "Saudi Arabia", "Australia", "Netherlands", "Germany"), "1", 
                                                ifelse(Agri_resources_data$country_name %in% c("India", "China", "Sri Lanka", "Indonesia", "Brazil", "Mexico"), "0", "other"))

# Print the updated dataset
print(Agri_resources_data)
# Calculate point-biserial correlation
correlation_point_biserial <- cor.test(Agri_resources_data$cer_prod, as.numeric(Agri_resources_data$devlpng_or_devlpd), method = "pearson")

# Print the result
print(paste("Point-Biserial Correlation:", round(correlation_point_biserial$estimate, 2)))

