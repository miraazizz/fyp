#STATION 1 
# Load required library
library(lmomco)

# Define parameters for each distribution
gev_para = vec2par(c(3.08395543, 2.41087887, 0.04542615), type = "gev")  # GEV: Location, Scale, Shape
glo_para = vec2par(c(0.34982765, 6.0547059, 0.5055058), type = "glo")   # GLO: Location, Scale, Shape
gpa_para = vec2par(c(4.0027214, 1.5531227, -0.1410622), type = "gpa")  # GPA: Location, Scale, Shape

# Define return periods
return_periods = c(10, 50, 100)

# Convert return periods to cumulative probabilities
probabilities = 1 - (1 / return_periods)

# Calculate quantiles for each distribution
gev_quantiles = sapply(probabilities, function(p) quagev(p, gev_para))
glo_quantiles = sapply(probabilities, function(p) quaglo(p, glo_para))
gpa_quantiles = sapply(probabilities, function(p) quagpa(p, gpa_para))

# Combine results into a data frame
quantile_results = data.frame(
  Return_Period = return_periods,
  Probability = probabilities,
  GEV = gev_quantiles,
  GPA = gpa_quantiles,
  GLO = glo_quantiles
)

# Print the results
print(quantile_results)





















#STATION 2 
# Define parameters for each distribution
gev_para1 = vec2par(c(9.281621938, 4.210972481, 0.008529011), type = "gev")  # GEV: Location, Scale, Shape
glo_para1 = vec2par(c(10.9039578,  2.7689600, -0.1644555), type = "glo")   # GLO: Location, Scale, Shape
gpa_para1 = vec2par(c( 4.624789, 10.120521,  0.435082), type = "gpa")  # GPA: Location, Scale, Shape

# Define return periods
return_periods1 = c(10, 50, 100)

# Convert return periods to cumulative probabilities
probabilities1 = 1 - (1 / return_periods1)

# Calculate quantiles for each distribution
gev_quantiles1 = sapply(probabilities, function(p) quagev(p, gev_para1))
glo_quantiles1 = sapply(probabilities, function(p) quaglo(p, glo_para1))
gpa_quantiles1 = sapply(probabilities, function(p) quagpa(p, gpa_para1))

# Combine results into a data frame
quantile_result1 = data.frame(
  Return_Period1 = return_periods1,
  Probability1 = probabilities1,
  GEV1 = gev_quantiles1,
  GLO1 = glo_quantiles1,
  GPA1 = gpa_quantiles1
)

# Print the results
print(quantile_result1)

#STATION 3 
# Define parameters for each distribution
gev_para2 = vec2par(c( 267.4724445, 1182.7803467, -0.8984644 ), type = "gev")  # GEV: Location, Scale, Shape
glo_para2 = vec2par(c( 744.9103277, 1211.0531567, -0.8962669), type = "glo")   # GLO: Location, Scale, Shape
gpa_para2 = vec2par(c( -538.8723745, 1292.8163885, -0.8905923), type = "gpa")  # GPA: Location, Scale, Shape

# Define return periods
return_periods2 = c(10, 50, 100)

# Convert return periods to cumulative probabilities
probabilities2 = 1 - (1 / return_periods2)

# Calculate quantiles for each distribution
gev_quantiles2 = sapply(probabilities, function(p) quagev(p, gev_para2))
glo_quantiles2 = sapply(probabilities, function(p) quaglo(p, glo_para2))
gpa_quantiles2 = sapply(probabilities, function(p) quagpa(p, gpa_para2))

# Combine results into a data frame
quantile_result2 = data.frame(
  Return_Period2 = return_periods2,
  Probability2 = probabilities2,
  GEV2 = gev_quantiles2,
  GLO2 = glo_quantiles2,
  GPA2 = gpa_quantiles2
)

# Print the results
print(quantile_result2)

