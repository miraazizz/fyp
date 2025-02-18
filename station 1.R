install.packages(lmom)
install.packages(lmomco)
install.packages("nortest")
install.packages("evd") 

library(lmom)
library(lmomco)
library(nortest)
library(evd)

D=read.csv("C:\\Users\\user\\Documents\\station 1.csv")
DD=complete.cases(D)
newdata=na.omit(D)

d=newdata[,2]
data=sort(d)
str(data)

lmr= lmoms(data)  # L-moments of the data
print(lmr)

#ESTIMATE PARAMETER
gev_par= pargev(lmr)
glo_par= parglo(lmr)
gpa_par= pargpa(lmr)
print(gev_par)
print(glo_par)
print(gpa_par)

gev_lmoments = lmomgev(gev_par)
print(gev_lmoments)
gpa_lmoments = lmomgpa(gpa_par)
print(gpa_lmoments)
glo_lmoments = lmomglo(glo_par)
print(glo_lmoments)

#quantile estimate
#quagev(0.9,gev_par)
#quaglo(0.9,glo_par)
#quagpa(0.9,gpa_par)

install.packages("ADGofTest")
library(ADGofTest)

# AD TEST AND KOLMOGOROV SMIRNOV TEST
# GEV
ad_gev = ad.test(data, function(x) cdfgev(x, gev_par))
print(ad_gev)
ks_gev = ks.test(data, function(x) cdfgev(x, gev_par))
print(ks_gev)

#GPA
ad_gpa = ad.test(data, function(x) cdfgpa(x, gpa_par))
print(ad_gpa)
data_unique = unique(data) #bcs data cleaning many repetitive number need to use this one
ks_gpa = ks.test(data_unique, function(x) cdfgpa(x, gpa_par))
#ks_gpa = ks.test(data, function(x) cdfgpa(x, gpa_par))
print(ks_gpa)

#GLO
ad_glo = ad.test(data, function(x) cdfglo(x, glo_par))
print(ad_glo)
ks_glo = ks.test(data, function(x) cdfglo(x,glo_par))
print(ks_glo)


# ACCURACY PERFORMANCE MEASURE
library(lmomco)
install.packages("Metrics") 
library(Metrics)

# plotting position
pp = (rank(data) - 0.44) / (length(data) + 0.12)

gev_pred = quagev(pp, gev_par) 
print(gev_pred)
gpa_pred = quagpa(pp, gpa_par)

print(gpa_pred)
glo_pred = quaglo(pp, glo_par)
print(glo_pred)

# Calculate Euclidean distance function
calculate_euclidean_distance = function(observed, theoretical) {
  sqrt(sum((observed - theoretical)^2))
}

# Extract the 3rd (tau3) and 4th (tau4) L-moment ratios for comparison
observed_ratios = c(tau3 = lmr$ratios[3], tau4 = lmr$ratios[4])
theoretical_ratios_gev = c(tau3 = gev_lmoments$ratios[3], tau4 = gev_lmoments$ratios[4])
theoretical_ratios_gpa = c(tau3 = gpa_lmoments$ratios[3], tau4 = gpa_lmoments$ratios[4])
theoretical_ratios_glo = c(tau3 = glo_lmoments$ratios[3], tau4 = glo_lmoments$ratios[4])

# Calculate Euclidean distances
distance_gev = calculate_euclidean_distance(observed_ratios, theoretical_ratios_gev)
distance_gpa = calculate_euclidean_distance(observed_ratios, theoretical_ratios_gpa)
distance_glo = calculate_euclidean_distance(observed_ratios, theoretical_ratios_glo)

# Print results
cat("Euclidean Distance (GEV):", distance_gev, "\n")
cat("Euclidean Distance (GPA):", distance_gpa, "\n")
cat("Euclidean Distance (GLO):", distance_glo, "\n")

#plot(pp, observed, type = "p", pch = 20, col = "black",
    # xlab = "Gringorten Plotting Position", ylab = "Peakflow Reading",
    # main = "Peakflow Reading vs Gringorten Plotting Position")

#points(pp, gev_pred, col = "blue", lty = 1, lwd = 2,pch=20)  # GEV predictions (line)
#points(pp, gpa_pred, col = "red", lty = 2, lwd = 2, pch=20 )   # GPA predictions (line)
#points(pp, glo_pred, col = "green", lty = 3, lwd = 2, pch=20) # GLO predictions (line)

# Adding a legend
#legend("topleft", inset = 0.02, 
      # legend = c("Observed", "GEV", "GPA", "GLO"),
       #col = c("black", "blue", "red", "green"),
       #pch = c(20, 20, 20, 20),  # Point only for observed
       #bty = "n",  # No legend box
       #cex = 0.9)  # Adjust text size for the legend


####################################################################
# GEV
# MAPE (Mean Absolute Percentage Error)
mape_gev = mape(observed, gev_pred)

# MAE (Mean Absolute Error)
mae_gev = mae(observed, gev_pred)

# RMSE (Root Mean Square Error)
rmse_gev = rmse(observed, gev_pred)

# RMSPE (Root Mean Square Percentage Error)
rmspe = function(observed, predicted) {
  return(sqrt(mean(((observed - predicted) / observed)^2)) * 100)
}
rmspe_gev = rmspe(observed, gev_pred)

# Coefficient of Determination (R²)
r2_gev = cor(observed, gev_pred)^2


#GEV results
#("GEV Metrics:\n")
cat("MAPE for GEV:", mape_gev, "\n")
cat("MAE for GEV:", mae_gev, "\n")
cat("RMSE for GEV:", rmse_gev, "\n")
cat("RMSPE for GEV:", rmspe_gev, "\n")
cat("R² for GEV:", r2_gev, "\n")

##########################################################
# GPA

# MAPE (Mean Absolute Percentage Error)
mape_gpa = mape(observed, gpa_pred)

# MAE (Mean Absolute Error)
mae_gpa = mae(observed, gpa_pred)

# RMSE (Root Mean Square Error)
rmse_gpa = rmse(observed, gpa_pred)

# RMSPE (Root Mean Square Percentage Error)
rmspe <- function(observed, predicted) {
  return(sqrt(mean(((observed - predicted) / observed)^2)) * 100)
}
rmspe_gpa = rmspe(observed, gpa_pred)

# Coefficient of Determination (R²)
r2_gpa = cor(observed, gpa_pred)^2

#GPA results
cat("MAPE for GPA:", mape_gpa, "\n")
cat("MAE for GPA:", mae_gpa, "\n")
cat("RMSE for GPA:", rmse_gpa, "\n")
cat("RMSPE for GPA:", rmspe_gpa, "\n")
cat("R² for GPA:", r2_gpa, "\n")

###################################################################
# GLO

# MAPE (Mean Absolute Percentage Error)
mape_glo = mape(observed, glo_pred)

# MAE (Mean Absolute Error)
mae_glo = mae(observed, glo_pred)

# RMSE (Root Mean Square Error)
rmse_glo = rmse(observed, glo_pred)

# RMSPE (Root Mean Square Percentage Error)
rmspe_glo = rmspe(observed, glo_pred)

# Coefficient of Determination (R²)
r2_glo = cor(observed, glo_pred)^2

#GLO results
cat("MAPE for GLO:", mape_glo, "\n")
cat("MAE for GLO:", mae_glo, "\n")
cat("RMSE for GLO:", rmse_glo, "\n")
cat("RMSPE for GLO:", rmspe_glo, "\n")
cat("R² for GLO:", r2_glo, "\n")

# ranking

