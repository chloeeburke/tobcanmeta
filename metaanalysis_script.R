## SCRIPT DETAILS:

# Script run in R version 4.4.1 (2024-06-14 ucrt) -- "Race for Your Life"
# Contains script for main meta-analysis, sensitivity analysis using unadjusted estimates, subgroup analysis, meta-regression and Doi plots for publication bias tests
# Updated by Chloe Burke on 25/09/2023 to reflect updates in packages (e.g., meta) which resulted in changes in some functions (e.g., update.meta --> update)
# If encounter any issues with running, see guidance on installing earlier packages

# Install and load R packages
install.packages(c("metafor", "meta", "dplyr", "gridExtra", "grid", "metasens"))
if (!require("remotes")) {
  install.packages("remotes")
}
remotes::install_github("MathiasHarrer/dmetar")
library(metafor)
library(meta)
library(dplyr)
library(gridExtra)
library(grid)
library(metasens)
library(dmetar)

#--------------------------------------------------------#

################# TOBACCO AND MOOD ######################

#Import data: load file 'tobmod_arr'

#DATA PREPARATION
tobmod_arr <- tobmod_arr %>%
  mutate(
    perc_fem = as.numeric(perc_fem),
    smpsize = as.numeric(smpsize),
    flw_up = as.numeric(flw_up),
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE),
    high_qual = as.character(high_qual))

tobmod_arr <- tobmod_arr %>%
  mutate(
    age_grp = factor(age_grp),
    high_qual = factor(high_qual),
    exp_msr = factor(exp_msr),
    exp_type = factor(exp_type),
    otcm_type = factor(otcm_type),
    pop_type = factor(pop_type),
    otcm_msr = factor(otcm_msr),
    cfdr_ad = factor(cfdr_ad),
    cfdr_couse = factor (cfdr_couse))

#Log transforming study reported effect estimate and generating SEs
tobmod_arr$log_RR <- replmiss(tobmod_arr$log_RR, log(tobmod_arr$effest))
tobmod_arr$SE <- ifelse(is.na(tobmod_arr$SE), (log(tobmod_arr$up_ci) - log(tobmod_arr$low_ci))/3.92, tobmod_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
tobmod_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobmod_arr, sm = "RR",  fixed = FALSE, random = TRUE, method.tau = "PM")
summary(tobmod_ma)

#GENERATING FOREST PLOT
# Set up TIFF device with 800 DPI resolution
tiff("tobmod_fp.tiff", width = 18.3, height = 26.0, units = "cm", res = 800, compression = "lzw")

#Generating forest plot for meta-analysis results
forest(tobmod_ma, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"), fontfamily = "sans", fontsize = 8, col.predict = "darkgrey", col.predict.lines = "black")

# Close the TIFF device
dev.off()

#OUTLIERS
#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
tobmod_out <- find.outliers(tobmod_ma)
summary (tobmod_out)

#SUBGROUP ANALYSES
sg_xtype <- update(tobmod_ma, subgroup = exp_type, tau.common = FALSE)
summary(sg_xtype)
sg_xmes <- update(tobmod_ma, subgroup = exp_msr, tau.common = FALSE)
summary(sg_xmes)
sg_ymes <- update(tobmod_ma, subgroup = otcm_msr, tau.common = FALSE)
summary(sg_ymes)
sg_ytype <- update(tobmod_ma, subgroup = otcm_type, tau.common = FALSE)
summary(sg_ytype)
sg_age <- update(tobmod_ma, subgroup = age_grp, tau.common = FALSE)
summary(sg_age)
sg_qual <- update(tobmod_ma, subgroup = high_qual, tau.common = FALSE)
summary(sg_qual)
sg_cfad <- update(tobmod_ma, subgroup = cfdr_ad, tau.common = FALSE)
summary(sg_cfad)
sg_cfcu <- update(tobmod_ma, subgroup = cfdr_couse, tau.common = FALSE)
summary(sg_cfcu)

#META-REGRESSION
#Not run for % female due to high missing data and typically only reported for whole sample (i.e. may not represent exposure level used in main meta-analysis)
metareg(tobmod_ma, ~flw_up)
metareg(tobmod_ma, ~smpsize)

#META-BIASES
#LFK and Doi plot
lfk.tobmod <- lfkindex(tobmod_arr$log_RR, tobmod_arr$SE, data = tobmod_arr)
doiplot(lfk.tobmod, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import data: load file 'tobmod_crude'
#Preparing data
tobmod_crude <- tobmod_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
tobmod_crude$log_RR <- replmiss(tobmod_crude$log_RR, log(tobmod_crude$effest))
tobmod_crude$SE <- ifelse(is.na(tobmod_crude$SE), (log(tobmod_crude$up_ci) - log(tobmod_crude$low_ci))/3.92, tobmod_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
tobmod_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobmod_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(tobmod_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(tobmod_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

################# TOBACCO AND ANXIETY ######################

#Import data: load file 'tobanx_arr'

#DATA PREPARATION
tobanx_arr <- tobanx_arr %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
tobanx_arr$log_RR <- replmiss(tobanx_arr$log_RR, log(tobanx_arr$effest))
tobanx_arr$SE <- ifelse(is.na(tobanx_arr$SE), (log(tobanx_arr$up_ci) - log(tobanx_arr$low_ci))/3.92, tobanx_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
tobanx_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobanx_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(tobanx_ma)

#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
tobanx_out <- find.outliers(tobanx_ma)
summary (tobanx_out)

#META-BIASES
#LFK and Doi plots
lfk.tobanx <- lfkindex(tobanx_arr$log_RR, tobanx_arr$SE, data = tobanx_arr)
doiplot(lfk.tobanx, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import file 'tobanx_crude'

#Preparing data
tobanx_crude <- tobanx_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
tobanx_crude$log_RR <- replmiss(tobanx_crude$log_RR, log(tobanx_crude$effest))
tobanx_crude$SE <- ifelse(is.na(tobanx_crude$SE), (log(tobanx_crude$up_ci) - log(tobanx_crude$low_ci))/3.92, tobanx_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
tobanx_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobanx_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(tobanx_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(tobanx_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

################# TOBACCO AND PSYCHOSIS ######################

#Import data: loaf file 'tobpsy_arr'

#DATA PREPARATION
tobpsy_arr <- tobpsy_arr %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
tobpsy_arr$log_RR <- replmiss(tobpsy_arr$log_RR, log(tobpsy_arr$effest))
tobpsy_arr$SE <- ifelse(is.na(tobpsy_arr$SE), (log(tobpsy_arr$up_ci) - log(tobpsy_arr$low_ci))/3.92, tobpsy_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
tobpsy_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobpsy_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(tobpsy_ma)

#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
tobpsy_out <- find.outliers(tobpsy_ma)
summary (tobpsy_out)

#Results without Zammit 2003
tobpsy_outremove <- metagen(TE = log_RR, seTE = SE, studlab = ID, subset = !(ID %in% c("Kendler, 2015", "King, 2020", "Mustonen, 2018", "Weiser, 2004")), exclude = grep ("Zammit", ID), data = tobpsy_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary (tobpsy_outremove)

#META-BIASES
#LFK and Doi plot
lfk.tobpsy <- lfkindex(tobpsy_arr$log_RR, tobpsy_arr$SE, data = tobpsy_arr)
doiplot(lfk.tobpsy, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import data: load file 'tobpsy_crude'

#Preparing data
tobpsy_crude <- tobpsy_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
tobpsy_crude$log_RR <- replmiss(tobpsy_crude$log_RR, log(tobpsy_crude$effest))
tobpsy_crude$SE <- ifelse(is.na(tobpsy_crude$SE), (log(tobpsy_crude$up_ci) - log(tobpsy_crude$low_ci))/3.92, tobpsy_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
tobpsy_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = tobpsy_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(tobpsy_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(tobpsy_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

#################  COMBINING PLOTS  ######################

# Set up TIFF device with 800 DPI resolution
tiff("tobmix_fp.tiff", width = 18.3, height = 26.0, units = "cm", res = 800, compression = "lzw")

# Define a blank plot as a spacer (to add height to top of plot)
blank_plot <- grid.grabExpr({
  grid.rect(gp = gpar(col = "white"))  # A blank rectangle to create space
})

# Generate the first forest plot and capture it as a grob
plot1 <- grid.grabExpr({
  forest(tobanx_ma, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Tobacco and Anxiety Disorders", x = 0.2, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Repeat for the second plot
plot2 <- grid.grabExpr({
  forest(tobpsy_ma, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Tobacco and Psychotic Disorders", x = 0.205, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Repeat for the third plot
plot3 <- grid.grabExpr({
  forest(tobpsy_outremove, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Tobacco and Psychotic Disorders (Outlier Removed)", x = 0.28, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Combine the plots and the blank space into one display using grid.arrange
grid.arrange(blank_plot, plot1, plot2, plot3, nrow = 4, heights = c(0.5, 3.6, 3.2, 3.2))

# Close the TIFF device
dev.off()

################# CANNABIS AND MOOD ######################

#Import data: load file 'canmod_arr'

#DATA PREPARATION
canmod_arr <- canmod_arr %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
canmod_arr$log_RR <- replmiss(canmod_arr$log_RR, log(canmod_arr$effest))
canmod_arr$SE <- ifelse(is.na(canmod_arr$SE), (log(canmod_arr$up_ci) - log(canmod_arr$low_ci))/3.92, canmod_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
canmod_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = canmod_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(canmod_ma)

#OUTLIERS
#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
canmod_out <- find.outliers(canmod_ma)
summary (canmod_out)

#META-BIASES
#LFK and Doi plots
lfk.canmod <- lfkindex(canmod_arr$log_RR, canmod_arr$SE, data = canmod_arr)
doiplot(lfk.canmod, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import data: load file 'canmod_crude'

canmod_crude <- canmod_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
canmod_crude$log_RR <- replmiss(canmod_crude$log_RR, log(canmod_crude$effest))
canmod_crude$SE <- ifelse(is.na(canmod_crude$SE), (log(canmod_crude$up_ci) - log(canmod_crude$low_ci))/3.92, canmod_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
canmod_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = canmod_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(canmod_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(canmod_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

################# CANNABIS AND ANXIETY ######################

#Import data: load file 'cananx_arr'

#DATA PREPARATION
cananx_arr <- cananx_arr %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
cananx_arr$log_RR <- replmiss(cananx_arr$log_RR, log(cananx_arr$effest))
cananx_arr$SE <- ifelse(is.na(cananx_arr$SE), (log(cananx_arr$up_ci) - log(cananx_arr$low_ci))/3.92, cananx_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
cananx_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = cananx_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(cananx_ma)

#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
cananx_out <- find.outliers(cananx_ma)
summary (cananx_out)

#META-BIASES
#LFK and Doi plots
lfk.cananx <- lfkindex(cananx_arr$log_RR, cananx_arr$SE, data = cananx_arr)
doiplot(lfk.cananx, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import data: load file 'cananx_crude'

cananx_crude <- cananx_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
cananx_crude$log_RR <- replmiss(cananx_crude$log_RR, log(cananx_crude$effest))
cananx_crude$SE <- ifelse(is.na(cananx_crude$SE), (log(cananx_crude$up_ci) - log(cananx_crude$low_ci))/3.92, cananx_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
cananx_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = cananx_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(cananx_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(cananx_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

################# CANNABIS AND PSYCHOSIS ######################

#Import data: load file 'canpsy_arr'

#DATA PREPARATION
canpsy_arr <- canpsy_arr %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
canpsy_arr$log_RR <- replmiss(canpsy_arr$log_RR, log(canpsy_arr$effest))
canpsy_arr$SE <- ifelse(is.na(canpsy_arr$SE), (log(canpsy_arr$up_ci) - log(canpsy_arr$low_ci))/3.92, canpsy_arr$SE)

#META-ANALYSIS
#Running random-effects meta-analysis using generic inverse variance method
canpsy_ma <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = canpsy_arr, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(canpsy_ma)

#Finding outliers based on studies for which upper bound of 95% CI is lower than lower bound  pooled effect confidence interval, or lower bound of 95% CI is higher than upper bound of pooled effect.
canpsy_out <- find.outliers(canpsy_ma)
summary (canpsy_out)

#META-BIASES
#LFK and Doi plots
lfk.canpsy <- lfkindex(canpsy_arr$log_RR, canpsy_arr$SE, data = canpsy_arr)
doiplot(lfk.canpsy, xlab = "ln(RR)", pos.lfkindex = 9)

#UNADJUSTED SENSITIVITY ANALYSIS
#Import data: load file 'canpsy_crude'

canpsy_crude <- canpsy_crude %>%
  mutate(
    effest = as.numeric(effest),
    low_ci = as.numeric(low_ci),
    up_ci = as.numeric(up_ci),
    log_RR = as.numeric(log_RR),
    SE = as.numeric(SE))

#Log transforming study reported effect estimate and generating SEs
canpsy_crude$log_RR <- replmiss(canpsy_crude$log_RR, log(canpsy_crude$effest))
canpsy_crude$SE <- ifelse(is.na(canpsy_crude$SE), (log(canpsy_crude$up_ci) - log(canpsy_crude$low_ci))/3.92, canpsy_crude$SE)

#Running random-effects meta-analysis using generic inverse variance method
canpsy_sen <- metagen(TE = log_RR, seTE = SE, studlab = ID, data = canpsy_crude, sm = "RR", method.tau = "PM", fixed = FALSE, random = TRUE)
summary(canpsy_sen)

#Generating forest plot for unadjusted meta-analysis results
forest(canpsy_sen, studlab = TRUE, prediction = TRUE, print.tau2 = TRUE, xlim = c(0.1, 10), leftlabs = c("ID", "log_RR", "SE"))

#################  COMBINING PLOTS  ######################

# Set up TIFF device with 800 DPI resolution
tiff("canmix_fp.tiff", width = 18.3, height = 26.0, units = "cm", res = 800, compression = "lzw")

# Define a blank plot as a spacer
blank_plot <- grid.grabExpr({
  grid.rect(gp = gpar(col = "white"))  # A blank rectangle to create space
})

# Generate the first forest plot and capture it as a grob
plot1 <- grid.grabExpr({
  forest(canmod_ma, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Cannabis and Mood Disorders", x = 0.2, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Repeat for the second plot
plot2 <- grid.grabExpr({
  forest(cananx_ma, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Cannabis and Anxiety Disorders", x = 0.208, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Repeat for the third plot
plot3 <- grid.grabExpr({
  forest(canpsy_ma, 
         studlab = TRUE, 
         prediction = TRUE, 
         print.tau2 = TRUE, 
         xlim = c(0.1, 10), 
         leftlabs = c("ID", "log_RR", "SE"), 
         fontfamily = "sans", 
         fontsize = 8, 
         col.predict = "darkgrey", 
         col.predict.lines = "black")
  grid.text("Cannabis and Psychotic Disorders", x = 0.213, y = 0.95, 
            gp = gpar(fontsize = 8, fontface = "bold", fontfamily = "sans"))
})

# Combine the plots and the blank space into one display using grid.arrange
grid.arrange(blank_plot, plot1, plot2, plot3, nrow = 4, heights = c(0.5, 4, 4, 3.25))

# Close the TIFF device
dev.off()
