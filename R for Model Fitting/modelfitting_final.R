library(readxl)
library(mfGARCH)
library(readr)
library(DBI)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)


### Reading in data
### Train
train <- read_excel('allData090324.xlsx', sheet='train') 


### Changing date dtype
train$date <- as.Date(train$date, format='Y%m%d') 
train$year_month <- as.Date(train$year_month, format='Y%m%d') 



### Changing indicies dtype
numeric_indices <- c(3,4,5,6,7,8,9,10,11,12)
train[, numeric_indices] <- lapply(train[, numeric_indices], as.numeric)


### fixing random bug 
train$ssec_RV[1] <- train$ssec_RV[2]


### Scaling Indexes by 100 as per mf_garch suggestion
train$CU <- train$CU *100
train$CEU <- train$CEU *100
train$UCPU <- train$UCPU *100
train$CEPU <- train$CEPU *100



##### Model Fitting Begins Here #####


### Manually set K (2 - 15)
K = 15



### Fit Models using Train Data
print('========================= CSI 300 with CU =========================')
model_csi_rv_cu <- fit_mfgarch(data = train, y="csi_lnret100", x="CU", low.freq ="year_month", 
                               K=K, weighting="beta.restricted", gamma=TRUE, x.two="csi_RV", K.two=K, low.freq.two = 'year_month')
#model_csi_rv_cu
print('===================================================================')


print('========================= CSI 300 with CEU =========================')
model_csi_rv_ceu <- fit_mfgarch(data = train, y="csi_lnret100", x="CEU", low.freq ="year_month", 
                                K=K, weighting="beta.restricted", gamma=TRUE, x.two="csi_RV", K.two=K, low.freq.two = 'year_month')
#model_csi_rv_ceu
print('====================================================================')


print('========================= CSI 300 with CEPU =========================')
model_csi_rv_cepu <- fit_mfgarch(data = train, y="csi_lnret100", x="CEPU", low.freq ="year_month", 
                                 K=K, weighting="beta.restricted", gamma=TRUE, x.two="csi_RV", K.two=K, low.freq.two = 'year_month')
#model_csi_rv_cepu
print('=====================================================================')


print('========================= CSI 300 with UCPU =========================')
model_csi_rv_ucpu <- fit_mfgarch(data = train, y="csi_lnret100", x="UCPU", low.freq ="year_month", 
                                 K=K, weighting="beta.restricted", gamma=TRUE, x.two="csi_RV", K.two=K, low.freq.two = 'year_month')
#model_csi_rv_ucpu
print('====================================================================')


print('========================= SSEC with CU =========================')
model_ssec_rv_cu <- fit_mfgarch(data = train, y="ssec_lnret100", x="CU", low.freq ="year_month", 
                                K=K, weighting="beta.restricted", gamma=TRUE, x.two="ssec_RV", K.two=K, low.freq.two = 'year_month')
#model_ssec_rv_cu
print('================================================================')


print('========================= SSEC with CEU =========================')
model_ssec_rv_ceu <- fit_mfgarch(data = train, y="ssec_lnret100", x="CEU", low.freq ="year_month", 
                                 K=K, weighting="beta.restricted", gamma=TRUE, x.two="ssec_RV", K.two=K, low.freq.two = 'year_month')
#model_ssec_rv_ceu
print('=================================================================')


print('========================= SSEC with CEPU =========================')
model_ssec_rv_cepu <- fit_mfgarch(data = train, y="ssec_lnret100", x="CEPU", low.freq ="year_month", 
                                  K=K, weighting="beta.restricted", gamma=TRUE, x.two="ssec_RV", K.two=K, low.freq.two = 'year_month')
#model_ssec_rv_cepu
print('==================================================================')


print('========================= SSEC with UCPU =========================')
model_ssec_rv_ucpu <- fit_mfgarch(data = train, y="ssec_lnret100", x="UCPU", low.freq ="year_month", 
                                  K=K, weighting="beta.restricted", gamma=TRUE, x.two="ssec_RV", K.two=K, low.freq.two = 'year_month')
#model_ssec_rv_ucpu
print('=================================================================')


print('========================= CSI with RV =========================')
model_csi_rv <- fit_mfgarch(data=train, y="csi_lnret100", x="csi_RV", low.freq = "year_month", weighting = "beta.restricted", gamma=TRUE, K=K)
#model_csi_rv
print('=================================================================')

print('========================= SSEC with RV =========================')
model_ssec_rv <- fit_mfgarch(data=train, y="ssec_lnret100", x="ssec_RV", low.freq = "year_month", weighting = "beta.restricted", gamma=TRUE, K=K)
#model_ssec_rv
print('=================================================================')

### Printing out each model's parameters at this lag
#model_csi_rv_cu[["par"]]
#model_csi_rv_ceu[["par"]]
#model_csi_rv_cepu[["par"]]
#model_csi_rv_ucpu[["par"]]
#model_ssec_rv_cu[["par"]]
#model_ssec_rv_ceu[["par"]]
#model_ssec_rv_cepu[["par"]]
#model_ssec_rv_ucpu[["par"]]
#model_csi_rv[["par"]]
#model_ssec_rv[["par"]]

### Printing out BIC
model_csi_rv_cu[["llh"]]
model_csi_rv_ceu[["llh"]]
model_csi_rv_cepu[["llh"]]
model_csi_rv_ucpu[["llh"]]
model_ssec_rv_cu[["llh"]]
model_ssec_rv_ceu[["llh"]]
model_ssec_rv_cepu[["llh"]]
model_ssec_rv_ucpu[["llh"]]
model_csi_rv[["llh"]]
model_ssec_rv[["llh"]]
#
#
### Printing out BIC
model_csi_rv_cu[["bic"]]
model_csi_rv_ceu[["bic"]]
model_csi_rv_cepu[["bic"]]
model_csi_rv_ucpu[["bic"]]
model_ssec_rv_cu[["bic"]]
model_ssec_rv_ceu[["bic"]]
model_ssec_rv_cepu[["bic"]]
model_ssec_rv_ucpu[["bic"]]
model_csi_rv[["bic"]]
model_ssec_rv[["bic"]]

### Parameters saved to excel manually

### Manually update K at ln[43] for next lag

