install.packages("FeatureHashing")
library(FeatureHashing)
install.packages("Matrix")
library(Matrix)
require(randomForest)
require(caret)
require(dplyr)
require(ggplot2)
library(pROC)
library(stringr)
install.packages("dummies")
library(dummies)
install.packages("Metrics")
library(Metrics)
install.packages("kernlab")
library(kernlab)
install.packages("mlbench")
library(mlbench)
install.packages("summariser")
library(summariser)
setwd("/Users/adithyajob/Desktop/stat project")   
mydata <- read.csv("train.csv")
mydata$paved[mydata$Street == "Pave"] <- 1
mydata$paved[mydata$Street != "Pave"] <- 0

mydata$regshape[mydata$LotShape == "Reg"] <- 1
mydata$regshape[mydata$LotShape != "Reg"] <- 0

mydata$flat[mydata$LandContour == "Lvl"] <- 1
mydata$flat[mydata$LandContour != "Lvl"] <- 0

mydata$pubutil[mydata$Utilities == "AllPub"] <- 1
mydata$pubutil[mydata$Utilities != "AllPub"] <- 0

mydata$gentle_slope[mydata$LandSlope == "Gtl"] <- 1
mydata$gentle_slope[mydata$LandSlope != "Gtl"] <- 0

mydata$culdesac_fr3[mydata$LotConfig %in% c("CulDSac", "FR3")] <- 1
mydata$culdesac_fr3[!mydata$LotConfig %in% c("CulDSac", "FR3")] <- 0

nbhdprice <- summarize(group_by(mydata, Neighborhood),mean(SalePrice, na.rm=T))

nbhdprice_lo <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 140000)
nbhdprice_med <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` < 200000 &
                          nbhdprice$`mean(SalePrice, na.rm = T)` >= 140000 )
nbhdprice_hi <- filter(nbhdprice, nbhdprice$`mean(SalePrice, na.rm = T)` >= 200000)

mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3
mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_lo$Neighborhood] <- 1
mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_med$Neighborhood] <- 2
mydata$nbhd_price_level[mydata$Neighborhood %in% nbhdprice_hi$Neighborhood] <- 3

mydata$pos_features_1[mydata$Condition1 %in% c("PosA", "PosN")] <- 1
mydata$pos_features_1[!mydata$Condition1 %in% c("PosA", "PosN")] <- 0

mydata$pos_features_2[mydata$Condition2 %in% c("PosA", "PosN")] <- 1
mydata$pos_features_2[!mydata$Condition2 %in% c("PosA", "PosN")] <- 0


mydata$twnhs_end_or_1fam[mydata$BldgType %in% c("1Fam", "TwnhsE")] <- 1
mydata$twnhs_end_or_1fam[!mydata$BldgType %in% c("1Fam", "TwnhsE")] <- 0

housestyle_price <- summarize(group_by(mydata, HouseStyle),
                              mean(SalePrice, na.rm=T))

housestyle_lo <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 140000)
housestyle_med <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` < 200000 &
                           housestyle_price$`mean(SalePrice, na.rm = T)` >= 140000 )
housestyle_hi <- filter(housestyle_price, housestyle_price$`mean(SalePrice, na.rm = T)` >= 200000)
mydata$house_style_level[mydata$HouseStyle %in% housestyle_lo$HouseStyle] <- 1
mydata$house_style_level[mydata$HouseStyle %in% housestyle_med$HouseStyle] <- 2
mydata$house_style_level[mydata$HouseStyle %in% housestyle_hi$HouseStyle] <- 3


mydata$roof_hip_shed[mydata$RoofStyle %in% c("Hip", "Shed")] <- 1
mydata$roof_hip_shed[!mydata$RoofStyle %in% c("Hip", "Shed")] <- 0

mydata$roof_matl_hi[mydata$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 1
mydata$roof_matl_hi[!mydata$RoofMatl %in% c("Membran", "WdShake", "WdShngl")] <- 0

price <- summarize(group_by(mydata, Exterior1st),
                   mean(SalePrice, na.rm=T))
matl_lo_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med_1<- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
                      price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi_1 <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)

mydata$exterior_1[mydata$Exterior1st %in% matl_lo_1$Exterior1st] <- 1
mydata$exterior_1[mydata$Exterior1st %in% matl_med_1$Exterior1st] <- 2
mydata$exterior_1[mydata$Exterior1st %in% matl_hi_1$Exterior1st] <- 3
price <- summarize(group_by(mydata, Exterior2nd),
                   mean(SalePrice, na.rm=T))
price
matl_lo <- filter(price, price$`mean(SalePrice, na.rm = T)` < 140000)
matl_med <- filter(price, price$`mean(SalePrice, na.rm = T)` < 200000 &
                     price$`mean(SalePrice, na.rm = T)` >= 140000 )
matl_hi <- filter(price, price$`mean(SalePrice, na.rm = T)` >= 200000)
mydata$exterior_2[mydata$Exterior2nd %in% matl_lo$Exterior2nd] <- 1
mydata$exterior_2[mydata$Exterior2nd %in% matl_med$Exterior2nd] <- 2
mydata$exterior_2[mydata$Exterior2nd %in% matl_hi$Exterior2nd] <- 3


mydata$exterior_mason_1[mydata$MasVnrType %in% c("Stone", "BrkFace") | is.na(mydata$MasVnrType)] <- 1
mydata$exterior_mason_1[!mydata$MasVnrType %in% c("Stone", "BrkFace") & !is.na(mydata$MasVnrType)] <- 0

price <- summarize(group_by(mydata, ExterCond),
                   mean(SalePrice, na.rm=T))
price  # explanation of the ordinal flow
mydata$exterior_cond[mydata$ExterQual == "Ex"] <- 4
mydata$exterior_cond[mydata$ExterQual == "Gd"] <- 3
mydata$exterior_cond[mydata$ExterQual == "TA"] <- 2
mydata$exterior_cond[mydata$ExterQual == "Fa"] <- 1

mydata$exterior_cond2[mydata$ExterCond == "Ex"] <- 5
mydata$exterior_cond2[mydata$ExterCond == "Gd"] <- 4
mydata$exterior_cond2[mydata$ExterCond == "TA"] <- 3
mydata$exterior_cond2[mydata$ExterCond == "Fa"] <- 2
mydata$exterior_cond2[mydata$ExterCond == "Po"] <- 1


mydata$found_concrete[mydata$Foundation == "PConc"] <- 1
mydata$found_concrete[mydata$Foundation != "PConc"] <- 0

price <- summarize(group_by(mydata, BsmtQual),
                   mean(SalePrice, na.rm=T))
price #explanation of the ordinal data
mydata$bsmt_cond1[mydata$BsmtQual == "Ex"] <- 5
mydata$bsmt_cond1[mydata$BsmtQual == "Gd"] <- 4
mydata$bsmt_cond1[mydata$BsmtQual == "TA"] <- 3
mydata$bsmt_cond1[mydata$BsmtQual == "Fa"] <- 2
mydata$bsmt_cond1[is.na(mydata$BsmtQual)] <- 1

mydata$bsmt_cond2[mydata$BsmtCond == "Gd"] <- 5
mydata$bsmt_cond2[mydata$BsmtCond == "TA"] <- 4
mydata$bsmt_cond2[mydata$BsmtCond == "Fa"] <- 3
mydata$bsmt_cond2[is.na(mydata$BsmtCond)] <- 2
mydata$bsmt_cond2[mydata$BsmtCond == "Po"] <- 1


mydata$bsmt_exp[mydata$BsmtExposure == "Gd"] <- 5
mydata$bsmt_exp[mydata$BsmtExposure == "Av"] <- 4
mydata$bsmt_exp[mydata$BsmtExposure == "Mn"] <- 3
mydata$bsmt_exp[mydata$BsmtExposure == "No"] <- 2
mydata$bsmt_exp[is.na(mydata$BsmtExposure)] <- 1


mydata$bsmt_fin1[mydata$BsmtFinType1 == "GLQ"] <- 5
mydata$bsmt_fin1[mydata$BsmtFinType1 == "Unf"] <- 4
mydata$bsmt_fin1[mydata$BsmtFinType1 == "ALQ"] <- 3
mydata$bsmt_fin1[mydata$BsmtFinType1 %in% c("BLQ", "Rec", "LwQ")] <- 2
mydata$bsmt_fin1[is.na(mydata$BsmtFinType1)] <- 1

price <- summarize(group_by(mydata, BsmtFinType2),
                   mean(SalePrice, na.rm=T))
price
mydata$bsmt_fin2[mydata$BsmtFinType2 == "ALQ"] <- 6
mydata$bsmt_fin2[mydata$BsmtFinType2 == "Unf"] <- 5
mydata$bsmt_fin2[mydata$BsmtFinType2 == "GLQ"] <- 4
mydata$bsmt_fin2[mydata$BsmtFinType2 %in% c("Rec", "LwQ")] <- 3
mydata$bsmt_fin2[mydata$BsmtFinType2 == "BLQ"] <- 2
mydata$bsmt_fin2[is.na(mydata$BsmtFinType2)] <- 1

mydata$gasheat[mydata$Heating %in% c("GasA", "GasW")] <- 1
mydata$gasheat[!mydata$Heating %in% c("GasA", "GasW")] <- 0

mydata$heatqual[mydata$HeatingQC == "Ex"] <- 5
mydata$heatqual[mydata$HeatingQC == "Gd"] <- 4
mydata$heatqual[mydata$HeatingQC == "TA"] <- 3
mydata$heatqual[mydata$HeatingQC == "Fa"] <- 2
mydata$heatqual[mydata$HeatingQC == "Po"] <- 1


mydata$air[mydata$CentralAir == "Y"] <- 1
mydata$air[mydata$CentralAir == "N"] <- 0

mydata$standard_electric[mydata$Electrical == "SBrkr" | is.na(mydata$Electrical)] <- 1
mydata$standard_electric[!mydata$Electrical == "SBrkr" & !is.na(mydata$Electrical)] <- 0


mydata$kitchen[mydata$KitchenQual == "Ex"] <- 4
mydata$kitchen[mydata$KitchenQual == "Gd"] <- 3
mydata$kitchen[mydata$KitchenQual == "TA"] <- 2
mydata$kitchen[mydata$KitchenQual == "Fa"] <- 1

mydata$fire[mydata$FireplaceQu == "Ex"] <- 5
mydata$fire[mydata$FireplaceQu == "Gd"] <- 4
mydata$fire[mydata$FireplaceQu == "TA"] <- 3
mydata$fire[mydata$FireplaceQu == "Fa"] <- 2
mydata$fire[mydata$FireplaceQu == "Po" | is.na(mydata$FireplaceQu)] <- 1


mydata$gar_attach[mydata$GarageType %in% c("Attchd", "BuiltIn")] <- 1
mydata$gar_attach[!mydata$GarageType %in% c("Attchd", "BuiltIn")] <- 0


mydata$gar_finish[mydata$GarageFinish %in% c("Fin", "RFn")] <- 1
mydata$gar_finish[!mydata$GarageFinish %in% c("Fin", "RFn")] <- 0

mydata$garqual[mydata$GarageQual == "Ex"] <- 5
mydata$garqual[mydata$GarageQual == "Gd"] <- 4
mydata$garqual[mydata$GarageQual == "TA"] <- 3
mydata$garqual[mydata$GarageQual == "Fa"] <- 2
mydata$garqual[mydata$GarageQual == "Po" | is.na(mydata$GarageQual)] <- 1


mydata$garqual2[mydata$GarageCond == "Ex"] <- 5
mydata$garqual2[mydata$GarageCond == "Gd"] <- 4
mydata$garqual2[mydata$GarageCond == "TA"] <- 3
mydata$garqual2[mydata$GarageCond == "Fa"] <- 2
mydata$garqual2[mydata$GarageCond == "Po" | is.na(mydata$GarageCond)] <- 1


mydata$paved_drive[mydata$PavedDrive == "Y"] <- 1
mydata$paved_drive[!mydata$PavedDrive != "Y"] <- 0
mydata$paved_drive[is.na(mydata$paved_drive)] <- 0

mydata$housefunction[mydata$Functional %in% c("Typ", "Mod")] <- 1
mydata$housefunction[!mydata$Functional %in% c("Typ", "Mod")] <- 0


mydata$pool_good[mydata$PoolQC %in% c("Ex")] <- 1
mydata$pool_good[!mydata$PoolQC %in% c("Ex")] <- 0

mydata$priv_fence[mydata$Fence %in% c("GdPrv")] <- 1
mydata$priv_fence[!mydata$Fence %in% c("GdPrv")] <- 0

mydata$sale_cat[mydata$SaleType %in% c("New", "Con")] <- 5
mydata$sale_cat[mydata$SaleType %in% c("CWD", "ConLI")] <- 4
mydata$sale_cat[mydata$SaleType %in% c("WD")] <- 3
mydata$sale_cat[mydata$SaleType %in% c("COD", "ConLw", "ConLD")] <- 2
mydata$sale_cat[mydata$SaleType %in% c("Oth")] <- 1

mydata$sale_cond[mydata$SaleCondition %in% c("Partial")] <- 4
mydata$sale_cond[mydata$SaleCondition %in% c("Normal", "Alloca")] <- 3
mydata$sale_cond[mydata$SaleCondition %in% c("Family","Abnorml")] <- 2
mydata$sale_cond[mydata$SaleCondition %in% c("AdjLand")] <- 1

mydata$zone[mydata$MSZoning %in% c("FV")] <- 4
mydata$zone[mydata$MSZoning %in% c("RL")] <- 3
mydata$zone[mydata$MSZoning %in% c("RH","RM")] <- 2
mydata$zone[mydata$MSZoning %in% c("C (all)")] <- 1

mydata$alleypave[mydata$Alley %in% c("Pave")] <- 1
mydata$alleypave[!mydata$Alley %in% c("Pave")] <- 0

#removing unwanted variable after  introducing integres

mydata$Street <- NULL
mydata$LotShape <- NULL
mydata$LandContour <- NULL
mydata$Utilities <- NULL
mydata$LotConfig <- NULL
mydata$LandSlope <- NULL
mydata$Neighborhood <- NULL
mydata$Condition1 <- NULL
mydata$Condition2 <- NULL
mydata$BldgType <- NULL
mydata$HouseStyle <- NULL
mydata$RoofStyle <- NULL
mydata$RoofMatl <- NULL

mydata$Exterior1st <- NULL
mydata$Exterior2nd <- NULL
mydata$MasVnrType <- NULL
mydata$ExterQual <- NULL
mydata$ExterCond <- NULL

mydata$Foundation <- NULL
mydata$BsmtQual <- NULL
mydata$BsmtCond <- NULL
mydata$BsmtExposure <- NULL
mydata$BsmtFinType1 <- NULL
mydata$BsmtFinType2 <- NULL

mydata$Heating <- NULL
mydata$HeatingQC <- NULL
mydata$CentralAir <- NULL
mydata$Electrical <- NULL
mydata$KitchenQual <- NULL
mydata$FireplaceQu <- NULL

mydata$GarageType <- NULL
mydata$GarageFinish <- NULL
mydata$GarageQual <- NULL
mydata$GarageCond <- NULL
mydata$PavedDrive <- NULL

mydata$Functional <- NULL
mydata$PoolQC <- NULL
mydata$Fence <- NULL
mydata$MiscFeature <- NULL
mydata$SaleType <- NULL
mydata$SaleCondition <- NULL
mydata$MSZoning <- NULL
mydata$Alley <- NULL

#removal of NA 
mydata$GarageYrBlt[is.na(mydata$GarageYrBlt)] <- 0
mydata$MasVnrArea[is.na(mydata$MasVnrArea)] <- 0
mydata$LotFrontage[is.na(mydata$LotFrontage)] <- 0

#model preparation

lm_model_16<-lm(SalePrice~.,data=mydata) 
summary(lm_model_16)
backward<-step(lm_model_16, direction='backward')
coefficients(backward) #not working,need to work on missing values.
lm_model_15 <- lm(SalePrice ~ MSSubClass+LotArea+BsmtUnfSF+
                    X1stFlrSF+X2ndFlrSF+GarageCars+
                    WoodDeckSF+nbhd_price_level+
                    exterior_cond+pos_features_1+
                    bsmt_exp+kitchen+housefunction+pool_good+sale_cond, data=mydata)
summary(lm_model_15)
#tree
install.packages("tree")
library(tree)
tree0 = tree(SalePrice~., mydata)
summary(tree0)
plot(tree0) #prototype
text(tree0,pretty=0)
tree0
