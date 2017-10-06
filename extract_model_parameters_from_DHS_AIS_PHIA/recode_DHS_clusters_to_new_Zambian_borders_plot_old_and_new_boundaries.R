# Load in spatial libraries

library(sp)
library(rgdal)
library(raster)
library(data.table)
library(prevR)
library(readstata13)
library(survey)
library(plyr)
library(splancs)


setwd("Z:/mint/Dropbox (IDM)/research/HIV/2016/Zambia_Calibration/src/R_geocoded_DHS")

data_top_directory <- "Z:/mint/Dropbox (IDM)/research/HIV/2016/Zambia_Calibration/datasets/DHS/"




#Bring in Zambia DHS data from STATA, which was downloaded from dhsprogram.org and renamed.
zm.2007.men <- read.dta13(file.path(data_top_directory,"2007_men.dta"), convert.factors = TRUE, generate.factors = TRUE,
                          encoding = NULL, fromEncoding = NULL, convert.underscore = FALSE,
                          missing.type = FALSE, convert.dates = TRUE, replace.strl = FALSE,
                          add.rownames = FALSE, nonint.factors = TRUE)

# label variable mcaseid  "Case Identification"
# label variable mv000    "Country code and phase"
# label variable mv001    "Cluster number"
# label variable mv002    "Household number"
# label variable mv003    "Respondent's line number"
# label variable mv004    "Ultimate area unit"
# label variable mv005    "Men's sample weight (6 decimals)"
# label variable mv006    "Month of interview"
# label variable mv007    "Year of interview"
# label variable mv008    "Date of interview (CMC)"
# label variable mv009    "Month of birth"
# label variable mv010    "Year of birth"
# label variable mv011    "Date of birth (CMC)"
# label variable mv012    "Current age"
# label variable mv013    "Age in 5-year groups"
# label variable mv014    "Completeness of age information"
# label variable mv015    "Result of interview"
# label variable mv016    "Day of interview"
# label variable mv021    "Primary sampling unit"
# label variable mv022    "Sample stratum number"
# label variable mv023    "Sample domain"
# label variable mv024    "Region"
# label define MV101   
# 1 "Central"
# 2 "Copperbelt"
# 3 "Eastern"
# 4 "Luapula"
# 5 "Lusaka"
# 6 "Muchinga"
# 7 "Northern"
# 8 "North Western"
# 9 "Southern"
# 10 "Western"
#revalue(zm.2007.men$mv101, c("1"="Central")
#                             , 2="Copperbelt", 3="Eastern", 4="Luapula", 
#                             5="Lusaka", 6="Muchinga", 7="Northern", 8 ="North Western",
#                             9="Southern",10 "Western"))

zm.2007.women <- read.dta13(file.path(data_top_directory,"2007_women.dta"), convert.factors = TRUE, generate.factors = TRUE,
                            encoding = NULL, fromEncoding = NULL, convert.underscore = FALSE,
                            missing.type = FALSE, convert.dates = TRUE, replace.strl = FALSE,
                            add.rownames = FALSE, nonint.factors = TRUE)

zm.2007.hiv <- read.dta13(file.path(data_top_directory,"2007_hiv.dta"), convert.factors = TRUE, generate.factors = TRUE,
                          encoding = NULL, fromEncoding = NULL, convert.underscore = FALSE,
                          missing.type = FALSE, convert.dates = TRUE, replace.strl = FALSE,
                          add.rownames = FALSE, nonint.factors = TRUE)

# label variable hivclust "Cluster"
# label variable hivnumb  "Household"
# label variable hivline  "Line"
# label variable hiv01    "Bar code"
# label variable hiv02    "Lab DBS number"
# label variable hiv03    "Blood test result"
# label variable hiv05    "Sample weight"
# label variable hiv06    "CD4 test result"
# 
# #delimit ;
# label define HIV03   
# 0 "HIV negative"
# 1 "HIV  positive"
# 2 "HIV2 positive"
# 3 "HIV1 & HIV2 positive"
# 4 "ERROR : V-, W+, M+"
# 5 "ERROR : V-, W+, M-"
# 6 "ERROR : V-, W-, M+"
# 7 "Indeterminan

# read in DHS geographic file which has latitude and longitude of clusters
zm.2007.geog         <- read.csv(file.path(data_top_directory,"2007_geog.csv")) # x is LONGNUM, y is LATNUM
print(paste(length(which(zm.2007.geog$SOURCE=="MIS")),"DHS clusters had missing latitude/longitude coordinates and will be omitted from analysis"))
zm.2007.geog <- zm.2007.geog[zm.2007.geog$SOURCE!="MIS",] # this should do nothing if there are no missing clusters
zm.2007.cluster.locations      <- as.points(zm.2007.geog$LONGNUM, zm.2007.geog$LATNUM)
zm.2007.geog.spatial.points.df <- SpatialPointsDataFrame(zm.2007.cluster.locations, zm.2007.geog, coords.nrs = numeric(0), 
                                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"), match.ID = TRUE)
provincial_borders <- readRDS(file.path(data_top_directory,"ZMB_adm1.rds")) # province, there's a website that has these

old_provincial_borders <- readOGR(dsn= 'Z:/mint/Dropbox (IDM)/research/HIV/2016/Zambia_Calibration/datasets/DHS_raw_downloads/ZM_2007_sdr_subnational_boundaries_2016-06-09/shps',
                                  layer='sdr_subnational_boundaries')
## Inspect to see if cluster points are on the Zambia map and have correct province names
#provincial_borders$NAME_1
plot(provincial_borders)
points(zm.2007.cluster.locations, pch=18, col = "blue", cex=0.7)

plot(old_provincial_borders)
points(zm.2007.cluster.locations, pch=18, col = "blue", cex=0.7)

# key step -- do a spatial merge to recode the DHS clusters to the specified boundaries
zm.2007.spatially.recoded.clusters <- intersect(zm.2007.geog.spatial.points.df, provincial_borders) # same as 'over' but gives attributes from both layers. more like a spatial merge of data frames.
zm.2007.spatially.recoded.clusters$province <- zm.2007.spatially.recoded.clusters$NAME_1
zm.2007.spatially.recoded.clusters <- zm.2007.spatially.recoded.clusters[,c("DHSCLUST","province","LATNUM","LONGNUM")] #keep only columns I'll need
rm(zm.2007.geog,zm.2007.cluster.locations,zm.2007.geog.spatial.points.df)

# merge new province codes with DHS questionnaire data based on DHSCLUST ID, which is called mv001 in men's questionnare and v001 in women's questionnaire 
zm.2007.men$DHSCLUST   <- zm.2007.men$mv001 # rename mv001 to DHSCLUST which is what cluster was called in the spatial CSV
zm.2007.women$DHSCLUST <- zm.2007.women$v001

zm.2007.men   <- merge(zm.2007.men,   zm.2007.spatially.recoded.clusters, by= "DHSCLUST")
zm.2007.women <- merge(zm.2007.women, zm.2007.spatially.recoded.clusters, by= "DHSCLUST")


zm.2007.hiv$hivstat <- NA 
# hiv03 only has values "hiv negative" or "hiv positive" based on table(zm.2007.hiv$hiv03)
zm.2007.hiv$hivstat[zm.2007.hiv$hiv03=="hiv negative" | zm.2007.hiv$hiv03=="hiv2 positive"] <- 0
zm.2007.hiv$hivstat[zm.2007.hiv$hiv03=="hiv  positive" | zm.2007.hiv$hiv03=="hiv1 & hiv2 positive"] <- 1
zm.2007.hiv <- zm.2007.hiv[is.na(zm.2007.hiv$hivstat)==F,]
#summary(zm.2007.hiv$hivstat)
#length(zm.2007.hiv$hivstat)


zm.2007.hiv$matching <-paste(zm.2007.hiv$hivclust, zm.2007.hiv$hivnumb, zm.2007.hiv$hivline, sep= "_")

zm.2007.men$matching <-paste(zm.2007.men$mv001, zm.2007.men$mv002, zm.2007.men$mv003, sep= "_")
zm.2007.men.merge <- merge(zm.2007.men, zm.2007.hiv, by="matching")
zm.2007.men.merge$cluster <- zm.2007.men.merge$mv001

zm.2007.women$matching <-paste(zm.2007.women$v001, zm.2007.women$v002, zm.2007.women$v003, sep= "_")
zm.2007.women.merge <- merge(zm.2007.women, zm.2007.hiv, by="matching")
zm.2007.women.merge$cluster <- zm.2007.women.merge$v001


# Sampling weight men (mV005/10000, cf. DHS documentation)
zm.2007.men.merge$hiv.weight <- NA
zm.2007.men.merge$hiv.weight[is.na(zm.2007.men.merge$hivstat)==FALSE] <- zm.2007.men.merge$hiv05[is.na(zm.2007.men.merge$hivstat)==FALSE]/1000000 #hiv05 provides HIV weight
zm.2007.men.merge$men.weight <- zm.2007.men.merge$mv005/1000000 # mv005 for non-HIV weight
#summary(zm.2007.men.merge$hivstat)
#summary(zm.2007.men.merge$hiv.weight)


# Sampling weight women (V005/10000, cf. DHS documentation)
zm.2007.women.merge$hiv.weight <- NA
zm.2007.women.merge$hiv.weight[is.na(zm.2007.women.merge$hivstat)==FALSE] <- zm.2007.women.merge$hiv05[is.na(zm.2007.women.merge$hivstat)==FALSE]/1000000
zm.2007.women.merge$women.weight <- zm.2007.women.merge$v005/1000000
#summary(zm.2007.women.merge$hivstat)
#summary(zm.2007.women.merge$hiv.weight)


rm("zm.2007.hiv", "zm.2007.men", "zm.2007.women")

## Omit all but ages 14-49

zm.2007.men.merge.15.49   <- zm.2007.men.merge[zm.2007.men.merge$mv012>=15 & zm.2007.men.merge$mv012<50,]
zm.2007.women.merge.15.49 <- zm.2007.women.merge[zm.2007.women.merge$v012>=15 & zm.2007.women.merge$v012<50,]# unnecessary, womens report is only 15-49 anyway

## Define survey design: two-stage sampling at primary sampling unit and household levels

zm.2007.hiv.design.male <- svydesign(ids=~mv021 + mv002, strata= ~mv023, weights = zm.2007.men.merge.15.49$hiv.weight, data = zm.2007.men.merge.15.49, variables=NULL)
zm.2007.hiv.design.female <- svydesign(ids=~v021 + v002, strata= ~v023, weights = zm.2007.women.merge.15.49$hiv.weight, data = zm.2007.women.merge.15.49, variables=NULL)

## Compute survey-weighted mean and 95% CI for HIV prevalence for each gender.
## Note, some old DHS papers use logit error bounds but I found that beta-distributed error bounds more closely match StatCompiler and recent publications

# HIV prevalence by old provinces:
zm.2007.hiv.male.prevtable   <- svyby(~hivstat, by=~mv101, design = zm.2007.hiv.design.male, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)
zm.2007.hiv.female.prevtable <- svyby(~hivstat, by=~v101, design = zm.2007.hiv.design.female, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)

# HIV prevalence by new provinces:
zm.2007.hiv.male.prevtable   <- svyby(~hivstat, by=~province, design = zm.2007.hiv.design.male, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)
zm.2007.hiv.female.prevtable <- svyby(~hivstat, by=~province, design = zm.2007.hiv.design.female, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)

## Stack male and female results into a single table with a gender variable

zm.2007.hiv.male.prevtable$province <- zm.2007.hiv.male.prevtable$mv101; zm.2007.hiv.male.prevtable$mv101 <- NULL
zm.2007.hiv.female.prevtable$province <-  zm.2007.hiv.female.prevtable$v101; zm.2007.hiv.female.prevtable$v101 <- NULL
zm.2007.hiv.male.prevtable$gender <- 'male'
zm.2007.hiv.female.prevtable$gender <- 'female'
zm.2007.hiv.by.gender.prevtable <- rbind(zm.2007.hiv.male.prevtable, zm.2007.hiv.female.prevtable)

## Calculate parameters A and B of beta distribution that has the CI's provided by svyciprop

zm.2007.hiv.by.gender.prevtable$a <- NA
zm.2007.hiv.by.gender.prevtable$b <- NA

for(iter in seq(1,length(zm.2007.hiv.by.gender.prevtable$hivstat))){

fitToBeta <- function(a) {
    mu = zm.2007.hiv.by.gender.prevtable$hivstat[iter]
   lower_CI = zm.2007.hiv.by.gender.prevtable$ci_l[iter]
   upper_CI = zm.2007.hiv.by.gender.prevtable$ci_u[iter]
    
    b <- a * (1 / mu-1)
    calculated_lower_CI <- qbeta(0.025, a, b)
    calculated_upper_CI <- qbeta(0.975, a, b)
    penalty <-
    return((calculated_lower_CI - lower_CI)^2 + (calculated_upper_CI - upper_CI)^2)
}

zm.2007.hiv.by.gender.prevtable$a[iter] <- optimize(100, f=fitToBeta,lower=10, upper=10000,  maximum = FALSE)$minimum
}
zm.2007.hiv.by.gender.prevtable$b     <- zm.2007.hiv.by.gender.prevtable$a * (1 / zm.2007.hiv.by.gender.prevtable$hivstat-1)
zm.2007.hiv.by.gender.prevtable$N_eff <- zm.2007.hiv.by.gender.prevtable$a + zm.2007.hiv.by.gender.prevtable$b + 2

#write.csv(zm.2007.hiv.by.gender.prevtable, file=file.path(data_top_directory,'DHS_2007_new_borders.csv'))


### Calculate MC prevalence according to old borders. No need to calculate beta parameters here since I'll put the point est. in as model input. ###

zm.2007.men.merge.18p <- zm.2007.men.merge[zm.2007.men.merge$mv012>=18,] # restrict trad MC to 18+ in case some get circumcised later
#table(zm.2007.men.merge.18p$mv483)  #mv483 "Is respondent circumcised"

zm.2007.men.merge.18p$is_circumcised <- NA
zm.2007.men.merge.18p$is_circumcised[zm.2007.men.merge.18p$mv483=='yes'] <- 1
zm.2007.men.merge.18p$is_circumcised[zm.2007.men.merge.18p$mv483=='no']  <- 0
#table(zm.2007.men.merge.18p$is_circumcised)

# MC prevalence oaccording to old borders:
zm.2007.mc.design.male      <- svydesign(ids=~mv021 + mv002, strata= ~mv023, weights = zm.2007.men.merge.18p$men.weight, data = zm.2007.men.merge.18p, variables=NULL)
zm.2007.mc.male.prevtable   <- svyby(~is_circumcised, by=~mv101, design = zm.2007.mc.design.male, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)
#write.csv(zm.2007.mc.male.prevtable, file=file.path(data_top_directory,'DHS_2007_MC_old_borders.csv')) # these don't quite match the StatCompiler ones probably because there's a difference in age group.

# MC prevalence oaccording to old borders:
zm.2007.mc.male.prevtable   <- svyby(~is_circumcised, by=~province, design = zm.2007.mc.design.male, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)
#write.csv(zm.2007.mc.male.prevtable, file=file.path(data_top_directory,'DHS_2007_MC_new_borders.csv')) # these don't quite match the StatCompiler ones probably because there's a difference in 


# StatCompiler says that the denominator is all men who were interviewed. So let's take the full men's dataset.
# To execute the below code, first reload zm.2007.men from the original DTA file. I'd cleared the variable above to save RAM.
zm.2007.men.merge$is_circumcised <- NA
zm.2007.men.merge$is_circumcised[zm.2007.men.merge$mv483=='yes'] <- 1
zm.2007.men.merge$is_circumcised[zm.2007.men.merge$mv483=='no']  <- 0
zm.2007.mc.design.male.all.ages      <- svydesign(ids=~mv021 + mv002, strata= ~mv023, weights = zm.2007.men.merge$men.weight, data = zm.2007.men.merge, variables=NULL)
svyby(~is_circumcised, by=~mv101, design = zm.2007.mc.design.male.all.ages, FUN = svyciprop, vartype = c('ci'), method = "beta", level=0.95)



### Take other StatCompiler values (e.g., 2014 DHS which doesn't need recoding) to compute beta distribution a, b, and N_effective = a+b-2 based on 95% CIs
zm.HIV.and.MC.data         <- read.csv(file.path(data_top_directory,"Zambia_DHS_HIV_and_MC_final_to_compute_a_b.csv")) # contains mean, lower 2.5%, and upper 97.5% estimates
zm.HIV.and.MC.data$a <- NA
zm.HIV.and.MC.data$b <- NA
zm.HIV.and.MC.data$Neff <- NA

for(iter in seq(1,length(zm.HIV.and.MC.data$mean))){
    
    fitToBeta <- function(a) {
        mu = zm.HIV.and.MC.data$mean[iter]
        lower_CI = zm.HIV.and.MC.data$ci_lower[iter]
        upper_CI = zm.HIV.and.MC.data$ci_upper[iter]
        
        b <- a * (1 / mu-1)
        calculated_lower_CI <- qbeta(0.025, a, b)
        calculated_upper_CI <- qbeta(0.975, a, b)
        penalty <-(calculated_lower_CI - lower_CI)^2 + (calculated_upper_CI - upper_CI)^2
        if(a<=0 | b<=0 | is.na(calculated_lower_CI) | is.na(calculated_upper_CI)){penalty<- 1000000000}
            return(penalty)
    }
    
    zm.HIV.and.MC.data$a[iter] <- optimize(100, f=fitToBeta,lower=10, upper=1000,  maximum = FALSE)$minimum
}
zm.HIV.and.MC.data$b     <- zm.HIV.and.MC.data$a * (1 / zm.HIV.and.MC.data$mean-1)
zm.HIV.and.MC.data$Neff <- zm.HIV.and.MC.data$a + zm.HIV.and.MC.data$b + 2

#write.csv(zm.HIV.and.MC.data, file=file.path(data_top_directory,"Zambia_DHS_HIV_and_MC_final_with_computed_a_b.csv"))

