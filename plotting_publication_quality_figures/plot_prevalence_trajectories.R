library('stringr')
library(plyr)
library(reshape2)
library(lattice)
#library(memisc) # panel.errbars function is defunct
library(grid)
library(scales)
library(latticeExtra)

curr_filename = 'ReportHIVByAgeAndGender'
curr_base_dir = 'Nyanza NonResident SQ Baseline Resample 826/'
first_plot = T


for(TPI_iter in 0:249){

data_imported = read.csv(paste(curr_base_dir,curr_filename,'/',curr_filename,'_TPI',str_pad(toString(TPI_iter), 4, pad = "0"),'_REP0001.csv',sep=''))    
    

data_imported <- as.data.frame(data_imported)

data_imported <- data_imported[data_imported$Age>=15 & data_imported$Age<50 &  data_imported$Year > 1995 & data_imported$Year < 2018.5,]
data_imported <- aggregate( cbind(Population, Infected) ~ Year + NodeId + Gender, data=data_imported, FUN=sum)
data_imported$Prevalence = data_imported$Infected/data_imported$Population
data_imported$Population <- NULL
data_imported$Infected <- NULL


data_imported$NodeId = as.factor( data_imported$NodeId  )
data_imported$NodeId = revalue( data_imported$NodeId, c(
    "1" = "Homa Bay",
    "2" = "Kisii",
    "3" = "Kisumu",
    "4" = "Migori",
    "5" = "Nyamira",
    "6" = "Siaya") )

data_imported$Gender = as.factor( data_imported$Gender  )
data_imported$Gender = revalue( data_imported$Gender, c(
    "0" = "Male",
    "1" = "Female") )

data_imported$Gender <- factor(data_imported$Gender, levels = c('Female','Male'))
data_imported$NodeId <- factor(data_imported$NodeId, levels = c('Kisii','Nyamira','Migori','Kisumu','Homa Bay','Siaya'))

plot_to_add <- xyplot(Prevalence~Year | factor(Gender) + factor(NodeId),
  data=data_imported,  
  xlab="Year", ylab="Prevalence",pch=19,type=c("l"),col=alpha(rgb(70/255,130/255,250/255), 0.2),ylim=c(0, .7))

if(first_plot == T){curr_plot = plot_to_add; first_plot=F} else{curr_plot = curr_plot + as.layer(plot_to_add)}
print(paste('Plotted TPI number ', toString(TPI_iter + 1),sep=''))

}

# point estimates

pt_f <- read.csv('Female_prev_point.csv')    
pt_f$Year <- pt_f$X
pt_f$X <- NULL
pt_f['Homa Bay'] <- pt_f$Homa.Bay
pt_f$Homa.Bay <- NULL
pt_f$Gender <- 'Female'

pt_m <- read.csv('Male_prev_point.csv')    
pt_m$Year <- pt_m$X
pt_m$X <- NULL
pt_m['Homa Bay'] <- pt_m$Homa.Bay
pt_m$Homa.Bay <- NULL
pt_m$Gender <- 'Male'

prev_wide <- rbind(pt_f, pt_m)

prev_long <- melt(prev_wide,id.vars = c("Year", "Gender"), variable.name = 'NodeId', value.name = 'Prevalence')
prev_long <- as.data.frame(prev_long)

prev_long$Gender <- factor(prev_long$Gender, levels = c('Female','Male'))
prev_long$NodeId <- factor(prev_long$NodeId, levels = c('Kisii','Nyamira','Migori','Kisumu','Homa Bay','Siaya'))

prev_dataset <- prev_long


# lower 95% CI

pt_f <- read.csv('Female_prev_low.csv')    
pt_f$Year <- pt_f$X
pt_f$X <- NULL
pt_f['Homa Bay'] <- pt_f$Homa.Bay
pt_f$Homa.Bay <- NULL
pt_f$Gender <- 'Female'

pt_m <- read.csv('Male_prev_low.csv')    
pt_m$Year <- pt_m$X
pt_m$X <- NULL
pt_m['Homa Bay'] <- pt_m$Homa.Bay
pt_m$Homa.Bay <- NULL
pt_m$Gender <- 'Male'

prev_wide <- rbind(pt_f, pt_m)

prev_long <- melt(prev_wide,id.vars = c("Year", "Gender"), variable.name = 'NodeId', value.name = 'Prevalence')
prev_long <- as.data.frame(prev_long)

prev_long$Gender <- factor(prev_long$Gender, levels = c('Female','Male'))
prev_long$NodeId <- factor(prev_long$NodeId, levels = c('Kisii','Nyamira','Migori','Kisumu','Homa Bay','Siaya'))
prev_long$LowCI <- prev_long$Prevalence
prev_long$Prevalence <- NULL

prev_dataset <- merge(prev_dataset, prev_long,by=c("Year","Gender","NodeId"))


# upper 95% CI

pt_f <- read.csv('Female_prev_high.csv')    
pt_f$Year <- pt_f$X
pt_f$X <- NULL
pt_f['Homa Bay'] <- pt_f$Homa.Bay
pt_f$Homa.Bay <- NULL
pt_f$Gender <- 'Female'

pt_m <- read.csv('Male_prev_high.csv')    
pt_m$Year <- pt_m$X
pt_m$X <- NULL
pt_m['Homa Bay'] <- pt_m$Homa.Bay
pt_m$Homa.Bay <- NULL
pt_m$Gender <- 'Male'

prev_wide <- rbind(pt_f, pt_m)

prev_long <- melt(prev_wide,id.vars = c("Year", "Gender"), variable.name = 'NodeId', value.name = 'Prevalence')
prev_long <- as.data.frame(prev_long)

prev_long$Gender <- factor(prev_long$Gender, levels = c('Female','Male'))
prev_long$NodeId <- factor(prev_long$NodeId, levels = c('Kisii','Nyamira','Migori','Kisumu','Homa Bay','Siaya'))
prev_long$HighCI <- prev_long$Prevalence
prev_long$Prevalence <- NULL

prev_dataset <- merge(prev_dataset, prev_long,by=c("Year","Gender","NodeId"))

prev_dataset$Gender <- factor(prev_dataset$Gender, levels = c('Female','Male'))
prev_dataset$NodeId <- factor(prev_dataset$NodeId, levels = c('Kisii','Nyamira','Migori','Kisumu','Homa Bay','Siaya'))




plot_to_add <- xyplot(Prevalence~Year | factor(Gender) + factor(NodeId),
                      data=prev_dataset,  
                      xlab="Year", ylab="Prevalence",pch=19,type=c("p"),par.settings=simpleTheme(col="black"),ylim=c(0, .7))
plot_low_CI <- xyplot(LowCI~Year | factor(Gender) + factor(NodeId),
                      data=prev_dataset,  
                      xlab="Year", ylab="Prevalence",pch='-',cex=3,type=c("p"),par.settings=simpleTheme(col="black"),ylim=c(0, .7))
plot_high_CI <- xyplot(HighCI~Year | factor(Gender) + factor(NodeId),
                      data=prev_dataset,  
                      xlab="Year", ylab="Prevalence",pch='-',cex=3,type=c("p"),par.settings=simpleTheme(col="black"),ylim=c(0, .7))
# see http://www.martin-elff.net/knitr/memisc/errbars.html 
saved_plot = curr_plot

curr_plot = curr_plot + as.layer(plot_to_add) + as.layer(plot_low_CI) + as.layer(plot_high_CI)

show(saved_plot + as.layer(plot_to_add) + as.layer(plot_low_CI) + as.layer(plot_high_CI))

png("Fig1_Res_7x9in_300dpi.png", width = 7, height = 9, units = 'in', res = 300)
show(saved_plot + as.layer(plot_to_add) + as.layer(plot_low_CI) + as.layer(plot_high_CI))
dev.off()
