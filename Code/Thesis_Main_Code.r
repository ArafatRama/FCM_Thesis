# Chapter 4: The Lee-Carter Model
# Section 4.4: The Data: Human Mortality Database 
# Importing the data from the HMD website
library(HMDHFDplus)
readRenviron(".Renviron")
hmd_username<- Sys.getenv("HMD_USERNAME")
hmd_password<- Sys.getenv("HMD_PASSWORD")
# One test pull: England & Wales, single-age single-year death rates
mx<-readHMDweb(CNTRY = "GBRTENW", item = "Mx_1x1",
                 username =hmd_username, password = hmd_password)

str(mx)
range(mx$Year)     # earliest and latest year HMD actually has
range(mx$Age)      # age range
head(mx)

# Construct the limited data frame needed
print(getHMDitemavail("GBRTENW"),n=86) # to check all the data the hmd offers

# Pulling the period data needed (cohort data id excluded)
country<- "GBRTENW"      # England & Wales
min_year<- 1950
max_year<- 2022          # HMD's current end year for E&W

columns<- c(
  "Deaths_1x1",       # D(x,t)-> Lee-Carter input(numerator)
  "Exposures_1x1",    # E(x,t)-> Lee-Carter input(offset/denominator)
  "Mx_1x1",           # m(x,t)-> pre-computed rate,for sanity-checking
  "fltper_1x1",       # female period life table(qx, ex, ...)
  "mltper_1x1",       # male period life table 
  "bltper_1x1"        # both-sexes period life table
)

dat_uk <- lapply(columns,function(x) {
  cat("pulling", x, "...\n")
  d<- readHMDweb(country,x, username = hmd_username, password = hmd_password)
  d[d$Year >= min_year & d$Year <= max_year, ]
})
names(dat_uk) <- columns

# Data exploration --------------------------------------------------------
# All variables has 8103 rows which is 73 years*111 ages 0-110
# Sanity checks for Mx
# Define each item alone
D_UK <- dat_uk$Deaths_1x1
E_UK <- dat_uk$Exposures_1x1
Mx_UK <- dat_uk$Mx_1x1
summary(Mx_UK)

# Order each by year and age 
D_UK  <- D_UK[order(D_UK$Year, D_UK$Age), ]
E_UK  <- E_UK[order(E_UK$Year, E_UK$Age), ]
Mx_UK <- Mx_UK[order(Mx_UK$Year, Mx_UK$Age), ]

# Check for indices match 
stopifnot(all(D_UK$Year == Mx_UK$Year), all(D_UK$Age == Mx_UK$Age),
          all(E_UK$Year == Mx_UK$Year), all(E_UK$Age == Mx_UK$Age))

# Performing the sanity check for the total Mx
max((D_UK$Total/E_UK$Total)-Mx_UK$Total) #shows NA since in really old ages exposure is zero like in year 1950 and age 110 
E_UK[which.min(E_UK$Total), ]

# Ignore the NA
max(abs((D_UK$Total/E_UK$Total)-Mx_UK$Total),na.rm = TRUE) #BIG VALUES 

diff_uk <- (D_UK$Total/E_UK$Total) - Mx_UK$Total
i <- which(abs(diff_uk)>.000001)
diffdat1 <- data.frame(Year = D_UK$Year[i],Age = D_UK$Age[i],
           deaths = D_UK$Total[i],expo = E_UK$Total[i],
           mx_hmd = Mx_UK$Total[i],ratio = D_UK$Total[i]/E_UK$Total[i])
summary(diffdat1)
table(diffdat1$Age)
sum(diffdat1$Age > 95)/nrow(diffdat1)
#we can see that 99.66% of the uk data that showed a difference more that 1e-6 are ages beyond 95 and the 
# reason for that is the precision of the deaths and exposure is to 2 decimal places while the Mx is calculated with more precision 
# in the HMD data and that effect is exploited in older ages the exposure is already tiny so any rounding will have a great effect
# also after researching the HMD setup, the Mx that is produced in the hmd reports is already smoothed by the Kannisto model above ages of 80 years and especially above 95 where the fitting is done automatically while the transition 
# from 80 to 95 depend of the number of deaths if it's less that 100 then we use the fitted Mx if not then we use the raw one
# that's why we see differences especially in those ages I will now check the deaths  uk were the diff was >1e-6 and see how many of them where smoothed 
# since the age spread for ireland was bigger in the difference
# Uk ----
band_uk <- diffdat1$Age >= 80 & diffdat1$Age <= 95
sub_uk  <- diffdat1[band_uk, ]

m_uk <-mean(sub_uk$deaths <= 100)  # expect ~1

band_uk_1 <- diffdat1$Age > 95
m1_uk <- mean(band_uk_1)
m_uk+m1_uk
#the .4% records
diffdat1[!band_uk_1,]
diffdat1[!band_uk_1,]$ratio-diffdat1[!band_uk_1,]$mx_hmd

#for male
max(abs((D_UK$Male/E_UK$Male)-Mx_UK$Male),na.rm = TRUE) #BIG VALUES 

diff_uk_m <- (D_UK$Male / E_UK$Male) - Mx_UK$Male
i <- which(abs(diff_uk_m) > .000001)
diffdat1_m <- data.frame(Year = D_UK$Year[i], Age = D_UK$Age[i],
                         deaths = D_UK$Male[i], expo = E_UK$Male[i],
                         mx_hmd = Mx_UK$Male[i], ratio = D_UK$Male[i]/E_UK$Male[i])
summary(diffdat1_m)
table(diffdat1_m$Age)
sum(diffdat1_m$Age > 95)/ nrow(diffdat1_m)

band_uk_m <- diffdat1_m$Age >= 80 & diffdat1_m$Age <= 95
sub_uk_m  <- diffdat1_m[band_uk_m, ]

m_uk_m <-mean(sub_uk_m$deaths <= 100)  # expect ~1

band_uk_1_m <- diffdat1_m$Age > 95
m1_uk_m <- mean(band_uk_1_m)
m_uk_m+m1_uk_m
#the .4% records
diffdat1_m[!band_uk_1_m,]
diffdat1_m[!band_uk_1_m,]$ratio-diffdat1_m[!band_uk_1_m,]$mx_hmd

# for female 
max(abs((D_UK$Female/E_UK$Female)-Mx_UK$Female),na.rm = TRUE) #BIG VALUES 

diff_uk_f <- (D_UK$Female / E_UK$Female) - Mx_UK$Female
i <- which(abs(diff_uk_f) > .000001)
diffdat1_f <- data.frame(Year = D_UK$Year[i], Age = D_UK$Age[i],
                       deaths = D_UK$Female[i], expo = E_UK$Female[i],
                       mx_hmd = Mx_UK$Female[i], ratio = D_UK$Female[i]/E_UK$Female[i])
summary(diffdat1_f)
table(diffdat1_f$Age)
sum(diffdat1_f$Age > 95)/ nrow(diffdat1_f)

band_uk_f <- diffdat1_f$Age >= 80 & diffdat1_f$Age <= 95
sub_uk_f  <- diffdat1_f[band_uk_f, ]

m_uk_f <-mean(sub_uk_f$deaths <= 100)  # expect ~1

band_uk_1_f <- diffdat1_f$Age > 95
m1_uk_f <- mean(band_uk_1_f)
m_uk_f+m1_uk_f

#the .4% records
diffdat1_f[!band_uk_1_f,]
diffdat1_f[!band_uk_1_f,]$ratio-diffdat1_f[!band_uk_1_f,]$mx_hmd

#we can see that for uk 99.6 % of the data with difference in Mx is for ages older than 95 so the smoothing was used and no younger ages recorded deaths less than 100
# so the .4% which is 2 records for age 95 and the difference between the raw and the hmd ratio is considerably small and is because of approximation

# Data quality for the Lee Carter to preeced without errors 
# check for NA or zeros 
quality_summary <- function(data,name) {
  cat("QUALITY SUMMARY:",name,"\n")
  cat("Duplicate Year-Age rows:\n")
  if ("Year" %in% names(data) & "Age" %in% names(data)) {
    key <- paste(data$Year,data$Age,sep = "_")
    cat(sum(duplicated(key)),"\n")
  } else { cat("No Year/Age columns found.\n") }
  
  numeric_cols <- names(data)[sapply(data, is.numeric)]
  
  for (col in numeric_cols) {
    cat("\nColumn:", col, "\n")
    cat("NA:", sum(is.na(data[[col]])), "\n")
    cat("Zero:", sum(data[[col]] == 0, na.rm = TRUE), "\n")
    cat("Negative:", sum(data[[col]] < 0, na.rm = TRUE), "\n")
    cat("Infinite:", sum(is.infinite(data[[col]])), "\n")
    cat("Summary:\n")
    print(summary(data[[col]]))
  }
}

quality_summary(D_UK, "D_UK")
quality_summary(E_UK, "E_UK")
quality_summary(Mx_UK, "Mx_UK")

# no NA's there is some zeros that we have to deal with before fitting the Lee Carter model
# pulling the zeros 
zero_D_UK_Male <- D_UK[D_UK$Male == 0 & !is.na(D_UK$Male), ]
zero_D_UK_Female <- D_UK[D_UK$Female == 0 & !is.na(D_UK$Female), ]
zero_D_UK_Total <- D_UK[D_UK$Total == 0 & !is.na(D_UK$Total), ]

table(zero_D_UK_Male$Age)
table(zero_D_UK_Female$Age)
table(zero_D_UK_Total$Age)

# the zero deaths are more frequent in the higher ages which is explained by lower exposure
# and the zeros in the exposure and deaths will cause issues for us when building the lee carter so we have to deal with them we will see how in following parts by seeting death+1

#3D surface plotting 
library(ggplot2)
library(plotly)
#create function
plot_3d_surface<-function(data,sex,value_type,label,z_label,title_text) {
  temp <- data.frame(
    Year=data$Year,
    Age=data$Age,
    value=data[[sex]] )
  if (value_type=="deaths") {
    temp$z<- log(temp$value+1)} #to avoid log 0
  if (value_type == "exposure") {
    temp$z <- log(temp$value)
    temp$z[temp$value <= 0]<-NA }
  if (value_type == "mx") {
    temp$z <- log(temp$value)
    temp$z[temp$value <= 0] <- NA}
  
  z_matrix <- xtabs(z ~ Age+Year,data=temp)
  z_matrix <- as.matrix(z_matrix)
  ages<-as.numeric(rownames(z_matrix))
  years<-as.numeric(colnames(z_matrix))
  
  p<-plot_ly(
    x=years,
    y=ages,
    z=z_matrix,
    type="surface")
  
  p <-layout(
    p,title=title_text,scene=list(
      xaxis = list(title="Year"),
      yaxis = list(title="Age"),
      zaxis = list(title=z_label)))
  return(p)}

#Death counts 3d plot
p_3d_D_UK_Male <- plot_3d_surface(
  D_UK,
  "Male",
  "deaths",
  "UK",
  "log(Deaths + 1)","")

p_3d_D_UK_Female <- plot_3d_surface(
  D_UK,
  "Female",
  "deaths",
  "UK",
  "log(Deaths + 1)","")

#Exposure 3d plot
p_3d_E_UK_Male <- plot_3d_surface(
  E_UK,
  "Male",
  "exposure",
  "UK",
  "log Exposure","")

p_3d_E_UK_Female<- plot_3d_surface(
  E_UK,
  "Female",
  "exposure",
  "UK",
  "log Exposure","")

#HMD mortality rate
p_3d_Mx_UK_Male <- plot_3d_surface(
  Mx_UK,
  "Male",
  "mx",
  "UK",
  "log Mx","")

p_3d_Mx_UK_Female <- plot_3d_surface(
  Mx_UK,
  "Female",
  "mx",
  "UK",
  "log Mx","")

# Deaths
p_3d_D_UK_Male
p_3d_D_UK_Female
# Exposures
p_3d_E_UK_Male
p_3d_E_UK_Female
# HMD Mx
par(mfrow = c(1,1))
p_3d_Mx_UK_Male
p_3d_Mx_UK_Female

# We can see that even with the Kannisto model the Mx is still noisy and inconsistent across all years that's why we need to think 
# if we need to cap the ages up to 100 for the lee carter fir to avoid noise capturing
# we can also see the expected shape of high exposure in the infant years, the hump around 20 and the steep rise in old ages.
# and the doenward trend in mortality across the years.
par(mfrow=c(2,1))
# PLOT: mortality decline over time at selected fixed ages
ages_selected <- c(10,20, 40, 60, 80)
mx_selected <- Mx_UK[Mx_UK$Age %in% ages_selected, ]
mx_selected$log_Male <- log(mx_selected$Male)
mx_selected$log_Female <- log(mx_selected$Female)
# remove non-finite values if present
mx_selected$log_Male[!is.finite(mx_selected$log_Male)] <- NA
mx_selected$log_Female[!is.finite(mx_selected$log_Female)] <- NA
# y-axis so the male and female figures are directly comparable
ylim_selected <- range(mx_selected$log_Male,mx_selected$log_Female,na.rm = TRUE)

cols<-c("black","darkblue","orange","darkgreen","yellow")
ltys<-c(1,2,3,4,5)

#Plot: Log-mortality over time at selected ages - male
first_age<-ages_selected[1]
temp<-mx_selected[mx_selected$Age==first_age,]
plot(temp$Year,temp$log_Male,
  type="l",lwd=2,
  col=cols[1],
  lty=ltys[1],
  ylim=ylim_selected,xlab="Year",ylab=expression(log(m[x,t])),
  main="Male log-mortality over time at selected ages")

for (i in 2:length(ages_selected)){
  temp<-mx_selected[mx_selected$Age==ages_selected[i], ]
  lines(temp$Year,temp$log_Male,
    lwd=2,
    col=cols[i],
    lty=ltys[i])}

legend("topright",
  legend=paste("Age", ages_selected),
  col=cols,
  lty=ltys,
  lwd=2,
  bty="o",
  bg="white",
  cex=0.8,
  seg.len=1.5,
  x.intersp=0.4,
  y.intersp=0.9,
  text.width=max(strwidth(paste("Age",ages_selected),cex = 0.8))
)

#Plot: Log-mortality over time at selected ages - female
first_age<-ages_selected[1]
temp<-mx_selected[mx_selected$Age == first_age,]

plot(temp$Year,temp$log_Female,
  type="l",lwd=2,
  col=cols[1],lty=ltys[1],
  ylim=ylim_selected,xlab="Year",ylab=expression(log(m[x,t])),
  main= "Female log-mortality over time at selected ages")

for (i in 2:length(ages_selected)){
  temp<-mx_selected[mx_selected$Age==ages_selected[i], ]
  lines(temp$Year,temp$log_Female,
    lwd=2,
    col=cols[i],
    lty=ltys[i])}

legend("topright",
  legend=paste("Age",ages_selected),
  col=cols,
  lty=ltys,
  lwd=2,
  bty="o",
  bg="white",
  cex=0.8,
  seg.len=1.5,
  x.intersp=0.4,
  y.intersp=0.9,
  text.width=max(strwidth(paste("Age",ages_selected),cex = 0.8))
)

# kappa preview
# I will now try to plot the mean log mortality across all ages for each year to try to see the expected kappa trend
# Safe function because log(0) = -Inf, and na.rm = TRUE does not remove Inf
mean_log <- function(x) {
  log_x<-log(x)
  log_x<-log_x[is.finite(log_x)]
  if (length(log_x)==0) {return(NA)
  } else {return(mean(log_x))}
} # or I could have made a condition afterwards to set the infinities to NA but this is one is easier

years_UK <- sort(unique(Mx_UK$Year))
mean_logmx_UK <- data.frame(
  Year = years_UK,
  Male = NA,
  Female = NA
)
for (i in 1:length(years_UK)){
  temp <- Mx_UK[Mx_UK$Year==years_UK[i], ]
  mean_logmx_UK$Male[i]<-mean_log(temp$Male)
  mean_logmx_UK$Female[i]<-mean_log(temp$Female)
}

#Plot: Mean log-mortality over time, England & Wales
ylim_UK <- range(
  mean_logmx_UK$Male[is.finite(mean_logmx_UK$Male)],
  mean_logmx_UK$Female[is.finite(mean_logmx_UK$Female)])
plot(mean_logmx_UK$Year,mean_logmx_UK$Male,
  type="l",
  lwd=2,
  xlab="Year",
  ylab="Mean log Mx",
  main="Mean log mortality over time - England and Wales",
  ylim=ylim_UK)
lines(
  mean_logmx_UK$Year,
  mean_logmx_UK$Female,
  lwd=2,
  lty=1,
  col="darkblue")
legend(
  "topright",
  legend=c("Male","Female"),
  lty=c(1,1),
  lwd=c(2,2),
  col=c("black","darkblue"),
  bty="n")

#PLOT: Heat map Year-on-year change in log-mortality, England & Wales 
#plot the difference in log mortality between each year and see if there is a big jump in mortality and in which years
library(plotly)
# Data already sorted earlier, so we do NOT sort again
stopifnot(all(D_UK$Year == E_UK$Year))
stopifnot(all(D_UK$Age == E_UK$Age))
ages_UK<- unique(D_UK$Age)
years_UK<- unique(D_UK$Year)

# Manual total Mx = total deaths / total exposure
manual_mx_UK <- D_UK$Total/E_UK$Total

# Remove impossible values before taking log
manual_mx_UK[E_UK$Total <= 0]<- NA
manual_mx_UK[manual_mx_UK <= 0]<- NA
manual_mx_UK[!is.finite(manual_mx_UK)]<- NA

# each column = one year, each row = one age
mx_matrix_UK <- matrix(
  manual_mx_UK,
  nrow= length(ages_UK),
  ncol= length(years_UK),
  byrow=FALSE)
rownames(mx_matrix_UK) <- ages_UK
colnames(mx_matrix_UK) <- years_UK

# Log mortality
log_mx_UK <- log(mx_matrix_UK)
log_mx_UK[!is.finite(log_mx_UK)] <- NA

# Difference from year to year
# log Mx(x,t) - log Mx(x,t-1)
diff_log_mx_UK <- log_mx_UK[, -1] - log_mx_UK[, -ncol(log_mx_UK)]
years_diff_UK <- years_UK[-1]

# Colour limit to avoid one extreme old-age value dominating the heatmap
lim_UK <- quantile(abs(diff_log_mx_UK), 0.99, na.rm = TRUE)
image(
  x = years_diff_UK,
  y = ages_UK,
  z = t(diff_log_mx_UK),
  col = colorRampPalette(c("blue", "white", "red"))(100),
  zlim = c(-lim_UK, lim_UK),
  xlab = "Year",
  ylab = "Age",
  main = "")
box()
abline(v = 2020, lty = 2)
abline(v = 2021, lty = 2)
mtext(
  "Red = mortality increased from previous year; Blue = mortality decreased",
  side = 3,
  line = 0.3,
  cex = 0.75
)

# YEARS WITH BIGGEST AVERAGE INCREASE - UK TOTAL
mean_change_UK <- colMeans(diff_log_mx_UK, na.rm = TRUE)
big_years_UK <- data.frame(
  Year = years_diff_UK,
  Mean_log_change = mean_change_UK
)

big_years_UK <- big_years_UK[order(-big_years_UK$Mean_log_change), ]
head(big_years_UK, 10)
length(mean_change_UK)
mean_change_UK[70:72]
# We can see that the 2020 mortality rates had the highest increase which is due to Covid-19 pandemic and we can also see that the year 2021 had an extra considerably lower increase in mortality with respect to 2020 but it's still positive and it hasn't gone
# down since the covid 19 was still going in those years so higher moralities.
# also, it's worth noting even though we will not consider cohort trend, that those who were 31 years old in 1951 meaning that they were born on 1920 which is the first generation born after the first world war where there was poverty and hunger, so  poor nutrition 
# in addition to the spread of infectious diseases in that generation had the mortality rates to be cared out as high.    

# Zoomed heatmap: 2020-2021 ---------------------------------------------
par(mfrow=c(1,1))

zoom_years <- c(2020,2021)
zoom_cols <- years_diff_UK %in% zoom_years
diff_zoom_UK <- diff_log_mx_UK[,zoom_cols,drop=FALSE]
lim_zoom_UK <- max(abs(diff_zoom_UK),na.rm=TRUE)
image(x=zoom_years,y=ages_UK,z=t(diff_zoom_UK),
      col=colorRampPalette(c("blue","white","red"))(100),
      zlim=c(-lim_zoom_UK,lim_zoom_UK),
      xlab="Year",ylab="Age",
      main="Mortality change around the onset of COVID-19",
      xaxt="n")
axis(1,at=zoom_years,labels=c("2020","2021"))
box()
mtext("Red = mortality increased from previous year; Blue = mortality decreased",
      side=3,line=0.3,cex=0.75)

#End of Data exploration 


# Chapter 4: The Lee-Carter Model
# Section 4.5: The Data: Human Mortality Database (England \& Wales)
# Data Preparation  -------------------------------------------------------
# Fit three Lee-Carter specifications: unadjusted SVD on manual Mx, unadjusted SVD on HMD Mx, and Poisson using deaths and exposures.
# Reserve 2020-2022 as the pandemic stress-test window.

# Set the data frames shape as needed 
# Use ages 50-100, train on 1950-2014, validate on 2015-2019, and stress-test on 2020-2022.
ages_to_fit<-50:100
years_train<-1950:2014
years_valid<-2015:2019
years_stress<-2020:2022

ages_length <- length(ages_to_fit)
years_train_length <- length(years_train)
years_valid_length <- length(years_valid)
years_stress_length <- length(years_stress)
sexes <- c("Male", "Female")

# Check that the data is still ordered by Year then Age
stopifnot(all(D_UK$Year == rep(years_UK, each = length(ages_UK))))
stopifnot(all(D_UK$Age == rep(ages_UK, times = length(years_UK))))

#create a list to easily differentiate between female and male data 
LC_UK<-list()
for (sex in sexes) {
  #calculating manual Mx
  manual_mx <- D_UK[[sex]]/E_UK[[sex]]
  manual_mx[E_UK[[sex]] <= 0] <- NA
  manual_mx[manual_mx <= 0] <- NA
  manual_mx[!is.finite(manual_mx)] <- NA
  #building the matrix format
  manual_mx_matrix <- matrix(manual_mx,
    nrow = length(ages_UK), ncol = length(years_UK),
    byrow = FALSE)
  
  HMD_mx_matrix <- matrix(Mx_UK[[sex]],
    nrow = length(ages_UK),ncol = length(years_UK),
    byrow = FALSE)
  
  D_matrix <- matrix(D_UK[[sex]],
    nrow = length(ages_UK),ncol = length(years_UK),
    byrow = FALSE)
  
  E_matrix <- matrix(E_UK[[sex]],
    nrow = length(ages_UK),ncol = length(years_UK),
    byrow = FALSE)
  
  #add rows and columns names 
  rownames(manual_mx_matrix) <- rownames(HMD_mx_matrix) <-rownames(D_matrix) <-rownames(E_matrix)<-ages_UK
  colnames(manual_mx_matrix) <- colnames(D_matrix) <-colnames(HMD_mx_matrix) <-colnames(E_matrix)<-years_UK
  
  #splitting the data
  age_rows <- rownames(manual_mx_matrix) %in% as.character(ages_to_fit)
  train_cols <- colnames(manual_mx_matrix) %in% as.character(years_train)
  valid_cols <- colnames(manual_mx_matrix) %in% as.character(years_valid)
  stress_cols <- colnames(manual_mx_matrix) %in% as.character(years_stress)
  
  LC_UK[[sex]] <- list(
    manual_mx_full = manual_mx_matrix,
    HMD_mx_full = HMD_mx_matrix,
    D_full = D_matrix,
    E_full = E_matrix,
    
    # Method 1 data: SVD on manual Mx
    manual_train = manual_mx_matrix[age_rows,train_cols, drop = FALSE],
    manual_valid = manual_mx_matrix[age_rows,valid_cols, drop = FALSE],
    manual_stress = manual_mx_matrix[age_rows,stress_cols, drop = FALSE],
    
    # Method 2: SVD on HMD Mx
    HMD_train = HMD_mx_matrix[age_rows,train_cols, drop = FALSE],
    HMD_valid = HMD_mx_matrix[age_rows,valid_cols, drop = FALSE],
    HMD_stress = HMD_mx_matrix[age_rows,stress_cols, drop = FALSE],
    
    # Method 3: Poisson Lee-Carter on deaths and exposures
    P_D_train = D_matrix[age_rows,train_cols,drop = FALSE],
    P_E_train = E_matrix[age_rows,train_cols,drop = FALSE],
    P_D_valid = D_matrix[age_rows,valid_cols,drop = FALSE],
    P_E_valid = E_matrix[age_rows,valid_cols,drop = FALSE],
    P_D_stress = D_matrix[age_rows,stress_cols,drop = FALSE],
    P_E_stress = E_matrix[age_rows,stress_cols,drop = FALSE]
  )
}

# Lee Carter Implementation -----------------------------------------------
# Fit the first two HMD calculated mortality by unadjusted SVD.
?demography::lca()
#fitting the lee carter
library(demography)
#construct the mortality demogdata object for manual and HMD data and for all sex
#for manual mx
for (sex in sexes){
  LC_UK[[sex]]$manual_demogdata <- demogdata(
    data = LC_UK[[sex]]$manual_train,
    pop = LC_UK[[sex]]$P_E_train,
    ages = as.numeric(rownames(LC_UK[[sex]]$manual_train)),
    years = as.numeric(colnames(LC_UK[[sex]]$manual_train)),
    type = "mortality",
    label = "England and Wales",
    name = sex)
  
  LC_UK[[sex]]$manual_lca_sett_none <- lca(
    data = LC_UK[[sex]]$manual_demogdata,
    series = sex,
    adjust = "none",
  )}

#for HMD mx
for (sex in sexes){
  LC_UK[[sex]]$HMD_demogdata <- demogdata(
    data = LC_UK[[sex]]$HMD_train,
    pop = LC_UK[[sex]]$P_E_train,
    ages = as.numeric(rownames(LC_UK[[sex]]$HMD_train)),
    years = as.numeric(colnames(LC_UK[[sex]]$HMD_train)),
    type = "mortality",
    label = "England and Wales",
    name = sex)
  LC_UK[[sex]]$hmd_lca_sett_none <- lca(
    data = LC_UK[[sex]]$HMD_demogdata,
    series = sex,
    adjust = "none",
  )}

names(LC_UK$Male$manual_lca_sett_none)
# #> names(LC_UK$Male$manual_lca_sett_none)
# [1] "label"     "age"       "year"      "Male"      "ax"       
# [6] "bx"        "kt"        "residuals" "fitted"    "varprop"  
# [11] "y"         "mdev"      "call"      "adjust"    "type"

# #double checking the lee carter constraints
# sum(LC_UK$Male$manual_lca_sett_none$kt) # close to 0
# sum(LC_UK$Male$manual_lca_sett_none$bx) #1
# sum(LC_UK$Female$manual_lca_sett_none$kt) # close to 0
# sum(LC_UK$Female$manual_lca_sett_none$bx) #1

LC_UK$Male$manual_lca_sett_none$ax
LC_UK$Male$manual_lca_sett_none$bx
LC_UK$Male$manual_lca_sett_none$kt

# Plot alpha beta and kappa for each model to compare 
#PLOT: Sanity check: κt from manual versus HMD rates
par(mfrow=c(1,2))
# Male
plot(
  LC_UK$Male$manual_lca_sett_none$year,
  LC_UK$Male$manual_lca_sett_none$kt,
  type="l",
  xlab="Year",
  ylab=expression(kappa[t]),
  main=expression("Comparison of " * kappa[t] * " - Male"))
lines(
  LC_UK$Male$hmd_lca_sett_none$year,
  LC_UK$Male$hmd_lca_sett_none$kt,
  lwd=2,
  col="darkblue")
legend(
  "topright",
  legend=c("Manual Mx","HMD Mx"),
  col=c("black","darkblue"),
  lwd=c(2, 2),
  bty="n")
# Female
plot(
  LC_UK$Female$manual_lca_sett_none$year,
  LC_UK$Female$manual_lca_sett_none$kt,
  type="l",
  lwd=2,
  xlab="Year",
  ylab=expression(kappa[t]),
  main=expression("Comparison of " * kappa[t] * " - Female"))
lines(
  LC_UK$Female$hmd_lca_sett_none$year,
  LC_UK$Female$hmd_lca_sett_none$kt,
  lwd=4,
  col="darkblue")
legend(
  "topright",
  legend=c("Manual Mx","HMD Mx"),
  col=c("black","darkblue"),
  lwd=c(2,4),
  bty="n")

max(abs(LC_UK$Female$manual_lca_sett_none$kt - LC_UK$Female$hmd_lca_sett_none$kt),na.rm = TRUE)
summary(LC_UK$Male$manual_lca_sett_none$kt - LC_UK$Male$hmd_lca_sett_none$kt)
# we can see that the difference between the manual and the HMD usage of Mx didn't have a great affect on the kappa-t fitted values of the two models 
# and that is explain by the exploratory analysis which established that 99.6 % of the Mx rates with big difference between the two methods where as a result of ages <95 and since 
# we capped the ages in out model the difference is almost negligible causing the two curves to fall closely on top of each other with maximum male difference of 0.0007765565.
# and female difference of 0.0007421772.

#plot comparason with alpha and bets
#male
plot(
  LC_UK$Male$manual_lca_sett_none$age,
  LC_UK$Male$manual_lca_sett_none$ax,
  type="l",
  xlab="Age",
  ylab=expression(alpha[x]),
  main=expression("Comparison of " * alpha[x] * " - Male"))
lines(
  LC_UK$Male$hmd_lca_sett_none$age,
  LC_UK$Male$hmd_lca_sett_none$ax,
  lwd=2,
  col="red")
legend(
  "topright",
  legend=c("Manual Mx","HMD Mx"),
  col=c("black","red"),
  lwd=c(2,2),
  bty="n")
plot(
  LC_UK$Male$manual_lca_sett_none$age,
  LC_UK$Male$manual_lca_sett_none$bx,
  type = "l",
  xlab = "Age",
  ylab = expression(beta[x]),
  main = expression("Comparison of " * beta[x] * " - Male"))
lines(
  LC_UK$Male$hmd_lca_sett_none$age,
  LC_UK$Male$hmd_lca_sett_none$bx,
  lwd = 2,
  col = "red")
legend(
  "topright",
  legend = c("Manual Mx","HMD Mx"),
  col = c("black","red"),
  lwd = c(2,2),
  bty = "n")
# Female
plot(
  LC_UK$Female$manual_lca_sett_none$age,
  LC_UK$Female$manual_lca_sett_none$ax,
  type="l",
  lwd=2,
  xlab="age",
  ylab=expression(alpha[x]),
  main=expression("Comparison of " * alpha[x] * " - Female"))
lines(
  LC_UK$Female$hmd_lca_sett_none$age,
  LC_UK$Female$hmd_lca_sett_none$ax,
  lwd=2,
  col="red")
legend(
  "topright",
  legend=c("Manual Mx","HMD Mx"),
  col=c("black","red"),
  lwd=c(2,2),
  bty="n")
plot(
  LC_UK$Female$manual_lca_sett_none$age,
  LC_UK$Female$manual_lca_sett_none$bx,
  type = "l",
  lwd = 2,
  xlab = "age",
  ylab = expression(beta[x]),
  main = expression("Comparison of " * beta[x] * " - Female"))
lines(
  LC_UK$Female$hmd_lca_sett_none$age,
  LC_UK$Female$hmd_lca_sett_none$bx,
  lwd = 2,
  col = "red")
legend(
  "topright",
  legend = c("Manual Mx","HMD Mx"),
  col = c("black","red"),
  lwd = c(2,2),
  bty = "n")
max(abs( LC_UK$Female$manual_lca_sett_none$ax - LC_UK$Female$hmd_lca_sett_none$ax),na.rm = TRUE)
max(abs( LC_UK$Female$manual_lca_sett_none$bx - LC_UK$Female$hmd_lca_sett_none$bx),na.rm = TRUE)

# The same explanation would follow to the bx and alpha x 


# Chapter 4: The Lee-Carter Model
# Section 4.5: Fitting the Lee-Carter Model
# Poisson and adjusted lee carter -----------------------------------------
#implementing Poisson 
library(StMoMo)
?StMoMo
?lc()
?fit
?fit.StMoMo 
#Poisson Lee-Carter model, Deaths ~ Poisson(expected deaths)
poisson_LC_model <- lc(link="log",const="sum")
for (sex in sexes) {
  D_temp <- as.matrix(LC_UK[[sex]]$P_D_train)
  E_temp <- as.matrix(LC_UK[[sex]]$P_E_train)
  storage.mode(D_temp) <- "numeric"
  storage.mode(E_temp) <- "numeric"
  
  LC_UK[[sex]]$poisson_lc_fit <- fit(
    poisson_LC_model,
    Dxt = D_temp,
    Ext = E_temp,
    ages = as.numeric(rownames(D_temp)),
    years = as.numeric(colnames(D_temp)),
    ages.fit = as.numeric(rownames(D_temp)),
    years.fit = as.numeric(colnames(D_temp)),
    verbose = FALSE
  )
}
#adjusted lee carter on manual Mx 
for (sex in sexes){
  LC_UK[[sex]]$manual_lca_sett_dt <- lca(
    data = LC_UK[[sex]]$manual_demogdata,
    series = sex,adjust = "dt")
}
LC_UK$Female$manual_lca_sett_dt
LC_UK$Female$poisson_lc_fit$ages

#PLOT : Fitted parameters from the three Lee–Carter estimation methods
sexes<-c("Male","Female")
par(mfcol=c(3, 2))

for (sex in sexes){
  poisson_ax<-LC_UK[[sex]]$poisson_lc_fit$ax
  poisson_bx<-LC_UK[[sex]]$poisson_lc_fit$bx[,1]
  poisson_kt<-LC_UK[[sex]]$poisson_lc_fit$kt[1,]
  
  ages_plot<-LC_UK[[sex]]$manual_lca_sett_dt$age
  years_plot<-LC_UK[[sex]]$manual_lca_sett_dt$year
  
  ylim_alpha<-range(
    LC_UK[[sex]]$manual_lca_sett_none$ax,
    LC_UK[[sex]]$manual_lca_sett_dt$ax,
    poisson_ax,
    na.rm=TRUE)
  
  plot(
    ages_plot,
    LC_UK[[sex]]$manual_lca_sett_none$ax,
    type="l",
    lwd=5,
    ylim=ylim_alpha,
    xlab="Age",
    ylab="",
    main=paste("Comparison of alpha -",sex))
  mtext(expression(alpha[x]),side=2,line=2.5,cex=1)
  
  lines(
    ages_plot,
    LC_UK[[sex]]$manual_lca_sett_dt$ax,
    lwd=2,
    col="red")
  lines(
    ages_plot,
    poisson_ax,
    lwd=2,
    col="darkblue")
  legend(
    "topleft",
    legend=c("SVD LC - none","SVD LC - dt","Poisson LC"),
    col=c("black","red","darkblue"),
    lwd=c(5,2,2),
    bty="n")
  
  ylim_beta<-range(
    LC_UK[[sex]]$manual_lca_sett_none$bx,
    LC_UK[[sex]]$manual_lca_sett_dt$bx,
    poisson_bx,
    na.rm=TRUE)
  plot(
    ages_plot,
    LC_UK[[sex]]$manual_lca_sett_none$bx,
    type="l",
    lwd=5,
    ylim=ylim_beta,
    xlab="Age",
    ylab="",
    main=paste("Comparison of beta -",sex))
  mtext(expression(beta[x]),side=2,line=2.5,cex=1)
  lines(
    ages_plot,
    LC_UK[[sex]]$manual_lca_sett_dt$bx,
    lwd=2,
    col="red")
  lines(
    ages_plot,
    poisson_bx,
    lwd=2,
    col="darkblue")
  legend(
    "bottomleft",
    legend=c("SVD LC - none","SVD LC - dt","Poisson LC"),
    col=c("black","red","darkblue"),
    lwd=c(5,2,2),
    bty="n",
    cex=0.8)
  ylim_kappa<-range(
    LC_UK[[sex]]$manual_lca_sett_none$kt,
    LC_UK[[sex]]$manual_lca_sett_dt$kt,
    poisson_kt,
    na.rm=TRUE)
  
  plot(
    years_plot,
    LC_UK[[sex]]$manual_lca_sett_none$kt,
    type="l",
    lwd=5,
    ylim=ylim_kappa,
    xlab="Year",
    ylab="",
    main=paste("Comparison of kappa -",sex))
  mtext(expression(kappa[t]),side=2,line=2.5,cex=1)
  lines(
    years_plot,
    LC_UK[[sex]]$manual_lca_sett_dt$kt,
    lwd=2,
    col="red")
  lines(
    years_plot,
    poisson_kt,
    lwd=2,
    col="darkblue")
  legend(
    "bottomleft",
    legend=c("SVD LC - none","SVD LC - dt","Poisson LC"),
    col=c("black","red","darkblue"),
    lwd=c(5,2,2),
    bty="n",
    cex=0.8)
}
?legend
# we can see that alpha was considerably similar through the three models and the reason for that is because alpha represents average age mortality level 
# which isn't sensitive to the method used and should be similar when matching the underlying mortality data and the age ranges in each year. 

# for kappa which represents the overall mortality level in year t, in the three methods we can see the downward trend which indicated the advancement of life expectancy
# and we can also see a small drift of the SVD lee carter method which doesn't use any adjustment to get closer to the actual death exposures and that makes sense, since 
#both adjusted SVD and Poisson take into consideration the number of deaths occurred while building the model. Adjusted SVD tries to match the number of deaths while Poisson estimates by likelihood using deaths and exposures directly.

#as for beta, which measures how sensitive each age is to the time trend kappa we can see that Poisson has the most observable difference especially in old ages
#and this is because poisson deals with the low number of deaths and exposures at old ages differently to try to deal with the heteroscedasticity that the svd doesn't deal with directly.

# Residual diagnostics ----------------------------------------------------
# Compute residuals on a common log-mortality scale (since fit and residuals produce different outcomes)
# r_{x,t} = ln m_{x,t} - (a_x + b_x k_t)
for (sex in sexes) {
  # The log-mortality training data
  log_mx_train <- log(LC_UK[[sex]]$manual_train)
  # Any -Inf from log(0) or log(inf)
  log_mx_train[!is.finite(log_mx_train)]<-NA
  
  # Fitted values
  # SVD-none
  fit_none <- LC_UK[[sex]]$manual_lca_sett_none
  fitted_none <- outer(fit_none$ax,rep(1,length(fit_none$year))) + outer(fit_none$bx,fit_none$kt)
  rownames(fitted_none) <- fit_none$age
  colnames(fitted_none) <- fit_none$year
  
  # SVD-dt
  fit_dt <- LC_UK[[sex]]$manual_lca_sett_dt
  fitted_dt <- outer(fit_dt$ax,rep(1,length(fit_dt$year))) + outer(fit_dt$bx,fit_dt$kt)
  rownames(fitted_dt) <- fit_dt$age
  colnames(fitted_dt) <- fit_dt$year
  
  # Poisson (StMoMo returns ax as vector, bx as matrix, kt as matrix)
  fit_poi <- LC_UK[[sex]]$poisson_lc_fit
  ax_poi <- fit_poi$ax
  bx_poi <- fit_poi$bx[, 1]
  kt_poi <- fit_poi$kt[1, ]
  fitted_poi <- outer(ax_poi,rep(1,length(kt_poi)))+outer(bx_poi,kt_poi)
  rownames(fitted_poi) <- fit_poi$ages
  colnames(fitted_poi) <- fit_poi$years
  
  # Residuals on the common log-mortality scale
  LC_UK[[sex]]$residuals_none <- log_mx_train - fitted_none
  LC_UK[[sex]]$residuals_dt <- log_mx_train - fitted_dt
  LC_UK[[sex]]$residuals_poi <- log_mx_train - fitted_poi
}

# Residual heat maps with base R image()
plot_residual_heatmap <- function(residuals,title,zlim) {
  ages_r <- as.numeric(rownames(residuals))
  years_r <- as.numeric(colnames(residuals))
  image(
    x=years_r,
    y= ages_r,
    z= t(residuals),
    col= colorRampPalette(c("darkblue","white","red"))(100),
    zlim = zlim,
    xlab = "Year",
    ylab = "Age",
    main = title
  )
  box()
}

# For each sex, produce a three-panel side-by-side comparison.
# Colour scale is shared across the three panels so they are visually comparable.
#PLOT: Residual heat maps, males
#PLOT: Residual heat maps, females
for (sex in sexes) {
  all_r <- c(
    LC_UK[[sex]]$residuals_none,
    LC_UK[[sex]]$residuals_dt,
    LC_UK[[sex]]$residuals_poi
  )
  lim_sex <- quantile(abs(all_r), 0.99, na.rm = TRUE)
  zlim_sex <- c(-lim_sex, lim_sex)
  
  par(mfrow = c(1, 3))
  plot_residual_heatmap(
    LC_UK[[sex]]$residuals_none,
    paste("SVD unadjusted -", sex),
    zlim_sex
  )
  plot_residual_heatmap(
    LC_UK[[sex]]$residuals_dt,
    paste("SVD adjusted -", sex),
    zlim_sex
  )
  plot_residual_heatmap(
    LC_UK[[sex]]$residuals_poi,
    paste("Poisson -", sex),
    zlim_sex
  )
  
  mtext(
    "Red = observed above fitted; Blue = observed below fitted",
    side = 3,line = -1.5,outer = TRUE,cex = 0.8
  )
}

# PLOT: Residual standard deviation by age
par(mfrow=c(2,1))
for (sex in sexes) {
  sd_by_age_none <- apply(LC_UK[[sex]]$residuals_none, 1, sd, na.rm = TRUE)
  sd_by_age_dt   <- apply(LC_UK[[sex]]$residuals_dt,   1, sd, na.rm = TRUE)
  sd_by_age_poi  <- apply(LC_UK[[sex]]$residuals_poi,  1, sd, na.rm = TRUE)

  ages_r <- as.numeric(names(sd_by_age_none))
  ylim_sd <- range(sd_by_age_none, sd_by_age_dt, sd_by_age_poi,na.rm = TRUE)
  ylim_sd[2] <- ylim_sd[2] + 0.25 * diff(ylim_sd)
  plot(
    ages_r, sd_by_age_none,
    type = "l", lwd = 2,
    ylim = ylim_sd,
    xlab = "Age",
    ylab = "SD of residuals across years",
    main = paste("Residual SD by age -", sex))
  lines(ages_r, sd_by_age_dt,  lwd = 2, col = "red")
  lines(ages_r, sd_by_age_poi, lwd = 2, col = "darkblue")
  legend(
    "topleft",
    legend = c("SVD unadjusted", "SVD adjusted", "Poisson"),
    col = c("black", "red", "darkblue"),
    lwd = 2, bty = "n"
  )
  LC_UK[[sex]]$sd_by_age <- data.frame(
    age= ages_r,
    none= sd_by_age_none,
    dt= sd_by_age_dt,
    poi= sd_by_age_poi
  )
}

# Numerical summaries useful for the thesis
cat("\n--- Numerical summaries for the residual diagnostics section ---\n")
for (sex in sexes) {
  sd_df <- LC_UK[[sex]]$sd_by_age
  cat("\n",sex,"\n",sep = "")
  cat("Max residual SD across ages (unadjusted SVD): ",round(max(sd_df$none,na.rm = TRUE),4),"\n")
  cat("Max residual SD across ages (adjusted SVD): ",round(max(sd_df$dt,na.rm = TRUE),4),"\n")
  cat("Max residual SD across ages (Poisson): ",round(max(sd_df$poi,na.rm = TRUE),4),"\n")
  
  # Ratio at oldest age (90-100 average) vs middle age (50-70 average)
  # Higher ratio = more heteroscedastic
  old_none<- mean(sd_df$none[sd_df$age >= 90],na.rm = TRUE)
  mid_none<- mean(sd_df$none[sd_df$age >= 50 & sd_df$age <= 70],na.rm = TRUE)
  old_poi<- mean(sd_df$poi[sd_df$age >= 90],na.rm = TRUE)
  mid_poi<- mean(sd_df$poi[sd_df$age >= 50 & sd_df$age <= 70],na.rm = TRUE)
  cat("Heteroscedasticity ratio (old/mid), SVD-none: ",round(old_none/mid_none,2),"\n")
  cat("Heteroscedasticity ratio (old/mid), Poisson: ",round(old_poi/mid_poi,2),"\n")
}

# Chapter 5: Mortality Forecasting
# Section 5.1: ARIMA Forecasting of the Period Index
# Using the validation data -----------------------------------------------
# Forecast kappa_t using the validation data.

# Forecast kappa_t using a random walk with drift, i.e. ARIMA(0,1,0) with drift:
library(forecast)
kappa_methods <- c("svd_none", "svd_dt", "poisson")
?ts
?auto.arima
?forecast
?Arima
for (sex in sexes) { 
  kappa_list<- list(
    svd_none= as.numeric(LC_UK[[sex]]$manual_lca_sett_none$kt),
    svd_dt= as.numeric(LC_UK[[sex]]$manual_lca_sett_dt$kt),
    poisson= as.numeric(LC_UK[[sex]]$poisson_lc_fit$kt[1,])
  )
  
  LC_UK[[sex]]$kappa_forecast <- list()
  for (m in kappa_methods){
    kt_ts <- ts(kappa_list[[m]],start = years_train[1],frequency = 1)
    # Compare the imposed RWD with an automatically selected ARIMA model using d = 1 to maintain the normality and the one time parameter
    aa <- auto.arima(kt_ts,d = 1,max.p = 3,max.q = 3,seasonal = FALSE,trace=TRUE,allowdrift = TRUE)
    
    # Fit the ARIMA(0,1,0) model with drift.
    rwd_fit<- Arima(kt_ts,order = c(0,1,0),include.drift = TRUE)
    theta_hat<- unname(coef(rwd_fit)["drift"])
    sigma_hat<- sqrt(rwd_fit$sigma2)
    
    # Check the fitted drift against the closed-form RWD estimate.
    theta_closed_form <- (kappa_list[[m]][years_train_length] - kappa_list[[m]][1]) /(years_train_length-1)
    
    # Forecast the five validation years and three stress-test years.
    h_total<- years_valid_length + years_stress_length
    fc <- forecast(rwd_fit,h = h_total,level = c(80, 95))
    
    LC_UK[[sex]]$kappa_forecast[[m]] <- list(
      auto_arima_pick = aa,
      fit= rwd_fit,
      theta= theta_hat,
      theta_closed_form = theta_closed_form,
      sigma = sigma_hat,
      fc = fc
    )
    cat("\n---", sex, "-", m, "---\n")
    cat("auto.arima picked: ", paste(arimaorder(aa), collapse = ",")," | our model: 0,1,0 + drift\n")
    cat("drift  =", round(theta_hat, 5),"(closed form:", round(theta_closed_form, 5), ")\n")
    cat("sigma  =", round(sigma_hat, 5), "\n")
  }
}


# Forecasting Chapter ------------------------------------------------------
# Validation forecasting and model choice ----------------------------------
# ARIMA(0,1,0)+drift forecast of kappa_t, validation, LC
library(forecast)
# ARIMA forecasting of the period index 
# Define model labels and evaluation windows.
methods <- c("svd_none","svd_dt","poisson")
method_labels <- c(
  svd_none="SVD unadjusted",
  svd_dt="SVD adjusted",
  poisson="Poisson MLE"
)
method_cols <- c(
  svd_none="black",
  svd_dt="red",
  poisson="darkblue"
)
years_fc<- c(years_valid,years_stress)      # 2015-2022
length_fc<- length(years_fc)                  # 8 = 5 validation + 3 stress
n_sim<- 10000               # to obtain confiidence intervals 
set.seed(20260727)

# build the structure to pull (ax,bx,kt) out of the three different fit 
get_lc_params <- function(sex, method) {
  if (method == "svd_none") {
    f <- LC_UK[[sex]]$manual_lca_sett_none
    out <- list(
      ax= as.numeric(f$ax),bx= as.numeric(f$bx),kt = as.numeric(f$kt),
      ages= as.numeric(f$age),years= as.numeric(f$year)
    )
  } 
  else if (method == "svd_dt") {
    f <- LC_UK[[sex]]$manual_lca_sett_dt
    out <- list(
      ax= as.numeric(f$ax),bx= as.numeric(f$bx),kt= as.numeric(f$kt),
      ages= as.numeric(f$age),years= as.numeric(f$year)
    )
  } 
  else if (method == "poisson") {
    f<- LC_UK[[sex]]$poisson_lc_fit
    out<- list(
      ax= as.numeric(f$ax),bx = as.numeric(f$bx[,1]),
      kt= as.numeric(f$kt[1,]),
      ages= as.numeric(f$ages),years = as.numeric(f$years)
    )
  } 
  else {stop("unknown method: ",method)}
  names(out$ax)<- names(out$bx) <- as.character(out$ages)
  names(out$kt)<- as.character(out$years)
  out
}

# Align mortality matrices and fitted parameters by age.
align_ages <- function(mat,ages) mat[as.character(ages), ,drop = FALSE]

# Convert mortality rates to finite log rates.
log_rate_matrix <- function(mx) {
  lm<- log(mx)
  lm[!is.finite(lm)] <- NA
  lm}

# Calculating out-of-sample implied kappa_t by least squares with ax and bx fixed.
kappa_implied <- function(log_obs,ax,bx) {
  apply(log_obs,2,function(col) {
    temp <- is.finite(col)
    if (!any(temp)) return(NA_real_)
    sum(bx[temp] * (col[temp] - ax[temp]))/sum(bx[temp]^2)
  })
}
# Calculating errors on the log-mortality and death-count scales also use Poisson deviance as a secondary metric.
forecast_errors<- function(log_obs, log_fore,D_obs,E_obs) {
  e<- log_fore - log_obs
  temp<- is.finite(e)
  mx_obs<- exp(log_obs)
  mx_fc<- exp(log_fore)
  D_fc<- E_obs * mx_fc
  # Use valid observations only.
  temp_d<- is.finite(D_obs) & is.finite(D_fc) & E_obs > 0
  d_obs<- D_obs[temp_d]
  d_fc<- D_fc[temp_d]
  # Handle zero observed deaths separately in the Poisson deviance.
  dev_terms<- ifelse(
    d_obs > 0,
    d_obs*log(d_obs/d_fc)-(d_obs-d_fc),
    d_fc
  )
  
  c(
    MAE_log= mean(abs(e[temp])), #only used to compare the errors without the penalization for extremes 
    RMSE_log= sqrt(mean(e[temp]^2)),
    Poisson_deviance= 2 * sum(dev_terms),
    n_cells= sum(temp)
  )
}

# Construcing the forecasted log mortality function from the kappa paths, then we compute prediction intervals from them.
rates_from_kappa<- function(ax,bx,ages,kt_fc_mean,sims_kt,fc_years) {
  #the log mortality construction
  log_mx_hat<- outer(bx,kt_fc_mean) + ax
  dimnames(log_mx_hat) <- list(as.character(ages),as.character(fc_years))
  #the prediction interval
  pl80<- ph80 <- pl95 <- ph95 <- log_mx_hat
  pl80[]<- ph80[] <- pl95[] <- ph95[] <- NA_real_
  
  for (i in seq_len(length(ages))) {
    sim_lm<- ax[i] + bx[i] * sims_kt  
    qs<- apply(sim_lm,2,quantile,probs = c(0.025,0.10,0.90,0.975),na.rm = TRUE)
    pl95[i,]<- qs[1,]
    pl80[i,]<- qs[2,]
    ph80[i,]<- qs[3,]
    ph95[i,]<- qs[4,]
  }
  list(
    ages = ages,years = fc_years,log_mx_fc = log_mx_hat,
    prediction_l80 = pl80,prediction_h80 = ph80,prediction_l95 = pl95,prediction_h95 = ph95
  )
}

# Build the mortality forecasts -----------------------------------------------------
# Impose the RWD specification and retain auto-ARIMA as a robustness check.
for (sex in sexes) {
  #define an empty list to fill
  LC_UK[[sex]]$kappa_forecast <- list()
  for (m in methods) {
    p<- get_lc_params(sex, m)
    kt_ts<- ts(p$kt,start = min(p$years),frequency = 1)
    # Select an ARIMA model from the data for comparison.
    aa <- auto.arima(
      kt_ts,d = 1,max.p = 3,max.q = 3,
      seasonal= FALSE,allowdrift = TRUE,trace = FALSE)
    # Fit the RWD used in the thesis.
    rwd<- Arima(kt_ts, order = c(0,1,0),include.drift = TRUE)
    theta<- unname(coef(rwd)["drift"])
    se_theta<- sqrt(rwd$var.coef["drift","drift"]) # standard error in drift
    sigma<- sqrt(rwd$sigma2) # the size of the future yearly random shocks ε
    # Check the maximum-likelihood drift against the endpoint estimate.
    theta_cf <- (p$kt[length(p$kt)] - p$kt[1])/(length(p$kt) - 1)
    fc <- forecast(rwd,h = length_fc,level = c(80,95))
    
    # Simulate 10,000 kappa paths from the fitted RWD.
    kt_last<- p$kt[length(p$kt)]
    theta_sim<- rnorm(n_sim,mean = theta,sd = se_theta) #drift simulations per path , uncertainty in the true average annual change in κ
    eps <- matrix(rnorm(n_sim * length_fc,0,sigma),n_sim,length_fc) # epsilon simulation, yearly shocks, per path per year
    sims<- kt_last + outer(theta_sim,seq_len(length_fc)) + t(apply(eps,1,cumsum))
    colnames(sims) <- as.character(years_fc)
    
    LC_UK[[sex]]$kappa_forecast[[m]] <- list(
      auto_arima_pick= aa,
      auto_arima_order= arimaorder(aa),
      fited= rwd,
      theta= theta,
      se_theta= se_theta,
      theta_closed_form= theta_cf,
      sigma= sigma,
      fc= fc,
      kt_fc= as.numeric(fc$mean),
      sims= sims)
    
    cat("\n---",sex,"-",method_labels[m],"---\n")
    cat("auto.arima picked:",paste(arimaorder(aa),collapse = ","),"| model used: (0,1,0) + drift\n")
    cat("drift =",round(theta,4),
        " (form:",round(theta_cf,4),", s.e.:",round(se_theta,4),")\n")
    cat("sigma =",round(sigma,4),"\n")
  }
}
# Comment:the results show a really close drift values for both males and females
# suggesting that estimated long-term downward trend kappa is independent of the sex and modelling methods
# Comment:we can see that none of the auto arimas gave the (0,1,0) result, it preferred a more short term dynamic
# higher orders of autoregressive and moving-average were used.

# we reconstruct the forecasted log-mortality for all ages 
for (sex in sexes) {
  LC_UK[[sex]]$forecast_lm<- list()
  for (m in methods) {
    p<- get_lc_params(sex,m)
    kf<- LC_UK[[sex]]$kappa_forecast[[m]]
    LC_UK[[sex]]$forecast_lm[[m]]<- rates_from_kappa(
      ax= p$ax,bx=p$bx,ages= p$ages,kt_fc_mean= kf$kt_fc,sims_kt= kf$sims,fc_years= years_fc
    )}}
LC_UK$Female$forecast_lm
#extract the observed mortality rates (manually computed) in addition to deaths and exposures  for the error matric
obs_data <- list()
for (sex in sexes) {
  ages_sex <- get_lc_params(sex,"svd_none")$ages
  obs_data[[sex]] <- list(
    valid= list(
      log_mx= log_rate_matrix(align_ages(LC_UK[[sex]]$manual_valid,ages_sex)),
      D= align_ages(LC_UK[[sex]]$P_D_valid,ages_sex),
      E= align_ages(LC_UK[[sex]]$P_E_valid,ages_sex),
      years = years_valid),
    stress= list(
      log_mx = log_rate_matrix(align_ages(LC_UK[[sex]]$manual_stress,ages_sex)),
      D = align_ages(LC_UK[[sex]]$P_D_stress,ages_sex),
      E= align_ages(LC_UK[[sex]]$P_E_stress,ages_sex),
      years= years_stress)
  )
}

# Error metrics and prediction interval
metrics_rows<- list()
prediction_rows <- list()
kappa_rows<- list()

args(forecast_errors)
for (sex in sexes) {
  for (m in methods) {
    p<- get_lc_params(sex, m)
    fl<- LC_UK[[sex]]$forecast_lm[[m]]
    for (dat in c("valid", "stress")) {
      yrs<- as.character(obs_data[[sex]][[dat]]$years)
      log_obs<- obs_data[[sex]][[dat]]$log_mx
  
      # Check that all evaluation years exist in the forecast
      stopifnot(all(yrs %in% colnames(fl$log_mx_fc)))
      log_hat<- fl$log_mx_fc[, yrs, drop = FALSE]
      
      # Check if observed and forecast have identical dimensions
      stopifnot(identical(dim(log_obs), dim(log_hat)))
      
      em <- forecast_errors(
        log_obs= log_obs,
        log_fore= log_hat,
        D_obs= obs_data[[sex]][[dat]]$D,
        E_obs= obs_data[[sex]][[dat]]$E
      )
      metrics_rows[[length(metrics_rows) +1]] <- data.frame(
        sex= sex,
        method= method_labels[m],
        ts_model="RWD",
        window=dat,
        t(em),
        row.names= NULL,
        check.names= FALSE
      )
      
      # Check whether observed log-mortality is inside prediction intervals
      in80<- log_obs>= fl$prediction_l80[,yrs,drop = FALSE] & log_obs<= fl$prediction_h80[,yrs,drop = FALSE]
      in95<- log_obs>= fl$prediction_l95[,yrs,drop = FALSE] & log_obs<= fl$prediction_h95[,yrs,drop = FALSE]
      
      prediction_rows[[length(prediction_rows) +1]] <- data.frame(
        sex = sex,method= method_labels[m],
        ts_model= "RWD",window= dat,
        cover80= 100 * mean(in80,na.rm = TRUE),
        cover95= 100 * mean(in95,na.rm = TRUE),
        row.names= NULL)
      
      k_imp<- kappa_implied(log_obs,p$ax,p$bx)
      k_hat<- LC_UK[[sex]]$kappa_forecast[[m]]$kt_fc[match(yrs,as.character(years_fc))]
      #check again for dimensions
      stopifnot(length(k_hat) == length(k_imp))
      #the kappa 
      kappa_rows[[length(kappa_rows) +1]] <- data.frame(
        sex= sex,method= method_labels[m],
        ts_model= "RWD",window= dat,
        year= as.numeric(yrs),
        kappa_implied= as.numeric(k_imp),
        kappa_forecast= as.numeric(k_hat),
        error= as.numeric(k_hat - k_imp),
        row.names= NULL)
      }}}

metrics_tab<- do.call(rbind, metrics_rows)
prediction_tab<- do.call(rbind, prediction_rows)
kappa_tab<- do.call(rbind, kappa_rows)

# storing implied kappas for plotting and comparison 
for (sex in sexes) {
  LC_UK[[sex]]$kappa_implied_oos <- list()
  for (m in kappa_methods) {
    p <- get_lc_params(sex,m)
    LC_UK[[sex]]$kappa_implied_oos[[m]] <- c(
      kappa_implied(obs_data[[sex]]$valid$log_mx,p$ax,p$bx),
      kappa_implied(obs_data[[sex]]$stress$log_mx,p$ax,p$bx)
    )}}

# storing errors by age and years of the validation 
for (sex in sexes) {
  ages_sex<-get_lc_params(sex,"svd_none")$ages
  yrs<-as.character(years_valid)
  log_obs<-obs_data[[sex]]$valid$log_mx
  
  by_age<- data.frame(age= ages_sex)
  by_year<- data.frame(year= years_valid,h= seq_along(years_valid))
  
  for (m in methods) {
    e<- LC_UK[[sex]]$forecast_lm[[m]]$log_mx_fc[,yrs,drop = FALSE] - log_obs
    by_age[[m]]<- apply(e,1, function(v) sqrt(mean(v^2,na.rm = TRUE)))
    by_year[[m]]<- apply(e,2, function(v) sqrt(mean(v^2,na.rm = TRUE)))
  }
  
  LC_UK[[sex]]$valid_rmse_by_age<- by_age
  LC_UK[[sex]]$valid_rmse_by_year<- by_year
}

#plots
#comparing implied with forecasted
#PLOT: RWD forecast fans and implied period index, males
#PLOT: RWD forecast fans and implied period index, females 
par(mfrow = c(3,1))             
for (sex in sexes) {
  for (m in methods) {
    p<- get_lc_params(sex,m)
    kf<- LC_UK[[sex]]$kappa_forecast[[m]]
    k_imp<- LC_UK[[sex]]$kappa_implied_oos[[m]]
    lo95<- apply(kf$sims,2,quantile,0.025)
    hi95<- apply(kf$sims,2,quantile,0.975)
    lo80<- apply(kf$sims,2,quantile,0.10)
    hi80<- apply(kf$sims,2,quantile,0.90)
    #fitted kappa
    plot(
      p$years,p$kt,type = "l",lwd = 2,
      xlim = range(c(p$years,years_fc)),
      ylim = range(p$kt,lo95,hi95,k_imp,na.rm = TRUE),
      xlab = "Year",ylab = expression(kappa[t]),
      main = paste0(method_labels[m]," - ",sex)
    )
    #forecasted kappa confidence intervals
    polygon(c(years_fc, rev(years_fc)),c(lo95, rev(hi95)),
            col = rgb(0,0,0.5,0.15))
    polygon(c(years_fc, rev(years_fc)),c(lo80,rev(hi80)),
            col = rgb(0,0,0.5,0.25))
    lines(years_fc,kf$kt_fc,lwd = 2,col = "darkblue")
    points(years_fc,k_imp,pch = 19,cex = 0.8,col = "red")
    abline(v = 2014.5,lty = 2); abline(v = 2019.5,lty = 3)
    legend("bottomleft", bty = "n",cex = 1,
           legend = c("fitted","RWD forecast","implied by data"),
           col = c("black","darkblue","red"),
           lwd = c(2,2,NA),pch = c(NA,NA,19))
  }
}
#zoomed in version 
par(mfrow=c(3,1))
for (sex in sexes){
  for (m in methods){
    p<-get_lc_params(sex,m)
    kf<-LC_UK[[sex]]$kappa_forecast[[m]]
    k_imp<-LC_UK[[sex]]$kappa_implied_oos[[m]]
    lo95<-apply(kf$sims,2,quantile,0.025)
    hi95<-apply(kf$sims,2,quantile,0.975)
    lo80<-apply(kf$sims,2,quantile,0.10)
    hi80<-apply(kf$sims,2,quantile,0.90)
    
    keep_fit<- p$years>=2013

    plot(p$years[keep_fit],p$kt[keep_fit],type="l",lwd=2,
      xlim=c(2013,max(years_fc)),
      ylim=range(p$kt[keep_fit],lo95,hi95,k_imp,na.rm=TRUE),
      xlab="Year",ylab=expression(kappa[t]),
      main=paste0(method_labels[m]," - ",sex))
    last_year<-max(p$years)
    last_kappa<-tail(p$kt,1)
    years_fc_plot<-c(last_year,years_fc)
    kt_fc_plot<-c(last_kappa,kf$kt_fc)
    lo95_plot<-c(last_kappa,lo95)
    hi95_plot<-c(last_kappa,hi95)
    lo80_plot<-c(last_kappa,lo80)
    hi80_plot<-c(last_kappa,hi80)
    
    polygon(c(years_fc_plot,rev(years_fc_plot)),c(lo95_plot,rev(hi95_plot)),
            col=rgb(0,0,0.5,0.15))
    polygon(c(years_fc_plot,rev(years_fc_plot)),c(lo80_plot,rev(hi80_plot)),
            col=rgb(0,0,0.5,0.25))
    lines(years_fc_plot,kt_fc_plot,lwd=2,col="darkblue")
    points(years_fc,k_imp,pch=19,cex=0.8,col="red")
    
    abline(v=2014.5,lty=2)
    abline(v=2019.5,lty=3)
    legend("bottomleft",bty="n",cex=1,
      legend=c("fitted","RWD forecast","implied by data"),
      col=c("black","darkblue","red"),
      lwd=c(2,2,NA),
      pch=c(NA,NA,19)
    )
  }
}

# observed vs forecast log-mortality at selected old ages
ages_show <- c(55,65,75,85,95,100)
par(mfrow = c(2,3))
for (sex in sexes) {
  ages_sex <- get_lc_params(sex,"svd_none")$ages
  lm_full <- log_rate_matrix(LC_UK[[sex]]$manual_mx_full[as.character(ages_sex), ,drop = FALSE])
  yrs_hist <- as.numeric(colnames(lm_full))
  
  par(mfrow = c(3,2))
  for (a in ages_show) {
    ia<- match(as.character(a),rownames(lm_full))
    fl<- LC_UK[[sex]]$forecast_lm[["poisson"]]
    ia_f<- match(as.character(a),rownames(fl$log_mx_fc))
    
    plot(
      yrs_hist,lm_full[ia,],type = "l", lwd = 1.5,
      ylim= range(lm_full[ia,],fl$prediction_l95[ia_f,],fl$prediction_h95[ia_f,],na.rm= TRUE),
      xlab= "Year",ylab= expression(ln~m[x*t]),
      main= paste0(sex,", age ",a)
    )
    polygon(c(years_fc,rev(years_fc)),
            c(fl$prediction_l95[ia_f,],rev(fl$prediction_h95[ia_f,])),
            col = rgb(0,0,0.5,0.15),border = NA)
    for (m in methods) {
      lines(years_fc,LC_UK[[sex]]$forecast_lm[[m]]$log_mx_fc[ia_f,],
            lwd = 2,col = method_cols[m])
    }
    abline(v=2014.5,lty = 2); abline(v = 2019.5,lty = 3)
    legend("bottomleft",legend = method_labels,col = method_cols,
           lwd = 2,bty = "n",cex = 0.7)
  }
}
par(old_par)

#PLOT: Validation RMSE by age and forecast horizon
par(mfrow = c(2, 2))
for (sex in sexes) {
  ba<- LC_UK[[sex]]$valid_rmse_by_age
  bh<- LC_UK[[sex]]$valid_rmse_by_year
  
  matplot(ba$age,as.matrix(ba[,methods]),type ="l",lty=1,
          lwd= 2,col= method_cols[methods],
          xlab= "Age",ylab= "RMSE of ln m",
          main= paste("Validation RMSE by age -",sex))
  legend("topleft",legend = method_labels,col = method_cols,lwd=2,bty="n",cex = 0.7)
  
  matplot(bh$year,as.matrix(bh[,methods]),type="b",lty=1,
          lwd= 2,pch = 19,col= method_cols[methods],
          xlab= "Year",ylab= "RMSE of ln m",
          main= paste("Validation RMSE by horizon -",sex))
  legend("topleft",legend=method_labels,col=method_cols,lwd=2,bty="n",cex=0.7)
}
par(old_par)

# Model selection on the validation window
selection <- list()
for (sex in sexes) {
  sub<- metrics_tab[metrics_tab$sex == sex & metrics_tab$window == "valid" & metrics_tab$ts_model == "RWD",]
  sub$method_key<-methods[match(sub$method,method_labels[methods])]
  best_rmse<- sub$method_key[which.min(sub$RMSE_log)]
  best_mae<- sub$method_key[which.min(sub$MAE_log)]
  best_dev<- sub$method_key[which.min(sub$Poisson_deviance)]
  
  selection[[sex]] <- list(
    table = sub,best_by_rmse=best_rmse,best_bymae=best_mae,
    best_by_devian=best_dev,selected=best_rmse)
  LC_UK[[sex]]$selected_method<- best_rmse
  
  cat("LC model selection,",sex)
  cat("lowest validation RMSE (log scale):",method_labels[best_rmse], "\n")
  cat("lowest validation Poisson deviance:",method_labels[best_dev], "\n")
  cat("lowest validation best_mae:",method_labels[best_mae], "\n")
}

# Validation RMSE restricted to bond-relevant ages
bond_ages<- ages_to_fit[ages_to_fit >= 65]
rmse_65plus_rows<- list()

for (sex in sexes) {
  log_obs_all<- obs_data[[sex]]$valid$log_mx
  yrs <- as.character(years_valid)
  for (m in methods) {
    log_hat_all<-LC_UK[[sex]]$forecast_lm[[m]]$log_mx_fc[,yrs,drop = FALSE]
    # Keep only ages 65+
    ages_use <- intersect(as.character(bond_ages),
                          intersect(rownames(log_obs_all),rownames(log_hat_all)) )
    if (length(ages_use)== 0) {stop("No bond-relevant ages found ",sex, ", ",m)}
    
    log_obs_65<- log_obs_all[ages_use,yrs,drop = FALSE]
    log_hat_65<- log_hat_all[ages_use,yrs,drop = FALSE]
    error_65<- log_hat_65-log_obs_65
    
    # Exclude non-finite values
    keep<-is.finite(error_65)
    rmse_65plus<- sqrt(mean(error_65[keep]^2))
    mae_65plus <- mean(abs(error_65[keep]))
    rmse_65plus_rows[[length(rmse_65plus_rows) +1]] <-
      data.frame(sex = sex,method_key= m,
                 method= method_labels[m],age_range= paste0(min(as.numeric(ages_use)),"-", max(as.numeric(ages_use))),
                 MAE_65plus = mae_65plus,RMSE_65plus = rmse_65plus,
                 n_cells = sum(keep),row.names = NULL
      )}}
rmse_65plus_tab<- do.call(rbind,rmse_65plus_rows)

# Compare overall validation RMSE with age-65+ validation RMSE
overall_valid<- metrics_tab[ metrics_tab$window == "valid" & metrics_tab$ts_model == "RWD",
                              c("sex","method","RMSE_log")]
names(overall_valid)[names(overall_valid) == "RMSE_log"] <-"RMSE_50_100"

rmse_comparison<- merge(overall_valid,
                         rmse_65plus_tab[,c("sex","method","RMSE_65plus")],
                         by = c("sex","method"),
                         sort = FALSE)

#checking if there is any outliers for Log mx
log_error_rows<-list()
for(sex in sexes){
  for(m in methods){
    fl<-LC_UK[[sex]]$forecast_lm[[m]]
    for(dat in c("valid","stress")){
      yrs<-as.character(obs_data[[sex]][[dat]]$years)
      log_obs<-obs_data[[sex]][[dat]]$log_mx
      log_hat<-fl$log_mx_fc[,yrs,drop=FALSE]
      log_error<-log_hat-log_obs
      
      temp<-expand.grid(
        age=as.numeric(rownames(log_error)),
        year=as.numeric(colnames(log_error)))
      temp$error<-as.numeric(log_error)
      temp<-temp[is.finite(temp$error),]
      temp$sex<-sex
      temp$method<-method_labels[m]
      temp$window<-dat
      
      log_error_rows[[length(log_error_rows)+1]]<-temp
    }}}

log_error_tab<-do.call(rbind,log_error_rows)
rownames(log_error_tab)<-NULL
groups<-split(
  log_error_tab,
  list(log_error_tab$sex,log_error_tab$method,log_error_tab$window),
  drop=TRUE)
outlier_summary<-do.call(rbind,lapply(groups,function(d){
  Q1<-quantile(d$error,0.25,na.rm=TRUE)
  Q3<-quantile(d$error,0.75,na.rm=TRUE)
  IQR_error<-IQR(d$error,na.rm=TRUE)
  lower<-Q1-1.5*IQR_error
  upper<-Q3+1.5*IQR_error
  is_outlier<-d$error<lower | d$error>upper
  i<-which.max(abs(d$error))
  data.frame(
    sex=d$sex[1],
    method=d$method[1],
    window=d$window[1],
    Q1=Q1,
    Q3=Q3,
    IQR=IQR_error,
    lower=lower,
    upper=upper,
    n_outliers=sum(is_outlier),
    max_abs_error=abs(d$error[i]),
    max_error=d$error[i],
    age_max=d$age[i],
    year_max=d$year[i])
}))
rownames(outlier_summary)<-NULL
outlier_summary

log_outliers<-do.call(rbind,lapply(groups,function(d){
  Q1<-quantile(d$error,0.25,na.rm=TRUE)
  Q3<-quantile(d$error,0.75,na.rm=TRUE)
  IQR_error<-IQR(d$error,na.rm=TRUE)
  lower<-Q1-1.5*IQR_error
  upper<-Q3+1.5*IQR_error
  d[d$error<lower | d$error>upper,]
}))

log_outliers<-log_outliers[order(-abs(log_outliers$error)),]
rownames(log_outliers)<-NULL
log_outliers

# Chapter 5: Mortality Forecasting
# Section 5.2.2: LC-LSTM Based Forecasting, Training Setup and Hyperparameters setup
# LSTM Implementation -----------------------------------------------------
library(keras3) 
library(tseries) #ADF and Jarque-Bera tests
# Use the Poisson Lee-Carter fit as the forecasting benchmark.
benchmark_method <- "poisson"
lstm_lc_method <- benchmark_method

# Tuning the LC-LSTM over lags 1-5, lag 1 matches the one-step structure used in the reference paper and concides witht he RWD up to dome limit.
lag_grid <- 1:5
# Tuning the hidden units and learning rate for the single-layer reference architecture.
units_grid<- c(4,8,16,32,45) #number of neurons in the hidden layer
lr_grid<- c(0.05,0.01,0.005,0.001) 
# Using a fixed subtraining-validation split for hyperparameter tuning.
train_split<- 0.85        # 85% subtraining and the rest are validation
n_epochs<- 500 #the data can enter fully 500 times 
patience<- 50 # we will wait for 50 consecutive times with no improvement to early stop the training
batch_size<- 9  # it will reweight 6 times within each train

# Bagging the paper uses B = 1000
B_boot<- 500
boot_epochs<- 250
boot_refit<- "poisson"
boot_type<- "deviance" # resample deviance residuals and then get the deaths out of them 
alpha_level<- 0.05       # for 95% PI
z_alpha<-qnorm(1 - alpha_level/2) #critical value 

# Accumulate forecast noise through a random-walk representation.
noise_random_walk<-TRUE
base_seed<-20260727

# functions that will be needed
# Standardise kappa for training and reverse the scaling after forecasting.
std_fit <- function(x) {list(mean = mean(x),sd= sd(x))}
std_apply <- function(x, s) {(x - s$mean)/s$sd}
std_inv <- function(z, s) {z*s$sd + s$mean}

# create the lagged data set in j=1 case same as p=0 in the ARIMA 
make_lagged <- function(z,lag) { #we will get the lagged values in x and the corresponding observed kappa as y 
  n<- length(z)-lag #number of observations 
  X<-matrix(NA_real_,n,lag)
  for (i in seq_len(n)) X[i,] <- z[i:(i +lag-1)]
  list(X=array(X,dim =c(n,lag,1)), 
       y=z[(lag + 1):length(z)],n=n)
}
?keras_model_sequential
# Use one LSTM layer with ReLU activation, tanh recurrence, and a linear output layer.
build_lstm <- function(lag,units,lr) {
# 1 means one feature at each year: kappa
  model<-keras_model_sequential(input_shape=c(lag,1))
  # hidden LSTM layer
  model$add(layer_lstm(units = units,
      activation = "relu",recurrent_activation="tanh"))
  # output layer
  model$add(layer_dense(units = 1,activation="linear"))
  # the training procedure and updates
  model$compile(optimizer = optimizer_adam(learning_rate = lr),loss = "mse")
  return(model)
}
# a recursive prediction function over the forecast years h,
recursive_kappa <- function(model, finalobs , h) {
  W<-matrix(finalobs, nrow = 1)
  temp<-numeric(h)
  for (j in seq_len(h)) {
    p<-as.numeric(predict(model, array(W, dim = c(1, ncol(W), 1))))
    temp[j] <- p
    W<- cbind(W[,-1, drop = FALSE],p) #sinilar to a running mean the window moves
  }
  temp
}

# Statistical tests
# Skewness component: D'Agostino (1970). 
# Kurtosis component: Anscombe & Glynn (1983).
# D'Agostino function
dagostino_pearson_test<-function(x) {
  x<-x[is.finite(x)]
  n<-length(x)
  m2<-mean((x-mean(x))^2)
  m3<-mean((x-mean(x))^3)
  m4<-mean((x-mean(x))^4)
  # skewness
  b1<- m3/m2^(3/2)
  Y<- b1*sqrt((n+1)*(n+3)/(6*(n-2))) #account for sampling variability
  b2s<-(3*(n^2+(27*n)-70)*(n + 1)*(n + 3))/
    ((n-2)*(n+5)*(n+7)*(n+9))
  W2<-sqrt(2*(b2s-1)) - 1 
  del<- 1/sqrt(log(sqrt(W2)))
  alp<- sqrt(2/(W2-1))
  #the test statistic: if around 0 approximately  then normal no skewness
  Z1<- del*log(Y/alp+sqrt((Y/alp)^2+1))
  
  # kurtosis
  b2<-m4/ m2^2
  Eb2<-3*(n-1)/(n+1)
  Vb2<-(24*n*(n-2)*(n-3))/((n+1)^2*(n+3)*(n+5))
  Xk<- (b2-Eb2)/sqrt(Vb2)
  sb1<- 6*(n^2- 5*n +2)/((n+7)*(n+9)) *
    sqrt(6*(n+3)*(n+5)/(n*(n-2)*(n-3)))
  A <- 6+ (8/sb1)*(2/sb1+sqrt(1+ 4/sb1^2))
  Z2<-((1-2/(9*A)) - ((1- 2/A)/(1+Xk*sqrt(2/(A-4))))^(1/3))/sqrt(2/(9*A))
  
  K2 <- Z1^2 + Z2^2 # the combined test statistic  with two degrees of freedom since we have 2 summed values
  list(statistic = K2, 
       p.value=pchisq(K2,df = 2,lower.tail = FALSE),
       Z_skew=Z1, 
       Z_kurt=Z2)
}

# Reconstruct bootstrap deaths from resampled Poisson deviance residuals using bisection.
invert_deviance<-function(r_star,D_obs,iter=60){
  target<-r_star^2
  lo<-ifelse(r_star>0,1e-10,D_obs)
  hi<-ifelse(r_star>0,D_obs,pmax(D_obs*4,D_obs+10))
  
  # Poisson deviance function
  dev_of<-function(X){
    2*(ifelse(D_obs>0,D_obs*log(D_obs/X),0)-(D_obs-X))
  }
  
  # Grow the upper bracket where needed
  for(k in 1:40){
    need<-r_star<0&dev_of(hi)<target
    if(!any(need,na.rm=TRUE))break
    hi[need]<-D_obs[need]+2*(hi[need]-D_obs[need])
  }
  
  for(k in seq_len(iter)){
    mid<-(lo+hi)/2
    dm<-dev_of(mid)
    move_right<-(r_star>0&dm>target)|(r_star<0&dm<target)
    lo<-ifelse(move_right,mid,lo)
    hi<-ifelse(move_right,hi,mid)
  }
  (lo+hi)/2
}

# One bootstrap replicate of the kappa series.
bootstrap_kappa <- function(D,E,ax,bx,kt,ages,years,
                            type = boot_type,refit = boot_refit) {
  #FITTED D'S
  D_hat<- E*exp(outer(ax,rep(1,length(kt)))+ outer(bx,kt))
  D_hat[!is.finite(D_hat) | D_hat <= 0] <- NA
  # standard Poisson deviance contribution for every age-year cell
  dev <- 2*(ifelse(D > 0, D*log(D/D_hat), 0) -(D-D_hat))
  # remove deviance resulting from rounding since poisson won't result in a negative dev
  dev[dev<0] <- 0
  # signed Poisson deviance residuals
  r <- sign(D-D_hat) * sqrt(dev)
  # identify valid residuals just in case dev =0 and we got a perfect fit 
  ok <- is.finite(r) & is.finite(D) & D>=0
  
  # resample residuals with replacement
  r_star<-r
  r_star[ok]<-sample(r[ok],size = sum(ok),replace = TRUE)
  #construct the bootstrap death matrix
  D_star<-D
  D_star[ok]<-invert_deviance(r_star=r_star[ok],D_obs = D[ok])
  dimnames(D_star)<-list(as.character(ages),as.character(years))
  
  # refit the Poisson Lee-Carter model
  f <- StMoMo::fit(lc(link="log",const="sum"),
      Dxt=D_star,Ext=E,
      ages=ages,years=years,
      ages.fit=ages,years.fit=years,
      verbose=FALSE)
  # return the bootstrap kappa series
  as.numeric(f$kt[1,])
  }
  
# Chapter 5: Mortality Forecasting
# Section 5.2.2: LC-LSTM Based Forecasting, Training Setup and Hyperparameters implementation
# Split the training window 85:15 for tuning without using the test years.
lstm_setup<-list()
for (sex in sexes) {
  p<-get_lc_params(sex,lstm_lc_method)
  kt<-p$kt
  s<-std_fit(kt)
  z<-std_apply(kt,s)
  
  n_tr<-length(kt)
  n_train_sub<-floor(train_split * n_tr)
  
  lstm_setup[[sex]] <- list(
    lc_method = lstm_lc_method, ax = p$ax, bx = p$bx, ages = p$ages,
    years_tr = p$years, kt = kt, scaler = s, z = z,
    n_train_sub = n_train_sub,
    years_sub = p$years[seq_len(n_train_sub)],
    years_vs  = p$years[(n_train_sub + 1):n_tr]
  )
  cat(sex, min(p$years), max(p$years), n_tr,
      min(p$years), p$years[n_train_sub], p$years[n_train_sub + 1], max(p$years),
      min(years_fc), max(years_fc))
}

# The 65-year training window exceeds the reference paper's 40-observation minimum.
#Grid search on TR_sub
grid_rows<-list()
lstm_best<-list()

for(sex in sexes){
  st<-lstm_setup[[sex]]
  scaler_tune<-std_fit(st$kt[seq_len(st$n_train_sub)]) #only use the sub training data to find mean and variance so no leakage involved
  z_tune<-std_apply(st$kt,scaler_tune) #then we apply on all training data sub and validation sub
  best<-list(mse=Inf)
  for(lag in lag_grid){
    d<-make_lagged(z_tune,lag)
    tgt_year<-st$years_tr[(lag+1):length(z_tune)]
    i_sub<-which(tgt_year<=max(st$years_sub))
    i_vs<-which(tgt_year>max(st$years_sub))
    
    for(u in units_grid){
      for(lr in lr_grid){
        set_random_seed(base_seed)
        model<-build_lstm(lag,u,lr)
        # choose based on the split 
        x_sub<-d$X[i_sub,,,drop=FALSE]
        y_sub<-array(d$y[i_sub],dim=c(length(i_sub),1))
        x_vs<-d$X[i_vs,,,drop=FALSE]
        y_vs<-array(d$y[i_vs],dim=c(length(i_vs),1))
        #for the fit to work
        storage.mode(x_sub)<-"double"
        storage.mode(y_sub)<-"double"
        storage.mode(x_vs)<-"double"
        storage.mode(y_vs)<-"double"
        # fitting the LSTM
        history<-keras3::fit(
          model,x=x_sub,y=y_sub,
          epochs=n_epochs,batch_size=batch_size,
          shuffle=FALSE,
          validation_data=list(x_vs,y_vs),
          verbose=0,
          callbacks=list(
            callback_early_stopping(
              monitor="val_loss",
              patience=patience,
              restore_best_weights=TRUE
            )
          )
        )
        #calculating the mse for this pair 
        pv<-as.numeric(predict(model,x_vs,verbose=0))
        mse<-mean((std_inv(pv,scaler_tune)-std_inv(d$y[i_vs],scaler_tune))^2)
        best_epoch<-which.min(as.numeric(history$metrics$val_loss))
        #recording the values for each pair 
        grid_rows[[length(grid_rows)+1]]<-data.frame(
          sex=sex,lag=lag,
          units=u,lr=lr,
          MSE_VS=mse,
          n_epochs=best_epoch,
          row.names=NULL)
        #obtaing the best lr X unit grid 
        if(is.finite(mse)&&mse<best$mse){
          best<-list(
            mse=mse,
            lag=lag,
            units=u,
            lr=lr,
            n_epochs=best_epoch)
        }
        #clean up for storage 
        rm(model)
        gc(verbose=FALSE)
      }
    }
  }
  
  lstm_best[[sex]]<-best
  
  cat(
    "\n",sex,
    "- selected architecture: lag",best$lag,
    "| units",best$units,
    "| lr",best$lr,
    "| VS MSE",round(best$mse,4),
    "\n"
  )
}
# checking if the female error makes since
lstm_best
female_kappa <- lstm_setup$Female$kt
sd(female_kappa)
range(female_kappa)
diff(range(female_kappa))

# Refit the selected network on the full training window and forecast h steps.
h_total<-length(years_fc)
for(sex in sexes){
  st<-lstm_setup[[sex]]
  best_hp<-lstm_best[[sex]]

  scaler_final<-std_fit(st$kt)
  z_final<-std_apply(st$kt,scaler_final)
  d_final<-make_lagged(z_final,best_hp$lag)
  
  tgt_year<-st$years_tr[(best_hp$lag+1):length(z_final)]
  
  x_all<-d_final$X
  y_all<-array(d_final$y,dim=c(length(d_final$y),1))
  
  storage.mode(x_all)<-"double"
  storage.mode(y_all)<-"double"
  
  set_random_seed(base_seed)
  model<-build_lstm(
    best_hp$lag,
    best_hp$units,
    best_hp$lr)
  #fitting the model on the full training data 
  history_final<-keras3::fit(model,
    x=x_all,y=y_all,
    epochs=best_hp$n_epochs,
    batch_size=batch_size,
    shuffle=FALSE,verbose=0
  )
  
  #forecasting the future years kappa and unscaling it 
  w <-z_final[(length(z_final)-best_hp$lag+1):length(z_final)]
  kt_fc_scaled<-recursive_kappa(model,w,h_total)
  kt_fc<-std_inv(kt_fc_scaled,scaler_final)
  
  #retrieving the fitted kappas and unscaling them 
  fit_in_scaled<-as.numeric(predict(model,x_all,verbose=0))
  fit_in<-std_inv(
    fit_in_scaled,
    scaler_final)
  
  actual_in<-std_inv(
    as.numeric(y_all),
    scaler_final)
  
  #calculate the noise for the fitted 
  noise_in<-actual_in-fit_in
  
  #difference between implied and forecasted kappa 
  kt_implied_fc=LC_UK[[sex]]$kappa_implied_oos$poisson
  forecast_error<-kt_implied_fc-kt_fc
  
  lstm_setup[[sex]]$reference<-list(
    kt_fc=kt_fc,
    fitted_years=tgt_year,
    fitted=fit_in,
    noise=noise_in,
    forecast_error=forecast_error,
    scaler=scaler_final,
    epochs=best_hp$n_epochs
  )
  
  rm(model)
  keras3::clear_session()
  gc(verbose=FALSE)
  
  model_output<-data.frame(
    Year=years_fc,
    Kappa_Forecast=round(kt_fc,3),
    Forecast_Error=round(forecast_error,3)
  )
  
  cat("\n",sex,"- reference network trained. kappa forecast:\n")
  print(model_output,row.names=FALSE)
}

# Noise analysis
noise_test_rows<-list()
for(sex in sexes){
  g<-as.numeric(lstm_setup[[sex]]$reference$noise)
  g<-g[is.finite(g)]
  innov<-diff(g)
  var_gamma<-var(innov)
  #to test the following we use the shapiro.test 
  #H0:ηt follows a normal distribution
  #H1:ηt does not follow a normal distribution.
  sw<-shapiro.test(innov)
  # another normality test skewness and kurtosis.
  dp<-dagostino_pearson_test(innov)
  jb<-tseries::jarque.bera.test(innov) #doesn't scale we expect dp to give better result since our data size in small
  adf<-suppressWarnings(tseries::adf.test(g)) #test if g is stationary (to be certain of our choice of the random walk H0 assumes stationary) or has the unit root effect
  lstm_setup[[sex]]$var_gamma<-var_gamma
  lstm_setup[[sex]]$noise_innov<-innov
  noise_test_rows[[length(noise_test_rows)+1]]<-data.frame(
    sex=sex,
    sigma_gamma=sqrt(var_gamma),
    SW_stat=unname(sw$statistic),
    SW_p=sw$p.value,
    DP_stat=dp$statistic,
    DP_p=dp$p.value,
    JB_stat=unname(jb$statistic),
    JB_p=jb$p.value,
    ADF_stat=unname(adf$statistic),
    ADF_p=adf$p.value,
    row.names=NULL
  )
}
noise_tests<-do.call(rbind,noise_test_rows)

print(transform(
  noise_tests,
  sigma_gamma=round(sigma_gamma,4),
  SW_stat=round(SW_stat,5),
  SW_p=round(SW_p,5),
  DP_stat=round(DP_stat,5),
  DP_p=round(DP_p,5),
  JB_stat=round(JB_stat,5),
  JB_p=round(JB_p,5),
  ADF_stat=round(ADF_stat,5),
  ADF_p=round(ADF_p,5)
),row.names=FALSE)

#Comment: all of the tests fails to reject the normality assumptions of gamma also the ADF fail to reject the unit-root null for the noise-level series, so our assumption of a random walk is consistent
#The estimated innovation standard deviation is larger for females than for males, indicating greater unexplained year-to-year variation in the female period-index residuals.

# Chapter 5: Mortality Forecasting
# Section 5.2: LC-LSTM Based Forecasting, forcasting comparison
# Bagging
# Koissi bootstrap and network ensemble
cat("\nBootstrap:",B_boot,"replicates per sex\n")
for(sex in sexes){
  st<-lstm_setup[[sex]]
  best_hp<-lstm_best[[sex]]
  D<-align_ages(LC_UK[[sex]]$P_D_train,st$ages)
  E<-align_ages(LC_UK[[sex]]$P_E_train,st$ages)
  storage.mode(D)<-"double"
  storage.mode(E)<-"double"
  #construct empty matrices for the bootstraps 
  boot_kappa<-matrix(NA_real_,nrow=B_boot,ncol=length(st$kt))
  boot_pred<-matrix(NA_real_,nrow=B_boot,ncol=h_total)
  
  t0<-Sys.time()
  for(b in seq_len(B_boot)){
    set.seed(base_seed+b) # to replicate the same ones each time 
    kt_b<-bootstrap_kappa(
        D=D,E=E,ax=st$ax,
        bx=st$bx,kt=st$kt,
        ages=st$ages,years=st$years_tr)
    boot_kappa[b,]<-kt_b
    scaler_b<-std_fit(kt_b)
    z_b<-std_apply(kt_b,scaler_b)
    d_b<-make_lagged(z_b,best_hp$lag)
    x_b<-d_b$X
    y_b<-array(d_b$y,dim=c(length(d_b$y),1))
    storage.mode(x_b)<-"double"
    storage.mode(y_b)<-"double"
    set_random_seed(base_seed+b)
    model_b<-build_lstm(best_hp$lag,best_hp$units,best_hp$lr)
    fit_b<-keras3::fit(
        model_b,x=x_b,y=y_b,
        epochs=as.integer(best_hp$n_epochs),
        batch_size=batch_size,
        shuffle=FALSE,
        verbose=0
      )
    w_b<-z_b[(length(z_b)-best_hp$lag+1):length(z_b)]
    z_fc_b<-recursive_kappa(model_b,w_b,h_total)
    boot_pred[b,]<-std_inv(z_fc_b,scaler_b)
    
    rm(model_b)
    keras3::clear_session()
    
    if(b%%25==0){
      elapsed<-difftime(Sys.time(),t0,units="mins")
      cat(sprintf(
        "  %s: %d/%d (%.1f min elapsed,~%.1f min left)\n",
        sex,
        b,
        B_boot,
        as.numeric(elapsed),
        as.numeric(elapsed)/b*(B_boot-b)
      ))
      gc(verbose=FALSE)
    }
  }
#complete 
  keep<-stats::complete.cases(boot_pred)&stats::complete.cases(boot_kappa)
  lstm_setup[[sex]]$boot<-list(
    kappa=boot_kappa[keep,,drop=FALSE],
    pred=boot_pred[keep,,drop=FALSE],
    B_ok=sum(keep)
  )
  cat("  ",sex,": ",sum(keep)," usable replicates\n",sep="")
}

#defining a function to obtain the quantiles with no nirmality assumptions
mixture_pi <- function(base_pred,noise_sd,shift=0){
  lower<-numeric(ncol(base_pred))
  upper<-numeric(ncol(base_pred))
  for(h in seq_len(ncol(base_pred))){
    centers<-base_pred[,h]+shift
    centers<-centers[is.finite(centers)]
    s<-noise_sd[h]
    if(s<=0){
      qs<-quantile(centers,c(0.025,0.975),na.rm=TRUE)
    }else{
      Fmix<-function(x){
        mean(pnorm(x,mean=centers,sd=s))
      }
      search_lo<-min(centers)-8*s
      search_hi<-max(centers)+8*s
      qs<-c(
        uniroot(function(x)Fmix(x)-0.025,lower=search_lo,upper=search_hi)$root,
        uniroot(function(x)Fmix(x)-0.975,lower=search_lo,upper=search_hi)$root
      )
    }
    lower[h]<-qs[1]
    upper[h]<-qs[2]
  }
  list(lower=lower,upper=upper)
}

#matched RWD bootstrap: reuses the SAME kappa replicates as the LSTM in order to compare
h_seq <- seq_len(h_total)
for (sex in sexes) {
  st<- lstm_setup[[sex]]
  bk<- st$boot$kappa#the replicates already used for the LSTM
  T_len <- ncol(bk)
  # closed-form RWD drift
  theta_b <- (bk[, T_len] - bk[, 1])/(T_len - 1)
  kT_b <- bk[, T_len]
  # per-replicate innovation sd across each year sd(ξt)
  sigma_b <- apply(bk, 1, function(k) sd(diff(k)))
  
  # B x h matrix of RWD central forecasts
  rwd_boot_pred <- kT_b + outer(theta_b, h_seq)
  colnames(rwd_boot_pred) <- as.character(years_fc)
  
  # matched variance decomposition: ensemble spread + own innovation
  var_khat_rwd <- apply(rwd_boot_pred, 2, var)
  sigma_rwd <- as.numeric(LC_UK[[sex]]$kappa_forecast[[benchmark_method]]$sigma)
  sd_matched <- sqrt(var_khat_rwd + h_seq * sigma_rwd^2)
  q_rwd <- mixture_pi(base_pred=rwd_boot_pred,noise_sd=sqrt(h_seq)*sigma_rwd)
  
  LC_UK[[sex]]$rwd_matched <- list(
    boot_pred = rwd_boot_pred,
    theta_boot = theta_b,
    sigma_boot = sigma_b,
    kbar = colMeans(rwd_boot_pred),
    var_khat = var_khat_rwd,
    sd_total = sd_matched,
    lower = colMeans(rwd_boot_pred) - z_alpha * sd_matched,
    upper= colMeans(rwd_boot_pred) + z_alpha * sd_matched,
    lower_q = q_rwd$lower,
    upper_q = q_rwd$upper
  )
}

# Bagged mean and variance for LSTM
for(sex in sexes){
  st<-lstm_setup[[sex]]
  bp<-st$boot$pred
  gamma_T <- tail(as.numeric(st$reference$noise), 1)
  kbar    <- colMeans(bp) + gamma_T
  var_khat<-apply(bp,2,var)
  var_gamma<-st$var_gamma
  h_fac<-seq_len(h_total)
  var_total<-var_khat+h_fac*var_gamma
  sd_total<-sqrt(var_total)
  lower<-kbar-z_alpha*sd_total
  upper<-kbar+z_alpha*sd_total
  q_lstm <- mixture_pi(base_pred=bp,noise_sd=sqrt(h_fac*var_gamma),shift=gamma_T)
  lstm_setup[[sex]]$pi<-list(
    kbar=kbar,
    var_khat=var_khat,
    var_gamma=var_gamma,
    var_total=var_total,
    sd_total=sd_total,
    lower=lower,
    upper=upper,
    lower_q=q_lstm$lower,
    upper_q=q_lstm$upper
  )
  cat("\n",sex,"- variance decomposition\n")
  print(round(data.frame(
    year=years_fc,
    kbar=kbar,
    sd_ensemble=sqrt(var_khat),
    sd_noise=sqrt(h_fac*var_gamma),
    sd_total=sd_total,
    lower=lower,
    upper=upper,
    lower_q=q_lstm$lower,
    upper_q=q_lstm$upper
  ),3),row.names=FALSE)
  cat(sex, "bootstrap drift sd:", round(sd(LC_UK[[sex]]$rwd_matched$theta_boot), 4),
      "vs analytic s.e.:",
      round(as.numeric(LC_UK[[sex]]$kappa_forecast[[benchmark_method]]$se_theta), 4), "\n")
  cat(sex, "ensemble sd 2022 - LSTM:", round(sqrt(st$pi$var_khat[h_total]), 3),
      "| RWD:", round(sqrt(LC_UK[[sex]]$rwd_matched$var_khat[h_total]), 3), "\n")
}

# Mortality rates and prediction intervals
log_m_bounds<-function(ax,bx,k_lo,k_hi){
  lower_candidate<-outer(bx,k_lo)+ax
  upper_candidate<-outer(bx,k_hi)+ax
  list(
    lo=pmin(lower_candidate,upper_candidate),
    hi=pmax(lower_candidate,upper_candidate)
  )
}
#For the LSTM 
for(sex in sexes){
  st<-lstm_setup[[sex]]
  pi<-st$pi
  log_m_hat<-outer(st$bx,pi$kbar)+st$ax
  dimnames(log_m_hat)<-list(as.character(st$ages),as.character(years_fc))
  bounds<-log_m_bounds(st$ax,st$bx,pi$lower,pi$upper)
  dimnames(bounds$lo)<-dimnames(bounds$hi)<-dimnames(log_m_hat)
  bounds_q<-log_m_bounds(st$ax,st$bx,pi$lower_q,pi$upper_q)
  dimnames(bounds_q$lo)<-dimnames(bounds_q$hi)<-dimnames(log_m_hat)
  lstm_setup[[sex]]$rates<-list(
    log_m_fc=log_m_hat,
    lo=bounds$lo,
    hi=bounds$hi,
    lo_q=bounds_q$lo,
    hi_q=bounds_q$hi,
    ages=st$ages,
    years=years_fc
  )
}
#for lee carter RWD with bootstrap
for (sex in sexes) {
  st <- lstm_setup[[sex]]
  rwd <- LC_UK[[sex]]$rwd_matched
  k_lc <- as.numeric(rwd$kbar)
  lower_k <- as.numeric(rwd$lower)
  upper_k <- as.numeric(rwd$upper)
  lower_k_q <- as.numeric(rwd$lower_q)
  upper_k_q <- as.numeric(rwd$upper_q)
  stopifnot(length(k_lc) == h_total,length(lower_k) == h_total,length(upper_k) == h_total)

  log_m_lc <- outer(st$bx, k_lc) + st$ax
  dimnames(log_m_lc) <- list(as.character(st$ages),as.character(years_fc))
  bounds <- log_m_bounds(st$ax,st$bx,lower_k,upper_k)
  dimnames(bounds$lo) <- dimnames(bounds$hi) <- dimnames(log_m_lc)
  bounds_q <- log_m_bounds(st$ax,st$bx,lower_k_q,upper_k_q)
  dimnames(bounds_q$lo)<-dimnames(bounds_q$hi)<-dimnames(log_m_lc)
  
  lstm_setup[[sex]]$LC_benchmark <- list(
    kbar = k_lc,
    var_khat = rwd$var_khat,
    sd_total = rwd$sd_total,
    lower = lower_k,
    upper = upper_k,
    lower_q = lower_k_q,
    upper_q = upper_k_q,
    lo_q = bounds_q$lo,
    hi_q = bounds_q$hi,
    log_m_fc = log_m_lc,
    lo = bounds$lo,
    hi = bounds$hi
  )
}

#Performance metrics
#some function to ease the process
rmse_func<-function(obs,pred){
  keep<-is.finite(obs)&is.finite(pred)
  if(!any(keep))return(NA_real_)
  sqrt(mean((obs[keep]-pred[keep])^2))
}
picp_func<-function(obs,lo,hi){ #prediction coverage percentage 
  keep<-is.finite(obs)&is.finite(lo)&is.finite(hi)
  if(!any(keep))return(NA_real_)
  mean(obs[keep]>=lo[keep]&obs[keep]<=hi[keep])
}
mpiw_func<-function(lo,hi){ #Mean Prediction Interval Width
  keep<-is.finite(lo)&is.finite(hi)
  if(!any(keep))return(NA_real_)
  mean(hi[keep]-lo[keep])
}
                   
kappa_metrics_rows<-list()
for(sex in sexes){
  st<-lstm_setup[[sex]]
  for(fcy in c("valid","stress")){
    yrs<-as.character(obs_data[[sex]][[fcy]]$years)
    idx<-match(yrs,as.character(years_fc))
    k_obs<-kappa_implied(obs_data[[sex]][[fcy]]$log_mx,st$ax,st$bx)
    for(mdl in c("ARIMA","LSTM")){
      output<-if(mdl=="LSTM")st$pi else st$LC_benchmark
      #the mean value vs implied comparison 
      kappa_metrics_rows[[length(kappa_metrics_rows)+1]]<-data.frame(
        sex=sex,
        model=mdl,
        window=fcy,
        RMSE_k=rmse_func(k_obs,output$kbar[idx]),
        PICP_k=picp_func(k_obs,output$lower[idx],output$upper[idx]),
        MPIW_k=mpiw_func(output$lower[idx],output$upper[idx]),
        PICP_k_q=picp_func(k_obs,output$lower_q[idx],output$upper_q[idx]),
        MPIW_k_q=mpiw_func(output$lower_q[idx],output$upper_q[idx]),
        row.names=NULL
      )
    }
  }
}
kappa_metrics<-do.call(rbind,kappa_metrics_rows)
print(transform(
  kappa_metrics,
  RMSE_k=round(RMSE_k,3),
  PICP_k=round(PICP_k,3),
  MPIW_k=round(MPIW_k,3),
  PICP_k_q=round(PICP_k_q,3),
  MPIW_k_q=round(MPIW_k_q,3)
),row.names=FALSE)

#report specific ages mortality for the plots as well
ages_report<-c(55,65,75,85,95,100)
rate_metrics_rows<-list()
for(sex in sexes){
  st<-lstm_setup[[sex]]
  for(fcy in c("valid","stress")){
    yrs<-as.character(obs_data[[sex]][[fcy]]$years)
    log_obs<-obs_data[[sex]][[fcy]]$log_mx
    for(age in ages_report){
      age_obs<-match(as.character(age),rownames(log_obs))
      if(is.na(age_obs))next
      for(mdl in c("LC","LC-LSTM")){
        output<-if(mdl=="LC-LSTM")st$rates else st$LC_benchmark
        age_est<-match(as.character(age),rownames(output$log_m_fc))
        if(is.na(age_est))next
        rate_metrics_rows[[length(rate_metrics_rows)+1]]<-data.frame(
          sex=sex,
          model=mdl,
          window=fcy,
          age=age,
          RMSE_m=rmse_func(log_obs[age_obs,yrs],output$log_m_fc[age_est,yrs]),
          PICP_m=picp_func(log_obs[age_obs,yrs],output$lo[age_est,yrs],output$hi[age_est,yrs]),
          MPIW_m=mpiw_func(output$lo[age_est,yrs],output$hi[age_est,yrs]),
          PICP_m_q=picp_func(log_obs[age_obs,yrs],output$lo_q[age_est,yrs],output$hi_q[age_est,yrs]),
          MPIW_m_q=mpiw_func(output$lo_q[age_est,yrs],output$hi_q[age_est,yrs] ),
          row.names=NULL
        )
      }
    }
  }
}
rate_metrics<-do.call(rbind,rate_metrics_rows)
rate_metrics<-rate_metrics[order(rate_metrics$sex,rate_metrics$age,rate_metrics$window),]
cat("\n=== log-mortality metrics ===\n")
print(transform(
  rate_metrics,
  RMSE_m=round(RMSE_m,3),
  PICP_m=round(PICP_m,3),
  MPIW_m=round(MPIW_m,3),
  PICP_m_q=round(PICP_m_q,3),
  MPIW_m_q=round(MPIW_m_q,3)
),row.names=FALSE)

# Plots
# PLOT: RWD versus LC-LSTM forecasts with normal prediction bands
plot_lstm_panel<-function(sex,zoom=FALSE){
  st<-lstm_setup[[sex]]
  k_imp<-kappa_implied(cbind(obs_data[[sex]]$valid$log_mx,obs_data[[sex]]$stress$log_mx),
    st$ax,st$bx)
  if(zoom){
    xlim_use<-range(years_fc)
    ylim_use<-range(st$pi$lower,st$pi$upper,
      st$LC_benchmark$lower,st$LC_benchmark$upper,
      k_imp,
      na.rm=TRUE)
    main_use<-paste0(sex,": zoomed forecast period")
  }else{
    xlim_use<-c(min(st$years_tr),max(years_fc))
    ylim_use<-range(st$kt,
      st$pi$lower,st$pi$upper,
      st$LC_benchmark$lower,st$LC_benchmark$upper,
      k_imp,
      na.rm=TRUE)
    main_use<-paste0(sex,": LC-LSTM vs RWD")
  }
  plot(st$years_tr,st$kt,
    type="l",lwd=2,
    xlim=xlim_use,ylim=ylim_use,
    xlab="Year",ylab="",
    main=main_use)
  title(ylab=expression(kappa[t]),cex.lab=1.4)
  polygon(c(years_fc,rev(years_fc)),
    c(st$LC_benchmark$lower,rev(st$LC_benchmark$upper)),
    col=rgb(0.25,0.5,0.9,0.30),
    border=NA)
  polygon(c(years_fc,rev(years_fc)),
    c(st$pi$lower,rev(st$pi$upper)),
    col=rgb(0.95,0.65,0.15,0.55),
    border=NA)
  lines(st$years_tr,st$kt,lwd=2,col="black")
  lines(years_fc,st$LC_benchmark$kbar,lwd=2,col="navy")
  lines(years_fc,st$pi$kbar,lwd=2,col="darkorange3")
  points(years_fc,k_imp,pch=19,cex=0.9,col="red")
  abline(v=max(st$years_tr)+0.5,lty=2)
  abline(v=max(obs_data[[sex]]$valid$years)+0.5,lty=3)
  legend("bottomleft",
    bty="n",
    cex=0.75,
    legend=c("Fitted","LC-RWD","LC-LSTM","Implied kappa"),
    col=c("black","navy","darkorange3","red"),
    lwd=c(2,2,2,NA),
    pch=c(NA,NA,NA,19)
  )
}
plot_lstm_panel(sexes[1],zoom=FALSE)
plot_lstm_panel(sexes[2],zoom=FALSE)
plot_lstm_panel(sexes[1],zoom=TRUE)
plot_lstm_panel(sexes[2],zoom=TRUE)

par(old_par)

#PLOT: Female and male log-mortality forecasts at selected ages, normal bands 
for(sex in sexes){
  st<-lstm_setup[[sex]]
  log_full<-log_rate_matrix(
    LC_UK[[sex]]$manual_mx_full[as.character(st$ages),,drop=FALSE]
  )
  years_hist<-as.numeric(colnames(log_full))
  par(mfrow=c(3,2))
  for(age in ages_report){
    age_hist<-match(as.character(age),rownames(log_full))
    age_est<-match(as.character(age),rownames(st$rates$log_m_fc))
    plot(
      years_hist,
      log_full[age_hist,],
      pch=20,
      cex=0.5,
      col="Black",
      xlim=c(1990,max(years_fc)),
      ylim=range(
        log_full[age_hist,years_hist>=1990],
        st$rates$lo[age_est,],
        st$rates$hi[age_est,],
        st$LC_benchmark$lo[age_est,],
        st$LC_benchmark$hi[age_est,],
        na.rm=TRUE
      ),
      xlab="Year",
      ylab=expression(log~m[x,t]),
      main=paste0(sex,", x=",age)
    )
    polygon(
      c(years_fc,rev(years_fc)),
      c(st$LC_benchmark$lo[age_est,],rev(st$LC_benchmark$hi[age_est,])),
      col=rgb(0.25,0.5,0.9,0.30),
      border=NA
    )
    polygon(
      c(years_fc,rev(years_fc)),
      c(st$rates$lo[age_est,],rev(st$rates$hi[age_est,])),
      col=rgb(0.95,0.65,0.15,0.55),
      border=NA
    )
    lines(years_fc,st$LC_benchmark$log_m_fc[age_est,],lwd=2,col="navy")
    lines(years_fc,st$rates$log_m_fc[age_est,],lwd=2,col="darkorange3")
    abline(v=max(st$years_tr)+0.5,lty=3)
    legend(
      "bottomleft",
      legend=c("LC-RWD","LC-LSTM"),
      col=c("navy","darkorange3"),
      lwd=2,
      bty="n",
      cex=0.7
    )
  }
}
                   
# Bootstrap distributions and Q-Q plots of the 2019 forecast
par(mfrow=c(4,2))
for(sex in sexes){
  h_2019 <- match(2019, years_fc)
  # terminal-year bootstrap forecasts
  rwd_bp  <- LC_UK[[sex]]$rwd_matched$boot_pred[,h_2019]
  lstm_bp <- lstm_setup[[sex]]$boot$pred[,h_2019]
  
  # RWD bootstrap distribution
  hist(rwd_bp,
    breaks=60,
    col="grey85",
    border="white",
    xlab=expression(hat(kappa)[t]),
    main=paste0(sex,", ",years_fc[h_2019],": RWD bootstrap distribution"))
  abline(v=LC_UK[[sex]]$rwd_matched$kbar[h_2019],lwd=2,col="navy")
  # LC-LSTM bootstrap distribution
  hist(lstm_bp,
    breaks=60,
    col="grey85",
    border="white",
    xlab=expression(hat(kappa)[t]),
    main=paste0(sex,", ",years_fc[h_2019],": LC-LSTM bootstrap distribution"))
  abline(v=mean(lstm_bp),lwd=2,col="darkorange3")
  
  # RWD Q-Q plot
  qqnorm(rwd_bp,pch=1,
    cex=0.6,
    main=paste0(sex,", ",years_fc[h_2019],": RWD Q-Q plot") )
  qqline(rwd_bp,col="navy",lwd=2)
  # LC-LSTM Q-Q plot
  qqnorm(lstm_bp,
    pch=1,cex=0.6,
    main=paste0(sex,", ",years_fc[h_2019],": LC-LSTM Q-Q plot"))
  qqline(lstm_bp,col="darkorange3",lwd=2)
}
par(mfrow=c(1,1))

# Plots for pi not normal
# PLOT: RWD versus LC-LSTM forecasts with quantile-based prediction bands
old_par<-par(no.readonly=TRUE)
layout(matrix(c(1,3,2,4),nrow=2,byrow=TRUE),widths=c(1,1.25))
par(mar=c(4,4,3,1))

plot_lstm_panel_q<-function(sex,zoom=FALSE){
  st<-lstm_setup[[sex]]
  k_imp<-kappa_implied(
    cbind(obs_data[[sex]]$valid$log_mx,obs_data[[sex]]$stress$log_mx),
    st$ax,st$bx
  )
  if(zoom){
    xlim_use<-range(years_fc)
    ylim_use<-range(st$pi$lower_q,st$pi$upper_q,
      st$LC_benchmark$lower_q,st$LC_benchmark$upper_q,
      k_imp,
      na.rm=TRUE
    )
    main_use<-paste0(sex,": zoomed forecast period")
  }else{xlim_use<-c(min(st$years_tr),max(years_fc))
    ylim_use<-range(st$kt,
      st$pi$lower_q,st$pi$upper_q,
      st$LC_benchmark$lower_q,st$LC_benchmark$upper_q,
      k_imp,
      na.rm=TRUE)
    main_use<-paste0(sex,": LC-LSTM vs RWD")
  }
  plot(st$years_tr,st$kt,
    type="l",lwd=2,
    xlim=xlim_use,ylim=ylim_use,
    xlab="Year",ylab=expression(kappa[t]),
    main=main_use)
  polygon(c(years_fc,rev(years_fc)),
    c(st$LC_benchmark$lower_q,rev(st$LC_benchmark$upper_q)),
    col=rgb(0.25,0.5,0.9,0.30),
    border=NA)
  polygon(c(years_fc,rev(years_fc)),
    c(st$pi$lower_q,rev(st$pi$upper_q)),
    col=rgb(0.95,0.65,0.15,0.55),
    border=NA)
  lines(st$years_tr,st$kt,lwd=2,col="black")
  lines(years_fc,st$LC_benchmark$kbar,lwd=2,col="navy")
  lines(years_fc,st$pi$kbar,lwd=2,col="darkorange3")
  points(years_fc,k_imp,pch=19,cex=0.9,col="red")
  abline(v=max(st$years_tr)+0.5,lty=2)
  abline(v=max(obs_data[[sex]]$valid$years)+0.5,lty=3)
  legend("bottomleft",
    bty="n",cex=0.75,
    legend=c("Fitted","LC-RWD","LC-LSTM","Implied kappa"),
    col=c("black","navy","darkorange3","red"),
    lwd=c(2,2,2,NA),
    pch=c(NA,NA,NA,19)
  )
}
plot_lstm_panel_q(sexes[1],zoom=FALSE)
plot_lstm_panel_q(sexes[2],zoom=FALSE)
plot_lstm_panel_q(sexes[1],zoom=TRUE)
plot_lstm_panel_q(sexes[2],zoom=TRUE)
par(old_par)

#PLOT: Female and male log-mortality forecasts at selected ages, quantile-based bands
for(sex in sexes){
  st<-lstm_setup[[sex]]
  log_full<-log_rate_matrix(
    LC_UK[[sex]]$manual_mx_full[as.character(st$ages),,drop=FALSE]
  )
  years_hist<-as.numeric(colnames(log_full))
  par(mfrow=c(3,2))
  for(age in ages_report){
    age_hist<-match(as.character(age),rownames(log_full))
    age_est<-match(as.character(age),rownames(st$rates$log_m_fc))
    plot(
      years_hist,
      log_full[age_hist,],
      pch=20,
      cex=0.5,
      col="Black",
      xlim=c(1990,max(years_fc)),
      ylim=range(
        log_full[age_hist,years_hist>=1990],
        st$rates$lo_q[age_est,],
        st$rates$hi_q[age_est,],
        st$LC_benchmark$lo_q[age_est,],
        st$LC_benchmark$hi_q[age_est,],
        na.rm=TRUE
      ),
      xlab="Year",
      ylab=expression(log~m[x,t]),
      main=paste0(sex,", x=",age)
    )
    polygon(
      c(years_fc,rev(years_fc)),
      c(st$LC_benchmark$lo_q[age_est,],rev(st$LC_benchmark$hi_q[age_est,])),
      col=rgb(0.25,0.5,0.9,0.30),
      border=NA
    )
    polygon(
      c(years_fc,rev(years_fc)),
      c(st$rates$lo_q[age_est,],rev(st$rates$hi_q[age_est,])),
      col=rgb(0.95,0.65,0.15,0.55),
      border=NA
    )
    lines(years_fc,st$LC_benchmark$log_m_fc[age_est,],lwd=2,col="navy")
    lines(years_fc,st$rates$log_m_fc[age_est,],lwd=2,col="darkorange3")
    abline(v=max(st$years_tr)+0.5,lty=3)
    legend(
      "bottomleft",
      legend=c("LC-RWD","LC-LSTM"),
      col=c("navy","darkorange3"),
      lwd=2,
      bty="n",
      cex=0.7
    )
  }
}

#model selection 
# The Poisson Lee-Carter fit is fixed for both RWD and LSTM.
# RWD and LSTM are first compared on the validation window.
# RWD is selected as the main pricing model.
# Both mortality surfaces are then extended to age 120.
# the lstm is retained for further work 
benchmark_method<-"poisson"
forecast_models<-c("RWD","LSTM")
for(sex in sexes){LC_UK[[sex]]$selected_method<-benchmark_method}
get_boot_rates<-function(sex,ts_model){
  st<-lstm_setup[[sex]]
  out<-if(ts_model=="RWD") st$LC_benchmark 
  else if(ts_model=="LSTM") st$rates 
  out
}
cmp_rows<-list()
for(sex in sexes){
  log_obs<-obs_data[[sex]]$valid$log_mx
  for(ts_model in forecast_models){
    ro<-get_boot_rates(sex,ts_model)
    years_use<-intersect(as.character(obs_data[[sex]]$valid$years),colnames(ro$log_m_fc))
    ages_all<-intersect(rownames(log_obs),rownames(ro$log_m_fc))
    age_num<-as.numeric(ages_all)
    ages_50_100<-ages_all[age_num>=50 & age_num<=100]
    ages_65_100<-ages_all[age_num>=65 & age_num<=100]
    
    sub<-function(m,a)m[a,years_use,drop=FALSE]
    
    for(ar in c("50-100","65-100")){
      a<-if(ar=="50-100") ages_50_100 
      else ages_65_100
      obs<-sub(log_obs,a)
      pred<-sub(ro$log_m_fc,a)
      keep<-is.finite(obs) & is.finite(pred)
      cmp_rows[[length(cmp_rows)+1]]<-data.frame(
        sex=sex,
        lc_method=method_labels[benchmark_method],
        ts_model=ts_model,
        window="valid",
        ages=ar,
        MAE_log=mean(abs(obs[keep]-pred[keep])),
        RMSE_log=sqrt(mean((obs[keep]-pred[keep])^2)),
        PICP95_n=picp_func(obs,sub(ro$lo,a),sub(ro$hi,a)),
        MPIW95_n=mpiw_func(sub(ro$lo,a),sub(ro$hi,a)),
        PICP95_q=picp_func(obs,sub(ro$lo_q,a),sub(ro$hi_q,a)),
        MPIW95_q=mpiw_func(sub(ro$lo_q,a),sub(ro$hi_q,a)),
        n_cells=sum(keep),
        row.names=NULL
      )
    }
  }
}
cmp<-do.call(rbind,cmp_rows)
print(cmp,row.names=FALSE,digits=4)

# retrieve forecast objects
get_ts_objects<-function(sex,ts_model){
  m<-LC_UK[[sex]]$selected_method
  if(ts_model=="RWD"){
    return(list(kappa = LC_UK[[sex]]$kappa_forecast_rwd_matched,
                rates = LC_UK[[sex]]$forecast_lm_rwd_matched))}
  if(ts_model=="LSTM"){
    return(list(kappa=LC_UK[[sex]]$kappa_forecast_lstm,rates=LC_UK[[sex]]$forecast_lm_lstm))
  }
}
                   
save.image("workspace.RData")
                   
# Chapter 6: Longevity Bond Pricing
# Section 6.2: Monte Carlo Pricing under the Martingale Measure
# Use Poisson Lee-Carter with an RWD for pricing.
# Apply the primary Wang adjustment to kappa_t and retain the maturity-wise routes as benchmarks.
# Calibrate lambda to the Irish annuity quotes using matching product cash flows.
library(StMoMo)
library(forecast)
# Settings
required_objects<-c("LC_UK","sexes","ages_to_fit","get_lc_params")
price_lc_method<-"poisson"
# Refit the selected model on all pre-COVID observations for a 2020 issue year.
price_fit_source<-"refit_pre_covid"
price_fit_years<-1950:2019
x0_cohort<-65
# Extend the table to limiting age 125 with q_125 = 1.
age_terminal<-125
T_bond<-25
coupon<-1
principal<-1 # retained to mirror Denuit et al.'s bond cash flow. it won't effect anything as it will cancel out from both
# Set the bond and simulation horizons.
closure_model<-"kannisto"
closure_band<-85:100
closure_anchor<-TRUE #to connect with the lee carter frocast at age 100
n_sim_price<-10000
seed_price<-20260820
# Let the data determine the sign of alpha_cdf and use lambda_proc = -alpha_cdf for pricing.
lambda_source<-"market_annuity"
# Irish annuity quote used as the UK proxy.
Irishlife_file<-"../Data/Irishlife_annuity_data.xlsx"
Irishlife_sheet<-"Sheet1"
# Match the annuity commencement date to the pricing origin.
annuity_commencement_date<-as.Date("2020-01-01")
Irishlife_sex_column<-setNames(sexes,sexes) # "Male"->"Male", "Female"->"Female"
# Set the quoted annuity product terms.
annuity_fund<-100000
annuity_commission<-0.02    
annuity_payment_freq<-12
annuity_payment_timing<-"advance"
annuity_guarantee_years<-5
annuity_escalation<-0
annuity_age<-65
annuity_singlelife<-TRUE
apply_commission_to_fund<-TRUE
# Use the Bank of England nominal spot curve at the issue date for bond pricing.
boe_file<-"../Data/glcnominalddata/BoE_daily_SpotCurve_2016-2024.xlsx"
boe_sheet<-"4. spot curve"
# Use the ECB euro spot curve for annuity calibration.
annuity_discount_source<-"ecb_eur"
ecb_aaa_source <- read.csv(
  unz("../Data/ECB_AAA_SpotCurve.zip","data.csv"),
  stringsAsFactors=FALSE
)

# Functions                  
# Round numeric data-frame columns for reporting.
round_numeric_df<-function(x,digits=6){
  numeric_columns<-vapply(x,is.numeric,logical(1))
  x[numeric_columns]<-lapply(x[numeric_columns],round,digits=digits)
  x}

# Fit the pricing model and estimate its RWD parameters.
fit_lc_poisson<-function(sex,yrs,ages_fit=ages_to_fit){
  D<-LC_UK[[sex]]$D_full[as.character(ages_fit),as.character(yrs),drop=FALSE]
  E<-LC_UK[[sex]]$E_full[as.character(ages_fit),as.character(yrs),drop=FALSE]
  storage.mode(D)<-"double"
  storage.mode(E)<-"double"
  fit<-StMoMo::fit(
    StMoMo::lc(link="log",const="sum"),
    Dxt=D,Ext=E,
    ages=ages_fit,years=yrs,
    ages.fit=ages_fit,years.fit=yrs)
  out<-list(ax=as.numeric(fit$ax),
    bx=as.numeric(fit$bx[,1]),kt=as.numeric(fit$kt[1,]),
    ages=as.numeric(fit$ages),years=as.numeric(fit$years))
  names(out$ax)<-as.character(out$ages)
  names(out$bx)<-as.character(out$ages)
  names(out$kt)<-as.character(out$years)
  out
}
?forecast::Arima
# estimate its RWD parameters                   
rwd_params<-function(kt){
  years<-as.numeric(names(kt))
  kt_ts<-ts(kt,start=years[1],frequency=1)
  fit<-forecast::Arima(kt_ts,order=c(0,1,0),include.drift=TRUE,method="ML")
  theta<-unname(coef(fit)["drift"])
  sigma<-sqrt(fit$sigma2)
  se_theta<-sqrt(fit$var.coef["drift","drift"])
  list(fit=fit,theta=theta,
    theta_closed_form=(kt[length(kt)]-kt[1])/(length(kt)-1),
    sigma=sigma,sigma2=sigma^2,se_theta=se_theta,
    kt_last=unname(kt[length(kt)]),origin_year=years[length(years)])
}

#Implementing the pricing STEP BY STEP:                 
pricing<-setNames(vector("list",length(sexes)),sexes)
#ontain the Lee carter and RWD parameters
for(sex in sexes){
  params<-fit_lc_poisson(sex,price_fit_years)
  rw<-rwd_params(params$kt)
  pricing[[sex]]<-list(ax=params$ax,bx=params$bx,ages=params$ages,
    years=params$years,kt=params$kt,rwd=rw
  )}
pricing_fit_table<-do.call(rbind,lapply(sexes,function(sex){
  pr<-pricing[[sex]]
  data.frame(sex=sex,
    fit_start=min(pr$years),fit_end=max(pr$years),
    theta=pr$rwd$theta,theta_se=pr$rwd$se_theta,
    sigma=pr$rwd$sigma,sigma2=pr$rwd$sigma2,
    kappa_T=pr$rwd$kt_last,row.names=NULL
  )
}))

#last obtained year before the bond is alive
origin_years<-vapply(pricing,function(z) z$rwd$origin_year,numeric(1))

forecast_origin<-unique(origin_years)
issue_year<-forecast_origin+1
mortality_ages<-x0_cohort:age_terminal
h_seq_price<-seq_len(length(mortality_ages))
H_price<-length(h_seq_price)
attained_ages<-x0_cohort+h_seq_price
mortality_years<-issue_year+h_seq_price-1
payment_years<-issue_year+h_seq_price

# market input and discount curve
curve_target_date<-annuity_commencement_date

# Functions:
# Extract the quoted annual pension at the commencement date by sex.
read_Irishlife_quote<-function(){
  raw<-as.data.frame(readxl::read_excel(Irishlife_file,sheet=Irishlife_sheet,skip=4,col_names=FALSE))
  blocks<-list(Male=c(1,3),Female=c(5,7),GenderNeutral=c(9,11))
  quotes<-list()
  for(nm in names(blocks)){
    dcol<-blocks[[nm]][1]
    pcol<-blocks[[nm]][2]
    d<-as.Date(raw[[dcol]])
    pen<-suppressWarnings(as.numeric(raw[[pcol]]))
    ok<-!is.na(d)&is.finite(pen)
    quotes[[nm]]<-data.frame(date=d[ok],pension=pen[ok])
  }
  quotes
}

select_quote<-function(tbl,target_date){
  elig<-tbl[!is.na(tbl$date)&tbl$date<=target_date,,drop=FALSE]
  if(nrow(elig)==0){
    stop("No Irish Life quote on or before ",format(target_date,"%Y-%m-%d"))
  }
  elig[which.max(elig$date),,drop=FALSE][1,]
}

#Implement to obtain the quoted annual pension at the commencement date by sex
market_annuity_price<-setNames(rep(NA_real_,length(sexes)),sexes)
annuity_pension<-setNames(rep(NA_real_,length(sexes)),sexes)
annuity_quote_date<-setNames(rep(as.Date(NA),length(sexes)),sexes)

Irishlife<-read_Irishlife_quote()
for(sex in sexes){
  blk<-Irishlife_sex_column[[sex]]
  q<-select_quote(Irishlife[[blk]],curve_target_date)
  pension<-q$pension
  fund_for_annuity<-if(apply_commission_to_fund){
    annuity_fund*(1-annuity_commission)
    }else{
      annuity_fund
    }
  market_annuity_price[sex]<-fund_for_annuity/pension
  annuity_pension[sex]<-pension
  annuity_quote_date[sex]<-q$date
  }

# Extract the Bank of England spot curve (yield curve) at the issue date.
boe_spot_all<-as.data.frame(readxl::read_excel(boe_file,sheet=boe_sheet,skip=3))
names(boe_spot_all)[1]<-"date"
boe_spot_all$date<-as.Date(boe_spot_all$date)
required_bond_columns<-as.character(seq_len(T_bond))
if(!all(required_bond_columns%in%names(boe_spot_all))){
  stop("The Bank of England file does not contain all 1-25 year maturities")
}
complete_bond_rows<-complete.cases(boe_spot_all[,required_bond_columns,drop=FALSE])
eligible_rows<-which(!is.na(boe_spot_all$date)&
                       boe_spot_all$date<=curve_target_date&
                       complete_bond_rows)
selected_row<-eligible_rows[which.max(boe_spot_all$date[eligible_rows])]
boe_spot_quote<-boe_spot_all[selected_row,,drop=FALSE]
boe_curve_date<-boe_spot_quote$date[1]
curve_lag_days<-as.integer(curve_target_date-boe_curve_date)
rate_columns<-setdiff(names(boe_spot_quote),"date")
boe_maturity<-suppressWarnings(as.numeric(rate_columns))
boe_spot_pct<-as.numeric(unlist(boe_spot_quote[1,rate_columns,drop=FALSE],use.names=FALSE))

valid_rates<-is.finite(boe_maturity)&is.finite(boe_spot_pct)
boe_maturity<-boe_maturity[valid_rates]
boe_spot_pct<-boe_spot_pct[valid_rates]
rate_order<-order(boe_maturity)
boe_maturity<-boe_maturity[rate_order]
boe_spot_pct<-boe_spot_pct[rate_order]
max(boe_maturity)
min(boe_maturity)

boe_spot_cc<-boe_spot_pct/100 #from percentage to decimals 
# Extract annual spot rates and hold the curve flat beyond the published maturities.
annual_rates<-boe_spot_cc[seq(1,length(boe_maturity),by=2)] #only get the yearly ones and delete the half year ones.
#hold the curve flat afterwards 
spot_cc_curve<-annual_rates[pmin(h_seq_price,length(annual_rates))] 
disc_curve<-exp(-spot_cc_curve*h_seq_price)
disc_bond<-disc_curve[seq_len(T_bond)]
boe_last_maturity<-length(annual_rates)

boe_discount_curve<-data.frame(maturity=h_seq_price,spot_pct=100*spot_cc_curve,spot_cc=spot_cc_curve,
  discount_factor=disc_curve,extrapolated=h_seq_price>boe_last_maturity,
  row.names=NULL)

# Build the ECB AAA euro-area spot curve used only for $\lambda$ calibration.
# Function:
read_ecb_spot_curve<-function(source){
  raw<-source
  # keep pure whole-year spot rates SR_1Y..SR_nY only.
  maturity<-suppressWarnings(as.integer(sub("^SR_([0-9]+)Y$","\\1",
                                            raw$DATA_TYPE_FM)))
  is_year_spot<-grepl("^SR_[0-9]+Y$",raw$DATA_TYPE_FM)&!is.na(maturity)
  spot<-data.frame(date=as.Date(raw$TIME_PERIOD[is_year_spot]),
                   maturity=maturity[is_year_spot],
                   spot_pct=suppressWarnings(as.numeric(
                     raw$OBS_VALUE[is_year_spot])))
  spot<-spot[is.finite(spot$maturity)&is.finite(spot$spot_pct)&
               !is.na(spot$date),,drop=FALSE]
  spot<-spot[order(spot$date,spot$maturity),,drop=FALSE]
  spot<-spot[!duplicated(spot[,c("date","maturity")]),,drop=FALSE]
  spot
}

ecb_spot<-read_ecb_spot_curve(ecb_aaa_source)

# Select the nearest prior date carrying a complete curve.
by_date<-split(ecb_spot[,c("maturity","spot_pct")],ecb_spot$date)
need_maturities<-seq_len(T_bond)
eligible_dates<-as.Date(names(by_date))
keep<-vapply(names(by_date),function(d){
  m<-by_date[[d]]$maturity
  as.Date(d)<=curve_target_date&&all(need_maturities%in%m)
  },logical(1))
if(!any(keep)){
stop("No complete ECB euro curve exists on or before")}
ecb_curve_date<-max(eligible_dates[keep])
chosen<-by_date[[as.character(ecb_curve_date)]]
chosen<-chosen[order(chosen$maturity),,drop=FALSE]
ecb_maturity<-chosen$maturity
ecb_spot_cc<-chosen$spot_pct/100
# Hold the curve flat beyond the last published maturity.
spot_cc_curve_annuity<-approx(x=ecb_maturity,y=ecb_spot_cc,xout=h_seq_price,
                              method="linear",rule=2,ties="ordered")$y
annuity_curve_date<-ecb_curve_date
annuity_curve_ccy<-"EUR"
annuity_curve_last_maturity<-max(ecb_maturity)

if(any(!is.finite(spot_cc_curve_annuity))){
  stop("Invalid annuity discount curve")
}

annuity_curve_lag_days<-as.integer(curve_target_date-annuity_curve_date)
cat("\n--- Irish Life annuity inputs (UK proxy) ---\n")
print(data.frame(sex=sexes,quote_date=annuity_quote_date[sexes],
                 annuity_curve_date=annuity_curve_date,
                annuity_curve_ccy=annuity_curve_ccy, 
                fund=annuity_fund,commission=annuity_commission,
                annual_pension=as.numeric(annuity_pension[sexes]),
                annuity_price_per_unit=as.numeric(market_annuity_price[sexes]),row.names=NULL
                ))

# Chapter 6: Longevity Bond Pricing
# Section 6.2: Monte Carlo Pricing under the Martingale Measure

# Simulate real-world kappa paths.
sim_kappa_P<-function(rw,H,eps){
  n<-nrow(eps)
  cumulative_eps<-t(apply(eps,1,cumsum))
  rw$kt_last+matrix(rw$theta*seq_len(H),nrow=n,ncol=H,byrow=TRUE)+rw$sigma*cumulative_eps
}
set.seed(seed_price)
for(sex in sexes){
  eps<-matrix(rnorm(n_sim_price*H_price),nrow=n_sim_price,ncol=H_price)
  kappa_P<-sim_kappa_P(pricing[[sex]]$rwd,H_price,eps)
  colnames(kappa_P)<-as.character(mortality_years)
  pricing[[sex]]$eps<-eps
  pricing[[sex]]$kappa_P<-kappa_P
  pricing[[sex]]$kappa_central<-pricing[[sex]]$rwd$kt_last+h_seq_price*pricing[[sex]]$rwd$theta
}
# Extend mortality beyond age 100.
logit_f<-function(p) log(p/(1-p))
expit_f<-function(z) 1/(1+exp(-z))
# Fit the Kannisto closure for each fixed kappa value.
closure_logm<-function(ax,bx,kappa,age_out,band,model,anchor){
  band_names<-as.character(band)
  logm_band<-outer(as.numeric(bx[band_names]),kappa)+as.numeric(ax[band_names])
  #using kannisto mortality smoothness
  mu_band<-exp(logm_band)
  #to avoid exploding,use lower and upper floor away from 0 and 1 
  mu_band<-pmin(pmax(mu_band,1e-12),1-1e-12)
  response<-logit_f(mu_band)
  age_centered<-band-mean(band)
  slope<-as.numeric(crossprod(age_centered,response)/sum(age_centered^2))
  if(anchor){
    intercept<-response[which.max(band),]-slope*max(band)
  }else{ # the standard least square sbar,ybar
    intercept<-colMeans(response)-slope*mean(band)
  }
  linear_predictor<-intercept+slope*age_out
  if(model=="kannisto"){
    log(expit_f(linear_predictor))
  }else{
    linear_predictor
  }
}
# Chapter 6: Longevity Bond Pricing
# Section 6.4: Survival Probabilities and Term Structure

# Calculate cohort survival under P.
cohort_survival<-function(ax,bx,ages_fit,kappa_paths,mortality_ages,
                          mortality_years,payment_years,terminal_age,
                          band,model,anchor){
  n<-nrow(kappa_paths)
  H<-ncol(kappa_paths)
  logm<-matrix(NA_real_,nrow=n,ncol=H)
  for(j in seq_len(H)){
    age_j<-mortality_ages[j]
    kappa_j<-kappa_paths[,j]
    if(age_j<=max(ages_fit)){
      age_name<-as.character(age_j)
      logm[,j]<-as.numeric(ax[age_name])+as.numeric(bx[age_name])*kappa_j
    }else{
      logm[,j]<-closure_logm(ax=ax,bx=bx,kappa=kappa_j,age_out=age_j,
                             band=band,model=model,anchor=anchor)
    }
  }
  # p_{x,t}=exp(-m_{x,t}).
  p_one_year<-exp(-exp(logm))
  terminal_column<-which(mortality_ages==terminal_age)
  if(length(terminal_column)!=1){
    stop("more than one occurance for the terminal, must be exactly once in mortality_ages")
  }
  p_one_year[,terminal_column]<-0 # q_terminal=1
  survival<-t(apply(p_one_year,1,cumprod)) #survival probability for each year and age
  colnames(p_one_year)<-as.character(mortality_years)
  colnames(survival)<-as.character(payment_years)
  list(logm=logm,p_one_year=p_one_year,survival=survival)
}
for(sex in sexes){
  pr<-pricing[[sex]]
  surv_P<-cohort_survival(ax=pr$ax,bx=pr$bx,ages_fit=pr$ages,
                          kappa_paths=pr$kappa_P,mortality_ages=mortality_ages,
                          mortality_years=mortality_years,payment_years=payment_years,
                          terminal_age=age_terminal,band=closure_band,
                          model=closure_model,anchor=closure_anchor)
  surv_ref<-cohort_survival(ax=pr$ax,bx=pr$bx,ages_fit=pr$ages,
                            kappa_paths=matrix(pr$kappa_central,nrow=1),
                            mortality_ages=mortality_ages,
                            mortality_years=mortality_years,
                            payment_years=payment_years,terminal_age=age_terminal,
                            band=closure_band,model=closure_model,
                            anchor=closure_anchor)$survival[1,]
  pricing[[sex]]$surv_P<-surv_P
  pricing[[sex]]$EP<-colMeans(surv_P$survival)
  pricing[[sex]]$surv_ref<-as.numeric(surv_ref)
  pricing[[sex]]$surv_sorted<-apply(surv_P$survival,2,sort)
}
pricing$Male$EP
terminal_closed<-vapply(pricing,function(pr){
  tail(pr$surv_ref,1)==0 && all(pr$surv_P$survival[,H_price]==0)
},logical(1))
pricing$Female

for(sex in sexes){
  h_show<-c(5,10,15,20,25)
  out<-data.frame(h=h_show,attained_age=x0_cohort+h_show,
                  payment_year=issue_year+h_show,
                  p_ref=pricing[[sex]]$surv_ref[h_show],
                  EXP_P_I=pricing[[sex]]$EP[h_show],
                  path_gap=pricing[[sex]]$EP[h_show]-pricing[[sex]]$surv_ref[h_show])
  cat(sex) 
  print(round(out,6),row.names=FALSE)}

# Chapter 6: Longevity Bond Pricing
# Section 6.3: Calibration of the Wang Parameter $\lambda$

# Define empirical Wang weights for simulated survival probabilities.
wang_weights_upper<-function(n,alpha){
  u<-1-(0:n)/n #n+1 points and desending matching the survival way
  weights<- -diff(pnorm(qnorm(u)+alpha)) #diff result in n points #minus to define the weights as positive
  if(any(weights < -1e-12)||abs(sum(weights)-1)>1e-10){
    stop("Invalid Wang weights")
  }
  pmax(weights,0)
}
# build a function that gives back tilted average 
wang_mean_upper_sorted<-function(sorted_x,alpha){
  sum(sorted_x*wang_weights_upper(length(sorted_x),alpha))
}
wang_mean_upper<-function(x,alpha){
  wang_mean_upper_sorted(sort(x),alpha)
}

# Apply the Wang transform to the death-probability CDF following Denuit.
wang_survival_curve<-function(p_ann,alpha_cdf){
  q<-1-p_ann
  qstar<-q
  interior<-q>0&q<1
  qstar[interior]<-pnorm(qnorm(q[interior])+alpha_cdf)
  1-qstar # get the distorted survival 
}

# Value the quoted single-life annuity with monthly advance payments, a level guarantee, and no escalation.
# Interpolate survival log-linearly within each year and treat guaranteed payments as certain.
annuity_value<-function(surv_cum,disc_cc,horizon_years,freq,timing,guarantee_years){
  H<-length(surv_cum)
  if(length(disc_cc)!=H){
    stop("The discount curve and survival curve have different lengths")
  }
  if(any(diff(c(1,surv_cum))>1e-12)){
    stop("surv_cum must be a non-increasing cumulative survival curve")
  }
  surv_full<-c(1,surv_cum) # survival to times 0,1,...,H
  spot_full<-c(disc_cc[1],disc_cc) # cc spot to times 0,1,...,H
  n_months<-min(horizon_years,H)*freq
  offset<-if(timing=="advance") 0 else 1
  value<-0
  for(k in seq_len(n_months)){
    t<-(k-1+offset)/freq
    yr<-floor(t)
    frac<-t-yr
    s0<-surv_full[yr+1]
    s1<-surv_full[yr+2]
    st<-if(s0>0&&s1>0) s0^(1-frac)*s1^frac else 0 # the blend between both
    r0<-spot_full[yr+1]; r1<-spot_full[yr+2] # interpolate the spot rate too
    rt<-r0*(1-frac)+r1*frac
    guaranteed<- t<guarantee_years
    pay_prob<-if(guaranteed) 1 else st
    value<-value+(1/freq)*exp(-rt*t)*pay_prob
  }
  value
}
# the wang transformed annuty value
annuity_value_wang<-function(surv_cum,alpha_cdf,disc_cc,horizon_years,freq,
                             timing,guarantee_years){
  surv_star<-wang_survival_curve(surv_cum,alpha_cdf)
  annuity_value(surv_star,disc_cc,horizon_years,freq,timing,guarantee_years)
}

# Solve for alpha_cdf using the selected annuity discount curve.
calibrate_alpha_from_annuity<-function(p_ann,target_price,
                                       disc_cc=spot_cc_curve_annuity,
                                       interval=c(-3,3)){
  f<-function(alpha_cdf){
    annuity_value_wang(p_ann,alpha_cdf,disc_cc=disc_cc,
                       horizon_years=H_price,freq=annuity_payment_freq,
                       timing=annuity_payment_timing,
                       guarantee_years=annuity_guarantee_years)-target_price}
  f_interval<-vapply(interval,f,numeric(1))
  if(prod(f_interval)>0){
    stop("The annuity target ",round(target_price,4),
         " is not bracketed by alpha in [",interval[1],", ",interval[2],
         "]. Annuity values at the endpoints: ",
         round(f_interval[1]+target_price,4)," and ",
         round(f_interval[2]+target_price,4))
  }
  uniroot(f,interval=interval,tol=1e-10)$root
}

# Calibrate alpha_cdf by distorting the reference survival curve.
lambda_proc<-setNames(numeric(length(sexes)),sexes)
lambda_calibration_table<-NULL
calibration_rows<-list()
for(sex in sexes){
  target_price<-unname(market_annuity_price[sex])
  p_ann<-pricing[[sex]]$surv_ref
  #undisorted price
  plugin_price<-annuity_value_wang(p_ann,0,spot_cc_curve_annuity,H_price,
                                     annuity_payment_freq,annuity_payment_timing,
                                     annuity_guarantee_years)
  alpha_cdf<-calibrate_alpha_from_annuity(p_ann,target_price,
                                            disc_cc=spot_cc_curve_annuity)
  fitted_price<-annuity_value_wang(p_ann,alpha_cdf,spot_cc_curve_annuity,H_price,
                                     annuity_payment_freq,annuity_payment_timing,
                                     annuity_guarantee_years)
  lambda_proc[sex]<- -alpha_cdf
  calibration_rows[[length(calibration_rows)+1]]<-data.frame(
      sex=sex,quote_date=annuity_quote_date[sex],
      annuity_curve_date=annuity_curve_date,annuity_curve_ccy=annuity_curve_ccy,
      fund=annuity_fund,commission=annuity_commission,annual_pension=annuity_pension[sex],
      target_annuity_price=target_price,plugin_annuity_price=plugin_price,
      alpha_cdf=alpha_cdf,lambda_proc=-alpha_cdf,
      fitted_annuity_price=fitted_price,
      calibration_error=fitted_price-target_price,row.names=NULL
    )
}
calibration_rows
lambda_calibration_table<-do.call(rbind,calibration_rows)
lambda_abs<-abs(lambda_proc)
cat("Wang parameter used in Chapter 7")
print(data.frame(sex=sexes,source=lambda_source,
                 alpha_cdf=-as.numeric(lambda_proc[sexes]),
                 lambda_proc=as.numeric(lambda_proc[sexes]),
                 lambda_abs=as.numeric(lambda_abs[sexes]),
                 row.names=NULL))
print(round_numeric_df(lambda_calibration_table,8),row.names=FALSE)

# Chapter 6: Longevity Bond Pricing
# Section 6.2: Monte Carlo Pricing under the Martingale Measure

# Apply the Wang transform using the calibrated lambda.
# Process route
shift_kappa_Q<-function(kappa_P,rw,lambda){
  H<-ncol(kappa_P)
  shift<-lambda*rw$sigma*seq_len(H)          # lambda*sigma*h, grows with h
  kappa_P-matrix(shift,nrow=nrow(kappa_P),ncol=H,byrow=TRUE)
}
for(sex in sexes){
  pr<-pricing[[sex]]
  lambda_sex<-unname(lambda_proc[sex])
  pricing[[sex]]$lambda_proc<-lambda_sex
  pricing[[sex]]$lambda_abs<-abs(lambda_sex)
  # shift every kappa path by lambda*sigma*h, then recompute survival
  kappa_Q<-shift_kappa_Q(pr$kappa_P,pr$rwd,lambda_sex)
  surv_Q<-cohort_survival(ax=pr$ax,bx=pr$bx,ages_fit=pr$ages,
                          kappa_paths=kappa_Q,mortality_ages=mortality_ages,
                          mortality_years=mortality_years,payment_years=payment_years,
                          terminal_age=age_terminal,band=closure_band,
                          model=closure_model,anchor=closure_anchor)
  
  pricing[[sex]]$theta_Q<-pr$rwd$theta-lambda_sex*pr$rwd$sigma # new drift
  pricing[[sex]]$kappa_Q<-kappa_Q
  pricing[[sex]]$surv_Q<-surv_Q
  pricing[[sex]]$EQ_path<-colMeans(surv_Q$survival)
}

# maturity wise, distort each horizon's survival distribution with α_h = λ√h
for(sex in sexes){
  pr<-pricing[[sex]]
  lambda_sex<-unname(lambda_proc[sex])
  pricing[[sex]]$rho_mat_sqrt<-vapply(seq_len(H_price),function(h){
    wang_mean_upper_sorted(pr$surv_sorted[,h], lambda_sex*sqrt(h)) #alpha_h=lambda*sqrt(h)
  },numeric(1))
}
#Denuit constant route
for(sex in sexes){
  pr<-pricing[[sex]]
  lambda_sex<-unname(lambda_proc[sex])
  pricing[[sex]]$rho_Denuit<-vapply(seq_len(H_price),function(h){
    wang_mean_upper_sorted(pr$surv_sorted[,h], lambda_sex) # constant alpha = lambda
  },numeric(1))
}
pricing$Female
  
# Chapter 6: Longevity Bond Pricing
# Section 6.5: The Relative Additive Margin

# Price the bond and calculate the relative additive margin.
relative_additive_margin<-function(rho,p_ref,disc,maturity){
  h<-seq_len(maturity)
  sum(disc[h]*(rho[h]-p_ref[h]))/sum(disc[h])
}

relative_margin_curve<-function(rho,p_ref,disc){
  cumsum(disc*(rho-p_ref))/cumsum(disc)} #extra to check the progration of k telda 

price_Denuit_bond<-function(rho,p_ref,disc,maturity,coupon,principal){
  h<-seq_len(maturity)
  k_tilde<-relative_additive_margin(rho,p_ref,disc,maturity)
  k_star<-coupon*k_tilde
  coupon_ce<-coupon*(1+p_ref[h]-rho[h])+k_star
  pv_survivor<-sum(disc[h]*coupon_ce)+principal*disc[maturity]
  pv_fixed<-coupon*sum(disc[h])+principal*disc[maturity]
  list(k_tilde=k_tilde,k_star=k_star,coupon_ce=coupon_ce,
       pv_survivor=pv_survivor,pv_fixed=pv_fixed,
       indifference_identity_error=pv_survivor-pv_fixed)}

price_standard_LB<-function(expected_survival,disc,maturity,notional=1){
  h<-seq_len(maturity)
  notional*sum(disc[h]*expected_survival[h])}

standard_LB_path_values<-function(survival_paths,disc,maturity,notional=1){
  h<-seq_len(maturity)
  as.numeric(notional*survival_paths[,h,drop=FALSE]%*%disc[h]) #to finds the sd and distribution
}

# Chapter 7: Empirical Results
# Section 7.3: Bond Pricing Results

bond_rows<-list()
for(sex in sexes){
  pr<-pricing[[sex]]
  denuit_path<-price_Denuit_bond(pr$EQ_path,pr$surv_ref,disc_curve,T_bond,coupon,principal)
  denuit_mat_sqrt<-price_Denuit_bond(pr$rho_mat_sqrt,pr$surv_ref,disc_curve,T_bond,coupon,principal)
  denuit_benchmark<-price_Denuit_bond(pr$rho_Denuit,pr$surv_ref,disc_curve,T_bond,coupon,principal)
  pv_standard_P<-price_standard_LB(pr$EP,disc_curve,T_bond,notional=coupon)
  pv_standard_Q<-price_standard_LB(pr$EQ_path,disc_curve,T_bond,notional=coupon)
  pv_standard_Q_mat_sqrt<-price_standard_LB(pr$rho_mat_sqrt,disc_curve,T_bond,notional=coupon)
  pv_standard_Q_benchmark<-price_standard_LB(pr$rho_Denuit,disc_curve,T_bond,notional=coupon)
  pv_paths_P<-standard_LB_path_values(pr$surv_P$survival,disc_curve,T_bond,notional=coupon)
  pv_paths_Q<-standard_LB_path_values(pr$surv_Q$survival,disc_curve,T_bond,notional=coupon)
  pv_loading<-pv_standard_Q-pv_standard_P
  mc_se_loading<-sd(pv_paths_Q-pv_paths_P)/sqrt(n_sim_price)
  loading_to_mc_se<-if(mc_se_loading>0) abs(pv_loading)/mc_se_loading else NA_real_
  pricing[[sex]]$denuit_path<-denuit_path
  pricing[[sex]]$denuit_mat_sqrt<-denuit_mat_sqrt
  pricing[[sex]]$denuit_benchmark<-denuit_benchmark
  pricing[[sex]]$standard_LB<-list(
    P=pv_standard_P,Q_path=pv_standard_Q,Q_mat_sqrt=pv_standard_Q_mat_sqrt,
    Q_Denuit=pv_standard_Q_benchmark,loading=pv_loading,
    loading_per=100*(pv_standard_Q/pv_standard_P-1),
    mc_se_P=sd(pv_paths_P)/sqrt(n_sim_price),
    mc_se_Q=sd(pv_paths_Q)/sqrt(n_sim_price),
    mc_se_loading=mc_se_loading,loading_to_mc_se=loading_to_mc_se
  )
  pricing[[sex]]$k_tilde_curve_path<-relative_margin_curve(pr$EQ_path,pr$surv_ref,disc_curve)
  pricing[[sex]]$k_tilde_curve_mat_sqrt<-relative_margin_curve(pr$rho_mat_sqrt,pr$surv_ref,disc_curve)
  pricing[[sex]]$k_tilde_curve_Denuit<-relative_margin_curve(pr$rho_Denuit,pr$surv_ref,disc_curve)
  bond_rows[[length(bond_rows)+1]]<-data.frame(
    sex=sex,lambda_proc=pr$lambda_proc,lambda_abs=pr$lambda_abs,
    k_tilde_path_per=100*denuit_path$k_tilde,
    k_tilde_mat_sqrt_per=100*denuit_mat_sqrt$k_tilde,
    k_tilde_Denuit_per=100*denuit_benchmark$k_tilde,
    k_star_path=denuit_path$k_star,Denuit_bond_PV=denuit_path$pv_survivor,
    fixed_bond_PV=denuit_path$pv_fixed,
    indifference_identity_error=denuit_path$indifference_identity_error,
    standard_LB_P=pv_standard_P,standard_LB_Q=pv_standard_Q,
    standard_LB_Q_mat_sqrt=pv_standard_Q_mat_sqrt,standard_LB_loading=pv_loading,
    standard_LB_loading_per=100*(pv_standard_Q/pv_standard_P-1),
    standard_LB_mc_se_Q=sd(pv_paths_Q)/sqrt(n_sim_price),
    standard_LB_mc_se_loading=mc_se_loading,
    standard_LB_loading_to_mc_se=loading_to_mc_se,row.names=NULL
  )
}
bond_table<-do.call(rbind,bond_rows)
cat("\n--- Chapter 7 bond-pricing summary ---\n")
print(round_numeric_df(bond_table,6),row.names=FALSE)

# Chapter 7: Empirical Results
# Section 7.2: Forecasting Results

# Calculate the survival term structure.
term_rows<-list()
for(sex in sexes){
  pr<-pricing[[sex]]
  lo_P<-apply(pr$surv_P$survival,2,quantile,probs=0.025)
  hi_P<-apply(pr$surv_P$survival,2,quantile,probs=0.975)
  lo_Q<-apply(pr$surv_Q$survival,2,quantile,probs=0.025)
  hi_Q<-apply(pr$surv_Q$survival,2,quantile,probs=0.975)
  pricing[[sex]]$term<-list(EP=pr$EP,EQ_path=pr$EQ_path,rho_mat_sqrt=pr$rho_mat_sqrt,
                            rho_Denuit=pr$rho_Denuit,p_ref=pr$surv_ref,
                            lo_P=lo_P,hi_P=hi_P,lo_Q=lo_Q,hi_Q=hi_Q)
  term_rows[[length(term_rows)+1]]<-data.frame(
    sex=sex,h=h_seq_price,mortality_age=mortality_ages,attained_age=attained_ages,
    mortality_year=mortality_years,payment_year=payment_years,
    p_ref=pr$surv_ref,p_P=pr$EP,p_Q_path=pr$EQ_path,
    rho_mat_sqrt=pr$rho_mat_sqrt,rho_Denuit=pr$rho_Denuit,
    survival_loading_pct=ifelse(pr$EP>0,100*(pr$EQ_path/pr$EP-1),NA_real_),
    k_tilde_path_pct=100*pr$k_tilde_curve_path,discount_factor=disc_curve,
    row.names=NULL
  )
}
term_table<-do.call(rbind,term_rows)
print(round_numeric_df(term_table[term_table$h%%5==0,],6),row.names=FALSE)

# Chapter 7: Empirical Results
# Section 7.4: Term Structure of the Risk Margin

margin_shape_rows<-list()
for(sex in sexes){
  pr<-pricing[[sex]]
  curve_list<-list(path=100*pr$k_tilde_curve_path,
                   maturity_sqrt=100*pr$k_tilde_curve_mat_sqrt,
                   Denuit_constant=100*pr$k_tilde_curve_Denuit)
  for(construction in names(curve_list)){
    curve<-curve_list[[construction]][attained_ages<=age_terminal]
    early_h<-seq_len(min(T_bond,length(curve)))
    late_h<-seq.int(max(1L,length(curve)-9L),length(curve))
    early_slope<-unname(coef(lm(curve[early_h]~early_h))[2])
    late_slope<-unname(coef(lm(curve[late_h]~late_h))[2])
    margin_shape_rows[[length(margin_shape_rows)+1L]]<-data.frame(
      sex=sex,construction=construction,maximum_margin_per=max(curve),
      maturity_at_max=which.max(curve),bond_maturity_margin_per=curve[T_bond],
      terminal_margin_per=curve[length(curve)],under_five_per=all(curve<5),
      early_slope_perc_per_year=early_slope,late_slope_perc_per_year=late_slope,
      late_slope_smaller=abs(late_slope)<abs(early_slope),row.names=NULL
    )
  }
}
margin_shape_table<-do.call(rbind,margin_shape_rows)

merge(margin_shape_table[,c("sex","construction","bond_maturity_margin_per")],
      data.frame(sex=bond_table$sex,path=bond_table$k_tilde_path_per,
                 maturity_sqrt=bond_table$k_tilde_mat_sqrt_per,
                 Denuit_constant=bond_table$k_tilde_Denuit_per))
pathunc_rows<-list()
for(sex in sexes){
  pr<-pricing[[sex]]
  h<-seq_len(T_bond)
  A<-sum(disc_curve[h])
  k_pathunc<-sum(disc_curve[h]*(pr$EP[h]-pr$surv_ref[h]))/A
  k_risk<-sum(disc_curve[h]*(pr$EQ_path[h]-pr$EP[h]))/A
  pathunc_rows[[length(pathunc_rows)+1L]]<-data.frame(
    sex=sex,
    k_tilde_total_per=100*(k_pathunc+k_risk),
    k_tilde_pathunc_per=100*k_pathunc,
    k_tilde_risk_per=100*k_risk,
    pathunc_share_per=100*k_pathunc/(k_pathunc+k_risk),
    row.names=NULL)
}
pathunc_split<-do.call(rbind,pathunc_rows)
print(round_numeric_df(pathunc_split,6),row.names=FALSE)

# Chapter 7: Empirical Results
# Section 7.4: Term Structure of the Risk Margin

# Evaluate sensitivity to lambda and the discount curve.
lambda_grid<-c(-1.00,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1.00)
run_lambda_sensitivity<-TRUE
spot_shift_grid<-c(-0.01,-0.005,0,0.005,0.01)
stopifnot(x0_cohort<age_terminal,
          T_bond>=1,
          T_bond<=age_terminal-x0_cohort,
          n_sim_price>=1000)

reprice_path_lambda<-function(sex,lambda){
  pr<-pricing[[sex]]
  kappa_Q<-shift_kappa_Q(pr$kappa_P,pr$rwd,lambda) #we reuse the same kappa 
  colMeans(cohort_survival(ax=pr$ax,bx=pr$bx,ages_fit=pr$ages,kappa_paths=kappa_Q,
                           mortality_ages=mortality_ages,mortality_years=mortality_years,
                           payment_years=payment_years,terminal_age=age_terminal,
                           band=closure_band,model=closure_model,
                           anchor=closure_anchor)$survival)
}
lambda_sensitivity<-NULL
if(run_lambda_sensitivity){
  sensitivity_rows<-list()
  for(sex in sexes){
    pr<-pricing[[sex]]
    for(lambda_value in lambda_grid){
      rho_value<-reprice_path_lambda(sex,lambda_value)
      pv_value<-price_standard_LB(rho_value,disc_curve,T_bond,notional=coupon)
      sensitivity_rows[[length(sensitivity_rows)+1]]<-data.frame(
        sex=sex,lambda_proc=lambda_value,
        k_tilde_pct=100*relative_additive_margin(rho_value,pr$surv_ref,disc_curve,T_bond),
        standard_LB_price=pv_value,
        price_loading_pct=100*(pv_value/pricing[[sex]]$standard_LB$P-1),row.names=NULL
      )
    }
  }
  lambda_sensitivity<-do.call(rbind,sensitivity_rows)
  cat("Sensitivity to the Wang parameter")
  print(round_numeric_df(lambda_sensitivity,6),row.names=FALSE)
}
# The shift is applied to the pound bond curve only. lambda was calibrated on
# the euro annuity curve and is held fixed, so this isolates the discounting
# channel from the mortality-loading channel.
rate_rows<-list()
for(sex in sexes){
  pr<-pricing[[sex]]
  for(shift in spot_shift_grid){
    shifted_spot_cc<-spot_cc_curve+shift
    disc_value<-exp(-shifted_spot_cc*h_seq_price)
    rate_rows[[length(rate_rows)+1]]<-data.frame(
      sex=sex,shift =shift,lambda_proc_held_fixed=pr$lambda_proc,
      k_tilde_pct=100*relative_additive_margin(pr$EQ_path,pr$surv_ref,disc_value,T_bond),
      standard_LB_price=price_standard_LB(pr$EQ_path,disc_value,T_bond,notional=coupon),
      row.names=NULL
    )
  }
}
rate_sensitivity<-do.call(rbind,rate_rows)
cat("\n--- Sensitivity to parallel sterling spot-curve shifts ---\n")
print(round_numeric_df(rate_sensitivity,6),row.names=FALSE)

# Chapter 7: Empirical Results
# Section 7.1: Lee-Carter Calibration Results

# Plots
# PLOT: Kannisto closure of the mortality surface along the cohort diagonal . 
par(mfrow=c(1,length(sexes)))
for(sex in sexes){
  pr<-pricing[[sex]]
  surv_c<-cohort_survival(ax=pr$ax,bx=pr$bx,ages_fit=pr$ages,
                          kappa_paths=matrix(pr$kappa_central,nrow=1),
                          mortality_ages=mortality_ages,mortality_years=mortality_years,
                          payment_years=payment_years,terminal_age=age_terminal,
                          band=closure_band,model=closure_model,anchor=closure_anchor)
  plot(NA,xlim=c(65,125),ylim=range(surv_c$logm[1,],na.rm=TRUE),
       xlab="Attained age",ylab=expression(log~m),
       main=paste0(sex,": cohort log-mortality, central path"))
  lines(mortality_ages,surv_c$logm[1,],lwd=2,col="navy")
  abline(v=100.5,lty=2,col="red")
  text(100.5,min(surv_c$logm[1,],na.rm=TRUE),"Kannisto extension",
       pos=4,cex=0.7,col="red")
}
par(mfrow=c(1,1))

# Chapter 7: Empirical Results
# Section 7.2: Forecasting Results
#PLOT: Period-index forecasts under P and Q^W 
old_par<-par(no.readonly=TRUE)
par(mfrow=c(1,length(sexes)))
for(sex in sexes){
  pr<-pricing[[sex]]
  lo_P<-apply(pr$kappa_P,2,quantile,probs=0.025)
  hi_P<-apply(pr$kappa_P,2,quantile,probs=0.975)
  lo_Q<-apply(pr$kappa_Q,2,quantile,probs=0.025)
  hi_Q<-apply(pr$kappa_Q,2,quantile,probs=0.975)
  plot(NA,xlim=c(min(pr$years),max(mortality_years)),
       ylim=range(pr$kt,lo_P,hi_P,lo_Q,hi_Q),xlab="Calendar year",
       ylab=expression(kappa[t]),main=paste0(sex,": period index under P and Q^W"))
  polygon(c(mortality_years,rev(mortality_years)),c(lo_P,rev(hi_P)),
          col=rgb(0.25,0.50,0.90,0.25),border=NA)
  polygon(c(mortality_years,rev(mortality_years)),c(lo_Q,rev(hi_Q)),
          col=rgb(0.90,0.35,0.15,0.25),border=NA)
  lines(pr$years,pr$kt,lwd=2,col="black")
  lines(mortality_years,colMeans(pr$kappa_P),lwd=2,col="navy")
  lines(mortality_years,colMeans(pr$kappa_Q),lwd=2,col="darkorange3")
  abline(v=forecast_origin+0.5,lty=2)
  legend("bottomleft",bty="n",cex=0.75,legend=c("fitted","P mean","Q^W mean"),
         col=c("black","navy","darkorange3"),lwd=2)
}
for(sex in sexes){
  pr<-pricing[[sex]]
  w_P<-apply(pr$kappa_P,2,quantile,.975)-apply(pr$kappa_P,2,quantile,.025)
  w_Q<-apply(pr$kappa_Q,2,quantile,.975)-apply(pr$kappa_Q,2,quantile,.025)
  cat(sex,"max width difference:",max(abs(w_P-w_Q)),"\n")
}

# Chapter 7: Empirical Results
# Section 7.2: Forecasting Results
# PLOT: Survival term structures under both measures 
par(mfrow=c(1,length(sexes)))
for(sex in sexes){
  tm<-pricing[[sex]]$term
  plot(NA,xlim=range(attained_ages),ylim=c(0,1),xlab="Attained age at payment",
       ylab=expression({}[h]*p[x[0]]),
       main=paste0(sex,": cohort aged ",x0_cohort," in ",issue_year))
  polygon(c(attained_ages,rev(attained_ages)),c(tm$lo_P,rev(tm$hi_P)),
          col=rgb(0.25,0.50,0.90,0.20),border=NA)
  polygon(c(attained_ages,rev(attained_ages)),c(tm$lo_Q,rev(tm$hi_Q)),
          col=rgb(0.90,0.35,0.15,0.20),border=NA)
  lines(attained_ages,tm$EP,lwd=2,col="navy")
  lines(attained_ages,tm$EQ_path,lwd=2,col="darkorange3")
  lines(attained_ages,tm$p_ref,lwd=2,lty=2,col="black")
  abline(v=x0_cohort+T_bond,lty=3)
  abline(v=max(pricing[[sex]]$ages),lty=2)
  legend("bottomleft",bty="n",cex=0.70,
         legend=c("E^P[I_h]","E^{Q^W}[I_h]","reference table"),
         col=c("navy","darkorange3","black"),lwd=2,lty=c(1,1,2))
}

for(sex in sexes) 
  cat(sex, 65 + which.max(pricing[[sex]]$EQ - pricing[[sex]]$EP), "\n")

# Chapter 7: Empirical Results
# Section 7.4: Term Structure of the Risk Margin
# PLOT: Term structure of the relative additive margin .
par(mfrow=c(1,length(sexes)))
for(sex in sexes){
  pr<-pricing[[sex]]
  matplot(payment_years,
          100*cbind(pr$k_tilde_curve_path,
                    pr$k_tilde_curve_mat_sqrt,
                    pr$k_tilde_curve_Denuit),
          type="l",lwd=c(2.5,1.5,1.5),lty=c(1,2,3),
          col=c("navy","darkorange3","grey45"),
          xlab="Final payment year",ylab="Relative additive margin (%)",
          main=paste0(sex,": margin term structure"))
  abline(v=issue_year+T_bond,lty=3,col="grey70")
  legend("bottomright",bty="n",cex=0.8,
         legend=c("Process-level","Maturity-wise sqrt(h)","Denuit constant"),
         col=c("navy","darkorange3","grey45"),lty=c(1,2,3),lwd=c(2.5,1.5,1.5))
}
# PLOT: Sensitivity of the 25-year margin to the Wang parameter 
par(mfrow=c(1,1))
if(run_lambda_sensitivity){
  sensitivity_matrix<-do.call(cbind,lapply(sexes,function(sex){
    lambda_sensitivity$k_tilde_pct[lambda_sensitivity$sex==sex]
  }))
  colnames(sensitivity_matrix)<-sexes
  matplot(lambda_grid,sensitivity_matrix,type="b",pch=19,lty=1,lwd=2,
          col=c("navy","darkorange3"),xlab=expression(lambda),
          ylab="Relative additive margin (%)",main="Margin against the Wang parameter")
  abline(v=lambda_proc, lty=3)
  legend("topleft",legend=sexes,col=c("navy","darkorange3"),lwd=2,bty="n")
}else{
  plot.new()
  title("Lambda sensitivity not run")
}
