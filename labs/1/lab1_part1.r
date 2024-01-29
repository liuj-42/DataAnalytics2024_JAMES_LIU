read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("./2010EPI_data.xls")

# View(mysheets$`EPI2010_all countries`)



epi_data <- read.csv("./2010EPI_data.csv", skip=1)


head(epi_data)
summary(epi_data)
View(epi_data)


attach(epi_data)

# fix(epi_data) # simple df editor

# read in EPI data and remove null values
epi <- epi_data$EPI
tf <- is.na(epi)
epi_cleaned <- epi[!tf]

summary(epi)
summary(epi_cleaned)

fivenum(epi, na.rm=T)

# stem and leaf plot
stem(epi_cleaned)

# histogram with auto fitting
hist(epi_cleaned)
# histogram with custom fitting
hist(epi_cleaned, seq(30., 95., 1.0), prob=T)

# draw a best fit line with different algorithms
lines(density(epi_cleaned,bw=1.))
lines(density(epi_cleaned,bw="SJ"))
# lines(density(epi,na.rm=T, bw=1.))
# lines(density(epi,na.rm=T, bw="SJ"))
# rug plot
rug(epi_cleaned)

# plot the ecdf of the EPI data w/ lines in between points
plot(ecdf(epi), do.points=F, verticals=T)

# make the graph a square
par(pty="s")

# quantile-quantile plot of the EPI data
qqnorm(epi_cleaned)
# overlay y=x line over the plot
qqline(epi_cleaned)

# temp<-seq(30,95,1)
# qqplot(qt(ppoints(250), df=5), temp, xlab="Q-Q plot for t dsn")
# qqline(temp)


# do the same as above with the DALY data
daly <- epi_data$DALY
tf <- is.na(daly)
daly_cleaned <- daly[!tf]

summary(daly_cleaned)

fivenum(daly_cleaned)
stem(daly_cleaned)
hist(daly_cleaned)
hist(daly_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(daly_cleaned,bw="SJ"))
rug(daly_cleaned)

plot(ecdf(daly), do.points=F, verticals=T)
par(pty="s")
qqnorm(daly_cleaned)
qqline(daly_cleaned)

# do the same as above with the water health data
water_h <- epi_data$WATER_H
tf <- is.na(water_h)
water_h_cleaned <- water_h[!tf]

stem(water_h_cleaned)
hist(water_h_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(water_h_cleaned, bw="SJ"))
rug(water_h_cleaned)

plot(ecdf(water_h))
plot(ecdf(water_h), do.points=F, verticals=T)
par(pty="s")
qqnorm(water_h_cleaned)
qqline(water_h_cleaned)

# do the same as above with the ecosystem data
ecosystem <- epi_data$ECOSYSTEM
ecosystem_cleaned <- ecosystem[!is.na(ecosystem)]
boxplot(ecosystem_cleaned)
hist(ecosystem_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(ecosystem_cleaned, bw="SJ"))
rug(ecosystem_cleaned)

plot(ecdf(ecosystem_cleaned), do.points=F, verticals=T)
# par(pty="s")
qqnorm(ecosystem_cleaned)
qqline(ecosystem_cleaned)

# do the same as above with the air health data
air_h <- epi_data$AIR_H
air_h_cleaned <- air_h[!is.na(air_h)]
# boxplot(air_h_cleaned)
hist(air_h_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(air_h_cleaned, bw="SJ"))
rug(air_h_cleaned)

plot(ecdf(air_h_cleaned), do.points=F, verticals=T)

qqnorm(air_h_cleaned)
qqline(air_h_cleaned)

# do the same as above with the AIR_E data

air_e <- epi_data$AIR_E
air_e_cleaned <- air_e[!is.na(air_e)]
# boxplot(air_e_cleaned)
hist(air_e_cleaned)
hist(air_e_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(air_e_cleaned, bw="SJ"))
rug(air_e_cleaned)

plot(ecdf(air_e_cleaned), do.points=F, verticals=T)
qqnorm(air_e_cleaned)
qqline(air_e_cleaned)

# do the same as above with the WATER_E data
water_e <- epi_data$WATER_E
water_e_cleaned <- water_e[!is.na(water_e)]

hist(water_e_cleaned)
hist(water_e_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(water_e_cleaned, bw="SJ"))
rug(water_e_cleaned)

plot(ecdf(water_e_cleaned), do.points=F, verticals=T)

qqnorm(water_e_cleaned)
qqline(water_e_cleaned)

help(qqnorm)


# do the same as above with biodiversity
biodiversity <- epi_data$BIODIVERSITY
biodiversity_cleaned <- biodiversity[!is.na(biodiversity)]

hist(biodiversity_cleaned)
hist(biodiversity_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(biodiversity_cleaned, bw="SJ"))
rug(biodiversity_cleaned)

plot(ecdf(biodiversity_cleaned), do.points=F, verticals=T)

qqnorm(biodiversity_cleaned)
qqline(biodiversity_cleaned)


# do the same as above with environment health
envhealth <- epi_data$ENVHEALTH
envhealth_cleaned <- envhealth[!is.na(envhealth)]

hist(envhealth_cleaned)
hist(envhealth_cleaned, seq(0.,100.,5.0), prob=T)
lines(density(envhealth_cleaned, bw="SJ"))
rug(envhealth_cleaned)

plot(ecdf(envhealth_cleaned), do.points=F, verticals=T)

qqnorm(envhealth_cleaned)
qqline(envhealth_cleaned)






par(pty="m")
names <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIVERSITY")
boxplot(epi_cleaned, envhealth_cleaned, ecosystem_cleaned, daly_cleaned, air_h_cleaned, water_h_cleaned, air_e_cleaned, water_e_cleaned, biodiversity_cleaned, names=names, main="EPI vs all data")

temp <- c("EPI", "ENVHEALTH")
boxplot(epi_cleaned, envhealth_cleaned, names=temp, main="EPI vs ENVHEALTH")

temp[2] <- "ECOSYSTEM"
boxplot(epi_cleaned, ecosystem_cleaned, names=temp, main="EPI vs ECOSYSTEM")

temp[2] <- "DALY"
boxplot(epi_cleaned, daly_cleaned, names=temp, main="EPI vs DALY")

temp[2] <- "AIR_H"
boxplot(epi_cleaned, air_h_cleaned, names=temp, main="EPI vs AIR_H")

temp[2] <- "WATER_H"
boxplot(epi_cleaned, water_h_cleaned, names=temp, main="EPI vs WATER_H")

temp[2] <- "AIR_E"
boxplot(epi_cleaned, air_e_cleaned, names=temp, main="EPI vs AIR_E")

temp[2] <- "WATER_E"
boxplot(epi_cleaned, water_e_cleaned, names=temp, main="EPI vs WATER_E")

temp[2] <- "BIODIVERSITY"
boxplot(epi_cleaned, biodiversity_cleaned, names=temp, main="EPI vs BIODIVERSITY")



extract_column <- function(df, column_name) {
  data_prelim <- df[[column_name]]
  data <- data_prelim[!is.na(data_prelim)]
  return(data)
}

epi_data_2016 <- read.csv("./2016EPI_data.csv")


epi_2016 <- extract_column(epi_data_2016, "EPI")
envhealth_2016 <- extract_column(epi_data_2016, "ENVHEALTH")
daly_2016 <- extract_column(epi_data_2016, "DALY")
ecosystem_2016 <- extract_column(epi_data_2016, "ECOSYSTEM")
air_h_2016 <- extract_column(epi_data_2016, "AIR_H")
water_h_2016 <- extract_column(epi_data_2016, "WATER_H")
air_e_2016 <- extract_column(epi_data_2016, "AIR_E")
water_e_2016 <- extract_column(epi_data_2016, "WATER_E")
biodiversity_2016 <- extract_column(epi_data_2016, "BIODIVERSITY")



par(pty="m")
names <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIVERSITY")
boxplot(epi_2016, envhealth_2016, ecosystem_2016, daly_2016, air_h_2016, water_h_2016, air_e_2016, water_e_2016, biodiversity_2016, names=names, main="EPI vs all data")

temp <- c("EPI", "ENVHEALTH")
boxplot(epi_2016, envhealth_2016, names=temp, main="EPI vs ENVHEALTH")

temp[2] <- "ECOSYSTEM"
boxplot(epi_2016, ecosystem_2016, names=temp, main="EPI vs ECOSYSTEM")

temp[2] <- "DALY"
boxplot(epi_2016, daly_2016, names=temp, main="EPI vs DALY")

temp[2] <- "AIR_H"
boxplot(epi_2016, air_h_2016, names=temp, main="EPI vs AIR_H")

temp[2] <- "WATER_H"
boxplot(epi_2016, water_h_2016, names=temp, main="EPI vs WATER_H")

temp[2] <- "AIR_E"
boxplot(epi_2016, air_e_2016, names=temp, main="EPI vs AIR_E")

temp[2] <- "WATER_E"
boxplot(epi_2016, water_e_2016, names=temp, main="EPI vs WATER_E")

temp[2] <- "BIODIVERSITY"
boxplot(epi_2016, biodiversity_2016, names=temp, main="EPI vs BIODIVERSITY")


# view only landlocked countries
landlock <- epi_data[epi_data$Landlock==TRUE,]

epi_landlock <- landlock$EPI

envhealth_landlock <- extract_column(landlock, "ENVHEALTH")
daly_landlock <- extract_column(landlock, "DALY")
ecosystem_landlock <- extract_column(landlock, "ECOSYSTEM")
air_h_landlock <- extract_column(landlock, "AIR_H")
water_h_landlock <- extract_column(landlock, "WATER_H")
air_e_landlock <- extract_column(landlock, "AIR_E")
water_e_landlock <- extract_column(landlock, "WATER_E")
biodiversity_landlock <- extract_column(landlock, "BIODIVERSITY")



par(pty="m")
names <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIVERSITY")
boxplot(epi_landlock, envhealth_landlock, ecosystem_landlock, daly_landlock, air_h_landlock, water_h_landlock, air_e_landlock, water_e_landlock, biodiversity_landlock, names=names, main="EPI vs all data")

temp <- c("EPI", "ENVHEALTH")
boxplot(epi_landlock, envhealth_landlock, names=temp, main="EPI vs ENVHEALTH")

temp[2] <- "ECOSYSTEM"
boxplot(epi_landlock, ecosystem_landlock, names=temp, main="EPI vs ECOSYSTEM")

temp[2] <- "DALY"
boxplot(epi_landlock, daly_landlock, names=temp, main="EPI vs DALY")

temp[2] <- "AIR_H"
boxplot(epi_landlock, air_h_landlock, names=temp, main="EPI vs AIR_H")

temp[2] <- "WATER_H"
boxplot(epi_landlock, water_h_landlock, names=temp, main="EPI vs WATER_H")

temp[2] <- "AIR_E"
boxplot(epi_landlock, air_e_landlock, names=temp, main="EPI vs AIR_E")

temp[2] <- "WATER_E"
boxplot(epi_landlock, water_e_landlock, names=temp, main="EPI vs WATER_E")

temp[2] <- "BIODIVERSITY"
boxplot(epi_landlock, biodiversity_landlock, names=temp, main="EPI vs BIODIVERSITY")



south_asia_data <-epi_data[epi_data$GEO_subregion=="South Asia",]



df2 <- read.csv("./GPW3_GRUMP_SummaryInformation_2010.csv")

hist_ <- function(data) {
  hist(data, seq(0.,100.,5.0), prob=T)
  lines(density(data, bw="SJ"))
  rug(data)
}

plot_ <- function(data) {
  plot(ecdf(data), do.points=F, verticals=T)
}

qq_ <- function(data) {
  qqnorm(data)
  qqline(data)
}

df2 <- read.csv("./2016EPI_data.csv")

par(pty="s")
epi_grump <- extract_column(df2, "EPI")
summary(epi_grump)
hist_(epi_grump)
plot_(epi_grump)
qq_(epi_grump)
envhealth_grump <- extract_column(df2, "ENVHEALTH")
summary(envhealth_grump)
hist_(envhealth_grump)
plot_(envhealth_grump)
qq_(envhealth_grump)
daly_grump <- extract_column(df2, "DALY")
summary(daly_grump)
hist_(daly_grump)
plot_(daly_grump)
qq_(daly_grump)
ecosystem_grump <- extract_column(df2, "ECOSYSTEM")
summary(ecosystem_grump)
hist_(ecosystem_grump)
plot_(ecosystem_grump)
qq_(ecosystem_grump)
air_h_grump <- extract_column(df2, "AIR_H")
summary(air_h_grump)
hist_(air_h_grump)
plot_(air_h_grump)
qq_(air_h_grump)
water_h_grump <- extract_column(df2, "WATER_H")
summary(water_h_grump)
hist_(water_h_grump)
plot_(water_h_grump)
qq_(water_h_grump)
air_e_grump <- extract_column(df2, "AIR_E")
summary(air_e_grump)
hist_(air_e_grump)
plot_(air_e_grump)
qq_(air_e_grump)
water_e_grump <- extract_column(df2, "WATER_E")
summary(water_e_grump)
hist_(water_e_grump)
plot_(water_e_grump)
qq_(water_e_grump)
biodiversity_grump <- extract_column(df2, "BIODIVERSITY")
summary(biodiversity_grump)
hist_(biodiversity_grump)
plot_(biodiversity_grump)
qq_(biodiversity_grump)


par(pty="m")
names <- c("EPI", "ENVHEALTH", "ECOSYSTEM", "DALY", "AIR_H", "WATER_H", "AIR_E", "WATER_E", "BIODIVERSITY")
boxplot(epi_grump, envhealth_grump, ecosystem_grump, daly_grump, air_h_grump, water_h_grump, air_e_grump, water_e_grump, biodiversity_grump, names=names, main="EPI vs all data")

temp <- c("EPI", "ENVHEALTH")
boxplot(epi_grump, envhealth_grump, names=temp, main="EPI vs ENVHEALTH")

temp[2] <- "ECOSYSTEM"
boxplot(epi_grump, ecosystem_grump, names=temp, main="EPI vs ECOSYSTEM")

temp[2] <- "DALY"
boxplot(epi_grump, daly_grump, names=temp, main="EPI vs DALY")

temp[2] <- "AIR_H"
boxplot(epi_grump, air_h_grump, names=temp, main="EPI vs AIR_H")

temp[2] <- "WATER_H"
boxplot(epi_grump, water_h_grump, names=temp, main="EPI vs WATER_H")

temp[2] <- "AIR_E"
boxplot(epi_grump, air_e_grump, names=temp, main="EPI vs AIR_E")

temp[2] <- "WATER_E"
boxplot(epi_grump, water_e_grump, names=temp, main="EPI vs WATER_E")

temp[2] <- "BIODIVERSITY"
boxplot(epi_grump, biodiversity_grump, names=temp, main="EPI vs BIODIVERSITY")
