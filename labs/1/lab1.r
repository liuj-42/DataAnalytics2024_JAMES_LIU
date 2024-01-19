


epi_data <- read.csv("./2010EPI_data.csv", skip=1)
df2 <- read.csv("./GPW3_GRUMP_SummaryInformation_2010.csv")


# head(epi_data)
# summary(epi_data)
# View(epi_data)

attach(epi_data)

# fix(epi_data) # simple df editor

epi <- epi_data$EPI
tf <- is.na(epi)
epi_cleaned <- epi[!tf]

summary(epi)
summary(epi_cleaned)

fivenum(epi, na.rm=T)

stem(epi_cleaned)
hist(epi_cleaned)
hist(epi_cleaned, seq(30., 95., 1.0), prob=T)

lines(density(epi_cleaned,bw=1.))
lines(density(epi_cleaned,bw="SJ"))
# lines(density(epi,na.rm=T, bw=1.))
# lines(density(epi,na.rm=T, bw="SJ"))
rug(epi_cleaned)

plot(ecdf(epi), do.points=F, verticals=T)

par(pty="s")

qqnorm(epi_cleaned)

qqline(epi_cleaned)

# temp<-seq(30,95,1)
# qqplot(qt(ppoints(250), df=5), temp, xlab="Q-Q plot for t dsn")
# qqline(temp)

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


water_h <- epi_data$WATER_H
tf <- is.na(water_h)
water_h_cleaned <- water_h[!tf]

stem(water_h_cleaned)
hist(water_h_cleaned, seq(0., 100., 5.0), prob=T)
lines(density(water_h_cleaned, bw="sj"))
rug(water_h_cleaned)

plot(ecdf(water_h))
plot(ecdf(water_h), do.points=F, verticals=T)
par(pty="s")
qqnorm(water_h_cleaned)
qqline(water_h_cleaned)





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

View(mysheets$`EPI2010_all countries`)
