setwd('/Users/Ridam/Downloads/fomb & data science/')
dat <- read.csv("Life.csv")

library(plyr)
library(dplyr)
ddply(dat,~Country,summarise )
ddply(out1,~Country, summarise)
ddply(out2,~Country, summarise)

out <- split( dat , f = dat$Status )
head(out[[1]])

out1 <- out[[1]]
out2 <- out[[2]]

out2new <- data.frame(out2$Country, out2$Life.expectancy)
colnames(out2new) <- c("Country", "Life.expectancy")

dat1 <- data.frame(dat$GDP, dat$Population)

dat <- dat[complete.cases(dat), ]
nrow(dat)


sum(is.na(dat))

library(DataExplorer)
data_list <- list(dat)
plot_str(data_list)
plot_bar(dat)
plot_bar(out1)
plot_bar(out2)


qq_data <- out1[ , match(c( "Life.expectancy", "Adult.Mortality"),colnames(out1))]
plot_qq(qq_data, sampled_rows = 1000L)

qq_data1 <- out2[ , match(c( "Life.expectancy", "Adult.Mortality"),colnames(out2))]
plot_qq(qq_data1)

qq_data2 <- out2[ , match(c( "Life.expectancy", "Adult.Mortality","Country"),colnames(out2))]
plot_qq(qq_data2, by = "Country")

plot_correlation(na.omit(out1), maxcat = 22L)

plot_histogram(dat)

DataExplorer::plot_missing(dat)
DataExplorer::plot_intro(dat)
DataExplorer::plot_missing(dat)

library(ggplot2)
total_coun <- ggplot(dat, aes(x=Life.expectancy)) + geom_density()
total_coun + geom_vline(aes(xintercept=mean(Life.expectancy)),
              color="blue", linetype="dashed", size=1)
developed_coun <- ggplot(out1, aes(x=Life.expectancy)) + geom_density()
developed_coun + geom_vline(aes(xintercept=mean(Life.expectancy)),
                        color="blue", linetype="dashed", size=1)

developing_coun <- ggplot(out2new, aes(x=Life.expectancy)) + geom_density()
developing_coun + geom_vline(aes(xintercept=mean(Life.expectancy)),
                            color="blue", linetype="dashed", size=1)

filter_dat = dplyr::filter(dat,dat$Life.expectancy<60)
filter_dat$Country <- droplevels(filter_dat$Country)
unique(filter_dat$Country)
View(unique(filter_dat$Country))

filter_dat = dplyr::filter(out1,out1$Life.expectancy<75)
unique(filter_dat$Country)
View(unique(filter_dat$Country))

filter_dat = dplyr::filter(out2,out2$Life.expectancy>75)
View(unique(filter_dat$Country))

filter_dat = dplyr::filter(out2,out2$Life.expectancy<75)
View(unique(filter_dat$Country))









