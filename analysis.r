library(tidyverse)
library(lubridate)
library(ggpubr)

#Patent data can be extracted from amadeus bv dataset
#Y02 - Technologies or applications for mitigation or adaptation against climate change
data0 <- readxl::read_xls("a1.xls")
data1 <- readxl::read_xls("Amadeus_Export_5.xls")
data2 <- readxl::read_xls("Amadeus_Export_6.xls")
data3 <- readxl::read_xls("Amadeus_Export_7.xls")
data4 <- readxl::read_xls("Amadeus_Export_8.xls")
data5 <- readxl::read_xls("Amadeus_Export_9.xls")
data6 <- readxl::read_xls("Amadeus_Export_10.xls")
data7 <- readxl::read_xls("Amadeus_Export_11.xls")
data8 <- readxl::read_xls("Amadeus_Export_12.xls")
data9 <- readxl::read_xls("Amadeus_Export_13.xls")
data10 <- readxl::read_xls("Amadeus_Export_14.xls")
data11 <- readxl::read_xls("Amadeus_Export_15.xls")
data12 <- readxl::read_xls("Amadeus_Export_16.xls")
data13 <- readxl::read_xls("Amadeus_Export_17.xls")
data14 <- readxl::read_xls("Amadeus_Export_18.xls")
data15 <- readxl::read_xls("Amadeus_Export_19.xls")
data16 <- readxl::read_xls("Amadeus_Export_20.xls")
data17 <- readxl::read_xls("Amadeus_Export_21.xls")
data18 <- readxl::read_xls("Amadeus_Export_22.xls")
data19 <- readxl::read_xls("Amadeus_Export_23.xls")
data20 <- readxl::read_xls("Amadeus_Export_24.xls")
data21 <- readxl::read_xls("Amadeus_Export_25.xls")
data22 <- readxl::read_xls("Amadeus_Export_26.xls")
data23 <- readxl::read_xls("Amadeus_Export_27.xls")
data24 <- readxl::read_xls("Amadeus_Export_28.xls")
data25 <- readxl::read_xls("Amadeus_Export_29.xls")
data26 <- readxl::read_xls("Amadeus_Export_30.xls")
data27 <- readxl::read_xls("Amadeus_Export_31.xls")
data28 <- readxl::read_xls("Amadeus_Export_32.xls")
data29 <- readxl::read_xls("Amadeus_Export_33.xls")
data30 <- readxl::read_xls("Amadeus_Export_34.xls")
data31 <- readxl::read_xls("Amadeus_Export_35.xls")
data32 <- readxl::read_xls("Amadeus_Export_36.xls")
data33 <- readxl::read_xls("Amadeus_Export_37.xls")
data34 <- readxl::read_xls("Amadeus_Export_38.xls")
data35 <- readxl::read_xls("Amadeus_Export_39.xls")
data36 <- readxl::read_xls("Amadeus_Export_40.xls")
data37 <- readxl::read_xls("Amadeus_Export_41.xls")
data38 <- readxl::read_xls("Amadeus_Export_42.xls")
data39 <- readxl::read_xls("Amadeus_Export_43.xls")
data40 <- readxl::read_xls("Amadeus_Export_44.xls")
data41 <- readxl::read_xls("Amadeus_Export_45.xls")
data42 <- readxl::read_xls("Amadeus_Export_46.xls")
data43 <- readxl::read_xls("Amadeus_Export_47.xls")
data44 <- readxl::read_xls("Amadeus_Export_48.xls")
data45 <- readxl::read_xls("Amadeus_Export_49.xls")
data46 <- readxl::read_xls("Amadeus_Export_50.xls")
data47 <- readxl::read_xls("Amadeus_Export_51.xls")

data <- rbind(data0[,-1], data1[,-1], data2[,-1],
              data3[,-1], data4[,-1], data5[,-1],
              data6[,-1], data7[,-1], data8[,-1],
              data9[,-1], data10[,-1], data11[,-1],
              data12[,-1], data13[,-1], data14[,-1],data15[,-1],
              data16[,-1], data17[,-1], data18[,-1],data19[,-1],
              data20[,-1], data21[,-1], data22[,-1],
              data23[,-1], data24[,-1], data25[,-1],
              data26[,-1], data27[,-1], data28[,-1],
              data29[,-1], data30[,-1], data31[,-1],
              data32[,-1], data33[,-1], data34[,-1],data35[,-1],
              data36[,-1], data37[,-1], data38[,-1],
              data39[,-1], data40[,-1], data41[,-1],
              data42[,-1], data43[,-1], data44[,-1],data45[,-1],
              data46[,-1], data47[,-1])

data <- data[,1:8]
#cleaning
#, data37[,-1], data38[,-1])
#length(which(is.na(data$`International classification`)))
data$`Current owner(s) country code`[which(is.na(data$`Current owner(s) country code`))] = data$`Inventor(s) country code`[which(is.na(data$`Current owner(s) country code`))]
data$`Inventor(s) country code`[which(is.na(data$`Inventor(s) country code`))] = data$`Current owner(s) country code`[which(is.na(data$`Inventor(s) country code`))]
data <- data[-which(is.na(data$`International classification`)),]
data <- data[-which(is.na(data$`Publication title`)),]
data <- data[-which(is.na(data$`Current owner(s) country code`)),]
data <- data[-which(is.na(data$`Current owner(s)`)),]
data$Granted <- as.factor(data$Granted)
data$`Current owner(s) country code` <- as.factor(data$`Current owner(s) country code`)
data$`Inventor(s) country code` <- as.factor(data$`Inventor(s) country code`)
data <- distinct(data)
data$year <- ymd(data$`Publication date`) %>% year()
data <- drop_na(data) 
unique(data$year)
data <- data[which(data$year >= 1960),]
data$year <- as.factor(data$year)

#filter yrs below 1960 as there were no regulations on evironment as at then
summary(data)

sort_country <- data %>% count(`Current owner(s) country code`, sort = TRUE)
sort_country <- rename(sort_country, "no of innovations"= "n" )

#take of ussr (SU), add to russia (RU)
#replace dd with de
#replace CS with ME
#replace YU=ME, AN=NL, GI=GB, UK=GB, RE, UN, BV, EP, FL, RK, TK, UD

sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "CN")] = 56832 +	28182
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "TW"),]
sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "RU")] = 10789 +	13150
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "SU"),] 
sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "DE")] = 248834 +	1393
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "DD"),] 
sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "ME")] = 5 +	1044 + 326
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "CS" | sort_country$`Current owner(s) country code` == "YU"),] 
sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "NL")] = 173 +	21188
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "AN"),] 
sort_country$`no of innovations`[which(sort_country$`Current owner(s) country code` == "GB")] = 53822 +	103 + 8 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "GI" | sort_country$`Current owner(s) country code` == "UK"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "RE"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "UN"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "BV"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "EP"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "FL"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "RK"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "TK"),] 
sort_country <- sort_country[-which(sort_country$`Current owner(s) country code` == "UD"),] 

#view the top 10 countries with the highest innovations
ggplot(data = sort_country[1:10,]) +
  geom_bar(stat = "identity", aes(x=`Current owner(s) country code`, y = `no of innovations`))


#extract the top ten countries
data_sub_us <- data[, c(8,9)] %>% subset(`Current owner(s) country code`== "US")
data_sub_us_count <- data_sub_us %>% count(year)
data_sub_us  <- data_sub_us %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_us <- droplevels(data_sub_us)

data_sub_jp <- data %>% subset(`Current owner(s) country code`== "JP")
data_sub_jp_count <- data_sub_jp %>% count(year)
data_sub_jp  <- data_sub_jp %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_jp <- droplevels(data_sub_jp)

data_sub_de <- data %>% subset(`Current owner(s) country code`== "DE" | `Current owner(s) country code`== "DD")
data_sub_de_count <- data_sub_de %>% count(year)
#replace de with dd
data_sub_de$`Current owner(s) country code`[which(data_sub_de$`Current owner(s) country code`=="DD")] = "DE"
data_sub_de  <- data_sub_de %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_de <- droplevels(data_sub_de)

data_sub_ko <- data %>% subset(`Current owner(s) country code`== "KR")
data_sub_ko_count <- data_sub_ko %>% count(year)
data_sub_ko  <- data_sub_ko %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_ko <- droplevels(data_sub_ko)

data_sub_fr <- data %>% subset(`Current owner(s) country code`== "FR")
data_sub_fr_count <- data_sub_fr %>% count(year)
data_sub_fr  <- data_sub_fr %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_fr <- droplevels(data_sub_fr)

data_sub_cn <- data %>% subset(`Current owner(s) country code`== "CN")
data_sub_cn_count <- data_sub_cn %>% count(year)
#replace tw with cn
data_sub_cn$`Current owner(s) country code`[which(data_sub_cn$`Current owner(s) country code`=="TW")] = "CN"
data_sub_cn  <- data_sub_cn %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_cn <- droplevels(data_sub_cn)

data_sub_ca <- data %>% subset(`Current owner(s) country code`== "CA")
data_sub_ca_count <- data_sub_ca %>% count(year)
data_sub_ca  <- data_sub_ca %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_ca <- droplevels(data_sub_ca)

data_sub_gb <- data %>% subset(`Current owner(s) country code`== "GB" | `Current owner(s) country code`== "GI" | `Current owner(s) country code`== "UK")
data_sub_gb_count <- data_sub_gb %>% count(year)
unique(data$`Current owner(s) country code`)
#replace uk, gi with gb
data_sub_gb$`Current owner(s) country code`[which(data_sub_gb$`Current owner(s) country code`== "GI" | data_sub_gb$`Current owner(s) country code`== "UK")] = "GB"
data_sub_gb  <- data_sub_gb %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_gb <- droplevels(data_sub_gb)

data_sub_se <- data %>% subset(`Current owner(s) country code`== "SE")
data_sub_se_count <- data_sub_se %>% count(year)
data_sub_se  <- data_sub_se %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_se <- droplevels(data_sub_se)

data_sub_ch <- data %>% subset(`Current owner(s) country code`== "CH")
data_sub_ch_count <- data_sub_ch %>% count(year)
data_sub_ch  <- data_sub_ch %>% group_by(`Current owner(s) country code`) %>% count(year)
data_sub_ch <- droplevels(data_sub_ch)


data_sub <- rbind(data_sub_us, data_sub_jp, data_sub_de, data_sub_ko, data_sub_fr,
                  data_sub_cn, data_sub_gb, data_sub_ch, data_sub_se, data_sub_ca)

data_sub$`Current owner(s) country code` <- as.factor(data_sub$`Current owner(s) country code`)
data_sub$year <- as.numeric(data_sub$year)

summary(data_sub)

#time series no of innovation curves for the top 10 innovative countries 
ggplot(data_sub)+geom_line(aes(x=year, y=n, color=`Current owner(s) country code`)) + labs(color = "Country")

#environmental regulations
regulation <- read_csv("EPS.csv")
unique(regulation$Variable)
regulation_eps <- regulation %>% subset(Variable=="Environmental Policy Stringency")
regulation_eps$Country[which(regulation_eps$Country=="China (People's Republic of)")]= "China"
regulation_eps$COU <- as.factor(regulation_eps$COU)
regulation_eps <- regulation_eps %>% group_by(COU)
class(regulation_eps$COU)
summary(regulation_eps)

#count of environmental regulation by the total value all through the year for each country
regulation_counts <- regulation_eps %>% group_by(COU) %>% summarise(sum = sum(Value))
regulation_counts <- regulation_counts[order(regulation_counts$sum),]
regulation_counts <- left_join(regulation_counts, world[,c(44,45)], c("COU" = "iso_a3"))
regulation_counts <-regulation_counts[,-4]
regulation_counts <- regulation_counts %>% rename("CODE"="iso_a2")
#count of env regulation per total value per year for each top 10 innovative countries
regulation_counts_sub <- regulation_counts %>% subset(CODE == "US" | CODE == "JP" | 
                                                        CODE == "DE" | CODE == "KR" | CODE == "FR"|
                                                        CODE == "CA" | CODE == "CN" | 
                                                        CODE== "GB" | CODE== "SE" |CODE == "CH")

#distribution of environmental regulation across top 10 innovative countries
ggplot(data = regulation_counts_sub) +
  geom_bar(stat = "identity", aes(x=CODE, y = sum)) + xlab("Total no of Environmental Stringency Policy Index")+ylab("Top 10 Innovative Countries")

#extraction of regulation for the top 10 innovative countries 
regulation_eps_ <- left_join(regulation_eps, world[,c(44,18)], c("Country" = "name"))
regulation_eps_ <-regulation_eps_[,-17]
regulation_eps_ <- regulation_eps_ %>% rename("CODE"="iso_a2")
regulation_eps_sub <- regulation_eps_ %>% subset(CODE == "US" | CODE == "JP" | 
                                                   CODE == "DE" | CODE == "KR" | CODE == "FR"|
                                                   CODE == "CA" | CODE == "CN" | 
                                                   CODE== "GB" | CODE== "SE" |CODE == "CH")
#time series env regulation curve for the top 10 innovative countries 
unique(regulation_eps_sub$CODE)
ggplot(data = regulation_eps_sub) +
  geom_line(aes(x=Year, y = Value, color=CODE)) + labs(color = "Country") + ylab("Environmental Policy Stringency Index")

#usa
x1 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "US")]
y1 = data_sub$n[which(data_sub$`Current owner(s) country code` == "US" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_us <- cbind(x=x1, y=y1)
xy_us <- as.data.frame(xy_us)
ggscatter(xy_us, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in US", ylab = "no of innovations in US")

#japan
x2 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "JP")]
y2 = data_sub$n[which(data_sub$`Current owner(s) country code` == "JP" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_jp <- cbind(x=x2, y=y2)
xy_jp <- as.data.frame(xy_jp)
ggscatter(xy_jp, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in JP", ylab = "no of innovations in JP")


#germany
x3 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "DE")]
y3 = data_sub$n[which(data_sub$`Current owner(s) country code` == "DE" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_de <- cbind(x=x3, y=y3)
xy_de <- as.data.frame(xy_de)
ggscatter(xy_de, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in DE", ylab = "no of innovations in DE")


#korea
x4 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "KR")]
y4 = data_sub$n[which(data_sub$`Current owner(s) country code` == "KR" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_kr <- cbind(x=x4, y=y4)
xy_kr <- as.data.frame(xy_kr)
ggscatter(xy_kr, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in KR", ylab = "no of innovations in KR")



#France
x5 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "FR")]
y5 = data_sub$n[which(data_sub$`Current owner(s) country code` == "FR" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_fr <- cbind(x=x5, y=y5)
xy_fr <- as.data.frame(xy_fr)
ggscatter(xy_fr, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in FR", ylab = "no of innovations in FR")

#China
x6 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "CN")]
y6 = data_sub$n[which(data_sub$`Current owner(s) country code` == "CN" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_cn <- cbind(x=x6, y=y6)
xy_cn <- as.data.frame(xy_cn)
ggscatter(xy_cn, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in CN", ylab = "no of innovations in CN")


#Great Britain
x7 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "GB")]
y7 = data_sub$n[which(data_sub$`Current owner(s) country code` == "GB" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_gb <- cbind(x=x7, y=y7)
xy_gb <- as.data.frame(xy_gb)
ggscatter(xy_gb, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in GB", ylab = "no of innovations in GB")

#Canada
x8 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "CA")]
y8 = data_sub$n[which(data_sub$`Current owner(s) country code` == "CA" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_ca <- cbind(x=x8, y=y8)
xy_ca <- as.data.frame(xy_ca)
ggscatter(xy_ca, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in CA", ylab = "no of innovations in CA")



#Chile
x9 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "CH")]
y9 = data_sub$n[which(data_sub$`Current owner(s) country code` == "CH" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_ch <- cbind(x=x9, y=y9)
xy_ch <- as.data.frame(xy_ch)
ggscatter(xy_ch, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in CH", ylab = "no of innovations in CH")


#Sweden
x10 = regulation_eps_sub$Value[which(regulation_eps_sub$CODE == "SE")]
y10 = data_sub$n[which(data_sub$`Current owner(s) country code` == "SE" & (data_sub$year >= 1990 & data_sub$year <= 2015 ))]
xy_se <- cbind(x=x10, y=y10)
xy_se <- as.data.frame(xy_se)
ggscatter(xy_se, x = "x", y = "y", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Environmental Protection Stringency Index (EPSI) in SE", ylab = "no of innovations in SE")














