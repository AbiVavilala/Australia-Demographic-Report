
library(tidyverse)
library(plotly)
library(ggrepel)
install.packages("directlabels")
install.packages("viridis")
library(viridis)
library(directlabels)
library(ggthemes)
urlfile <- "https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv"

corona <- read.csv(url(urlfile))
View(corona)

tinytex::install_tinytex()
urlfile <- "https://raw.githubusercontent.com/vaksakalli/datasets/master/diabetes.csv"

diabates <- read.csv(url(urlfile))
View(diabates)


library(dslabs)
install.packages("dslabs")
library(dslabs)

s <- take_poll(25)

pnorm(s)
11/30

s



urlfile2 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/Births.csv"


birth_aus <- read.csv(url(urlfile2))
birth_aus
View(birth_aus)


urlfile3 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/NOM.csv"

number_of_internationalmigrants_in_aus <- read.csv(url(urlfile3))


View(number_of_migrants_in_aus)


urlfile4 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/TFR.csv"

urllife5 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/TFR.csv"



TFR_women_in_her_life <- read.csv(url(urllife5))
View(TFR_women_in_her_life)

urlfile6 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/Deaths.csv"

deaths_in_aus <- read.csv(url(urlfile6))

View(deaths_in_aus)




urlfile7 <- "https://raw.githubusercontent.com/Agewerc/exploratory-data-analysis/master/NIM.csv"


number_of_inter_state_migrants_in_aus <- read.csv(url(urlfile7))

View(number_of_inter_state_migrants_in_aus)


birth_aus <- birth_aus %>% gather(NSW:NT, key = "State", value = "Number of Births")
View(birth_aus)
deaths_in_aus <- deaths_in_aus %>% gather(NSW:ACT, key = "State", value = "Number of Deaths")


View(deaths_in_aus)

identical(birth_aus$State, deaths_in_aus$State)

Aus_birth_death_stats <- left_join(birth_aus, deaths_in_aus, by = "Year")


View(Aus_birth_death_stats)


str(birth_aus)
rename()
Aus_birth_death_stats$State.y <- NULL


fix(birth_aus)
  
  
View(birth_aus)

fix(number_of_inter_state_migrants_in_aus)
  
fix(deaths_in_aus)  


View(deaths_in_aus)


s <- birth_aus %>% ggplot(aes(Year, `Number of Births`, group = State, col = State)) + geom_line() 
                                                                                                          

g <- deaths_in_aus

View(g)

ggplotly(s)

str(birth_aus$`Number of Births`)

unique(birth_aus$State)

birth_aus$State <- factor(birth_aus$State)

geom_lin


?geom_text




library(dplyr)
jan = data_frame(year = 2017, month = 1, day = 1:31, 
                 head = rpois(31, 50))
feb = data_frame(year = 2017, month = 2, day = 1:28, 
                 head = rpois(28, 50))
View(jan)
View(feb)



combo <- bind_rows(jan,feb)
combo
view(combo)

Aus_birth_death_stats <- bind_rows(birth_aus, deaths_in_aus)

View(Aus_birth_death_stats)

Aus_birth_death_stats <- left_join(birth_aus, deaths_in_aus, by = c("Year" , "State"))
Aus_birth_death_stats

View(Aus_birth_death_stats)


Aus_birth_death_stats %>% filter(State == "SA")


n <- Aus_birth_death_stats %>% ggplot(aes(`Number of Births`, `Number of Deaths`, color = State, fill = Year)) + geom_point()
ggplotly(n)

TFR_women_in_Aus <- TFR_women_in_her_life %>% gather(NSW:ACT, key = "State", value = "fertility_rate")


Aus_stats_test <- left_join(Aus_stats, )
View(TFR_women_in_Aus)
fix(TFR_women_in_Aus)
Aus_birth_death_stats <- left_join(TFR_women_in_Aus, Aus_birth_death_stats, by = c("Year", "State"))

View(Aus_birth_death_stats)


number_of_inter_state_migrants_in_aus <- number_of_inter_state_migrants_in_aus %>% gather(NSW:ACT, key = "State", value = "Inter_state_migration")
View(number_of_inter_state_migrants_in_aus)

Aus_stats <- left_join(Aus_birth_death_stats, number_of_inter_state_migrants_in_aus, by = c("Year", "State"))
View(Aus_stats)
fix(number_of_internationalmigrants_in_aus)

number_of_migrants_in_aus <- number_of_migrants_in_aus %>% gather()



number_of_internationalmigrants_in_aus <- number_of_internationalmigrants_in_aus %>% gather(NSW:ACT, key = "State", value = "International_migration")

Aus_stats <- left_join(Aus_stats, number_of_internationalmigrants_in_aus, by = c("State", "Year"))

View(Aus_stats)

write.csv(Aus_stats, "C:/Users/60132690/Documents/Aus_stats.csv")
c <- read.csv("c:/users/60132690/Documents/Aus_stats.csv")
View(c)
Aus_stats$State <- factor(Aus_stats$State)

str(Aus_stats)

ggplot(data = Aus_stats, aes(x = State, y = `number of deaths`   ))
N <- 25

p <- seq(0,1, length.out = 100)
n <- 25

n*25
 p*n


se <- sqrt(p *(1-p)/N)

plot(p,se)

p
p <- seq(100, by = 0.01)

?seq
seq(17)

seq(1, 9, by = 2)  

fix(Aus_stats)

s <- Aus_stats %>% ggplot(aes(Year, Number_of_Births,group= State, color = State)) + geom_line() + geom_text(data = Aus_stats, aes(label = State, x = Year, y = Number_of_Births), hjust = -.1) + 
  scale_color_discrete(guide = "none") + theme(plot.margin = unit(c(1,3,1,1,1,1,1,1), "lines"))

gt <- ggplotGrob(s)
gt$layout$clip[gt$layout$name == "panel"] <- "off"
grid.draw(gt)
ggplotly(gt)
View(Aus_stats)
str(Aus_stats)
label = Aus_stats$State
Aus_stats %>% rename(Number_of_Births = `Number of Births`) 
library(ggrepel)
s <- Aus_stats %>% ggplot(aes(Year, Number_of_Births,group= State, color = State)) + geom_line() + scale_colour_discrete(guide = 'none') + scale_x_discrete(expand=c(0, 1)) + geom_dl(aes(label = State), method = list(dl.combine("first.points", "last.points")), cex = 0.8) 
s
label
ggplotly(s)

as.tibble(Aus_stats)
fix(Aus_stats)
View(Aus_stats)

str(Aus_stats)

View(Aus_stats)

Aus_stats$Nuber_of_Births <- Aus_stats$`Number of Births`
Aus_stats$Number_of_Deaths <- Aus_stats$`Number of Deaths`
View(Aus_stats)

Aus_stats$`Number of Births` == Aus_stats$Nuber_of_Births


Aus_stats$`Number of Births` <- NULL
Aus_stats$`Number of Deaths` <- NULL

Aus_stats$State <- factor(Aus_stats$State)


Aus_stats %>% ggplot(aes(x = Year, fill= State, y = Nuber_of_Births)) + geom_bar(position = "dodge", stat = "identity")


g <- Aus_stats %>% ggplot(aes(Year, Inter_state_migration, group = State, color = State)) + geom_line()  

ggplotly(g)


NSW <- Aus_stats %>% filter(State == "NSW")

NSW_migration <- NSW %>% ggplot(aes(Year)) + geom_line(aes(y = Inter_state_migration, color = "Inter_state_migration")) + geom_line(aes(y = International_migration, color = "International_migration"))

ggplotly(NSW_migration)




test_data <-
  data.frame(
    var0 = 100 + c(0, cumsum(runif(49, -20, 20))),
    var1 = 150 + c(0, cumsum(runif(49, -10, 10))),
    date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
  )

View(NSW)

View(test_data)


install.packages("reshape2")

str(NSW)

NSW_migration_info <- NSW %>% select(Year, Inter_state_migration, International_migration)

View(NSW_migration_info)
library("reshape2")

test_data_long <- melt(test_data, id="date")  # convert to long format
View(test_data_long)
NSW_melt_data_migration <- melt(NSW_migration_info, id = "Year")
NSW_melt_data_migration <- na.omit(NSW_melt_data_migration)
View(NSW_melt_data_migration)

ggplot(data = NSW_melt_data_migration, aes(x = Year, y = value, colour = variable), na.rm = T) + geom_line()
View(Aus_stats)

migration_line_nsw <- NSW_melt_data_migration %>% ggplot(aes(Year, value, group = variable, colour = variable)) + geom_line() + geom_point()
ggplotly(migration_line_nsw)


NSW_birth_death <- Aus_stats %>% select(Year, `Number of Births`, `Number of Deaths`)
View(NSW_melt_birth_death)

NSW_melt_birth_death <- melt(NSW_birth_death, id = "Year")
NSW_melt_birth_death <- na.omit(NSW_melt_birth_death)

birth_death_bar <- NSW_melt_birth_death %>% ggplot(aes(Year, value, fill = variable)) + geom_bar(stat = "identity", position = "dodge")

ggplotly(birth_death_bar)

ggplot(Aus_stats, aes(Year, `Number of Births`)) + geom_line()

TAS_stats <- Aus_stats %>% filter(State == "TAS")

TAS_birth_death <- TAS_stats %>% select(Year, `Number of Births`, `Number of Deaths`)

TAS_melt_birth_death <- melt(TAS_birth_death, id = "Year")
TAS_melt_birth_death <- na.omit(TAS_melt_birth_death)
TAS_melt_birth_death %>% ggplot(aes(Year, value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") + geom_text(stat= "identity", aes(Year, value, label = value), position = position_dodge(width = 1), vjust= -0.5)


TFR <- Aus_stats %>% ggplot(aes(Year, fertility_rate, group = State, fill = State, colour = State)) + geom_line() + geom_point() 

ggplotly(TFR)


migration_bar_nsw <- NSW_melt_data_migration %>% ggplot(aes(Year, value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") + geom_text(stat= "identity", aes(Year, value, label = value), position = position_dodge(width = 0.5), vjust= -0.5)

ggplotly(migration_bar_nsw)

TAS_melt_birth_death %>% ggplot(aes(Year, value, fill = variable)) + geom_bar(stat = "identity", position = "dodge") + geom_text(stat= "identity", aes(Year, value, label = value), position = position_dodge(width = 1), hjust= -0.5) + coord_flip() + ggtitle("Number of Births and Deaths in Tasmania from 1977 to 2015") + theme_economist() 




year_stat <- Aus_stats %>% group_by(Year) %>% summarise(fertility_rate)

View(year_stat)


fertility_by_year_aus <- aggregate(fertility_rate ~ Year, data = Aus_stats, mean)
fertility_by_year_aus
rename(fertility_by_year_aus, `National Average` = fertility_rate)
library(ggplot2)

fertility_state_wise <- Aus_stats %>% select(Year, fertility_rate, State)

fertility_state_wise

fertility_by_year_this <- merge(fertility_state_wise, fertility_by_year_aus, by = "Year")
fertility_by_year_this
head(fertility_by_year)
fertility_by_year %>% ggplot(aes(Year, value, fill = variable, colour = variable))



ggplot() + geom_line(data = Aus_stats, aes(Year, fertility_rate, group = State , colour = State)) + geom_line(data = fertility_by_year_aus, aes(Year, `National Average`  ))

 

str(fertility_by_year)


fertility_by_year_this <- fertility_by_year_this[, !duplicated(colnames(fertility_by_year_this))]

fertility_by_year_this_melted <- reshape2::melt(fertility_by_year_this, id.var = "Year")
head(fertility_by_year_this_melted)

fertility_by_year_this_melted %>% ggplot() +  geom_line(aes(Year, variable, fill = variable, group = variable, colour = variable))



aus_stat_test <- left_join(Aus_stats, fertility_by_year_aus, by = "Year")

rename(aus_stat_test, fertility_rate_state = fertility_rate.x)
rename(aus_stat_test, National_Average = fertility_rate.y)


ggplot(data = aus_stat_test) + geom_line(aes(Year, fertility_rate.x, group = State, colour = State)) + geom_line(aes(Year, fertility_rate.y))

head(aus_stat_test)
head(Aus_stats)
aus_stat_test <- full_join(Aus_stats, fertility_by_year_aus)
head(aus_stat_test)
View(aus_stat_test)
Number_of_births <- aggregate(`Number of Births` ~ Year, data = Aus_stats, mean)

Number_of_births
Number_of_deaths <- aggregate(`Number of Deaths` ~ Year, data = Aus_stats, mean)
Inter_state_migration_national_average <- aggregate(Inter_state_migration ~ Year, data = Aus_stats, mean)
Inter_state_migration_national_average
International_migration_national_average <- aggregate(International_migration ~ Year, data = Aus_stats, mean)


Aus_national_average <- full_join(Number_of_births, Number_of_deaths, by = "Year")
head(Aus_national_average)

Aus_national_average <- full_join(Aus_national_average, fertility_by_year_aus, by = "Year")

Aus_national_average <- full_join(Aus_national_average, Inter_state_migration_national_average,by = "Year")


head(Aus_national_average)

Aus_national_average <- Aus_national_average[, !duplicated(colnames(Aus_national_average))]

Aus_national_average$Inter_state_migration <- Aus_national_average$Inter_state_migration.x
Aus_national_average$Inter_state_migration.x <- NULL


Aus_national_average <- full_join(Aus_national_average, International_migration_national_average, by = "Year")

aus_stat_test_test <- full_join(Aus_stats, Aus_national_average)

Aus_national_average$State <- "National Average"
View(aus_stat_test_test)


s <- aus_stat_test_test %>% ggplot(aes(Year, fertility_rate, group = State, colour = State)) + geom_line() + geom_point()
ggplotly(s)


b <- aus_stat_test_test %>% ggplot(aes(Year, `Number of Births`, group = State,  colour = State)) + geom_line()
ggplotly(b)

nat_migrat <- aus_stat_test_test %>% ggplot(aes(Year, Inter_state_migration, group = State, colour = State)) + geom_line() + geom_point()

c$Number.of.Births
ggplotly(nat_migrat)

View(Aus_stats)


library(stringr)
library("writexl")
install.packages("writexl")
library(readxl)
one_rr <- read_excel("c:/Users/60132690/Documents/EUS-1RR Desktop Support.xlsx", sheet = "Main")

one_rr_filtered <- one_rr %>% filter(`Assignment group` == "eH-SD-EUS-1RR Desktop Support")

View(one_rr_filtered)

one_rr$word <- str_extract(one_rr$`Short description`, "Remote|remote|wifi|Wi-fi|")

 
Extract <- c("remote", "Remote", "VPN", "BigIP", "Big ip" , "big ip", "big IP", "bigip", "Big-Ip", "Big-ip", "big-ip", "Big-IP", "print", "papercut", "outlook", "outlook", "Teams", "teams", "Onsite", "Monitor", "monitor", "Mouse", "mice", "display", "Excel", "excel", "Scan", "scan", "HP", "Adobe", "adobe", "pdf", "PDF", "password", "Password", "Citrix", "citrix", "mailbox", "Mailbox", "softphone", "mobile", "Mobile", "Onedrive", "one drive", "onedrive", "wifi", "Wi-Fi", "wi-fi", "WI-FI", "network", "Network", "file", "File", "email", "Outlook", "outlook", "Email", "E-mail", "e-mail", "OnBoarding ", "I require assistance", "Skype", "skype", "broken", "log in", "Log in", "log-in", "Log on", "logon", "Reimage", "reimage", "re-image", "Re-image", "Avaya Equinox ", "avaya equinox ", "equinox", "Anitivirus ", "anitivirus", "Symantec", "symantec", "keyboard", " Access", "access", "ras", "Remote Access", " FindMePrint", "hardware", "headset", "install", "Install", "Softphone")


one_rr_filtered$word <- str_extract(one_rr_filtered$`Short description`, paste(Extract, collapse = "|"))

length(which(!complete.cases(one_rr_filtered$word)))

View(one_rr_filtered)


