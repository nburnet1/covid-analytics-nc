#Noah Burnette 
#11/4/20
library(png)
library(ggplot2)
library(magick)
library(dplyr)
#1
# a
getwd()
covid = read.csv("us-counties20201102.csv",sep = ",")

# b
nrow(covid)

# c
covid_nc = filter(covid,state == "North Carolina")

# d
covid_nc = covid_nc[order(covid_nc$cases,decreasing = TRUE),]
# As of 11/01/20 Mecklenburg county has the highest case count.
# 34668

# e
covid_nc2 = filter(covid_nc,county == "Mecklenburg" | county == "Buncombe")

# f
typeof(covid_nc2[1,1])
covid_nc2_oct_nov = filter(covid_nc2,date == "2020-11-01"|date == "2020-10-01")
typeof(covid_nc2_oct_nov[1,5])
# Percent increase = (new - orig)/orig
Mecklenburg_per_inc = (covid_nc2_oct_nov[1,5]-covid_nc2_oct_nov[2,5])/covid_nc2_oct_nov[2,5] 
Buncombe_per_inc = (covid_nc2_oct_nov[3,5]-covid_nc2_oct_nov[4,5])/covid_nc2_oct_nov[4,5] 
# Predicted case count = current * (1 + percent increase)
Mecklenburg_case_dec = covid_nc2_oct_nov[1,5] * (1 + Mecklenburg_per_inc)
Buncombe_case_dec = covid_nc2_oct_nov[3,5] * (1 + Buncombe_per_inc)
# Printing the values
Mecklenburg_per_inc
Buncombe_per_inc
Mecklenburg_case_dec
Buncombe_case_dec

# g
ggplot(covid_nc2,aes(as.Date(date),cases))+
  geom_line(aes(color = county))

# h
ggplot(covid_nc2,aes(as.Date(date),cases))+
  geom_line(aes(color = county))+
  scale_y_log10()+
  xlab("Date")+
  ylab("Cases")


#2
# a
covid_buncombe = filter(covid_nc2,county == "Buncombe")
covid_buncombe = covid_buncombe[order(as.Date(covid_buncombe$date)),]
rownames(covid_buncombe) = NULL  
covid_buncombe$daily_cases = 0 
covid_buncombe$daily_cases[1] = covid_buncombe$cases[1]

x = 2
while (x <= nrow(covid_buncombe)){
  covid_buncombe$daily_cases[x] = covid_buncombe$cases[x]-covid_buncombe$cases[x-1]
  x = x + 1
}

# c
ggplot(covid_buncombe,aes(as.Date(date),daily_cases))+
  geom_col(width = 1) 

# d
#  Moving average function
smooth7 = function(num_vec){
  func_vec = c()
  x = 1 
  while (x <= 7){
    func_vec = c(func_vec, sum(num_vec[1:x])/length(num_vec[1:x]))
    x = x + 1
  }
  x = 2
  while (x <= length(num_vec[7:length(num_vec)])){
    move_avg = sum(num_vec[x:(x+6)]/7)
    print(move_avg)
    func_vec = c(func_vec, move_avg)
    x = x + 1
  }
  
  return(func_vec)
}
smooth7(c(1:10))

covid_buncombe$daily_cases_smooth = smooth7(covid_buncombe$daily_cases)

ggplot(covid_buncombe,aes(as.Date(date)))+
  geom_col(aes(y = daily_cases),width = 1)+
  geom_line(aes(y = daily_cases_smooth),color = "blue",size = 2)

#3
# a
county_pop_csv = read.csv("co-est2019-alldata.csv",sep = ",")
county_pop = county_pop_csv[,6:7]
x = 1 
while(x <= nrow(county_pop)){
  county_pop$POPESTIMATE2019[x] = county_pop_csv[x,19]
  x = x + 1
  
}
# b 
#   While Loop to get rid of County,Bourough,Parish, etc...
x = 1 
while(x <= nrow(county_pop)){
  if (grepl('County$', county_pop$CTYNAME[x]) == TRUE){
    county_pop$CTYNAME[x] = gsub(" County","",county_pop$CTYNAME[x])
  }
  if (grepl('Census Area$', county_pop$CTYNAME[x]) == TRUE){
    county_pop$CTYNAME[x] = gsub(" Census Area","",county_pop$CTYNAME[x])
  }
  if (grepl('Municipality$', county_pop$CTYNAME[x]) == TRUE){
    county_pop$CTYNAME[x] = gsub(" Municipality","",county_pop$CTYNAME[x])
  }
  if (grepl('Borough$', county_pop$CTYNAME[x]) == TRUE){
    county_pop$CTYNAME[x] = gsub(" Borough","",county_pop$CTYNAME[x])
  }
  if (grepl('Parish$', county_pop$CTYNAME[x]) == TRUE){
    county_pop$CTYNAME[x] = gsub(" Parish","",county_pop$CTYNAME[x])
  }
  x = x + 1
}
# c
covid_merged = merge(covid,county_pop,by.x = c("county","state"),by.y = c("CTYNAME","STNAME"))
# d
covid_merged$cases_perM = (covid_merged$cases/covid_merged$POPESTIMATE2019) * 1000000
covid_merged = covid_merged[order(covid_merged$cases_perM,decreasing = TRUE),]
#Highest cases_perM
max_cases_perM = covid_merged[1,1]
# e
covid_graph = filter(covid_merged,state == "North Carolina" & county == "Mecklenburg" | state == "North Carolina" & county == "Buncombe"| state == "Arkansas" & county == "Lincoln")

ggplot(covid_graph,aes(as.Date(date),cases_perM))+
  geom_line(aes(color = county))+
  scale_y_log10()+
  ylab("Cases per 1M People")+
  xlab("Date")

