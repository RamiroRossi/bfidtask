# In this code you will find the results to "Gravity Estimation"
# rrossi@iese.edu 
# rrossi@udesa.edu.ar

# Packs to be used 
library(dplyr)     # data manipulation
library(tidyverse) # data manipulation
library(readxl)     # read excel files
library(xtable)     # write excel tabs
library(data.table) # manipulating tables
library(pastecs)    # descriptive stats
library(writexl)  # write excel tabs
library(openxlsx) # manipulate xlsx files
library(stargazer) #cute latex tab
library(broom) # tables
library(ggplot2) # plots
library(reshape2) # panel data structure 
library(lfe) # fix effects
library(collapse) # tool needed for gravity
library(bdsmatrix) # tool needed for gravity
library(gravity) # toolbox for estimating gravity equations

getwd()   #where's directory
setwd("c:/Users/rross/Downloads/chicago_trade_assignment") #new directory

# Dataset construction and summary statistics 
# collapse the dataset in trade.csv by origin-destination-year. 
# The row of the new dataset will be two countries (origin and destination) in a year
# for each row, you should have a variable with the sum of the trade flows across all HS2 products
# associated with the same origin-destination-year triple 

# import raw dataset 
trade = read.csv("./data/trade.csv")
gravity = read.csv("./data/gravity.csv")


# Collapsing trade db 
colnames(t_od)
t_od = trade
t_od$O_D = apply(t_od[,c(2,3)],1,paste,collapse="_")# building ID for origin-destination
t_od = t_od%>% group_by(origin,destination,O_D, year) %>% summarise(sumt = sum(trade)) # summing trade by ID and year 
colnames(t_od)
t_od = t_od[,c(4,3,1,2,5)]

# Collapsing gravity db
g_od = gravity
g_od = unite(gravity,O_D,c(origin,destination))
# merging databases 
db_od = merge(t_od,g_od) # data in both db by default 
# drop na 
db_od = drop_na(db_od)
# exclusion of trade flows equal to 0
db_od = db_od[db_od$sumt != 0,]
# summary statistics for all db 
stargazer(db_od)
# summary for 2015 
db_od2015 = filter(db_od,year == "2015")
p10 = quantile(db_od2015$sumt,probs = 0.1)
p50 = quantile(db_od2015$sumt,probs = 0.5)
p90 = quantile(db_od2015$sumt,probs = 0.9)
p10
p50
p90
stargazer(db_od2015)

# estimation
colnames(db_od2015)
db_plot = db_od2015[,c(2,5,6)] # selecting data
db_plot$log_btflow = log(db_plot$sumt) # log of trade flow
db_plot$log_dist = log(db_plot$distance) # log of distance
colnames(db_plot)
scat = ggplot(db_plot, mapping = aes(y = log_btflow, x =log_dist)) + geom_point() + geom_smooth(method=lm) # scatter + trend
scat
cor(db_plot$log_btflow,db_plot$log_dist) #calculating correlation
# correlation is -0.26
# log-log model. A 1% increase in log_distance will predict a -0.26 decrease in the trade flow


# 2.b 
df = db_od # using new db 
df$log_t = log(df$sumt) # taking log of trade flow 
df$log_d = log(df$distance) # taking log of distance 
df_r = df %>% group_by(year) %>% do(tidy(lm(log_t ~ log_d, data=.))) # one regression by year
df_r2 = unique(df_r$year) # taking just one obs per year
df_results = df_r[df_r$term == "log_d",] # selecting the beta hat
dff = df_results[,c(1,3)] 
scat2 = ggplot(dff, mapping = aes(x = year, y =estimate)) +geom_line() +geom_point() # plotting the estimates by year
scat2

# 2.c fix effects by exporter-year 
# Need to assign numeric iso-code to alpha 3 code to use gravity toolbox
db = db_od # raw dataset
isocode = read_xls("./data/iso_3digit_alpha_country_codes.xls") # iso codes for countries
colnames(isocode)[3]="origin"
isocode$destination = isocode$origin 
colnames(db_od)
db_code = merge(db_od[,c(3,4)],isocode,by=c("origin"))
db_code = db_code[,-5]
colnames(db_code)[3]="Code origin"
db_code = db_code[,-4]
colnames(db_code)[2]="destination"
db_code2 = merge(db_code,isocode[,c(1,4)],by ="destination")
colnames(db_code2)
colnames(db_code2)[1]="origin"
colnames(db_code2)[4]="code_origin"
colnames(db_code2)[2]="destination"
colnames(db_code2)[3]="code_destination"
db_od_ver = merge(db_od,db_code2,by=c("origin","destination"))
db_od_ver1 = unique(db_od_ver) # done


# building the model

db = db_od_ver1 # raw data 
colnames(db)
db = db[,c(3,4,1,2,5,6,7,8,9,10,12,11)] #arranging
db$log_t = log(db$sumt) # logs trade 
db$log_d = log(db$distance) # logs distance
db$
d_fe= db %>% group_by(year) %>% do(tidy(fixed_effects(dependent_variable = "log_t", dist = "log_d", code_origin = "code_origin",code_destination = "code_destination",vce_robust=TRUE,data=.))) # one regression by year using FE by exporter and importer
d_fe
d_fe2= db %>% group_by(year) %>% do(tidy(fixed_effects(dependent_variable = "sumt", dist = "distance", code_origin = "code_origin",code_destination = "code_destination",vce_robust=TRUE,data=.))) # one regression by year
d_fe2_r = d_fe2[d_fe2$term=="dist_log",]

scat3 = ggplot(d_fe2_r, mapping = aes(x = year, y =estimate)) +geom_line() +geom_point() # plotting the estimates by year
scat3


#### 2.d 
dbb$
dbb = db_od_ver1
dbb = filter(dbb,year =="2015")
dbb_fe <- fixed_effects(
  dependent_variable = "sumt",
  distance = "distance",
  additional_regressors = c("contiguity", "language", "colonial","rta"),
  code_origin = "code_origin",
  code_destination = "code_destination",
  robust = TRUE,
  data = dbb
)
summary(dbb_fe)


