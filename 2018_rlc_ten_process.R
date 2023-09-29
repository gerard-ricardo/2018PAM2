#
#rapid light curves - Gerard Ricardo.


#1) Label your file names correctly
#2) Import RLC data
library(tidyr)
library(readr)
library(dplyr)
library(plyr)
# data0 <- list.files(full.names = TRUE) %>% lapply(read_delim, delim = ';') %>% bind_rows #Import multiple csvs into one dataframe
read_csv_filename1 <- function(filename){
    ret <- read_csv(filename)
    ret$disc <- filename #EDIT
    ret
}   #also read_delim(delim = ';')
filenames = list.files(path = "./rlc_ten_csvs", full.names = T)
data0 <- ldply(filenames, read_csv_filename1)
head(data0)
options(scipen = 999)  # turn off scientific notation
str(data0) #check data type is correct
data0$disc <- as.factor(as.character(data0$disc))
data0$no.f <- as.factor(as.character(data0$No.))
#data0$disc1 = data0 %>% separate(disc, c("a", "b", 'c', 'd')) %>% .$b %>% as.factor() #gets disc ID from label
data0$disc1 = data0 %>% separate(disc, into = c("dir", "subdir", "filename"), sep = "/") %>%
  separate(filename, into = c("code", "text"), sep = " ", extra = "merge") %>% pull(code) %>% as.factor() #gets disc ID from label

str(data0)

#3) Import and join Env. treatment data
env.fact <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header= TRUE,dec=",", na.strings=c("",".","NA"))
#Import
#from colums A:E from 1 exp design
###################################
# read.excel <- function(header=TRUE,...) {read.table("clipboard",sep="\t",header=header,...)}
# env.fact=read.excel() #read clipboard from excel
################################

# env.fact = data.frame(read_csv("C:/Users/g_ric/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/4 analyses/env data.csv", col_names = T))
#     env.fact[sapply(env.fact, is.character)] <- lapply(env.fact[sapply(env.fact, is.character)], as.factor)


str(env.fact)
env.fact$dli <- as.numeric(as.character(env.fact$dli))
env.fact$spp <- as.factor(as.character(env.fact$spp))
env.fact$disc1 <- as.factor(as.character(env.fact$disc))
env.fact$spec <- as.factor(as.character(env.fact$spec))
#env.fact$tank <- as.factor(as.character(env.fact$tank))
str(data0)
data2 = left_join(data0, env.fact, by = 'disc1')  #joined only for the data
data1 = data2


#Trim the fat and long form
data.s = dplyr::select (data1,c('disc1', 'PAR','dli', 'spec', 'Y(II)1','Y(II)2', 'Y(II)3'))  #remove column
str(data.s)
data.s$disc1 <- as.factor(as.character(data.s$disc1))
data1_long <- data.s %>% pivot_longer(-c(disc1, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")
data1_long <- arrange(data1_long, spec, dli, rep)
data1_long = data1_long[complete.cases(data1_long), ]
data1_long$id <- paste(data1_long$disc1, data1_long$rep, sep = "")  #individual ID
str(data1_long)
data1_long$rep <- as.factor(as.character(data1_long$rep))
data1_long$id <- as.factor(as.character(data1_long$id))

# #################rETR#################
#4)
data1_long$rETR = data1_long$PAR * data1_long$meas
library(ggplot2)
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code
p0 = ggplot()+geom_point(data1_long, mapping = aes(x = PAR, y = rETR), size = 1 )+facet_wrap(~id)
p0

#Data cleaning
data1_la = data1_long[which(data1_long$rETR<100),]   #remove rTER anomalies
data1_l = data1_la[which(data1_la$PAR<1200),]   #remove highest treatment
p0 = ggplot()+geom_point(data1_l, mapping = aes(x = PAR, y = rETR), size = 1 ) + facet_wrap(~disc1+rep)
p0

#Modeling
#Fit one
data1.s = split(data1_l, data1_l$id)
data1.s$`t001Y(II)1`
p0 = ggplot()+geom_point(data1.s$`t001Y(II)1`, mapping = aes(x = PAR, y = rETR), size = 1 )
p0
#Add small amount to x and y to allow for fit of model
data1.s$`t001 Y(II)`$PAR <- ifelse(data1.s$`t001Y(II)1`$PAR <= 0, 0.1, data1.s$`t001Y(II)1`$PAR)  #
data1.s$`t001 Y(II)1`$rETR <- ifelse(data1.s$`t001Y(II)1`$rETR <= 0, 0.01, data1.s$`t001Y(II)1`$rETR)  #

########Starting values#####################################################################
source("https://raw.githubusercontent.com/gerard-ricardo/data/master/ssplattmy")
library(minpack.lm)
md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=-30,alpha=0.35, beta=-0.5), data = data1.s$`t031Y(II)1`)
#Ys = Ys, apha = inital slope, beta = second slope,
# library(stats)
# library(Platt)
# df = data1.s$`t001Y(II)1` %>%  data.frame()
# getInitial(rETR ~ SSPlatt0(I, alpha, beta, Pmax), data = df)
# start = print(getInitial(rETR ~ SSPlatt0(PAR, alpha, beta, Pmax), data1.s$`t001 Y(II)1`) )
# str(start.df)
# start.df = start %>% unlist()%>% t(.) %>% data.frame()
# start.df$Pmax = start.df$Pmax*-1
# start = as.list(getInitial(rETR ~ SSPlatt(PAR, alpha, beta, Pmax), data1.s$`t001 Y(II)1` ))
# library(minpack.lm)
# md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=38.77,alpha=0.35, beta=-0.295), data = data1.s$`t001Y(II)1`)
df_x <- data.frame(PAR = seq(0.1, 926, length = 100)) #setting up  new  data frame (df) defining log.x values to run
vec_x =df_x[,1]
plot(data1.s$`t031Y(II)1`$PAR, data1.s$`t031Y(II)1`$rETR, col = 'red')
lines(vec_x, predict(md1, df_x)) #looks good

##All model fits
data1_l$PAR <- ifelse(data1_l$PAR <= 0, 0.1, data1_l$PAR)  #
data1_l$rETR <- ifelse(data1_l$rETR <= 0, 0.01, data1_l$rETR)  #

#Intial values
starts = data1_l %>% group_by(id) %>% do(broom::tidy(stats::getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Ys), data = . ))) %>%
    pivot_wider(names_from = names, values_from = x, names_prefix = "") %>% dplyr::select (.,-c('NA'))
colnames(starts) <- c("id", "alpha.s", 'beta.s', 'Pmax.s')
library(IDPmisc)
starts = NaRV.omit(starts)
starts$id
data1_l$id

##########6)The models#############################
library(dplyr)
library(nlme)
# groupedData(rETR ~ PAR | id, data1_l)

#Does work. But note that right join instead of left may remove some before error
params = data1_l  %>% right_join(.,starts, by = 'id') %>%
  group_by(id) %>%
do(model = try(broom::tidy(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)),
                     start = list(Pmax = mean(.$Pmax.s),
                          alpha = mean(.$alpha.s),
                          beta = mean(.$beta.s)),
                     data = .),silent = TRUE)) )

params$model
params_filtered <- params %>%
  rowwise() %>%
  filter(inherits(model, "tbl_df"))

unest.params = params_filtered %>% unnest(model)
df_param  = dplyr::select(unest.params, c(id, term, estimate))
dat_wide <- df_param %>% pivot_wider(names_from = term, values_from = estimate) # %>% dplyr::select(.,-c("NA")) #optional remove NA at end. DOn't run last pipe if no NA's
dat_wide$ETRm = dat_wide$Pmax*(dat_wide$alpha/(dat_wide$alpha+dat_wide$beta))*((dat_wide$beta/(dat_wide$alpha+dat_wide$beta)))^(dat_wide$beta/dat_wide$alpha)
dat_wide$Ek = dat_wide$ETRm/dat_wide$alpha
dat_wide$Em =(dat_wide$Pmax/dat_wide$alpha)*log((dat_wide$alpha+dat_wide$beta)/dat_wide$beta)
dat_wide$disc = substr(dat_wide$id, start = 1, stop = 4)
dat_wide$disc <- as.factor(as.character(dat_wide$disc))
final_df = left_join(dat_wide, env.fact, by = "disc")
data1 = data.frame(final_df)
data1

#save(data1, file = file.path("./Rdata", "ten.rlc.param.RData"))
load("./Rdata/ten.rlc.param.RData")
