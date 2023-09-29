# 
# max.PAR = 613  #1250 or 613
# 
# ######Run everything in R###################################################
# #1) Label your file names correctly
# 
# # 2) Import RLC data
# #setwd("C:/Users/gerar/OneDrive/1 Work/3 Results/6 Post-settlement/2 2018/2018 lab/3 PAM/180119 rlc/r mil csvs")
# library(tidyr)
# library(readr)
# library(dplyr)
# library(plyr)
# library(minpack.lm)
# library(ggplot2)
# library(IDPmisc)
# library(dplyr)
# library(nlme)
# source("https://raw.githubusercontent.com/gerard-ricardo/data/master/theme_sleek2")  #set theme in code
# source("https://raw.githubusercontent.com/gerard-ricardo/data/master/ssplattmy")
# 
# 
# # data1 <- list.files(full.names = TRUE) %>% lapply(read_delim, delim = ';') %>% bind_rows #Import multiple csvs into one dataframe
# read_csv_filename1 <- function(filename){
#     ret <- read_csv(filename)
#     ret$disc <- filename #EDIT
#     ret
# }   #also read_delim(delim = ';')
# filenames = list.files(path = "./rlc_mil_csvs", full.names = T)
# data1 <- ldply(filenames, read_csv_filename1)
# head(data1)
# options(scipen = 999)  # turn off scientific notation
# str(data1) #check data type is correct
# data1$no.f <- as.factor(as.character(data1$No.))
# #data1$disc1 = data1 %>% separate(disc, c("a", "b", 'c', 'd')) %>% .$b
# data1$disc1 = data1 %>% separate(disc, into = c("dir", "subdir", "filename"), sep = "/") %>%
#   separate(filename, into = c("code", "text"), sep = " ", extra = "merge") %>% pull(code) %>% as.factor() #gets disc ID from label
# data1$disc1 <- as.factor(as.character(data1$disc1))
# 
# 
# #3) Import and join Env. treatment data
# env.fact <- read.table(file="https://raw.githubusercontent.com/gerard-ricardo/data/master/postset%20treat%20mil", header= TRUE,dec=",", na.strings=c("",".","NA"))
# str(env.fact)
# env.fact$dli <- as.numeric(as.character(env.fact$dli))
# env.fact$disc1 = env.fact$disc
# str(env.fact)
# env.fact$disc1 <- as.factor(as.character(env.fact$disc1))
# env.fact$spp <- as.factor(as.character(env.fact$spp))
# env.fact$disc <- as.factor(as.character(env.fact$disc))
# env.fact$spec <- as.factor(as.character(env.fact$spec))
# data2 = left_join(data1, env.fact, by = 'disc1')  #joined only for the data
# data1 = data2
# #Trim the fat and long form
# data.s = dplyr::select (data1,c('disc1', 'PAR','dli', 'spec', 'Y(II)1','Y(II)2', 'Y(II)3'))  #remove column
# data1$disc1 <- as.factor(as.character(data1$disc1))
# data1.long <- data.s %>% pivot_longer(-c(disc1, PAR ,dli, spec),  names_to = "rep" ,values_to = "meas")
# data1.long <- arrange(data1.long, spec, dli, rep)
# data1.long = data1.long[complete.cases(data1.long), ]
# data1.long$id <- paste(data1.long$disc1, data1.long$rep, sep = "")  #individual ID
# str(data1.long)
# data1.long$disc1 <- as.factor(as.character(data1.long$disc1))
# data1.long$rep <- as.factor(as.character(data1.long$rep))
# data1.long$id <- as.factor(as.character(data1.long$id))
# 
# 
# # #################rETR#################
# #4)
# data1.long$rETR = data1.long$PAR * data1.long$meas
# p0 = ggplot()+geom_point(data1.long, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek2()+facet_wrap(~id)
# p0
# 
# #Data cleaning
# data1.l = data1.long[which(data1.long$rETR<100),]   #remove rTER anomalies
# data1.l = data1.l[which(data1.l$PAR<=max.PAR),]   #remove highest treatment 1250 or 500 for otehrs
# max(data1.l$PAR)
# p0 = ggplot()+geom_point(data1.l, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek2() + facet_wrap(~disc1+rep)
# p0  #clean multi-plot
# 
# #Modeling
# #Fit one
# data1.s = split(data1.l, data1.l$id)
# data1.s$`m001Y(II)1`
# p0 = ggplot()+geom_point(data1.s$`m001Y(II)1`, mapping = aes(x = PAR, y = rETR), size = 1 )+theme_sleek2() 
# p0
# 
# #Add small amount to x and y to allow for fit of model
# data1.s$`m001Y(II)1`$PAR <- ifelse(data1.s$`m001Y(II)1`$PAR <= 0, 0.1, data1.s$`m001Y(II)1`$PAR)  #
# data1.s$`m001Y(II)1`$rETR <- ifelse(data1.s$`m001Y(II)1`$rETR <= 0, 0.01, data1.s$`m001Y(II)1`$rETR)  #
# 
# ########Starting values#####################################################################
# 
# start = unname(getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Pmax), data1.s$`m001Y(II)1`))
# md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=start[3],alpha=start[1], beta=start[2]), data = data1.s$`m001Y(II)1`)  
# #Ys = Ys, apha = inital slope, beta = second slope, 
# # library(stats)
# # library(Platt)
# # df = data1.s$`m001Y(II)1` %>%  data.frame()
# # getInitial(rETR ~ SSPlatt0(I, alpha, beta, Pmax), data = df)
# 
# # str(start.df)
# # start.df = start %>% unlist()%>% t(.) %>% data.frame() 
# # start.df$Pmax = start.df$Pmax*-1
# # start = as.list(getInitial(rETR ~ SSPlatt(PAR, alpha, beta, Pmax), data1.s$`m001 Y(II)1` ))
# # library(minpack.lm)
# # md1 = nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), start=list(Pmax=38.77,alpha=0.35, beta=-0.295), data = data1.s$`m001Y(II)1`)  
# df.x <- data.frame(PAR = seq(0.1, max(data1.l$PAR), length = 1000)) #setting up  new  data frame (df) defining log.x values to run 
# vec.x =df.x[,1]
# plot(data1.s$`m001Y(II)1`$PAR, data1.s$`m001Y(II)1`$rETR, col = 'red')
# lines(vec.x, predict(md1, df.x)) #looks good for m001Y(II)1
# 
# 
# ##All model fits
# data1.l$PAR <- ifelse(data1.l$PAR <= 0, 0.1, data1.l$PAR)  #
# data1.l$rETR <- ifelse(data1.l$rETR <= 0, 0.01, data1.l$rETR)  #
# 
# #Intial values
# #source("C:/Users/gricardo/OneDrive - Australian Institute of Marine Science/R/1 my functions/SSPlatt.my.R")
# starts = data1.l %>% group_by(id) %>% do(broom::tidy(stats::getInitial(rETR ~ SSPlatt.my(PAR, alpha, beta, Ys), data = . ))) %>% 
#     pivot_wider(names_from = names, values_from = x, names_prefix = "") %>% dplyr::select (.,-c('NA'))
# colnames(starts) <- c("id", "alpha.s", 'beta.s', 'Pmax.s') 
# starts = NaRV.omit(starts) #removes inf
# #starts$id
# #data1.l$id
# 
# ##########6)The models#############################
# 
# # groupedData(rETR ~ PAR | id, data1.l)
# 
# #Does work. But note that right join instead of left may remove some before error
# params = data1.l  %>% right_join(.,starts, by = 'id') %>% 
#   group_by(id) %>%
# do(model = try(broom::tidy(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                      start = list(Pmax = mean(.$Pmax.s),
#                           alpha = mean(.$alpha.s),
#                           beta = mean(.$beta.s)),
#                      data = .),silent = TRUE)) )  #this get parameters for all models
# 
# 
# params$model[[1]]  #check for model 1
# 
# params2 = data1.l  %>% right_join(.,starts, by = 'id') %>% 
#   group_by(id) %>%
#   do(model = try(nlsLM(rETR ~ Pmax*(1-exp(-alpha*PAR/Pmax))*(exp(-beta*PAR/Pmax)), 
#                                    start = list(Pmax = mean(.$Pmax.s),
#                                                 alpha = mean(.$alpha.s),
#                                                 beta = mean(.$beta.s)),
#                                    data = .),silent = TRUE) )   #this gets models for all models
# 
# params2$model[[1]]  #check for model 1
# #run loop to predict for all models
# usq=list()#apply(scenarios,MARGIN=1,FUN=function(x){#print(x)})
# for(i in 1:nrow(params2)) {
#   out <- try(predict(params2$model[[i]], df.x))
#   usq=c(usq,list(out))
# }
# usq
# 
# df3 = data.frame(t(do.call(rbind.data.frame, usq)), row.names = paste0("", 1:1000))  #put all prediciton in data.frame
# str(df3)
# names = params2$id
# names <- as.factor(as.character(names))  #add col names
# colnames(df3) <- names
# df3[] <- lapply(df3, function(x) as.numeric(as.character(x)))   #convert all to numeric which adds NAs for error
# df3$PAR = df.x$PAR
# df3_long = df3 %>% pivot_longer(-PAR,  names_to = "id" ,values_to = "rETR") %>% data.frame() #keep vec.x, add all other columns to factors , add all their values to meas)
# #names <- as.factor(as.character(names))  #add col names
# df3_long = dplyr::arrange(df3_long, id) 
# str(df3_long)
# df3_long$rETR <- as.numeric(as.character(df3_long$rETR))  #add col names
# p0 = ggplot()+geom_point(data1.l, mapping = aes(x = PAR, y = rETR), size = 1 )
# p0 = p0 + geom_line(df3_long, mapping = aes(x = PAR, y = rETR))
# p0 = p0 + facet_wrap(~id)
# p0  #clean multiplot
# 
# 
# ###
# params_filtered <- params %>%
#   rowwise() %>%
#   filter(inherits(model, "tbl_df"))
# unest.test = params_filtered %>% unnest(model)
# df.param  = dplyr::select(unest.test, c(id, term, estimate))
# dat_wide <- df.param %>% pivot_wider(names_from = term, values_from = estimate)  #%>% dplyr::select(.,-c("NA")) #year goes to columns, their areas go as the values, area is the prefix
# dat_wide$ETRm = dat_wide$Pmax*(dat_wide$alpha/(dat_wide$alpha+dat_wide$beta))*((dat_wide$beta/(dat_wide$alpha+dat_wide$beta)))^(dat_wide$beta/dat_wide$alpha)
# dat_wide$Ek = dat_wide$ETRm/dat_wide$alpha
# dat_wide$Em =(dat_wide$Pmax/dat_wide$alpha)*log((dat_wide$alpha+dat_wide$beta)/dat_wide$beta)
# final_df = left_join(dat_wide, data1.long, by = 'id')
# final_df1 = dplyr::select (final_df,-c('disc1', 'PAR' ,'meas', 'rep', 'rETR'))  #remove column
# data1 <- data.frame(unique( final_df1[] ))
# data1

#save(data1, file = file.path("./Rdata", "mil.rlc.param.RData"))
load('./Rdata/mil.rlc.param.RData')

