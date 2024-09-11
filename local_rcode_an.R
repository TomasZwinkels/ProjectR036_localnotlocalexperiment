library(stargazer)
library(interplot) 
library(multiwayvcov); library(lmtest)
library(tidyr);library(dplyr); library(xtable)
library(grDevices); library(readxl)
library(margins)
library(haven)

library(sjPlot)
library(ggplot2)


setwd("XXX")
df<-read_dta('dl2incl.dta')


# country identifier

table(df$userlanguage)
df$cntry <- df$userlanguage
# Replace 'cntry' with "BE" if 'userlanguage' is "BE-FR" or "BE-NL"
df$cntry[df$userlanguage == "BE-FR" | df$userlanguage == "BE-NL"] <- "BE"
df$cntry[df$userlanguage == "CH-FR" | df$userlanguage == "CH-DE"] <- "CH"
df$cntry[df$userlanguage == "EN-GB"]  <- "GB"
table(df$cntry)



# models for paper -appendix
#################################################
#balance test for randomization

balancetest <-lm(local_treat  ~ left_right_1 + gender + year_of_birth + interest_in_pol,data = df)
stargazer(balancetest, align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,star.cutoffs = c(.05,.01,.001))

#################################################
# Linear regression of main models
t.test(local_out4 ~ local_treat, data = df)
t.test(local_out1 ~ local_treat, data = df)


nz_close1<-lm(local_out4 ~ local_treat ,data = df)
nz_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +cntry-1,data = df)
nz_vote1<-lm(local_out1 ~ local_treat ,data = df)
nz_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +cntry-1,data = df)

stargazer(nz_close1, nz_close, nz_vote1, nz_vote, align=TRUE, omit.stat=c("LL","ser","f"), omit=c("cntry"), dep.var.labels=c("Localness (manipulation check)",
 "Localness (manipulation check)", "Probability to vote", "Probability to vote"),  
          covariate.labels=c("Local treatment","Left-right", "Gender", "Age", "Gender politician", "Facebook display", "Neutral display", "Twitter display"),
          no.space=TRUE, star.cutoffs = c(.05,.01,.001),  column.sep.width = "-15pt")

#################################################
#By country: manipulation check as depvar
be_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='BE',])
ch_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='CH',])
de_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='DE',])
fr_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='FR',])
gb_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='GB',])
nl_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='NL',])
pl_close<-lm(local_out4 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='PL',])

stargazer(be_close, ch_close, de_close, fr_close, gb_close, nl_close, title="Manipulation check by country", omit=c("local_channel1", "local_channel3", "local_channel4"), dep.var.labels=c("Localness (manipulation check)"), align=TRUE, 
covariate.labels=c("Local treatment","Left-right", "Gender", "Age", "Gender politician"),star.cutoffs = c(.05,.01,.001), column.sep.width = "-15pt", omit.stat=c("LL","ser","f"))

stargazer(pl_close, title="Manipulation check by country", omit=c("local_channel1", "local_channel3", "local_channel4"), dep.var.labels=c("Localness (manipulation check)"), align=TRUE, 
          covariate.labels=c("Local treatment","Left-right", "Gender", "Age", "Gender politician"),star.cutoffs = c(.05,.01,.001), column.sep.width = "-15pt", omit.stat=c("LL","ser","f"))


################################################
#By country: vote for politician as depvar
be_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='BE',])
ch_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='CH',])
de_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='DE',])
fr_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='FR',])
gb_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='GB',])
nl_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='NL',])
pl_vote<-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender +local_channel1+ local_channel3 +local_channel4,data = df[df$cntry=='PL',])

stargazer(be_vote, ch_vote, de_vote, fr_vote, gb_vote, nl_vote, pl_vote, title="Results by country", omit=c("local_channel1", "local_channel3", "local_channel4"),
          digits = 3, column.labels=c(''), align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,star.cutoffs = c(.05,.01,.001), covariate.labels=c("Local treatment","Left-right", "Gender", "Age", "Gender politician"),
          column.sep.width = "-15pt" )

#################################################
#social media specific models
# excluding neutral condition
vote_sm <- lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender, 
              data = df[df$local_channel3 != 1, ])
# only Twitter
vote_tw <- lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth + local_gender, 
             data = df[df$local_channel4 == 1, ])
#interactions social media with treatments
vote_int_tw <- lm(local_out1 ~ local_treat *local_channel4+ left_right_1 + gender + year_of_birth + local_gender +local_channel1 +local_channel3, data = df)
#plot_model(vote_int_tw, type = "pred", terms = c("local_channel4","local_treat" ))
vote_int_fb <- lm(local_out1 ~ local_treat *local_channel1+ left_right_1 + gender + year_of_birth + local_gender +local_channel4 +local_channel3,  data = df)
#plot_model(vote_int_fb, type = "pred", terms = c("local_channel1","local_treat" ))
vote_int_insta <- lm(local_out1 ~ local_treat *local_channel2+ left_right_1 + gender + year_of_birth + local_gender  +local_channel1 +local_channel4,  data = df)
#plot_model(vote_int_insta, type = "pred", terms = c("local_channel2","local_treat" ))

stargazer(vote_sm, vote_tw, vote_int_tw, vote_int_fb, vote_int_insta, title="Results by social media", 
          digits = 3, column.labels=c(''), align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,star.cutoffs = c(.05,.01,.001))

#################################################
#more robustness tests
#party-fixed effects
vote_party <-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +party_vote_orig-1,data = df)

#region-fixed effects 
vote_region <-lm(local_out1 ~ local_treat + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +region-1,data = df)

vote_leftright <-lm(local_out1 ~ local_treat * left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +countrylang1 + countrylang2 + countrylang3 
                  + countrylang4 + countrylang5 + countrylang6 + countrylang7 + countrylang8,data = df)
vote_salient <-lm(local_out1 ~ local_treat * econ_salient + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +countrylang1 + countrylang2 + countrylang3 
                   + countrylang4 + countrylang5 + countrylang6 + countrylang7 + countrylang8,data = df)
vote_cosmo <-lm(local_out1 ~ local_treat * nativism + left_right_1 + gender + year_of_birth +local_gender +local_channel1+ local_channel3 +local_channel4 +countrylang1 + countrylang2 + countrylang3 
                + countrylang4 + countrylang5 + countrylang6 + countrylang7 + countrylang8,data = df)

plot_model(vote_cosmo, type = "pred", terms = c("nativism","local_treat" ), axis.title ("Predicted margins of treatment"))


stargazer(vote_party, vote_region, vote_leftright, vote_salient, vote_cosmo, title="Robustness tests", omit=c("region", "party_vote_orig"), column.sep.width = "-15pt",
          digits = 3, column.labels=c(''), align=TRUE, omit.stat=c("LL","ser","f"), no.space=TRUE,star.cutoffs = c(.05,.01,.001),add.lines=list(c('Fixed effects', 'Party','Region', 'Country', 'Country','Country')))

#######################################################################################
#######################################################################################

#################################
#Figure "main results M1 closeness, M2 vote"
#################################


plot_frame<-function(label='Write something', scale=c(-.5,.5)){
  par(mar=c(2,5,1,1))
  plot(seq(0,5, length.out = 2),xaxs="i", 
       y=scale, type='n', ylab=label,
       cex.axis=.9,las=1,
       xaxt='n', xlab='')
  #for (i in seq(2,14,by=2)){
  #  abline(v=i,lty=2)
  #}
  #  abline(v=14,lwd=2)
  abline(0,0)
  axis(1,at=seq(1,5,by=2),tick=F,
       labels=c('Closeness ','Vote probablity', ''),
       col.axis='seagreen',
       cex.axis=.9)
}

#Summarizing results to plot
ls_vote<-list(nz_close,nz_vote)

#Extracting coefs/SEs from models
est_vote<-NULL;se_vote<-NULL


for (i in 1:2){
  rg11<-ls_vote[[i]]
  est_vote[i]<-coef(rg11)[1]
  se_vote[i]<-summary(rg11)$coefficients[1,2]
}

#pdf(file='fig4b.pdf', width=8.9,height=4)
plot_frame(label='Effect of localness treatment',
           scale=c(-.9,2))

pap_location<-seq(1,5,by=2)
for (i in 1:2){
  points(pap_location[i],est_vote[i],pch=20,col='seagreen')
  segments(pap_location[i],est_vote[i]-1.96*se_vote[i],
           y1=est_vote[i]+1.96*se_vote[i],col='seagreen')
  segments(pap_location[i],est_vote[i]-1*se_vote[i],
           y1=est_vote[i]+se_vote[i],col='seagreen',lwd=2)
}

#dev.off()

########################################
# # Figure -depvar: probability to vote by country
########################################

#Summarizing results to plot
l_vote<-list(be_vote,ch_vote,de_vote,fr_vote,gb_vote,nl_vote,pl_vote)

#Extracting coefs/SEs from models
est_vote<-NULL;se_vote<-NULL

for (i in 1:7){
  rg1<-l_vote[[i]]
  est_vote[i]<-coef(rg1)[2]
  se_vote[i]<-summary(rg1)$coefficients[2,2]
}

#Function to produce plots 
plot_frame<-function(label='Write something', scale=c(-.5,.5)){
  par(mar=c(2,5,1,1))
  plot(seq(0,15, length.out = 2),xaxs="i", 
       y=scale, type='n', ylab=label,
       cex.axis=.9,las=1,
       xaxt='n', xlab='')
  #for (i in seq(2,14,by=2)){
  #  abline(v=i,lty=2)
  #}
#  abline(v=14,lwd=2)
  abline(0,0)
  axis(1,at=seq(1,13,by=2),tick=F,
       labels=c('BE','CH','DE',
                'FR','GB','NL','PL'),
       col.axis='seagreen',
       cex.axis=.9)
#  axis(1,at=seq(15,15,by=2),labels=c( 'All countries'),tick = F, cex.axis=.9,
#     col.axis='darkviolet')
}

#dev.off()
#pdf(file='fig2.pdf', width=8.9,height=4)
plot_frame(label='Effect of localness on vote',
           scale=c(-.6,.6))

pap_location<-seq(1,15,by=2)
for (i in 1:7){
  points(pap_location[i],est_vote[i],pch=20,col='seagreen')
  segments(pap_location[i],est_vote[i]-1.96*se_vote[i],
           y1=est_vote[i]+1.96*se_vote[i],col='seagreen')
  segments(pap_location[i],est_vote[i]-1*se_vote[i],
           y1=est_vote[i]+se_vote[i],col='seagreen',lwd=2)
}

#dev.off()

########################################
# # Figure -depvar: closeness by country
########################################

#Summarizing results to plot
l_close<-list(be_close,ch_close,de_close,fr_close,gb_close,nl_close,pl_close)

#Extracting coefs/SEs from models
est_close<-NULL;se_close<-NULL

for (i in 1:7){
  rg2<-l_close[[i]]
  est_close[i]<-coef(rg2)[2]
  se_close[i]<-summary(rg2)$coefficients[2,2]
}

#Function to produce plots 
plot_frame<-function(label='Write something', scale=c(-.5,.5)){
  par(mar=c(2,5,1,1))
  plot(seq(0,15, length.out = 2),xaxs="i", 
       y=scale, type='n', ylab=label,
       cex.axis=.9,las=1,
       xaxt='n', xlab='')
  #for (i in seq(2,14,by=2)){
  #  abline(v=i,lty=2)
  #}
  #  abline(v=14,lwd=2)
  abline(0,0)
  axis(1,at=seq(1,13,by=2),tick=F,
       labels=c('BE','CH','DE',
                'FR','GB','NL','PL'),
       col.axis='seagreen',
       cex.axis=.9)
  #  axis(1,at=seq(15,15,by=2),labels=c( 'All countries'),tick = F, cex.axis=.9,
  #     col.axis='darkviolet')
}

#dev.off()
#pdf(file='fig2.pdf', width=8.9,height=4)
plot_frame(label='Effect of localness on closeness',
           scale=c(-.2,2.5))

pap_location<-seq(1,15,by=2)
for (i in 1:7){
  points(pap_location[i],est_close[i],pch=20,col='seagreen')
  segments(pap_location[i],est_close[i]-1.96*se_vote[i],
           y1=est_close[i]+1.96*se_close[i],col='seagreen')
  segments(pap_location[i],est_close[i]-1*se_close[i],
           y1=est_close[i]+se_close[i],col='seagreen',lwd=2)
}


