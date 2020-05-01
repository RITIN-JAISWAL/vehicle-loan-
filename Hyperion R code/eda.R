
library(corrplot)
library(tidyverse)
library(funModeling) 
library(Hmisc)
library(ggplot2)
library(knitr)
library(formattable)
library(table1)
library(stargazer)
library(dplyr)

df <- read.csv("train.csv",header= TRUE , )


#dataframe consisting of numerical continuous  variables
continuous_df <-df[,c("disbursed_amount","asset_cost","ltv","PERFORM_CNS.SCORE"
                   ,"PRI.NO.OF.ACCTS","PRI.ACTIVE.ACCTS","PRI.OVERDUE.ACCTS"
                   ,"PRI.CURRENT.BALANCE","PRI.SANCTIONED.AMOUNT","PRI.DISBURSED.AMOUNT","SEC.NO.OF.ACCTS","SEC.ACTIVE.ACCTS","SEC.OVERDUE.ACCTS"
                   ,"SEC.CURRENT.BALANCE","SEC.SANCTIONED.AMOUNT","SEC.DISBURSED.AMOUNT","PRIMARY.INSTAL.AMT","SEC.INSTAL.AMT","NEW.ACCTS.IN.LAST.SIX.MONTHS",
                   "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS","NO.OF_INQUIRIES")]

#produces a correlation matrix using pearson 
corr <- cor(continuous_df, method = "pearson")

corrplot(corr,method="circle",tl.cex = 0.4)

#creating tables for catergorical data in our dataset
variable <-as.data.frame(table(df$loan_default),stringsAsFactors=FALSE)
variable[variable==""]<-"Missing"

names(variable) <- c("loan_default","Freq")
#uncomment to produce table
#print(formattable(variable))

#produces a descriptive statistics summary of all numerical variables in our dataset
#produces html code which is used to render the table. can change the type parameter to later for latex code or text for table to be displayed in the console,
stargazer(as.data.frame(continuous_df) ,type = "text", 
            summary.stat = c("max","mean", "sd","median","min","p25","p75"))



#producing bar charts 
bar_chart<-ggplot(data=variable, aes(x=loan_default, y=Freq, fill=loan_default)) +
  geom_bar(stat="identity",color="black") + scale_fill_brewer(palette="Dark2")+ theme_linedraw() +
  theme(legend.key = element_rect(size = 5),
        legend.key.size = unit(1.5, 'lines') )




#uncomment if creating bar-charts with many catergories and need larger pallete size

# cols <- colorRampPalette(brewer.pal(12, "Set3"))
# myPal <- cols((20))
# bar_chart<-ggplot(data=variable, aes(x=PERFORM_CNS.SCORE.DESCRIPTION, y=Freq, fill=PERFORM_CNS.SCORE.DESCRIPTION)) +
#   geom_bar(stat="identity",color="black") + scale_fill_manual(values=mycolors)+ theme_linedraw() +
#   theme(legend.key = element_rect(size = 5),
#         legend.key.size = unit(1.5, 'lines') ,
#         axis.text.x= element_text(vjust=0.5,size=9,angle=90))

#print(bar_chart) 
# uncomment and add if scaling is needed : scale_y_continuous(breaks=seq(0,150000,10000))

numerical_var <- c("disbursed_amount","asset_cost","ltv","PERFORM_CNS.SCORE"
                        ,"PRI.NO.OF.ACCTS","PRI.ACTIVE.ACCTS","PRI.OVERDUE.ACCTS"
                        ,"PRI.CURRENT.BALANCE","PRI.SANCTIONED.AMOUNT","PRI.DISBURSED.AMOUNT","SEC.NO.OF.ACCTS","SEC.ACTIVE.ACCTS","SEC.OVERDUE.ACCTS"
                        ,"SEC.CURRENT.BALANCE","SEC.SANCTIONED.AMOUNT","SEC.DISBURSED.AMOUNT","PRIMARY.INSTAL.AMT","SEC.INSTAL.AMT","NEW.ACCTS.IN.LAST.SIX.MONTHS",
                        "DELINQUENT.ACCTS.IN.LAST.SIX.MONTHS","NO.OF_INQUIRIES")

# 
# for (val in numerical_var){
#   variable <- as.data.frame(df[,val])
#   names(variable) <- c(val)
#   +ylim(0,50000)+scale_x_continuous(limits=c(0,30))
  
hist_plot<-ggplot(as.data.frame(df),aes(x=SEC.SANCTIONED.AMOUNT))+
  geom_histogram(color="darkblue",fill="lightsalmon1",binwidth=10000)+xlim(0,1000000)+theme_linedraw()
  
print(hist_plot)

