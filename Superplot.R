library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(readxl)
library(tidyverse)
library(rstatix)
library(scales)

########## Setup main data set ##########


fname<- file.choose() # get user input for data location
DataSet <- read_excel(fname) #import excel data from user specified location

DataSet[,1] <- sapply(DataSet[,1], as.character) # convert independant variables to chracters from better plotting
DataSet[,2] <- sapply(DataSet[,2], as.character)
DataSet[,3] <- sapply(DataSet[,3], as.character)
DataSet[,4] <- sapply(DataSet[,4], as.character)

DataSet <- DataSet %>%
   mutate(SurfaceArea = Area * Count) %>% #generate total surface area cover by cells by multiplying average cell size by the number of cells
   mutate(ProportionLost = (1 - (Count/Count2))) #determine proprtion of cells lost in the washing step

####### set up average data set to plot over the individual biological replicate data ########################

Averages <- DataSet %>%
  group_by(Concentration, Biological, Time) %>% #group all individual biological replicates at each specific time and concentration
  summarise_each(list(mean)) # find averages of these biological replicate groups

########## Set up plotting variables ##########

Time.labs <- c("24 Hours", "48 Hours", "72 Hours")
names(Time.labs) <- c("1", "2", "3")
Concentration.labs <- c("Control (0 ng/mL)", "0.1 ng/mL", "1 ng/mL", "10 ng/mL")
Treatmentlabs <- c("Control", "0.1", "1", "10", "100", "1000")
names(Concentration.labs) <- c("A", "B", "C", "D")
my_comparisons <- list(c("0", "0.1"), c("0", "1"), c("0", "10"), c("0", "100"), c("0", "1000")) # comparisons for post-hoc tests
adjusted <- list(cutpoints = c(0, 0.00002, 0.0002, 0.002, 0.01, 1), symbols = c("****", "***", "**", "*", "ns"))
xlabel <- "Compound A concentration (ng/mL)"
ylabels <- c("","","","","Number of cells per well","Number of cells per well","Mean individual cell area per well (\U00B5m)","Mean total cell area per well (\U00B5m)","Proportion of cells lost during washing")
titlelabels <- c("","","","","Effect of Compound A on the cell proliferation from 24-72 hours (before washing).","Effect of Compound A on the cell proliferation from 24-72 hours (after washing).","Effect of Compound A on the mean individual size of cultured cells from 24-72 hours.","Effect of Compound A on the total surface of cultured cells from 24-72 hours.","Proportion of cells lost during fluorescent washing steps by time and Compound A concentration.")

########## Plotting loop uses the above array for graph titles and y-axis names
  
for (i in 5:9){
  p <- ggplot(DataSet, aes_string(x = "Concentration", y=colnames(DataSet[i]), color="factor(Biological)")) +
    geom_beeswarm(cex=2, alpha = 0.30, size= 1) + 
    facet_wrap(~Time, labeller = labeller(Time = Time.labs)) + #plot each time point seperately
    scale_colour_brewer(palette = "Set1") + # set colour palette
    labs(color = "Biological Replicate") + #colour biological replicates seperately
    geom_beeswarm(data=Averages, size=3, shape=18) + #plot averaged biological replicate data over individual data points
    stat_compare_means(data=Averages, comparisons = my_comparisons, method="t.test", label = "p.signif", symnum.args = adjusted ) + #perform t-test on predertmined comparisons (my_comparisons) and use adjusted t-test for multiple testing
    theme_bw() + #theme. purely visual
    scale_x_discrete(labels= Treatmentlabs) + #use pre-defined labels to include control
#    scale_y_continuous(labels = percent) + #disabled but allows for proportion data to be shown as percentage
    theme(legend.position="bottom") + #legend position
    theme(legend.background = element_rect(fill="lightgrey", size=0.5, linetype="solid", colour ="darkgrey")) + #legend aesthetics
    labs(x = xlabel, y = ylabels[i]) + #apply graph labels from variables above
    ggtitle(titlelabels[i]) + #apply graph title from array above
  print(p) #draw graph
}