library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggbeeswarm)
library(readxl)
library(tidyverse)
library(rstatix) 
library(scales)

########## Setup main data set ##########

Complete <- read_excel("D:/Documents/Research_Data/Repeats/Serum Repeat.xlsx")

# Data <- Complete %>%
#      filter(Time == 1)

Data[,1] <- sapply(Data[,1], as.character)
Data[,2] <- sapply(Data[,2], as.character)
Data[,3] <- sapply(Data[,3], as.character)
Data[,4] <- sapply(Data[,4], as.character)


# Data <- Data %>%
#    mutate(SurfaceArea = CellSize * Count2) %>%
#    mutate(ProportionLost = (1 - (Count2/Count))*100
  
Averages <- Data %>%
  group_by(Concentration, Biological, Time) %>%
  summarise_each(list(mean))

########## Set up plotting variables ##########

Time.labs <- c("24 Hours", "48 Hours", "72 Hours")
names(Time.labs) <- c("1", "2", "3")
Concentration.labs <- c("Control (0 ng/mL)", "0.1 ng/mL", "1 ng/mL", "10 ng/mL")
Treatmentlabs <- c("Serum-free", "Serfum-free + PDGF-bb", "Serum", "Serum + PDGF-bb")
names(Concentration.labs) <- c("A", "B", "C", "D")
my_comparisons <- list(c("A", "B"), c("A", "C"), c("A", "D"), c("C", "D")) # comparisons for post-hoc tests
adjusted <- list(cutpoints = c(0, 0.000025, 0.00025, 0.0025, 0.0125, 1), symbols = c("****", "***", "**", "*", "ns"))
#z <- c(1,2,3,4,"Number of cells per well",6 ,"Mean cell size per well (pixels)",8,9,10,11,12,13,14,15,16,17,18,19,20,"Number of cells per well","Relative fold change",23,24,25,26)
#v <- c(1,2,3,4,"proliferation","nuclear size","surface area","mask intensity","actin intensity","compactness","eccentricity","Euler Number","Extent","Form Factor","Solidity","nuclear compactness","nuclear eccentricity","nuclear extent","nuclear form factor","nuclear solidity","proliferation","proliferation")
#y <- which(names(Data) == "Count"        # toggle to select a subset of measured qualities to plot
#           | names(Data) == "FC"         # toggle to select a subset of measured qualities to plot
#           | names(Data) == "CellSize")  # toggle to select a subset of measured qualities to plot

########## Plot Data ##########
# for (i in y) {                          # toggle to select a subset of measured qualities to plot

# for (i in 9:9){
#   p <- ggplot(Data, aes_string(x = "Concentration", y=colnames(Data[i]), color="factor(Biological)")) +
#     geom_beeswarm(cex=2, alpha = 0.30, size= 1) + 
# #    facet_wrap(~Time, labeller = labeller(Time = Time.labs)) +
#     scale_colour_brewer(palette = "Set1") +
#     geom_beeswarm(data=Averages, size=3, shape=18) + 
#     labs(color = "Biological Replicate") +
#     stat_compare_means(data=Averages, comparisons = my_comparisons, method="t.test", label = "p.signif", symnum.args = adjusted ) +
#     theme_bw() +
#  #   labs(x = "Treatment Condition", y = bquote('Cells per well')) +
#  #   labs(x = "Treatment Condition", y = bquote('Mean cell area per well (pixels)')) +
#    labs(x = "Treatment Condition", y = bquote('Total cell area (\u00B5'~m^2~')')) +
#    # labs(x = "Treatment Condition", y = bquote('Relative change in cell count')) +
#    # labs(x = "Treatment Condition", y = bquote('Mean total cell area per well (\u00B5'~m^2~')')) +
#    # labs(x = "Treatment Condition", y = bquote('Mean total cell area per well (\u00B5'~m^2~')')) +
#     scale_x_discrete(labels= Treatmentlabs) +
#     scale_y_continuous(labels = comma) +
#     theme(legend.position="bottom") +
#     theme(legend.background = element_rect(fill="lightgrey", size=0.5, linetype="solid", colour ="darkgrey")) +
#  #   ggtitle("Effect of PDGF-bb on the proliferation of serum and serum-free cultured DR- cells at 48 hours.")
#  #   ggtitle("Effect of PDGF-bb on the area of serum and serum-free cultured DR- cells at 48 hours.")
#    ggtitle("Effect of PDGF-bb on the coverage of serum and serum-free cultured DR- cells at 48 hours.")
#   #  ggtitle("Effect of PDGF-bb on the proliferation of serum and serum-free cultured DR- cells at 48 hours.")
#  #   ggtitle("Effect of PDGF-bb on the total area of serum and serum-free cultured DR- cells at 48 hours.")
# #  ggtitle("Relative population change of DR- cells treated with PDGF-bb (serum and serum-free at 48 hours).")
# 
#   print(p)
# }

for (i in 9:9){
  p <- ggplot(Data, aes_string(x = "Concentration", y=colnames(Data[i]), color="factor(Biological)")) +
    geom_beeswarm(cex=2, alpha = 0.30, size= 1) + 
        facet_wrap(~Time, labeller = labeller(Time = Time.labs)) +
    scale_colour_brewer(palette = "Set1") +
    geom_beeswarm(data=Averages, size=3, shape=18) + 
    labs(color = "Biological Replicate") +
    stat_compare_means(data=Averages, comparisons = my_comparisons, method="t.test", label = "p.signif", symnum.args = adjusted ) +
    theme_bw() +
    #   labs(x = "Treatment Condition", y = bquote('Cells per well')) +
    #   labs(x = "Treatment Condition", y = bquote('Mean cell area per well (pixels)')) +
    labs(x = "Treatment Condition", y = bquote('Total cell area (\u00B5'~m^2~')')) +
    # labs(x = "Treatment Condition", y = bquote('Relative change in cell count')) +
    # labs(x = "Treatment Condition", y = bquote('Mean total cell area per well (\u00B5'~m^2~')')) +
    # labs(x = "Treatment Condition", y = bquote('Mean total cell area per well (\u00B5'~m^2~')')) +
    scale_x_discrete(labels= Treatmentlabs) +
    scale_y_continuous(labels = comma) +
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill="lightgrey", size=0.5, linetype="solid", colour ="darkgrey")) +
    #   ggtitle("Effect of PDGF-bb on the proliferation of serum and serum-free cultured DR- cells at 48 hours.")
    #   ggtitle("Effect of PDGF-bb on the area of serum and serum-free cultured DR- cells at 48 hours.")
    ggtitle("Effect of PDGF-bb on the coverage of serum and serum-free cultured DR- cells at 48 hours.")
  #  ggtitle("Effect of PDGF-bb on the proliferation of serum and serum-free cultured DR- cells at 48 hours.")
  #   ggtitle("Effect of PDGF-bb on the total area of serum and serum-free cultured DR- cells at 48 hours.")
  #  ggtitle("Relative population change of DR- cells treated with PDGF-bb (serum and serum-free at 48 hours).")
  
  print(p)
}
