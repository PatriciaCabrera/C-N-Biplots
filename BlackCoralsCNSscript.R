
          ##################################################################################
          ##################################################################################
          ##                                                                              ##
          ##                            BIPLOT SCRIPT BLACK CORALS                        ##
          ##                                       by                                     ##
          ##                             Patricia Martin-Cabrera                          ##
          ##                                                                              ##
          ##################################################################################
          ##################################################################################

# Set the mac to UTF-8 Code to get the ‰ sign printed
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# Load libraries
library(ggplot2)
library(ggrepel)
library(tidyverse)
library(SIBER)
library(gridGraphics)
library(cowplot)
library(readxl)
library(ggpubr)
          
# Import the data
getwd()
setwd(dir = "/Users/patriciamartin/Documents/Thesis/Data Analysis/Data")

CoralIsotopeData <- read_excel("Isotope_Data.xlsx")
#Isotopedata <- read.csv2("IsotopeData.csv", header = T,row.names = 1)
head(CoralIsotopeData)


################# FIRST DO CLASSIC ISOTOPE BIPLOTS ###################

# The classic isotope biplot requires adding the means and error bars in both directions, so first 
# I need to create summary data for each of the groups. So I create a new object called "Summary" where 
# I calculate the mean, sd, min and max for each group and I will name the mean of iso1 as "mC" for meanCarbon,
# the standard deviation of iso1 as "sdC", min value of iso1 as "minC" and max value of iso1 as "maxC"...
# and similarly for N and S. 
Summary <- CoralIsotopeData %>% group_by(Species) %>% 
  summarise(mC = mean(`d13C (‰)`), 
            sdC = sd(`d13C (‰)`),
            mN = mean(`d15N (‰)`), 
            sdN = sd(`d15N (‰)`),
            mS = mean(`d34S (‰)`),
            sdS = sd(`d34S (‰)`))
Summary
# Since Antipathes flabellum,Cirrhipathes rumphii,Cupressopathes nsp, have n=1 
# I remove them from the summary 
Summary <- Summary[-(c(2,6,10)),]

#Ploting: add the layers using the summary data in Summary
#select the shapes manually, look at the different options
ggpubr::show_point_shapes() 


library(ggplot2)
library(ggrepel)
CNbiplot=ggplot(data=Summary,aes(x=mC, y=mN))+
  geom_errorbar(data=Summary,aes(ymin=mN-sdN, ymax=mN+sdN), na.rm=TRUE, width=0.2)+
  geom_errorbarh(data=Summary,aes(xmin=mC-sdC, xmax=mC+sdC), na.rm=TRUE, height=0.2)+
  geom_point(aes(x=mC, y=mN,fill = factor(Species),shape=factor(Species)),colour="black",size = 3)+
  scale_fill_manual(values= c("limegreen","firebrick1","blueviolet","indianred4","lightpink",
                              "navy","aquamarine","lightgoldenrod","khaki4"),
                    name="Species",
                    breaks=c("Antipathes cf. virgata",
                             "Cirrhipathes anguina",
                             "Cirrhipathes contorta",
                             "Cirrhipathes densiflora",
                             "Cirrhipathes spiralis",
                             "Cupressopathes abies",
                             "Cupressopathes cf. pumila",
                             "Myriopathes cf. ulex",
                             "Stichopathes maldivensis"),
                    labels=expression("Antipathes cf. virgata",
                                      "Cirrhipathes anguina",
                                      "Cirrhipathes contorta",
                                      "Cirrhipathes densiflora",
                                      "Cirrhipathes spiralis",
                                      "Cupressopathes abies",
                                      "Cupressopathes cf. pumila",
                                      "Myriopathes cf. ulex",
                                      "Stichopathes maldivensis"))+
  
  scale_shape_manual(values= c(21, 22, 22, 22, 22, 23, 23, 24, 25),
                     name="Species",
                     breaks=c("Antipathes cf. virgata", "Cirrhipathes anguina", "Cirrhipathes contorta", 
                              "Cirrhipathes densiflora","Cirrhipathes spiralis","Cupressopathes abies",
                              "Cupressopathes cf. pumila","Myriopathes cf. ulex","Stichopathes maldivensis"),
                     labels=expression("Antipathes cf. virgata",
                                       "Cirrhipathes anguina",
                                       "Cirrhipathes contorta",
                                       "Cirrhipathes densiflora",
                                       "Cirrhipathes spiralis",
                                       "Cupressopathes abies",
                                       "Cupressopathes cf. pumila",
                                       "Myriopathes cf. ulex",
                                       "Stichopathes maldivensis"))+
  
  ylab(expression(paste(delta^{15}, "N (\u2030)")))+
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_bw()+
  theme(axis.text.x = element_text( color="black"),
        axis.text.y = element_text( color="black"),
        axis.title.y = element_text(face="bold",color="black"),axis.title.x = element_text(face="bold",color="black"),
        strip.text = element_text(size=11,face="bold"))+
  theme(panel.grid = element_blank(),legend.position = "none")+
  theme(panel.grid = element_blank(),legend.text=element_text(size=12),legend.title=element_text(size=13),
        legend.position='bottom',legend.margin=margin(4,4,4,4),
        legend.box.margin=margin(-8,-8,-8,-8))+
  theme(legend.text = element_text(face ="italic"))+
  guides(fill = guide_legend(nrow = 3))


CNbiplot

CSbiplot=ggplot(data=Summary,aes(x=mC, y=mS))+
  geom_errorbar(data=Summary,aes(ymin=mS-sdS, ymax=mS+sdS), na.rm=TRUE, width=0.2)+
  geom_errorbarh(data=Summary,aes(xmin=mC-sdC, xmax=mC+sdC), na.rm=TRUE, height=0.2)+
  geom_point(aes(x=mC, y=mS,fill = factor(Species),shape=factor(Species)),colour="black",size = 3)+
  scale_fill_manual(values= c("limegreen","firebrick1","blueviolet","indianred4","lightpink",
                              "navy","aquamarine","lightgoldenrod","khaki4"),
                    name="Species",
                    breaks=c("Antipathes cf. virgata",
                             "Cirrhipathes anguina",
                             "Cirrhipathes contorta",
                             "Cirrhipathes densiflora",
                             "Cirrhipathes spiralis",
                             "Cupressopathes abies",
                             "Cupressopathes cf. pumila",
                             "Myriopathes cf. ulex",
                             "Stichopathes maldivensis"),
                    labels=expression("Antipathes cf. virgata",
                                      "Cirrhipathes anguina",
                                      "Cirrhipathes contorta",
                                      "Cirrhipathes densiflora",
                                      "Cirrhipathes spiralis",
                                      "Cupressopathes abies",
                                      "Cupressopathes cf. pumila",
                                      "Myriopathes cf. ulex",
                                      "Stichopathes maldivensis"))+
  
  scale_shape_manual(values= c(21, 22, 22, 22, 22, 23, 23, 24, 25),
                     name="Species",
                     breaks=c("Antipathes cf. virgata", "Cirrhipathes anguina", "Cirrhipathes contorta", 
                              "Cirrhipathes densiflora","Cirrhipathes spiralis","Cupressopathes abies",
                              "Cupressopathes cf. pumila","Myriopathes cf. ulex","Stichopathes maldivensis"),
                     labels=expression("Antipathes cf. virgata",
                                       "Cirrhipathes anguina",
                                       "Cirrhipathes contorta",
                                       "Cirrhipathes densiflora",
                                       "Cirrhipathes spiralis",
                                       "Cupressopathes abies",
                                       "Cupressopathes cf. pumila",
                                       "Myriopathes cf. ulex",
                                       "Stichopathes maldivensis"))+
  
  ylab(expression(paste(delta^{34}, "S (\u2030)")))+
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) +
  theme_bw()+
  theme(axis.text.x = element_text( color="black"),
        axis.text.y = element_text( color="black"),
        axis.title.y = element_text(face="bold",color="black"),axis.title.x = element_text(face="bold",color="black"),
        strip.text = element_text(size=11,face="bold"))+
  theme(panel.grid = element_blank(),legend.position = "none")+
  theme(panel.grid = element_blank(),legend.text=element_text(size=12),legend.title=element_text(size=13),
        legend.position='bottom',legend.margin=margin(4,4,4,4),
        legend.box.margin=margin(-8,-8,-8,-8))+
  theme(legend.text = element_text(face ="italic"))+
  guides(fill = guide_legend(nrow = 3))

  
CSbiplot

#Plot the 2 plots together, so first need to extract the legend 
legend <- get_legend(CSbiplot + theme(legend.position='bottom'))
# arrange the 2 plots in a single row
PLOTS <- plot_grid( CNbiplot + theme(legend.position='none'),
                    CSbiplot + theme(legend.position="none"),
                   align = 'vh',
                   labels = c("A", "B"),
                   hjust = -1,
                   nrow = 1)
PLOTS
# add the legend to the row we made earlier. Give it one-third of the width
# of one plot (via rel_widths).
FINALPLOT <- plot_grid( PLOTS, legend, ncol = 1, rel_heights=c(2,.5), rel_widths = c(1, .2))

FINALPLOT
ggsave(filename = "CNS_CORALS.png", height = 6, width = 8, units = "in")


  
  
