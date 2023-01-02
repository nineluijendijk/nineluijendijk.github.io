
devtools::install_github("uashogeschoolutrecht/toolboxr")
library(readxl)
library(tidyverse)
library(toolboxr)
library(patchwork)
library(cowplot)
library(devtools)
library(here)



celegans_data <- read_excel(here("data_raw/CE.LIQ.FLOW.062_Tidydata.xlsx"))

plot_chr <- ggplot(data = celegans_data, aes(x = compConcentration, y = RawData))+
  geom_point(aes(color = compName,
                 shape = expType),
             size = 1)+
  rotate_axis_labels("x", 90)+
  labs(x = "Concentration",
       y = "Number of offspring",
       title = "Number of C. elegans offspring under\na number of circumstances, alphabetical",
       shape = "Type",
       color = "Compound")+
  scale_colour_manual(values = c("red", "darkgoldenrod1", "green", "royalblue3", "violet"))+
  theme(legend.key.size = unit(0.75,"line"),
        legend.text = element_text(size = 8))
plot_chr


#Change data type from chr to dbl
celegans_data$compConcentration <- as.double(celegans_data$compConcentration)

#Create a scatter plot for the concentration in nM
celegans_data_nM <- celegans_data %>% filter(compUnit == "nM")
plot_nM <- ggplot(data = celegans_data_nM, aes(x = log10(compConcentration), y = RawData))+
  geom_jitter(aes(color = compName, #add jitter
                  shape = expType),
              width = 0.5, height = 0.2)+
  labs(x = "log10 concentration (nM)",
       y = "Number of offspring")+
  coord_cartesian(ylim = c(0, 120))+
  scale_shape_manual(values = 3)+
  scale_colour_manual(values = c("red", "darkgoldenrod1", "royalblue3"))+
  theme(legend.position = "none")

#Create a scatter plot for the concentration in pct
celegans_data_pct <- celegans_data %>% filter(compUnit == "pct")
plot_pct <- ggplot(data = celegans_data_pct, aes(x = expType, y = RawData))+
  geom_jitter(aes(color = compName, #add jitter
                  shape = expType),
              size = 1.3,
              width = 0.1, height = 0.075)+
  xlab("Type")+
  coord_cartesian(ylim = c(23.7, 121))+
  scale_colour_manual(values = c("green", "violet"))+
  theme(legend.position = "none",
        axis.text.x=element_text(vjust=0.5, hjust=0.5, size = 8.75),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())+ #remove any y-axis labeling
  rotate_axis_labels("x", 90)

#obtain legend
legend <- get_legend(plot_chr)

#combine figures and legend for readability
ggdraw(plot_grid(plot_grid(plot_nM, plot_pct),
                 plot_grid(NULL, legend, ncol = 3),
                 rel_widths = c(1, 0.35)))+
  plot_annotation("Number of C. elegans offspring under\na number of circumstances")

dummydf <- data.frame(a = rep(c(1, 2, 3), each=2), #create dummy dataframe for the dummy plot
                      b = c(1, 4, 6, 3, 4, 7),
                      Control = rep(c("controlNegative", "controlPositive", "controlVehicleA"), each=2))

dummyplot <- ggplot(dummydf, aes(x=a, y=b, group=Control))+ #create dummy plot to obtain its legend
  geom_line(aes(linetype=Control, color=Control))+
  scale_linetype_manual(values=c("solid", "solid", "longdash"))+
  scale_color_manual(values=c("violet", "green", "green"))

dummylegend <- get_legend(dummyplot) #obtain dummy legend

#Obtain the means of the other 2 control groups
controlPos <- mutated %>% filter(expType == "controlPositive") %>% summarize(mean = mean(normalized, na.rm = TRUE))
controlVeh <- mutated %>% filter(expType == "controlVehicleA") %>% summarize(mean = mean(normalized, na.rm = TRUE))

#Obtain the mean of the RawData (negativeControl)
controlNeg <- celegans_data %>% filter(compName == "S-medium") %>% summarize(mean = mean(RawData, na.rm = TRUE))

#Use the mean to calculate fractions
mutated <- celegans_data %>% filter(RawData > 0) %>% 
  select(RawData, compName, compConcentration, expType) %>% na.omit() %>% 
  mutate(normalized = RawData/controlNeg$mean)

#create normalized plot
plotfraction <- mutated %>% filter(compName == "2,6-diisopropylnaphthalene" | compName == "decane" | compName == "naphthalene") %>%
  ggplot(aes(x = log10(compConcentration), y = normalized))+
  geom_jitter(aes(color = compName), width = 0.5, height = 0.1)+
  scale_colour_manual(values = c("red", "darkgoldenrod1",  "royalblue3"))+
  coord_cartesian(ylim = c(0, 1.5))+
  labs(x = "log10 concentration (nM)",
       y = "Normalized number of offspring",
       title = "Number of C. elegans offspring as a fraction\nof the negative control group",
       color = "Compound")+
  geom_hline(yintercept = 1, color = "violet")+
  geom_hline(yintercept = controlPos$mean, color = "green")+
  geom_hline(yintercept = controlVeh$mean, color = "green", linetype = "longdash")+
  guides(color = "none")+
  facet_wrap(~ compName)

#combine figure and legend
ggdraw(plot_grid(plotfraction, dummylegend,
       rel_widths = c(5, 1)))
