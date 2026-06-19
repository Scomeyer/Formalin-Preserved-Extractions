setwd("C:/Users/smeyer/OneDrive - California Department of Water Resources/Scott/Projects/20mm/Processing/Analysis")

library(ggplot2)
library(tidyr)
library(readxl)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)


# Wakasagi --------------------------------------------------------------------
#Read in your files, make sure you have the right ones selected
sheet <- "260612_Wakasagi.xls"
data <- read_xls(sheet, sheet = "Multicomponent Data", skip = 46)
samples <- read_xls(sheet, sheet = "Sample Setup", skip=46)
key <- read_xls(sheet, sheet = "Key")
results <- read_xls(sheet, sheet = "Results", skip=46)

formalin <- read_csv("formalin_fish.csv")

all_data <- inner_join(data, select(samples, 'Sample Name', 'Well'), by="Well")

all_data <- inner_join(all_data, select(key, 'Well', 'Assay', 'Treatment'), by="Well")

all_data <- inner_join(all_data, select(results, 'Well', 'CT'), by='Well')

ggplot(all_data, aes(x=as.numeric(Cycle), y=CY5)) + 
  geom_point() +
  facet_wrap(~Treatment)


ggplot(all_data, aes(x=`Sample Name`, y=CT)) + 
  geom_point() +
  facet_wrap(~Treatment)

all_data_F <- inner_join(all_data, select(formalin, 'Sample Name', '# of Days in Formalin', 'Fish Size'), by='Sample Name')

ggplot(filter(all_data_F, Treatment!="Kits"), aes(x=`Sample Name`, y=CT, colour = Treatment)) + 
  geom_point() 

ggplot(filter(all_data_F, CT>35, Treatment=="20mm"), aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment+`# of Days in Formalin`)

ggplot(filter(all_data_F, CT>30, Treatment=="20mm"), aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment+`Fish Size`)





# Delta Smelt -------------------------------------------------------------

#Read in your files, make sure you have the right ones selected
sheet <- "260612_Delta_Smelt.xls"
data <- read_xls(sheet, sheet = "Multicomponent Data", skip = 46)
samples <- read_xls(sheet, sheet = "Sample Setup", skip=46)
key <- read_xls(sheet, sheet = "Key")
results <- read_xls(sheet, sheet = "Results", skip=46)

all_data <- inner_join(data, select(samples, 'Sample Name', 'Well'), by="Well")

all_data <- inner_join(all_data, select(key, 'Well', 'Assay', 'Treatment', 
                                        'Kits', 'Formalin'), by="Well")

all_data <- inner_join(all_data, select(results, 'Well', 'CT', 'Quantity'), by="Well")

ggplot(all_data, aes(x=as.numeric(Cycle), y=FAM)) + 
  geom_point() +
  facet_wrap(~Treatment)

ggplot(filter(all_data, Kits=="Controls"), aes(x=as.numeric(Cycle), y=FAM,colour=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~`Sample Name`)

ggplot(all_data, aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment) +
  theme(legend.position = "none")

ggplot(filter(all_data, CT>30, Treatment=="Kits"), aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment)

ggplot(all_data, aes(x=`Sample Name`, y=CT, colour=Kits)) + 
  geom_point() +
  facet_wrap(~Formalin)

ggplot(all_data, aes(x=`Sample Name`, y=Quantity, colour=Kits)) + 
  geom_point() +
  facet_wrap(~Formalin)


ggplot(filter(all_data, Quantity<100000), aes(x=`Sample Name`, y=Quantity, colour=Kits)) + 
  geom_point() +
  facet_wrap(~Formalin)


# Rerun -------------------------------------------------------------------
sheet <- "260616_reruns.xls"
data <- read_xls(sheet, sheet = "Multicomponent Data", skip = 46)
samples <- read_xls(sheet, sheet = "Sample Setup", skip=46)
key <- read_xls(sheet, sheet = "Key")
results <- read_xls(sheet, sheet = "Results", skip=46)

all_data <- inner_join(data, select(samples, 'Sample Name', 'Well'), by="Well")

all_data <- inner_join(all_data, select(key, 'Well', 'Assay', 'Treatment'), by="Well")

all_data <- inner_join(all_data, select(results, 'Well', 'CT'), by='Well')

ggplot(all_data, aes(x=as.numeric(Cycle), y=CY5)) + 
  geom_point() +
  facet_wrap(~Treatment)


ggplot(all_data, aes(x=`Sample Name`, y=CT, colour = Assay)) + 
  geom_point() +
  facet_wrap(~Treatment)

all_data_F <- inner_join(all_data, select(formalin, 'Sample Name', '# of Days in Formalin', 'Fish Size'), by='Sample Name')

ggplot(filter(all_data_F, Treatment!="Kits"), aes(x=`Sample Name`, y=CT, colour = Treatment)) + 
  geom_point() 

ggplot(filter(all_data_F, CT>35, Treatment=="20mm"), aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment+`# of Days in Formalin`)

ggplot(filter(all_data_F, CT>30, Treatment=="20mm"), aes(x=`Sample Name`, y=CT, color=`Sample Name`)) + 
  geom_point() +
  facet_wrap(~Treatment+`Fish Size`)



