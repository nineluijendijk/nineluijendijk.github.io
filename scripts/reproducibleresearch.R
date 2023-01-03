library(tidyverse)

dataframe_criteria <- data.frame(TransparencyCriteria = c("Study Purpose", "Data Availability Statement", "Data Location", "Study Location", "Author Review", "Ethics Statement", "Funding Statement", "Code Availability"),
           Definition = c("A concise statement in the introduction of the article, often in the last paragraph, that establishes the reason the research was conducted. Also called the studyobjective.", "A statement, in an individual section offset from the main body of text, that explains how or if one can access a study’s data. The title of the section may vary, but it must explicitly mention data; it is therefore distinct from a supplementary materials section.", "Where the article’s data can be accessed, either raw or processed.", "Author has stated in the methods section where the study took place or the data’s country/region of origin.", "The professionalism of the contact information that the author has provided in the manuscript.", "A statement within the manuscript indicating any ethical concerns, including the presence of sensitive data.", "A statement within the manuscript indicating whether or not the authors received funding for their research.", "Authors have shared access to the most updated code that they used in their study, including code used for analysis."),
           ResponseType = c("Binary", "Binary", "Found Value", "Binary; Found Value", "Found Value", "Binary", "Binary", "Binary"))

criteria_scored <- dataframe_criteria %>% mutate(Score = c("Present", "Present", "Present, the data can be found at https://osf.io/4c7vr/.", "Present, the data was collected from US-based participants.", "One of four authors' contact information is listed: the address of the university they're from and their professional email address.", "Present", "Present", "Present"))

#----

library(tidyverse)
library(knitr)
library(psych)

qualtrics1 <- read_tsv("data/study1/cleandata/qualtrics1.tsv")
lqualtrics1 <- read_tsv("data/study1/cleandata/lqualtrics1.tsv")

qualtrics1 %>% 
  select(PVD8, PVD12r, PVD2, PVD14r, PVD10, PVD5r, PVD6) %>% 
  omega(title = "Perceived Infectability")

lqualtrics1 %>% 
  select(certainty, clarity, pvd_pinfect, pvd_germ, rchild_uncertain, rchild_ses, recentill) %>% 
  pairs.panels(scale = FALSE, pch = ".")


library(tidyverse)
library(knitr)
library(psych)
library(here)
library(GPArotation)

#Read data from preparation script
qualtrics1 <- read_tsv(here("data_raw/qualtrics1_share.tsv"))
lqualtrics1 <- read_tsv(here("data_raw/lqualtrics1_share.tsv"))

qualtrics1 %>% 
  select(PVD8, PVD12r, PVD2, PVD14r, PVD10, PVD5r, PVD6) %>% 
  omega(title = "Perceived Infectability")

lqualtrics1 %>% 
  select(certainty, clarity, pvd_pinfect, pvd_germ, rchild_uncertain, rchild_ses, recentill) %>% 
  pairs.panels(scale = FALSE, pch = ".") %>% title(main = "Scatterplot Matrix (ignoring stimuli variance)
", line = 1.5)

?pairs.panels
