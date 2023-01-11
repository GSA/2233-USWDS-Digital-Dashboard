library(tidyverse)
library(readxl)

dataPath <- "G://Shared drives//MSG Projects//1.0 Real Estate Solutions (LQ1)//2213 Equipping GSA Workforce at Home//03. Data Collection//"
pulseName <- "IT_survey_merge_export.csv"
ITName <- "EoW_Survey_Draft (Responses).xlsx"

# do a bit of analysis on survey responses alone
formResponses <- read_excel(paste0(dataPath, ITName), sheet = "Form Responses 1")
formQuestionLabels <- names(formResponses)

names(formResponses) <- c("timestamp",
  "email",
  "sat_timeToReceive",
  "sat_instructionQuality",
  "sat_ITSupport",
  "sat_equipment",
  "easy_order",
  "easy_receive",
  "easy_understandComms",
  "easy_setUp",
  "additionalFeedback",
  "haveResources",
  "satisfactionEquipment",
  "jobWithBetterPay",
  "jobWithBetterFlexibility",
  "recommendOrg")

formResponsesLong <- formResponses %>%
  pivot_longer(!c(timestamp, email), names_to="question", values_to="response") %>%
  count(question, response)

easyPlot <- ggplot(formResponsesLong %>% filter(question %in% c("jobWithBetterPay", "jobWithBetterFlexibility")), 
	aes(x=question, y=n, fill=response)) + 
  geom_bar(position="fill", stat="identity") +
  coord_flip() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

requestedEquipment <- read_excel(paste0(dataPath, ITName), sheet = "Requested Equipment")



df <- read.csv(paste0(dataPath, pulseName))

nrow(df %>% filter(equipment_request_timestamp!=""))