#Installs tidyverse 
install.packages(tidyverse)

#Load tidyverse
library(tidyverse)


#------------------------------------------------------------------------------#
##----Project Section 1----##

#Imports biontech_adolescents.csv file 
BIO_FILE <- read.csv("Desktop/INST314 Project1/biontech_adolescents.csv")

#Getting the data of the vaccine group 
Vaccine_Group <- filter(BIO_FILE, group == "vaccine")

#Sum of the number of the people in the vaccine group 
Vaccine_Group_Total <-sum(Vaccine_Group$outcome == "no COVID-19") + 
  sum(Vaccine_Group$outcome == "COVID-19")

#Sum of the number of the people who tested positive for COVID-19 in the vaccine group
Vaccine_Group_Total_CovidPositive <- sum(Vaccine_Group$outcome == "COVID-19")

#Getting the data of the placebo group
Placebo_Group <- filter(BIO_FILE, group == "placebo")

#Sum of the number of the people in the placebo group
Placebo_Group_Total <-sum(Placebo_Group$outcome == "no COVID-19") +
  sum(Placebo_Group$outcome == "COVID-19")

#Sum of the number of the people who tested positive for COVID-19 in the placebo group
Placebo_Group_Total_CovidPositive <- sum(Placebo_Group$outcome == "COVID-19")

#left-tailed Binomial test
#binom.test(x, n, p, alternative="greater")
#x = number of individuals COVID positive (Vaccine_Group_Total_CovidPositive)
#n = the number of individuals in the group (Vaccine_Group_Total)
#p = probability that a case is a positive COVID test (Vaccine_Group_Total_CovidPositive/Vaccine_Group_Total)

#binom.test(0, number in vaccine group, (18/# in ctrl), alternative = "less")
#binom.test(#covidtreatmentgroup, #peopletreatmentgroup, (#covidplacebo/#peopleplacebo), alternative = "less")
binom.test(Vaccine_Group_Total_CovidPositive, Vaccine_Group_Total, (Placebo_Group_Total_CovidPositive/Placebo_Group_Total), alternative="less")



#------------------------------------------------------------------------------#

##----Project Section 2----##
#Imports cancer_in_dogs.csv file 
CDOG_FILE <- read.csv("Desktop/INST314 Project1/cancer_in_dogs.csv")

###Bar Graph Graphic For Data

##Creating variables to take in the filtered in dataset 

#Getting the 2,4-D dogs with cancer
With_Cancer_24D <- filter(CDOG_FILE, order == "2,4-D", response == "cancer")


#Getting the no 2,4-D dogs with cancer
With_Cancer_No24D <- filter(CDOG_FILE, order == "no 2,4-D", response == "cancer")

#Getting the 2,4-D dogs with no cancer 
No_Cancer_24D <- filter(CDOG_FILE, order == "2,4-D", response == "no cancer")

#Getting the no 2,4-D dogs with no cancer
No_Cancer_No24D <- filter(CDOG_FILE, order == "no 2,4-D", response == "no cancer")

##Creating variables to represent the sum of each category listed below 

#Sum of 2,4-D dogs with cancer
Sum_WithCancer_24D <- sum(With_Cancer_24D$response == "cancer")

#Sum of no 2,4-D dogs with cancer
Sum_WithCancer_No24D <- sum(With_Cancer_No24D$response == "cancer")

#Sum of 2,4-D dogs with no cancer
Sum_NoCancer_24D <- sum(No_Cancer_24D$response == "no cancer")

#Sum of no 2,4-D dogs with no cancer
Sum_NoCancer_No24D <- sum(No_Cancer_No24D$response == "no cancer")

#Data frame for bar graph 
Bar_Dogs_24D_No24D <- data.frame("Type" = c('Cancer 24D', 'No Cancer 24D', 'Cancer No24D', 
                                            'No Cancer No24D'), "Freq" = 
                                   c(Sum_WithCancer_24D, Sum_NoCancer_24D, 
                                     Sum_WithCancer_No24D, Sum_NoCancer_No24D))

#Bar graph representing the data 
ggplot(data = Bar_Dogs_24D_No24D, aes(fill = Type)) +
  geom_bar(mapping = aes(x = Type, y = Freq,), stat = "identity", color = "black") +
  ggtitle("2,4-D and No 2,4-D Dogs With and Without Cancer") + 
  labs(y = "Number of Dogs", x = "Cancer and Control Type") +
  scale_fill_manual("Legend", values = c("Cancer 24D" = "blue", "Cancer No24D" = "red", 
                                         "No Cancer 24D" = "blue", "No Cancer No24D" = "red")) +
  geom_text(aes(label=Freq, x = Type, y = Freq), vjust=1.6, color="white", size=3.5) 
#-------------------------------------------------------------------------------#

###Pie Chart Graphic For Data

##Pie Chart 1 for 2,4-D Dogs with Cancer and without Cancer

#Calculating percentage of 2,4-D Dogs with Cancer: Round function used
Perc_Cancer_24D <-  round((Sum_WithCancer_24D/(Sum_WithCancer_24D + Sum_NoCancer_24D))*100, 2)

#Calculating percentage of 2,4-D Dogs with no Cancer: Round function used
Perc_NoCancer_24D <- round((Sum_NoCancer_24D/(Sum_WithCancer_24D + Sum_NoCancer_24D))*100, 2)

#Data frame for pie chart 1
Pi_Dogs_24D_No24D <- data.frame("Type" = c('Cancer', 'No Cancer'), "Freq" = c(Perc_Cancer_24D, Perc_NoCancer_24D))


#Pie chart 1
library(ggplot2)
ggplot(Pi_Dogs_24D_No24D, aes(x = "", y = Freq, fill = Type)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("2,4-D Dogs With and Without Cancer") + 
  geom_text(aes(label = paste0(Freq, "%\n", Type)), position = position_stack(vjust = 0.5),color="white") + 
  scale_fill_manual("Legend", values = c("red", "purple")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(), 
        axis.title = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(fill = "skyblue", colour = "black")) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))



#------------------------------------------------------------------------------#

##Pie Chart 2 for No 2,4-D Dogs with Cancer and without Cancer

#Calculating percentage of No 2,4-D Dogs with Cancer: Round function used 
Perc_Cancer_No24D <-  round((Sum_WithCancer_No24D/(Sum_WithCancer_No24D + Sum_NoCancer_No24D))*100, 2)


#Calculating percentage of No 2,4-D Dogs with no Cancer: Round function used
Perc_NoCancer_No24D <- round((Sum_NoCancer_No24D/(Sum_WithCancer_No24D + Sum_NoCancer_No24D))*100, 2)

#Data frame for pie chart 2
Pi2_Dogs_24D_No24D <- data.frame("Type" = c('Cancer', 'No Cancer'), "Freq" = c(Perc_Cancer_No24D, Perc_NoCancer_No24D))


#Pie chart 2
library(ggplot2)
ggplot(Pi2_Dogs_24D_No24D, aes(x = "", y = Freq, fill = Type)) +
  geom_bar(stat = "identity", color = "black", width = 1) +
  coord_polar("y", start = 0) +
  ggtitle("No 2,4-D Dogs With and Without Cancer") + 
  geom_text(aes(label = paste0(Freq, "%\n", Type)), position = position_stack(vjust = 0.5),color="white") + 
  scale_fill_manual("Legend", values = c("red", "purple")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(), 
        axis.title = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(fill = "skyblue", colour = "black")) + 
  theme(panel.background = element_rect(fill = 'white', colour = 'black'))













