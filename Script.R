
# By: Motte Vishwanatha Sanjana and Habiya Sana Kausar
# Project title: Tobacco-associated Cancers

#--------------------------Please restart the R Studio before executing the code--------------------------------------------------

cat("\f")  #clear console
rm(list=ls()) # clear the environment

# ----------------Please install all these Packages in R studio and run below code.------------------------------------------
library(ggplot2)
library(dplyr)
library(sf)
library(plotly)
library(usmap)
library(dplyr)

#-------------------Please replace the working directory path to the location where you saved file in 23 line within the ""----------------

#-----------------------NOTE: Do not include the data folder in your path-------------------------------------

# set the working directory by giving path name.
setwd("/Users/sanjanavishwanath/Desktop/data_cmap_project")

#-----------------------------------------------Do not touch the below code----------------------------------------------------------
print(getwd())

#--------------------------Cancer Type hover mouse----------------------------------------
tobacco_file_by_cancerType = read.csv("data/cancer_dataset_by_cancertype.csv", header = TRUE, sep = ",")

cancer_type_count_label <- tobacco_file_by_cancerType$Case.Count
cancer_type_label <- tobacco_file_by_cancerType$Cancer.Type

cancer_type_df<-data.frame(cancer_type_count_label, cancer_type_label)

cancer_type_plot <- ggplot(cancer_type_df, aes(x = cancer_type_count_label, y = cancer_type_label)) +
  geom_point(alpha = (1/2)) + scale_x_log10()

ggplotly(cancer_type_plot)

#-----------------------------------Cancer age--------------------------------------------
tobacco_file_by_age = read.csv("data/cancer_dataset_by_age.csv", header = TRUE,  sep = ",")

# pie chart for cancer age!
age_count_label <-  tobacco_file_by_age$Case.Count
age_byage_label <-  tobacco_file_by_age$Age

piepercent<- round(100*age_count_label/sum(age_count_label), 1)

# Plot the chart.
pie(age_count_label, labels = piepercent, main = "Rate of New Tobacco-associated Cancers By Age Group (years)",col = rainbow(length(age_count_label)))
legend("topright", c(age_byage_label), cex = 0.8,
       fill = rainbow(length(age_count_label)))


#--------------------------------------By State------------------------------------------------------

#--------Hover mouse code for state--------------------------------------

tobacco_file_by_state = read.csv("data/cancer_dataset_by_states.csv", header = TRUE, sep = ",")


fig <- plot_ly() 
fig <- fig %>%
  add_trace(
    type = "pie",
    name = "",
    values = tobacco_file_by_state$Case.Count,
    labels = tobacco_file_by_state$Area,
    hovertemplate = "%{label}: <br>Case Count: %{percent}")

fig

#---------------------------state diagram code-----------------------------
state_map = statepop #inbuild package for us states
stateMap_withoutState <-tobacco_file_by_state %>% filter(Area!="Puerto Rico" & Area!= "Indiana" & Area!= "Nevada")



#Applying geo spacial layer
stateMap_caseCount<- left_join(state_map, stateMap_withoutState, by=c("full" = "Area"))
stateMap_caseCount<- stateMap_caseCount %>% filter(full!= "Indiana" & full!= "Nevada")

plot_usmap(data = stateMap_caseCount, values = "Case.Count", labels = TRUE) + theme(legend.position = "right")

#--------------------------------------By Gender---------------------------------------------------------

tobacco_file_by_female = read.csv("data/FemaleCount.csv")
tobacco_file_by_men = read.csv("data/MaleCount.csv")

tobacco_file_by_female <- tobacco_file_by_female %>% rename(female.case.count = Case.Count)
tobacco_file_by_men <- tobacco_file_by_men %>% rename(male.case.count = Case.Count)

gender_analysis <- left_join(tobacco_file_by_female, tobacco_file_by_men, by=c("Cancer.Type"))

gender_frame <- data.frame(cancer_type = gender_analysis$Cancer.Type, female_count = gender_analysis$female.case.count, male_count = gender_analysis$male.case.count)

#Female data analysis
female_count<-ggplot(data=gender_frame, aes(x=cancer_type, y=female_count)) +
  geom_bar(stat="identity", fill = "pink")
female_count + coord_flip()

#Male data analysis
male_count<-ggplot(data=gender_frame, aes(x=cancer_type, y=male_count)) +
  geom_bar(stat="identity", fill = "skyblue")
male_count + coord_flip()

#NOTE: please increase the size of plots by dragging if you didn't saw count properly..........

#-------------------------------------together top cancers analysis for female and male-------------------------------------

#Regression analysis for both female and male
survey <- rbind(gender_analysis$female.case.count, gender_analysis$male.case.count)
barplot(survey,col = c("pink", "skyblue1"), names.arg = gender_analysis$Cancer.Type, main = "Rate of Tobacco-associated Cancers By Smoker Analysis",
        xlab = "Cancer Type", 
        ylab = "Case Count (Female, Male)")

#NOTE: please increase the size of plot by dragging if you didn't see all cancers name........ 

# ignore the warnings in case if you get any. Because it depends on the package Compatibility...

#------------------------------------------------do not touch the above code----------------------------------------------------