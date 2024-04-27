df_original = read.csv("FoodDataSet.csv")

install.packages("ggplot2")
install.packages("dplyr")
library("ggplot2")
library("dplyr")


summary(df_original) # no issue with Age data as statistical operations returned with value, not NA
str(df_original) # identify structure
dim(df_original) # identify dimensions
head(df_original) 
tail(df_original)


# check for typos, errors, inconsistencies
unique(df_original$Gender)
unique(df_original$Nationality)
unique(df_original$Food)
unique(df_original$Juice)
unique(df_original$Dessert)
colSums(is.na(df_original)) # to check for missing values
df_original[df_original == ""] # to check for blank spaces

# creating temporary data.frame for cleaning data
df_clean = data.frame(df_original)
df_clean[df_clean == ""] = NA # assign blank spaces with real NA value 
df_clean = unique(df_clean) # to check if there are any duplicated rows
colnames(df_clean)[7] = "Beverage" # rename Juice column

# replacing typos and standardising data in Nationality column
df_clean$Nationality[df_clean$Nationality == "China"] = "Chinese"
df_clean$Nationality[df_clean$Nationality == "Indonesain"] = "Indonesian"
df_clean$Nationality[df_clean$Nationality == "Indonesia"] = "Indonesian"
df_clean$Nationality[df_clean$Nationality == "Indonesian "] = "Indonesian"
df_clean$Nationality[df_clean$Nationality == "Japan"] = "Japanese"
df_clean$Nationality[df_clean$Nationality == "Malaysia"] = "Malaysian"
df_clean$Nationality[df_clean$Nationality == "Malaysia "] = "Malaysian"
df_clean$Nationality[df_clean$Nationality == "Malaysian "] = "Malaysian"
df_clean$Nationality[df_clean$Nationality == "MALAYSIAN"] = "Malaysian"
df_clean$Nationality[df_clean$Nationality == "Maldivian "] = "Maldivian"
df_clean$Nationality[df_clean$Nationality == "MY"] = "Malaysian"
df_clean$Nationality[df_clean$Nationality == "Pakistan"] = "Pakistani"
df_clean$Nationality[df_clean$Nationality == "Pakistani "] = "Pakistani"
df_clean$Nationality[df_clean$Nationality == "Muslim"] = NA # muslim is not a nationality so NA
df_clean$Nationality[df_clean$Nationality == "Nigerian "] = "Nigerian"
df_clean$Nationality[df_clean$Nationality == "Algerian "] = "Algerian"
df_clean$Nationality[df_clean$Nationality == "Korean "] = "Korean"

# standardise data in Food column
df_clean$Food[df_clean$Food == "Traditional food"] = "Traditional Food"
# standardise data in Beverage column
df_clean$Beverage[df_clean$Beverage == "Carbonated drinks"] = "Carbonated Drinks"
colSums(is.na(df_clean)) # to check after NA validation above


# creating data.frame with only NA value
df_NA = df_clean[rowSums(is.na(df_clean)) > 0, ]
summary(df_NA)

# creating data.frame with only non-NA value
df_nonNA = na.omit(df_clean) 
summary(df_nonNA)
unique(df_nonNA$Gender)
unique(df_nonNA$Nationality)
unique(df_nonNA$Food)
unique(df_nonNA$Beverage)
unique(df_nonNA$Dessert)


# 1 exploring relationship between Age and Gender
df_age_gender = df_nonNA[c("Age", "Gender")]

# creating Age Group to categorise Age data
min(df_age_gender[,1])
max(df_age_gender[,1])
mean(df_age_gender[,1])
median(df_age_gender[,1])
sd(df_age_gender$Age)
breaks = c(-Inf
            , 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85
            , Inf)
labels = c("0-4"
           ,"9 and below","10-14","15-19","20-24"
           ,"25-29","30-34","35-39","40-44"
           ,"45-49","50-54","55-59","60-64"
           ,"65-69","70-74","75-79","80-84"
           ,"85+")
df_age_gender$Age_Group = cut(df_age_gender$Age, breaks, labels, right = FALSE)


# Stacked bar chart for particpant distribution, by age group and gender
agedist_chart = ggplot(df_age_gender, aes(x = Age_Group, fill = Gender)) +
                geom_bar(position = "dodge") +
                labs(x = "Age Group", y = "Number of Participants", title = "Distribution of Survey Participants, by Age and Gender") +
                scale_fill_manual(values = c("pink", "lightblue")) +
                theme_classic()
agedist_chart = agedist_chart + coord_flip()
agedist_chart


# 2 exploring relationship between Nationality and Food
df_nationality_food = table(df_nonNA$Nationality, df_nonNA$Food) %>%
  as.data.frame()
colnames(df_nationality_food)[1] = "Nationality"
colnames(df_nationality_food)[2] = "Food"

# convert Nationality variable to character
df_nationality_food$Nationality = as.character(df_nationality_food$Nationality)

# convert non-Indian into Others
df_nationality_food$Nationality[df_nationality_food$Nationality != as.character("Indian")] = "Others"

# convert Nationality variable to factor
df_nationality_food$Nationality = as.factor(df_nationality_food$Nationality)

# total up Western Food and Traditional Food for Indians and Others
df_nationality_food = aggregate(Freq ~ Nationality + Food, df_nationality_food, sum)

# split Western Food and Traditional Food into separate data frames
df_nationality_tfood = df_nationality_food[1:2,]
df_nationality_wfood = df_nationality_food[3:4,]

# calculate proportion for Western Food and Traditional Food
tfood_prop = df_nationality_tfood$Freq / sum(df_nationality_tfood$Freq)
wfood_prop = df_nationality_wfood$Freq / sum(df_nationality_wfood$Freq)

# combine proportion to previous data.frames
df_nationality_tfood = cbind(df_nationality_tfood, tfood_prop)
colnames(df_nationality_tfood)[4] = "Proportion"
df_nationality_wfood = cbind(df_nationality_wfood, wfood_prop)
colnames(df_nationality_wfood)[4] = "Proportion"

# creating a chart to illustrate relationship between Nationality and Food
# Pie chart for Nationality and Traditional Food 
preftf_chart = ggplot(df_nationality_tfood, aes(x="", y=Proportion, fill=Nationality)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                theme_void() +
                labs(fill = "Nationality",
                     x = NULL,
                     y = NULL,
                     title = "Preference for Traditional Food, by Nationality") + 
                geom_text(aes(label = paste(round(Proportion*100),"%", sep = "")),
                              position = position_stack(vjust = 0.5),
                              color = "white", size=10) +
                              scale_fill_brewer(palette="Set1")
preftf_chart

# Pie chart for Nationality and Western Food
prefwf_chart = ggplot(df_nationality_wfood, aes(x="", y=Proportion, fill=Nationality)) +
                geom_bar(stat="identity", width=1, color="white") +
                coord_polar("y", start=0) +
                theme_void() +
                labs(fill = "Nationality",
                     x = NULL,
                     y = NULL,
                     title = "Preference for Western Food, by Nationality") + 
                geom_text(aes(label = paste(round(Proportion*100),"%", sep = "")),
                          position = position_stack(vjust = 0.5),
                          color = "white", size=10) +
                scale_fill_brewer(palette="Dark2")
prefwf_chart


# 3 exploring relationship between Gender and Dessert
df_gender_dessert = df_nonNA[, 3:length(df_nonNA)]
df_gender_dessert = df_gender_dessert[, -2:-5]
table_gender_dessert = table(df_gender_dessert$Gender, df_gender_dessert$Dessert)

# Grouped bar plot for Gender and Dessert 
df_gender_dessert$Dessert = factor(df_gender_dessert$Dessert, 
                                   levels = c("Yes", "Maybe", "No"))

prefdessert_hist = ggplot(df_gender_dessert, 
                          aes(x = Gender, fill = Dessert)) +
                          geom_bar(position = "dodge") +
                          labs(fill = "Preference", x = "Gender", y = "Frequency", 
                               title = "Preference for Dessert After Food and Beverage, by Gender") +
                          scale_fill_manual(values = c("#008450", "#EFB700", "#B81D13")) +
                          theme_classic()
prefdessert_hist
