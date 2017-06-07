library(dplyr)

# 0: load the data
mydata = read.csv("titanic3.csv", header=TRUE) 

# 1: Port of embarkation
# The embarked column has some missing values.
# Find the missing values and replace them with S.
x<- which(mydata$embarked =="")
mydata$embarked[x] <- "S"

# Check whether row 169 and row 285 are assinged to "S"
# mydata$embarked[169]
# mydata$embarked[285]

# Check whether embarked still has missing values
# which(mydata$embarked =="")
# is.na(mydata$embarked) 

# 2: Age
# 2-1: Calculate the mean of the Age column and use that value to populate(fill) the missing values
avg_age<-mean(mydata$age, na.rm = TRUE)
x<- is.na(mydata$age)
mydata$age[x] <-avg_age

# Another approach:
# for (i in seq_along(mydata$age)) {
#     if (is.na(mydata$age[i])){
#         mydata$age[i] <- avg_age 
#     }
# }


# 2-2:Think about other ways you could have populated the missing values in the age column. 
# Why would you pick any of those over the mean (or not)?

# load original data
oridata = read.csv("titanic3.csv", header=TRUE) 
mean(oridata$age, na.rm = TRUE)
median(oridata$age,na.rm = TRUE)
hist(oridata$age)

# Answere:
# I would use mean to fill the missing data in age column.
# Before populating NA in age, the median and mean of age are similar.
# Also, the distribution of age indicate the mean is around 30 with relatively normal distribution. 
# Thus, I think the mean of age can be used to represent the NA in age.

# 3: Lifeboat
# In boat column, fill empty slots with a dummy value (e.g. the string 'None' or 'NA')

for (i in seq_along(mydata$boat)) {
     if (mydata$boat[i]==""){
         mydata$boat[i] <- as.character(NA) 
     }
 }

# mydata$boat[i] <- "NA" 
# Warning messages:
# 1: In `[<-.factor`(`*tmp*`, i, value = structure(c(13L, 4L,  ... : invalid factor level, NA generated


# 4: Cabin
# You notice that many passengers don't have a cabin number associated with them.

# Q: Does it make sense to fill missing cabin numbers with a value?
# ANS: Yes, this might help us to analysis the relationship between cabin and other columns.

# Q: What does a missing value here mean?
# ANS: Maybe, it means that person did not have a room

# You have a hunch that the fact that the cabin number is missing might be a useful indicator of survival. 
# Create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise.

mydata<- mutate(mydata,has_cabin_number = ifelse(mydata$cabin=="",0,1))


# Check if people who had no room were likely not to survive
mydata %>% 
  group_by(has_cabin_number) %>% 
  summarise_each(funs(mean),survived)

# 5: Wrtie the cleaned file in csv
write.csv(mydata, "cleaned_data.csv")
