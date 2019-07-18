# load rat complaint and baiting data-------------------------------------------

complaints <- read.csv("./Data/ratComplaintsResponses.csv", header = TRUE, 
                       na.strings = c(""))

# data formatting---------------------------------------------------------------

names(complaints)[1] <- "Creation.Date"

# remove commas
complaints[, 'Number.of.Premises.Baited'] <- gsub(",","", complaints[, 'Number.of.Premises.Baited']) 
complaints[, 'Number.of.Premises.with.Garbage'] <- gsub(",","", complaints[, 'Number.of.Premises.with.Garbage']) 
complaints[, 'Number.of.Premises.with.Rats'] <- gsub(",","", complaints[, 'Number.of.Premises.with.Rats']) 

# change from character to numeric
complaints$Number.of.Premises.Baited <- as.numeric(complaints$Number.of.Premises.Baited)
complaints$Number.of.Premises.with.Garbage <- as.numeric(complaints$Number.of.Premises.with.Garbage)
complaints$Number.of.Premises.with.Rats <- as.numeric(complaints$Number.of.Premises.with.Rats)

# restrict to completed complaints 
complaints <- complaints %>%
  filter(Status == "Completed") 

# remove complaints with an unreasonably high number of premises baited
complaints <- complaints %>%
  filter(Number.of.Premises.Baited <= 100) 

# exclude NAs
complaints <- complaints[complete.cases(complaints), ]

# get rid of unneeded columns
complaints <- complaints[, -c(2, 4, 5, 9, 20:25)]


#write.csv(complaints, "./Data/cleanedComplaints.csv")

