#Esme Middaugh
#STATS 670
#Mini Project 2


library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
library(ade4)
library(magrittr)
library(tidyverse)
library(broom)
library(ggplot2)
#install.packages("ggthemes") 
library(ggthemes)
library(viridis)
#install.packages("ggfortify")
library(ggfortify)


#Question 1: Polarization in Two Years
#1989 and 2014
#Starting with code from Lecture 18 

vote_descriptions = read.csv("bills.csv")
## doing this to make the vote_id column in bills match the column names in votes
vote_descriptions = vote_descriptions %>% mutate(vote_id = str_replace(vote_id, "-", "."))

votes_1989 = read_csv('congress/1989/votes.csv')
member_1989 = read_csv('congress/1989/members.csv')
votes_2014 = read_csv('congress/2014/votes.csv')
member_2014 = read_csv('congress/2014/members.csv')
all_1989 = join(member_1989, votes_1989, by="id")
all_2014 = join(member_2014, votes_2014, by="id")
votes_1989 = all_1989[,c(-1, -2, -3, -4, -5, -6)]
members_1989 = all_1989[,1:6]

get_votes_members = function(year) {
  #x = path to a csv file
  votes = read.csv(paste('congress/', toString(year), '/votes.csv', sep=""))
  members = read.csv(paste('congress/', toString(year), '/members.csv', sep=""))
  #Joining and splitting to work in later functions and sensure same order 
  
  joined = join(members, votes, by = "id")
  votes = joined[,c(-1, -2, -3, -4, -5, -6)]
  members = joined[,1:6]
  year_data = list("votes" = votes, "members" = members)
  return (year_data)
}
votes_1989 = get_votes_members(1989)$votes
members_1989 = get_votes_members(1989)$members
votes_2014 = get_votes_members(2014)$votes
members_2014 = get_votes_members(2014)$members

#Recoding Yea to 1, Nay to -1 
recode = function(x) {
  if(is.na(x)) {
    return(0)
  } else if(x == "Yea") {
    return(1)
  } else if(x == "Nay") {
    return(-1)
  } else {
    return(0)
  }
}
#Recoding and Taking PCA for 1989 and 2014
votes_1989r = apply(votes_1989, 1:2, recode) #Recoding 
prcomp_1989 = prcomp(votes_1989r, scale. = FALSE) #Taking PCA

votes_2014r = apply(votes_2014, 1:2, recode) #Recoding 
prcomp_2014 = prcomp(votes_2014r, scale. = FALSE) #Taking PCA


#Instead of using ggbiplot, using autoplot and ggfortify
#https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

#Figure 1 
autoplot(prcomp_1989, data = members_1989, 
        colour = "party", size=3, alpha=.6) +
        scale_color_manual(values = c("darkblue","firebrick")) + #Referenced https://stackoverflow.com/questions/33640492/pca-plots-in-ggplot2-changing-point-colors-and-changing-the-color-of-frame-elli
        ggtitle("PCA of 1989 Senate Votes") +
        theme_hc() 

#Figure 2 
autoplot(prcomp_2014, data = members_2014, 
         colour = "party", size=3, alpha=.6) +
  scale_color_manual(values = c("darkblue","purple","firebrick")) + #Referenced https://stackoverflow.com/questions/33640492/pca-plots-in-ggplot2-changing-point-colors-and-changing-the-color-of-frame-elli
  ggtitle("PCA of 2014 Senate Votes") +
  theme_hc() 



all_files = list.files(path="congress", pattern = ".csv$", recursive = TRUE)
all_files[1]

#############################################################################
#Question 2: Polarization Over Time 

## Modified from Lecture 21 R Code
#for (my_year in 1989:2014) {
#  v = paste("votes_", my_year, sep="") #Create new variable names based on year
#  #m = paste("members_", my_year, sep="")
#  votes= get_votes_members(my_year)$votes
#  #members= get_votes_members(my_year)$members
#  #assign(v, votes)
#  #assign(m, members)
#}

years = seq(1989, 2014, 1)
n = length(years)
variance_by_year = data.frame(year = rep(NA, n), PC1 = rep(NA, n))

for (J in 1:n) {
  my_year = years[J]
  votes= get_votes_members(my_year)$votes
  votes_r = apply(votes, 1:2, recode) #Recoding 
  votes_pca= prcomp(votes_r, scale. = FALSE) #Taking PCA
  #Filling with year and variance of PC1
  variance_by_year[J, ] = c(my_year, cumsum(votes_pca$sdev^2 / sum(votes_pca$sdev^2))[1])
}

##Figure 3 
##Time to graph variance by year 
ggplot(data = variance_by_year, aes(x=year, y=PC1, color=PC1)) + 
  theme_minimal() + 
  geom_point() + 
  xlab('Year') + 
  ylab('PC1 Variance %') + 
  ggtitle('Variance in Congressional Voting Records Accounted for By PC1') +
  scale_color_viridis() +
  theme_minimal()

#For MDS Remember to Check Eigen Values 



###########################################################################
#Question #3 Ideological Position of One Senator 
## Take the previous graphs and overlay the two different senators on them 


##### Tried but didn't work 

#https://stackoverflow.com/questions/14351608/color-one-point-and-add-an-annotation-in-ggplot2
# create the subset
#kennedy = filter(members_1989, id == "S055")
#mcconnell = filter(members_1989, id =="S174")

# plot the data
#ggplot(a, aes(log10(Index1+1), Index2)) + geom_point(alpha=1/5) +  # this is the base plot
#  geom_point(data=g1, colour="red") +  # this adds a red point
#  geom_text(data=g1, label="G1", vjust=1) # this adds a label for the red point


#Function for coding Kennedy or McConnel for graphing 
#kmify = function(x) {
#  x =  x %>% 
#  mutate(color_id = ifelse(id=="S055", "Kennedy",
#                           ifelse(id=="S174", "McConnell",
#                                  ifelse(party=="D", "Democrat",
#                                         "Republican"))))
#  return(x)
#  }
  

#members_1989 = kmify(members_1989)                                                                                                       "Republican"))))
#members_2008 = kmify(members_2008)

#Graph of 1989
#autoplot(prcomp_1989, data = members_1989, 
#         colour = "color_id", size=3, alpha=.6) +
#  scale_color_manual(values = c("darkblue", "green", "orange", "firebrick")) + #Referenced https://stackoverflow.com/questions/33640492/pca-plots-in-ggplot2-changing-point-colors-and-changing-the-color-of-frame-elli
#  ggtitle("PCA of 1989 Senate Votes") +
#  theme_hc()

#Graph for 2008
#votes_2008r = apply(votes_2008, 1:2, recode) #Recoding 
#prcomp_2008 = prcomp(votes_2008r, scale. = FALSE) #Taking PCA

#autoplot(prcomp_2008, data = members_2008, 
#         colour = "color_id", size=3, alpha=.6) +
#  scale_color_manual(values = c("darkblue", "green", "orange", "firebrick")) + #Referenced https://stackoverflow.com/questions/33640492/pca-plots-in-ggplot2-changing-point-colors-and-changing-the-color-of-frame-elli
#  ggtitle("PCA of 2008 Senate Votes") +
#  theme_hc()

############################################
### Grosss-gross code coming up 
#Calculating distance of both Kennedy and McConnel from Party 
years = seq(1989, 2008, 1)
n = length(years)
kennedy_distance_by_year = data.frame(year = rep(NA, n), 
                                      dems_avg = rep(NA, n), 
                                      kennedy = rep(NA, n), 
                                      dist = rep(NA, n))
mcconnell_distance_by_year = data.frame(year = rep(NA, n), 
                                       reps_avg = rep(NA, n), 
                                       mcconnell = rep(NA, n), 
                                       dist  = rep(NA, n))

for (J in 1:n) {
  my_year = years[J]
  votes= get_votes_members(my_year)$votes
  members= get_votes_members(my_year)$members #All joining and splitting taken care of 
  
  #Recoding and applying prcomp (I really should have made a function combining these two...)
  votes_r = apply(votes, 1:2, recode) #Recoding 
  votes_pca= prcomp(votes_r, scale. = FALSE) #Taking PCA
  
  #Turning the prcomp x coordinates into a data frame and just taking PC1
  votes_PC1 = as.data.frame(votes_pca$x)$PC1
  #Merging the two 
  members$PC1 = votes_PC1
  
  #Filtering for Dems and Republicans to Create new
  dems = members %>% # Democrats
    filter(party=="D")
  
  reps = members%>% #Republicans 
    filter(party=="R")
  
  #Getting distance for Kennedy and McConnell
  kennedy_dist = abs(filter(members, id=="S055")$PC1) - abs(mean(dems$PC1)) #taking absolute value of both to ensure works across parties; ends up as a signed value
  mcconnell_dist= abs(filter(members, id=="S174")$PC1) - abs(mean(reps$PC1))
  
  #Filling with year and distance from political party 
  kennedy_distance_by_year[J, ] = c(my_year, 
                                    mean(dems$PC1),
                                    filter(members, id=="S055")$PC1,  
                                    kennedy_dist)
  mcconnell_distance_by_year[J, ] = c(my_year,
                                      mean(reps$PC1),
                                      filter(members, id=="S174")$PC1, 
                                      mcconnell_dist)
}

### Made it through!! Now it's time to graph:

#Figure 5 - McConnell
ggplot(mcconnell_distance_by_year, aes(x=year, y=dist, color=dist)) + 
  geom_point() + 
  ggtitle("McConnell's Vote Compared to Republicans as Whole over Time")+ 
  scale_color_viridis() + 
  ylab("Signed Distance from Republicans Mean")+ 
  theme_hc()
#Figure 6 - Kennedy 
ggplot(kennedy_distance_by_year, aes(x=year, y=dist, color=dist)) + 
  geom_point() + 
  ggtitle("Kennedy's Vote Compared to Democrats as Whole over Time")+ 
  scale_color_viridis() + 
  ylab("Signed Distance from Democrats Mean")+ 
  theme_hc()



#Finally both 
#Figure 4 

#Necessary for Legend 
cols <- c("Democrats"="darkblue","Republicans"="darkred","Kennedy"="lightblue", "McConnell"="orange")

#First plot attempt
ggplot() + 
  #Dems
  geom_line(data=kennedy_distance_by_year, aes(x=year,y=-abs(dems_avg), color="Democrats", alpha=)) + 
  #geom_smooth(data=kennedy_distance_by_year, aes(x=year,y=-abs(dems_avg), color="Democrats"))+
  #Reps
  geom_line(data = mcconnell_distance_by_year, aes(x=year, y=abs(reps_avg), color="Republicans", alpha=))+ 
  #geom_smooth(data = mcconnell_distance_by_year, aes(x=year, y=abs(reps_avg), color="Republicans"))+ 
  #Kennedy
  geom_point(data=kennedy_distance_by_year, aes(x=year,y=-abs(kennedy), color="Kennedy"))+ 
  #McConnell
  geom_point(data=mcconnell_distance_by_year, aes(x=year,y=abs(mcconnell), color="McConnell")) + 
  ggtitle("Comparison of Average Democrat and Republican Scores along PC1 Over Time") + 
  ylab("Distance from Center") + 
  xlab("Year") + 
  scale_colour_manual(name="Legend",values=cols) + 
  scale_fill_manual(name="Legned",values=cols) + 
  theme_hc() + 
  coord_flip()


#### Actual Plot used in Report 

mean_kennedy = -mean(abs(kennedy_distance_by_year$kennedy))
mean_mcconnell = mean(abs(mcconnell_distance_by_year$mcconnell))

ggplot() + 
  #Dems
  #geom_line(data=kennedy_distance_by_year, aes(x=year,y= mean_kennedy + dist, color="Democrats")) + 
  geom_smooth(data=kennedy_distance_by_year, aes(x=year,y= mean_kennedy + dist, color="Democrats"), span=.3) + 
  geom_hline(yintercept=mean_kennedy, colour="blue")+ 
  geom_text(aes(y=mean_kennedy, label="\nKennedy", x=1989), colour="blue", alpha=.2 )+ 
  #Reps
  #geom_line(data = mcconnell_distance_by_year, aes(x=year, y= mean_mcconnell - dist, color="Republicans"))+ 
  geom_smooth(data = mcconnell_distance_by_year, aes(x=year, y= mean_mcconnell - dist, color="Republicans"), span=.3)+
  geom_text(aes(y=mean_mcconnell - .3, label="\nMcConnell", x=1989), colour="red", alpha=.2) + 
  geom_hline(yintercept=mean_mcconnell, colour="red")+ 
  
  ggtitle("Parties Relative Distance to Kennedy and McConnell over Time") + 
  xlab("Year") + 
  ylab("Relative Position") + 
  scale_colour_manual(name="Legend",values=cols) + 
  scale_fill_manual(name="Legened",values=cols) + 
  theme_hc()  + 
  coord_flip()

  