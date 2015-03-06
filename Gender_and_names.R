##
## Here I am going to be using the gender package to "Guess" the gender of each person. But first I must get the first name.
## And since the package works better if it knows what year the person is born I will say that the average age is 45
require(gender)

crew.list <- na.omit(read.csv("crew_test.csv", ))
Given.name.list <- NULL
Birth.list <- as.numeric()
Count <- 0
Gender <- data.frame()

##The progress bar
percentage <- 100/nrow(crew.list)
t <- 1
pb   <- txtProgressBar(1, 100, style=3)

for (i in crew.list$Name){
                
        ##Progress bar
        t <- (t+percentage)
        Sys.sleep(0.02)
        setTxtProgressBar(pb, t) 
        
        #Counting loops
        Count <- Count+1
        #Taking the given name from everything else
        Given.name <- head(strsplit(i ,split=" ")[[1]],1)
        Given.name.list <- rbind(Given.name.list, Given.name)
        
        #Since birth year of most crew is missing I am assuming an average age of 45 which is a WILD guess.
        #Estimate birth year:
        Birth <- crew.list[Count,"Year"]-45
        Birth.list <- rbind(Birth.list, Birth)
         
        ##Now the guessing game of the gender
        results <- Map(gender, Given.name, years = Birth, method = "ssa")
        columnized <- do.call(rbind.data.frame, results) ##Don't know why this works :/
        Gender <- rbind(Gender, columnized)
}
print(paste("Number of Males:", length(grep("\\bmale\\b", Gender$gender, perl = TRUE, value = T))))
print(paste("Number of Females:", length(grep("\\bfemale\\b", Gender$gender, perl = TRUE, value = T))))
print(paste("Number of unkowns:", (sum(is.na(Gender$gender)))))

rownames(Gender) <- 1:nrow(Gender)
rownames(crew.list) <- 1:nrow(Gender) 
Final.list <- cbind.data.frame(crew.list, Gender)
