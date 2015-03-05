Rip_tmdb <- function(File, Column.name)
        {
        
require(XML)
require(RCurl)
        
#Prepare the data
        Rip.list <- read.csv(File)
        Mod.Rip.list <- as.data.frame(Rip.list[,c(Column.name)])
        colnames(Mod.Rip.list) <- c("Url")

#This is for the progress bar
t <- 1
pb   <- txtProgressBar(1, 500, style=3)

#Here the magic begins.
for (i in Mod.Rip.list) {
        #The progress bar first
        t <- (t+1)
        Sys.sleep(0.02)
        setTxtProgressBar(pb, t)   
        
#The scraping        
URL <- i
shortURL <- paste0("tt", as.numeric(gsub("[^\\d]+", "", URL, perl=TRUE)))
tmdbURL <- paste0("https://www.themoviedb.org/search?query=", shortURL)

# tmdb doest not contain xml information and therefore an extra step had to be taken
fixedURL <- getURL(tmdbURL)
parsed.html <- htmlParse(fixedURL)
URL.loc <- xpathSApply(parsed.html, "//div[@class='poster']/a", xmlGetAttr, 'href')
finalURL <- paste0("https://www.themoviedb.org", URL.loc, "/cast") 

# here I scrape the site
fixedURL <- getURL(finalURL)
parsed.html <- htmlParse(fixedURL)
#Name of Film
Film <- xpathSApply(parsed.html, "//div[@class='title']/h2", xmlValue)

#Year of Film
Year <- as.numeric(gsub("[^0-9]", "", xpathSApply(parsed.html, "//div[@class='title']", xmlValue)))

#Producers scraped, first names, than job title than combined in a data.frame
Prod.Person <- list(xpathSApply(parsed.html, "//table[@id='Production']//td[@class='person']", xmlValue))
Prod.Jobs <- list(xpathSApply(parsed.html, "//table[@id='Production']//td[@class='job']", xmlValue))
Producers <- data.frame(Prod.Person, Prod.Jobs)
        
if (nrow(Producers)==0) {
        Producers <- data.frame("NA", "Producers") 
        }

        colnames(Producers) <- c("Name", "Job")

#Director/s scraped
Dir.Person <- list(xpathSApply(parsed.html, "//table[@id='Directing']//td[@class='person']", xmlValue))
Dir.Jobs <- list(xpathSApply(parsed.html, "//table[@id='Directing']//td[@class='job']", xmlValue))
Directors <- data.frame(Dir.Person, Dir.Jobs)
        
if (nrow(Directors)==0) {
        Directors <- data.frame("NA", "Directors") 
        }

        colnames(Directors) <- c("Name", "Job")

#Writers scraped
Writ.Person <- list(xpathSApply(parsed.html, "//table[@id='Writing']//td[@class='person']", xmlValue))
Writ.Jobs <- list(xpathSApply(parsed.html, "//table[@id='Writing']//td[@class='job']", xmlValue))
Writers <- data.frame(Writ.Person, Writ.Jobs)
   
if (nrow(Writers)==0) {
        Writers <- data.frame("NA", "Writers") 
        }

        colnames(Writers) <- c("Name", "Job")

#Cameraman scraped
Cam.Person <- list(xpathSApply(parsed.html, "//table[@id='Camera']//td[@class='person']", xmlValue))
Cam.Jobs <- list(xpathSApply(parsed.html, "//table[@id='Camera']//td[@class='job']", xmlValue))
Camera <- data.frame(Cam.Person, Cam.Jobs)

if (nrow(Camera)==0) {
        Camera <- data.frame("NA", "Camera") 
        }        

        colnames(Camera) <- c("Name", "Job")

#Finally the Actors
Act.Person <- list(xpathSApply(parsed.html, "//table[@class='cast']//td[@class='person']", xmlValue))
Act.Role <- list(rep("Actor", sapply(Act.Person, NROW)))
Actors <- data.frame(Act.Person, Act.Role)
 
#To check if Actors dataframe is empty
if (nrow(Actors)==0) {
        Actors <- data.frame("NA", "Actors") 
        } 

        colnames(Actors) <- c("Name", "Job")

#Here I limit the Number of Actors to the First five since I don't need any more
Actors <- Actors[1:5,1:2]

#here I bind it all together
#First I bind the jobs and the names

Final <- rbind(Directors, Producers, Writers, Camera, Actors)

#Than add it to Title of Movie and Year
Film.list <- as.data.frame(rep(Film, nrow(Final)))
colnames(Film.list) <- c("Title")

Year.list <- as.list(rep(Year, nrow(Final)))
Film.list$Year <- Year.list

Bound <- cbind(Film.list, Final)

Final.list <- data.frame()

Final.list <- rbind(Final.list, Bound)


#Flatten the list
Final.list <- data.frame(lapply(Final.list, as.character), stringsAsFactors=FALSE)

}

write.table(Final.list, "Final_list.csv", sep=",", row.names = FALSE)
}
