# Rip top lists from IMDB - only URLS
# Seq of which years are to be ripped.
# Based on income

Top.Rip <- function(Year=2014, Increment=5, Amount=10, Filename="top_rip")
{

require(XML)       
        
      Increment <- -abs(Increment)
The.Seq <- seq(Year, by = Increment, length.out = Amount)
Rip.links=list()
for (i in The.Seq) {
        
        Movie.url <- paste0("http://www.imdb.com/search/title?at=0&sort=boxoffice_gross_us&title_type=feature&year=", i, ",", i)
        Rip.links <- rbind(Rip.links, Movie.url)    
}
# Clean up the colums i.e remove the row name which is [i]
rownames(Rip.links) <- NULL

##here I pull title, url and year
Allt <- data.frame()
t <- 1
pb   <- txtProgressBar(1, Amount, style=3)


for (i in Rip.links){

        t <- (t+1)
        Sys.sleep(0.02)
        setTxtProgressBar(pb, t)        
        
das.URL <- i
parsed.html <- htmlParse(das.URL)
Film.name <- xpathSApply(parsed.html, "//table[@class='results']//td[@class='title']/a", xmlValue)
Film.year <- xpathSApply(parsed.html, "//table[@class='results']//td[@class='title']/span[@class='year_type']", xmlValue)
Film.shorturl <- xpathSApply(parsed.html, "//table[@class='results']//td[@class='title']/a", xmlGetAttr, 'href')
        
#Here I create a full imdb address
        Film.url <- list(rep("http://www.imdb.com", 50), Film.shorturl)
        Film.url <- do.call("paste", c(Film.url, sep=""))

#Put it into data.frame
Allt <- rbind(Allt, data.frame(Name=Film.name, Year=Film.year, Url=Film.url))


}
Filename <- paste0(Filename, ".csv")
write.table(Allt, file = Filename, sep=",", row.names = FALSE )
}