archiveFile <- "NEI_data.zip"
if(!file.exists(archiveFile)) {
	archiveURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
	if(Sys.info()["sysname"] == "Darwin") {
		download.file(url=archiveURL,destfile=archiveFile,method="curl")
	} else {
		download.file(url=url,destfile=archiveFile)
	}
}
if(!(file.exists("summarySCC_PM25.rds") && 
	file.exists("Source_Classification_Code.rds"))) { unzip(archiveFile) }

# Load the NEI & SCC data frames.
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset NEI data by Baltimore's fip.
baltimoreNEI <- NEI[NEI$fips=="24510",]

# Aggregate using sum the Baltimore emissions data by year
aggTotalsBaltimore <- aggregate(Emissions ~ year, baltimoreNEI,sum)

png("plot3.png",width=480,height=480,units="px",bg="transparent")

library(ggplot2)

ggp <- ggplot(baltimoreNEI,aes(factor(year),Emissions,fill=type)) +
  geom_bar(stat="identity") +
  theme_bw() + guides(fill=FALSE)+
  facet_grid(.~type,scales = "free",space="free") + 
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Tons)")) + 
  labs(title=expression("PM"[2.5]*" Emissions, Baltimore City 1999-2008 by Source Type"))

print(ggp)

dev.off()

