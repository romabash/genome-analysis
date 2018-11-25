
#--------------------
# Using bigrquery to query Simons Genome data from Google BigQuery
#--------------------

# Load packages
library(bigrquery)
library(dplyr)
library(readr)
library(DBI)
library(ggplot2)

# Load Google Cloud Platform Project ID from a file
project_id <- read_file(file="project_ID.txt")

# Use Project ID as the billing project when working with free sample data
billing <- project_id

# Set up BigQuery connection with DBI
con <- DBI::dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "human_genome_variants",
  billing = billing
)
con
dbListTables(con)

# Connect to "1000_genomes_sample_info"
genome_metadata <- tbl(con, "1000_genomes_sample_info")

# Convert to a tibble to use locally
df <- as_tibble(genome_metadata)

# Select the columns and mutate some of them into factors
data <- df %>%
  select(Population_Description, Super_Population_Description, Gender) %>% 
  rename(Country = Population_Description,
         Region = Super_Population_Description)

#--------------------
# Look at overall Gender ratio
#--------------------

# Look at the Gender ratio for each Region
overall_gender_ratio <- data %>%
  group_by(Gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
overall_gender_ratio

# Plot Gender Ratio Information
data %>%
  ggplot() +
  geom_bar(aes(x = Gender, fill = Gender)) +
  geom_text(data = overall_gender_ratio, 
            aes(x = Gender, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = overall_gender_ratio, 
             aes(x = Gender, y = Percentage, label = paste0(Percentage, "%"), group = Gender), 
             position = position_stack(vjust = 15)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("1000 Genome Project Gender Ratio") +
  scale_x_discrete(name= "Gender") +
  scale_y_continuous(name = "Count") +
  scale_fill_discrete(name = "Gender", labels = c("Female", "Male"))

#--------------------
# Look at Gender ratio by Region
#--------------------

# Look at number of people from each Region
region_people <- data %>%
  group_by(Region) %>%
  summarize(Count = n())
region_people

# Look at the Gender ratio for each Region
region_gender_ratio <- data %>%
  group_by(Region, Gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
region_gender_ratio

# Plot People and Gender Ratio Information
data %>%
  ggplot() +
  geom_bar(aes(x = Region, fill = Gender)) +
  geom_text(data = region_people, 
            aes(x = Region, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = region_gender_ratio, 
             aes(x = Region, y = Count, label = paste0(Percentage, "%"), group = Gender), 
             position = position_stack(vjust = 0.5)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354")) +
  ggtitle("1000 Genome Project Gender Ratio") +
  scale_x_discrete(name= "Region") +
  scale_y_continuous(name = "Count") +
  scale_fill_discrete(name = "Gender", labels = c("Female", "Male"))


#--------------------
# Look at Gender ratio by Country
#--------------------

# Look at number of people from each Country
country_people <- data %>%
  group_by(Country) %>%
  summarize(Count = n())
country_people

# Look at the Gender ratio for each Country
country_gender_ratio <- data %>%
  group_by(Country, Gender) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = round(Count/sum(Count)*100))
country_gender_ratio

# Plot People and Gender Ratio Information
data %>%
  ggplot() +
  geom_bar(aes(x = Country, fill = Gender)) +
  geom_text(data = country_people, 
            aes(x = Country, y = Count, label = Count), 
            position = position_dodge(width=0.9), 
            vjust=-0.25, 
            fontface = "bold") +
  geom_label(data = country_gender_ratio, 
             aes(x = Country, y = Count, label = paste0(Percentage, "%"), group = Gender), 
             position = position_stack(vjust = 0.5), size=3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size=18, color = "#054354"),
        axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=8)) +
  ggtitle("1000 Genome Project Gender Ratio") +
  scale_x_discrete(name= "Country") +
  scale_y_continuous(name = "Count") +
  scale_fill_discrete(name = "Gender", labels = c("Female", "Male"))


#--------------------
# R session information
#--------------------
sessionInfo()