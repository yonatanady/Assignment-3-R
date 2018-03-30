#install.packages("knitr")
#install.packages("plotly")
library(tidyverse)
library(gridExtra)
library(knitr)
library(plotly)

#not sure if we need this part, but at the moment i put it here first
#downloading data
download.file("https://ndownloader.figshare.com/files/2292169", "data/portal_data_joined.csv")

#read_csv = tibble, diff from read.csv, it treat stringasfactor = FALSE by default, load faster
surveys <- read_csv("data/portal_data_joined.csv")
str(surveys)

summary(surveys)

#========Exploratory Analysis===========================
#distribution of the species and taxa
surveys_species <- surveys %>%
    filter(!is.na(taxa), !is.na(genus), !is.na(species), !species_id == "") %>%
    mutate(species_name = paste(genus, species)) %>% group_by(taxa, species_name, species_id) %>% 
    tally()

surveys_taxa <- surveys %>%
    filter(!is.na(taxa)) %>% group_by(taxa) %>% 
    tally()

#plot distribution pie chart
plot_ly(surveys_taxa, labels = ~taxa, values = ~n, type = 'pie',textposition = 'outside',
        domain = list(x = c(0, 0), y = c(0, 0)), textinfo = 'label+percent',
        rotation = -108, direction = "clockwise", 
        textfont = list(color = '#000000', size = 16)) %>%
    layout(title = 'Taxa Distribution', xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                                                     showticklabels = FALSE), 
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#further breakdown of each taxa
#bird distribution
bird_species <- surveys_species %>% filter(taxa == "Bird") %>% 
    group_by(taxa, species_id)

#rabbit distribution
rabbit_species <- surveys_species %>% filter(taxa == "Rabbit") %>% 
    group_by(taxa, species_id) 

#reptile distribution
reptile_species <- surveys_species %>% filter(taxa == "Reptile") %>% 
    group_by(taxa, species_id) 

#rodent distribution
rodent_species <- surveys_species %>% filter(taxa == "Rodent") %>% 
    group_by(taxa, species_id) 

#combined multiple pie charts
#to label each pie chart with bird, rabbit, reptile and rodent
plot_ly() %>%
    add_pie(data = bird_species, labels = ~species_name, values = ~n, name = "Bird",
            domain = list(x = c(0, 0.5), y = c(0.55, 1)), textposition = 'inside',
            textinfo = 'label+percent') %>%
    add_pie(data = rabbit_species, labels = ~species_name, values = ~n,
            domain = list(x = c(0.5, 1), y = c(0.55, 1)), textposition = 'inside',
            textinfo = 'label+percent') %>%
    add_pie(data = reptile_species, labels = ~species_name, values = ~n,
            domain = list(x = c(0, 0.5), y = c(0, 0.45)), textposition = 'inside',
            textinfo = 'label+percent') %>%
    add_pie(data = rodent_species, labels = ~species_name, values = ~n,
            domain = list(x = c(0.5, 1), y = c(0, 0.45)), textposition = 'inside',
            textinfo = 'label+percent') %>%
    layout(title = "Distribution of Species according to Taxa", showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#sex information according to taxa
surveys_taxa_sex <- surveys %>%
    filter(!is.na(sex)) %>% select(taxa,sex) %>%
    group_by(taxa, sex) %>% tally()
#only rodent that had the sex are, the other taxa sex is n/a

ggplot(surveys_taxa_sex, aes(x = sex, y = n)) + 
    geom_bar(stat = "identity") +
    theme_bw() + theme(legend.position = "right") +
    print(ggtitle("Sex Distribution of Rodent")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Count")

#taxa information according to years
surveys_taxa_years <- surveys %>%
    select(taxa,year) %>%
    group_by(taxa, year) %>% tally()

surveys_taxa_years$year <- as.factor(surveys_taxa_years$year)

p <- ggplot(surveys_taxa_years, aes(x = year, y = n)) + 
    geom_bar(stat = "identity")

p+facet_wrap(~taxa, scales = "free_y") +
    theme_bw() + theme(legend.position = "right") +
    print(ggtitle("Taxa Information According to Year")) +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(angle = 68, hjust = 1, size = 9)) +
    ylab("Count")

#========Statistical Analysis===========================

#weight according to species
#compare with weight according to genus
surveys_weight <- surveys %>%
    filter(!is.na(year), !is.na(weight), !species_id == "") %>%
    group_by(year, species_id) %>% 
    mutate(average_weight = mean(weight)) 

#weight according to genus
surveys_weight <- surveys %>%
  filter(!is.na(year), !is.na(weight), !genus == "") %>%
  group_by(year, genus) %>% 
  mutate(average_weight = mean(weight)) 

#just to check
#to see distribution of species according to year
#to find out only include only those have 1977-2002 data by Swan
species_year <- surveys %>%
  filter(!is.na(year), !is.na(weight), !genus == "") %>%
  group_by(year, genus, weight) %>% select(year, genus, weight) %>% tally()

#1st graph - time series plot
#plot using species
ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
  geom_line(aes(color = species_id)) 

#plot using genus
#i find this plot is cleaner than the one using species_id
ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
  geom_line(aes(color = genus))


#difference between female and male weight
surveys_weight_sex <- surveys %>%
  filter(!is.na(year), !is.na(weight), !is.na(sex), !species_id == "") %>%
  group_by(species_id, sex) %>% 
  mutate(average_weight = mean(weight))

ggplot(surveys_weight_sex, aes(x = species_id, y = weight)) + 
  geom_boxplot(aes(color = sex))  

#Same graph as above, but by genus
ggplot(surveys_weight_sex, aes(x = genus, y = weight)) + 
  geom_boxplot(aes(color = sex))

#t-test & anova
#by Nicole


#to find out how to beautify the plots like add plot title, axis label etc
#by Rio
ggplot(surveys_weight_sex, aes(x = species_id, y = weight)) + 
  geom_boxplot(aes(color = sex)) + 
  theme_bw() + theme(legend.position = "right") +
  print(ggtitle("Comparison Between Female \nand Male Weight")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean Weight (g)")

ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
  geom_line(aes(color = genus)) +
  theme_bw() + theme(legend.position = "right") +
  print(ggtitle("Average Weight Based on Genus")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean Weight (g)")

ggplot(surveys_weight, aes(x = year, y = average_weight)) + 
  geom_line(aes(color = species_id)) +
  theme_bw() + theme(legend.position = "right") +
  print(ggtitle("Average Weight Based on Species")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Mean Weight (g)")

#average rodent hindfoot length according to species_id for each taxa
#note that the only taxon with information about hindfoot length is Rodent (see summary below), so there is only 1 graph
surveys_hfoot_id <- surveys %>%
  filter(!is.na(hindfoot_length), !taxa == "", !species_id == "") %>% 
  group_by(species_id) %>%
  mutate(average_hfoot = mean(hindfoot_length))

summary(as.factor(surveys_hfoot_id$taxa))

ggplot(surveys_hfoot_id, aes(x = species_id, y = average_hfoot)) + 
  theme_bw() + theme(legend.position = "right") +
  print(ggtitle("Rodent Hindfoot Length")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Species ID", y = "Hindfoot Length (cm)") +
  stat_summary(fun.y = "mean", geom = "bar") +
  geom_text(aes(label=round(average_hfoot)), vjust=-0.3, size=3.5)

#average rodent hindfoot length according to plot type
surveys_hfoot_plottype <- surveys %>%
  filter(!is.na(hindfoot_length), !plot_type == "") 

ggplot(aes(x = plot_type, y = hindfoot_length), data = surveys_hfoot_plottype) + 
  stat_summary(fun.y = "mean", geom = "bar") +
  labs(x = "Plot Type", y = "Hindfoot Length (cm)", title = "Average Rodent Hindfoot Length by Plot Type") +
  theme(plot.title = element_text(hjust = 0.5))
