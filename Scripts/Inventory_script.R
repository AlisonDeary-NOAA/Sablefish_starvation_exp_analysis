###Filtering Sablefish rearing experimental data

#library
library(dplyr)
library(here)

#load data
getwd()
here("data_raw", "2022_rearing_metadata_sampling_spreadsheet.csv")
list.files("data_raw")
sab <- read.csv(here("data_raw", "2022_rearing_metadata_sampling_spreadsheet.csv"))

names(sab)
unique(sab$Subsample.Type..RNA.DNA..stable.isotopes..CS.)
sab_rna_dna=sab%>%
  filter(Subsample.Type..RNA.DNA..stable.isotopes..CS.=="RNA/DNA")%>%
  distinct(Vial..,.keep_all = TRUE)%>%
    as.data.frame()
length(sab_rna_dna$Date)

#add in the wetweights using a join: I want to keep all rows of sab_rna_dna, but only rows
  #in column wet weight that matches sab_rna_dna

sab_1=sab%>%
  select("Vial..","Wet.Weight..g.")%>%#only grab desired columns
  filter(!Wet.Weight..g. == "" )%>% #remove blank cells
  filter(!is.na(Vial..))#remove nas  so that one vial and one wet weight 
      
head(sab_1)

sab_rna_dna_wg=dplyr::left_join(sab_rna_dna,sab_1,by="Vial..")

length(sab_rna_dna_wg$Date)
names(sab_rna_dna_wg)

#drop columns 
to_remove = c('Prey.Present..Y.N.', 'Prey.Count.....in.stomach...gut.', 'Wet.Weight..g..x', 'Feeding.Incidence')

# Select the complement of the column names in the vector 'to_remove'.

sab_rna_dna_wg1=sab_rna_dna_wg%>%
  select(!all_of(to_remove))

write.csv(sab_rna_dna_wg1,"rna_dna_inventory.csv")
