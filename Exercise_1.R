library(tidyverse) 

# Dataset 1: Sequentie resultaten per monster (Breed formaat) 
dna_results <- tibble( 
  sample_id = c("S01", "S02", "S03", "S04", "CONTROL"), 
  meta_data = c("E.coli_v1", "B.subtilis_v1", "E.coli_v2", "S.aureus_v1", "ERROR"), 
  yield_run1 = c("450ng", "320ng", "510ng", "290ng", "0ng"), 
  yield_run2 = c("480ng", "310ng", "530ng", NA, "0ng") 
) 

# Dataset 2: Lab-condities per bacteriesoort 
lab_conditions <- tibble( 
  species_name = c("E.coli", "B.subtilis", "P.aeruginosa"), 
  temp_celsius = c(37, 30, 37), 
  incubatie_tijd = c(24, 48, 24) 
) 

dna_results2.2 <- dna_results %>%
                filter(sample_id != "CONTROL") %>%
                pivot_longer(cols=3:4,
                             names_to="Run",
                             values_to="Yield") %>%
               mutate(Run=case_when(Run == "yield_run1" ~ 1,
                                    Run == "yield_run2" ~ 2)) %>%
               mutate(Yield=parse_number(Yield)) %>%
               drop_na() %>%
               mutate(meta_data=case_when(meta_data == "E.coli_v1" ~ "E.coli",
                                          meta_data == "B.subtilis_v1" ~ "B.subtilis",
                                          meta_data == "S.aureus_v1" ~ "S.aureus",
                                          meta_data == "E.coli_v2" ~ "E.coli")) %>%
               rename("species_name"= 2) %>%
               left_join(lab_conditions, by="species_name") %>%
               rename("temperatuur" = 5) %>%
               relocate("temperatuur", .after=1) %>%
               mutate(temperatuur=replace_na(temperatuur,20)) %>%
               group_by(species_name) %>%
               summarise(mean_yield=mean(Yield, na.rm=TRUE),sd_yield=sd(Yield, na.rm=TRUE))

plot <- ggplot(data=dna_results2.2,mapping = aes(x=species_name,
                                                 y=mean_yield,
                                                 fill=species_name)) +
        geom_col() +
        geom_errorbar(aes(ymin=mean_yield - sd_yield, ymax= mean_yield + sd_yield), width=0.5) +
        scale_fill_brewer(palette = "PiYG") +
        theme_classic() +
        theme(plot.title=element_text(hjust=0.5,
                                      face="bold",
                                      family="sans"),
              axis.title.x=element_text(face="bold",
                                   family="sans"),
              axis.title.y=element_text(face="bold",
                                   family="sans"),
              axis.text=element_text(face="bold",
                                     family="sans")) +
        labs(title="Yield based on species type",
             x="Species",
             y="Yield")
plot



