# this snippet reads the .csv files containing
# 1. the cohort data, from the civil registry (updated 20/03/2017)
# 2. the screening data, from the screening database (updated 16/10/2017)
# 3. the date of diagnosis, from the cancer registry (updated 31/10/2017)
# 4. the co-morbidities, from the administrative database (updated 19/04/2017)

# cohort - 348451x18
cohort <- read_csv("Data/cohort.csv", 
                         col_types = cols( 
                               SESSO = col_skip(), data_em = col_date(format = "%d/%m/%Y"), 
                               data_im = col_date(format = "%d/%m/%Y"), 
                               data_mor = col_date(format = "%d/%m/%Y"), 
                               data_nas = col_date(format = "%d/%m/%Y"),
                               caso0 = col_skip(),caso1 = col_skip(),caso2 = col_skip(),
                               caso3 = col_skip(),caso4 = col_skip(),split = col_skip(),
                               anno_in1 = col_skip(),anno_in2 = col_skip(),
                               #CODICE_COMUNE_NASCITA = col_skip(),
                               CODICE_COMUNE_IMMIGRAZIONE = col_skip(),
                               eta_in = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               eta_out = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               time_in = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               time_out = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timein_eleg = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timein_eleg1 = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timein_eleg2 = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timeout_eleg = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timeout_eleg1 = col_skip(),#col_date(format = "%d/%m/%Y"), 
                               timeout_eleg2 = col_skip()),#col_date(format = "%d/%m/%Y")), 
                         locale = locale(decimal_mark = ","))

# screening - 1970343x10
screen_1 <- read_csv("Data/screening_part_1.csv", 
                     col_types = cols(Id_scr = col_character(), 
                           data_esame = col_date(format = "%d/%m/%Y"), 
                           data_invito = col_date(format = "%d/%m/%Y"), 
                           data_richiamo = col_date(format = "%d/%m/%Y")))
screen_2 <- read_csv("Data/screening_part_2.csv", 
                     col_types = cols(Id_scr = col_character(), 
                           data_esame = col_date(format = "%d/%m/%Y"), 
                           data_invito = col_date(format = "%d/%m/%Y"), 
                           data_richiamo = col_date(format = "%d/%m/%Y")))

screening <- rbind(screen_1,screen_2)
rm(screen_1,screen_2)
names(screening)[1] <- 'id'

# incident cases of breast cancer - 348451x2
cases <- read_csv("Data/cases.csv", 
                 col_types = cols(incidenza = col_date(format = "%d/%m/%Y")))

cohort <- full_join(cohort,cases) # diagnosis dates to be merged with cohort information
rm(cases)

# co-morbidities - 348451x4x10
bda <- list(bda_2007=read_csv("Data/coorte_bda_2007.csv"),
            bda_2008 = read_csv("Data/coorte_bda_2008.csv"),
            bda_2009 = read_csv("Data/coorte_bda_2009.csv"),
            bda_2010 = read_csv("Data/coorte_bda_2010.csv"),
            bda_2011 = read_csv("Data/coorte_bda_2011.csv"),
            bda_2012 = read_csv("Data/coorte_bda_2012.csv"),
            bda_2013 = read_csv("Data/coorte_bda_2013.csv"),
            bda_2014 = read_csv("Data/coorte_bda_2014.csv"),
            bda_2015 = read_csv("Data/coorte_bda_2015.csv"),
            bda_2016 = read_csv("Data/coorte_bda_2016.csv"))
