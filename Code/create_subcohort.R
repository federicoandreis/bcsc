# this snippet extract the sub-cohort used in the paper
#

# to apply the eligibility criteria based on invitation date (data_invito)
# we first need to clean the screening dataset of the missing invitations

screening_clean <- screening %>%
  filter(!is.na(data_invito))

# this brings the total number of records from 1970343 to 1932910 (~2% less)
# and the total number of unique ids from 348451 to 333030 (~5% less).
# The discrepancy (15421) is made up of individuals with no valid in-screening
# records (it is unclear, at the moment, the reason why).
# the cohort dataframe is updated accordingly:

cohort_clean <- cohort %>% 
  filter(id%in%screening_clean$id)
# 333030

# The remaining 19364 unique ids with some missing information on the
# invitation dateshave at least one valid screening record and are, thus, 
# retained in the dataset.

###############################################
# The eligibility criteria for the cohort were:
# 1. born [01/01/1957,31/12/1964]  - turning 50 not earlier than 2007 and no later than 2014
# 2. no breast cancer history before the first screening invitation  
# 3. the first invitation letter was received starting from 6 months prior
#    their 50th birthday
#
# Also, 
# 4. women with immigration events within the observation window
# i.e., officially registered in Milan after their first invitation,
# shall be excluded: we have no way to separate system errors
# from delays in recording immigration dates. This filter will be
# applied in the refining.R snippet, after all the relevant quantities
# have been defined.

# filter 1
id_filter_1 <- cohort_clean %>%
  filter(data_nas>='1957/01/01',data_nas<='1964/12/31') %>% 
  dplyr::select(id)
# 85409 individuals

# filter 2
id_filter_2 <- screening_clean %>%
  group_by(id) %>%
  slice(1) %>% 
  dplyr::select(id,data_invito) %>% 
  left_join(.,cohort) %>%
  mutate(flag_first=ifelse(is.na(incidenza),0,
                              ifelse(data_invito<incidenza,0,1))) %>% 
  filter(flag_first==0) %>% 
  dplyr::select(id) %>% 
  unique
# 325960


# filter 3
id_filter_3 <- screening_clean %>%
  group_by(id) %>% 
  slice(1) %>% 
  filter(data_invito>='2006/06/01') %>% 
  dplyr::select(id) %>% 
  unique
# 123474

# join all filters

sub_cohort <- cohort_clean %>% 
  filter(id%in%(id_filter_1 %>% unlist),
         id%in%(id_filter_2 %>% unlist),
         id%in%(id_filter_3 %>% unlist))
# 81848x19

sub_screening <- screening_clean %>% 
  filter(id%in%sub_cohort$id)
# 345345x10

