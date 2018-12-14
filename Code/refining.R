# this snippet cleans the datasets and creates indicator variables
# to be used for the analyses

# temporary single big dataframe containing only the relevant information
ss <- left_join(sub_screening,
                sub_cohort,by='id') %>%
  dplyr::select(id,id_prestazione,data_invito,data_esame,
                IdInvitationType,IdInvitationResult,
                CallBackLetter,STATO_CIVILE,
                NAZIONALITA,data_im,data_em,
                data_mor,incidenza)
# 345345x13

# flag compliance - defined as non-missing data_esame
ss$compliance <- ifelse(is.na(ss$data_esame),'No','Yes')

# flag whether the invitation is a reminder
ss$reminder <- ifelse(ss$IdInvitationType%in%
                        c(12,  # reminder without appointment
                          13,  # reminder
                          14,  # reminder respondents
                          31), # reminder, second level
                      'Yes','No')

# flag permanent exclusions
ss$permanent <- ifelse(ss$IdInvitationResult%in%
                         c(5,   # refusal with signature, exits screening
                           16,  # exits screening
                           20,  # refusal with signature, exits screening
                           26)| # exits screening, second level
                  ss$CallBackLetter%in%
                    c(17,       # permanent exclusion requested
                      150,      # excluded for age
                      209),     # SuperRifiuto (voluntary exclusion)
                  'Yes','No')

# flag temporary exclusion/postponement
ss$temporary <- ifelse(ss$IdInvitationResult%in%
                         c(3,   # 3 months recall
                           4,   # recent mammogram, 1 year recall
                           10,  # not available 1 year, next round recall
                           12,  # not available 6 months, next round recall
                           13,  # refusal no signature, next round recall
                           15,  # wrong address, next round recall
                           17,  # missing after reminder, next round recall
                           19,  # recent mammogram, 2 years recall
                           25,  # missing after reminder second level, next round recall
                           66)| # not available 1 year (only prosthesis)
                         ss$CallBackLetter%in%c(34,   # temporary exclusion by gp
                                                84)|  # temporary exclusion by gp
                         ss$IdInvitationType%in%c(6,  # temporary exclusion by gp
                                                  8), # temporary exclusion by gp
                         'Yes','No')

# flag mammografia recente
ss$recent <- factor(ifelse(ss$IdInvitationResult%in%
                             c(4,   # recent mammogram, 1 year recall
                               19), # recent mammogram, 2 years recall
                           'Yes','No'))

# create an individual-specific 'time from first invitation' variable
ss <- ss %>% 
  group_by(id) %>% 
  mutate(time_from_first=data_invito-first(data_invito)) %>%
  ungroup
# 345345x19

# exclude women not invited for the first time
id_first_invitation <- ss %>% 
  group_by(id) %>% 
  filter(time_from_first==0) %>% 
  filter(IdInvitationType%in%c(4,       # first invitation
                               5,       # first invitation+temporary exclusion gp
                               6,       # first invitation+temporary exclusion gp
                               7,       # first invitation+temporary exclusion gp
                               8,       # first invitation+temporary exclusion gp
                               9,       # first invitation+temporary exclusion gp
                               10,      # first invitation+temporary exclusion gp
                               20,      # first invitation
                               21)) %>% # first invitation (spontaneous)
         
  dplyr::select(id) %>% unique
# 80617

# update cohort
cc <- cohort_clean %>% 
  filter(id%in%(id_first_invitation %>% unlist))
# 80617x10

# update screening database
ss <- ss %>% 
  filter(id%in%cc$id)
# 339475x19

# create flag for 'special' situations (second level / prothesis / clinical studies)
ss$special <- ifelse(ss$IdInvitationType%in%
                       c(11,  # recall from second level
                         22,  # invitation for symptoms, second level
                         23,  # suspect, sent to second level
                         24,  # technical problems, sent to second level
                         25,  # recall from second level
                         26,  # suspect/positive, second level, RIBES study
                         27,  # technical problems, second level, RIBES study
                         28,  # negative, second level, RIBES study
                         30,  # invitation, second level, HOT study
                         31,  # reminder, second level
                         32)| # invitation, second level, prosthesis
                  ss$IdInvitationResult%in%
                    c(7,   # prosthesis, sent to second level
                      18,  # missing information, second level
                      19,  # recent mammogram, 2 years recall, second level
                      20,  # refusal with signature, exits screening 
                      21,  # missing, second invitation sent, second level
                      22,  # examined in another centre, 2 years recall, second level
                      23,  # mammogram done, second level
                      24,  # examined in another centre, 2 years recall, second level
                      25)| # missing after reminder, 2 years recall, second level
                  ss$CallBackLetter%in%
                    c(4,    # reminder, clinical study
                      5,    # invitation, clinical study
                      21,   # positive, second level
                      26,   # negative, second level
                      30,   # negative, second level
                      39,   # early recall, second level
                      44,   # negative, RIBES study
                      53,   # to be sent to second level
                      73,   # second level
                      165,  # early recall, prosthesis
                      199,  # 2 years recall, prosthesis only
                      200,  # 1 year recall, prosthesis only
                      201,  # 6 months recall, prosthesis only
                      8,    # invitation technical problems, second level
                      13,   # reminder technical problems, second level
                      28,   # reminder early recall, second level
                      38,   # invitation after phonecall, second level
                      41,   # invitation after phonecall, techincal problems, second level
                      45,   # further exams, RIBES study
                      86,   # further exams, RIBES study
                      202,  # 2 years recall, prosthesis only
                      203,  # 1 year recall, prosthesis only
                      204), # 6 months recall, prosthesis only
                  'Yes','No')

# flag whether italian or not, and whether ever with fixed 
# partner (married, widowed) or not
ss <- ss %>%
  mutate(italian=ifelse(NAZIONALITA=='I','Yes','No'),
         partnered=ifelse(STATO_CIVILE%in%c('C','V'),'Yes','No'))
# 339475x22

# flag immigration and emigration events: whether none (None),
# before (Previous), or during (InPeriod) the observation period,
# beginning with the first invitation date.
ss <- ss %>% 
  group_by(id) %>% 
  mutate(immigration=ifelse(is.na(data_im),'None',
                     ifelse(first(data_im)<first(data_invito),
                            'Previous','InPeriod')),
         emigration=ifelse(is.na(data_em),'None',
                    ifelse(first(data_em)<first(data_invito),
                           'Previous','InPeriod'))) %>% 
  ungroup

# create factors for the categorical variables
ss <- ss %>% 
  mutate(italian=factor(italian,levels = c('No','Yes')),
         partnered=factor(ifelse(is.na(partnered),'No',partnered),
                              levels=c('No','Yes')),
         immigration=factor(immigration,levels=c('InPeriod',
                                                 'None',
                                                 'Previous')),
         emigration=factor(emigration,levels=c('InPeriod',
                                               'None',
                                               'Previous'))) %>% 
  dplyr::select(-one_of('NAZIONALITA',
                        'STATO_CIVILE'))


# remove all those that had immigration events InPeriod (see create_subcohort.R)
ss <- ss %>% 
  filter(immigration!='InPeriod')
# 334598x22

# update cohort
cc <- cc %>% 
  filter(id%in%ss$id)
# 79407x19

# half-way backup
backup <- ss

###############

# create dummy variable to indicate comorbidities on a yearly basis
#
# cardiovascular diseases
bda_cvd <- bda %>% 
  map(function(x) ifelse(grepl('07',x$LISTA_BDA),1,0)) %>% 
  as.data.frame
names(bda_cvd) <- paste0('cvd_',2007:2016)
# diabetes
bda_diabetes <- bda %>% 
  map(function(x) ifelse(grepl('06',x$LISTA_BDA),1,0)) %>% 
  as.data.frame
names(bda_diabetes) <- paste0('diabetes_',2007:2016)
# other tumors
bda_tumors <- bda %>% 
  map(function(x) ifelse(grepl('05',x$LISTA_BDA),1,0)) %>% 
  as.data.frame
names(bda_tumors) <- paste0('tumors_',2007:2016)
# neuropathies
bda_neuro <- bda %>% 
  map(function(x) ifelse(grepl('10',x$LISTA_BDA),1,0)) %>% 
  as.data.frame
names(bda_neuro) <- paste0('neuro_',2007:2016)

# bind them by year and then all together
bdall_2007 <- cbind(1:nrow(bda_cvd),'2007',
                    bda_cvd$cvd_2007,
                    bda_diabetes$diabetes_2007,
                    bda_tumors$tumors_2007,
                    bda_neuro$neuro_2007)
names(bdall_2007) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2008 <- cbind(1:nrow(bda_cvd),'2008',
                    bda_cvd$cvd_2008,
                    bda_diabetes$diabetes_2008,
                    bda_tumors$tumors_2008,
                    bda_neuro$neuro_2008)
names(bdall_2008) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2009 <- cbind(1:nrow(bda_cvd),'2009',
                    bda_cvd$cvd_2009,
                    bda_diabetes$diabetes_2009,
                    bda_tumors$tumors_2009,
                    bda_neuro$neuro_2009)
names(bdall_2009) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2010 <- cbind(1:nrow(bda_cvd),'2010',
                    bda_cvd$cvd_2010,
                    bda_diabetes$diabetes_2010,
                    bda_tumors$tumors_2010,
                    bda_neuro$neuro_2010)
names(bdall_2010) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2011 <- cbind(1:nrow(bda_cvd),'2011',
                    bda_cvd$cvd_2011,
                    bda_diabetes$diabetes_2011,
                    bda_tumors$tumors_2011,
                    bda_neuro$neuro_2011)
names(bdall_2011) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2012 <- cbind(1:nrow(bda_cvd),'2012',
                    bda_cvd$cvd_2012,
                    bda_diabetes$diabetes_2012,
                    bda_tumors$tumors_2012,
                    bda_neuro$neuro_2012)
names(bdall_2012) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2013 <- cbind(1:nrow(bda_cvd),'2013',
                    bda_cvd$cvd_2013,
                    bda_diabetes$diabetes_2013,
                    bda_tumors$tumors_2013,
                    bda_neuro$neuro_2013)
names(bdall_2013) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2014 <- cbind(1:nrow(bda_cvd),'2014',
                    bda_cvd$cvd_2014,
                    bda_diabetes$diabetes_2014,
                    bda_tumors$tumors_2014,
                    bda_neuro$neuro_2014)
names(bdall_2014) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2015 <- cbind(1:nrow(bda_cvd),'2015',
                    bda_cvd$cvd_2015,
                    bda_diabetes$diabetes_2015,
                    bda_tumors$tumors_2015,
                    bda_neuro$neuro_2015)
names(bdall_2015) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')
bdall_2016 <- cbind(1:nrow(bda_cvd),'2016',
                    bda_cvd$cvd_2016,
                    bda_diabetes$diabetes_2016,
                    bda_tumors$tumors_2016,
                    bda_neuro$neuro_2016)
names(bdall_2016) <- c('id','anno_invito','cvd',
                       'diabetes','tumors','neuro')

# as no information for comorbidities in 2017 is available,
# we impute it using the last available (2016)
bdall_2017 <- bdall_2016
bdall_2017[,2] <- '2017'
bdall <- data.frame(rbind(bdall_2007,bdall_2008,bdall_2009,
                          bdall_2010,bdall_2011,bdall_2012,
                          bdall_2013,bdall_2014,bdall_2015,
                          bdall_2016,bdall_2017))
names(bdall) <- c('id','anno_invito','cvd','diabetes','tumors','neuro')

ss <- ss %>% 
  mutate(invitation_year=year(data_invito))

bdall$id <- as.integer(as.character(bdall$id))
bdall$invitation_year <- as.integer(as.character(bdall$anno_invito))

ss <- left_join(ss,bdall,by=c('id','invitation_year')) %>% 
  dplyr::select(-one_of('invitation_year')) %>% 
  mutate(cvd=ifelse(cvd==0,'No','Yes'),
         diabetes=ifelse(diabetes==0,'No','Yes'),
         tumors=ifelse(tumors==0,'No','Yes'),
         neuro=ifelse(neuro==0,'No','Yes'))
# 334598x27

rm(bdall_2007,bdall_2008,bdall_2009,bdall_2010,bdall_2011,
   bdall_2012,bdall_2013,bdall_2014,bdall_2015,bdall_2016,
   bdall_2017,bdall,bda,bda_cvd,bda_diabetes,bda_tumors,
   bda_neuro)

# second backup
backup <- ss

# flag regular invitations
ss <- ss %>% 
  mutate(regular=ifelse(
    (reminder=='Yes'&
      IdInvitationType%notin%c(4,20,32))|(special=='Yes'&
                     permanent!='Yes'&
                     IdInvitationType%notin%c(4,20,32)),
                  'No','Yes'))
ss$regular <- factor(ss$regular)
# 334598x28

# filter out those whose first invitation was not a regular one
ids <- ss %>% 
  group_by(id) %>%
  mutate(flag_first=first(regular)=='No') %>% 
  slice(1) %>% filter(flag_first) %>% 
  dplyr::select(id) %>% 
  unlist

ss <- ss %>% 
  filter(id%notin%ids)
# 334587x28

# update cohort
cc <- cc %>% 
  filter(id%in%(ss$id %>% unique %>% unlist))
# 79403x19 (only 4 removed this way)

rm(ids)

# clean the working space
rm(cohort,
   cohort_clean,
   id_filter_1,
   id_filter_2,
   id_filter_3,
   id_first_invitation,
   screening,
   screening_clean,
   sub_cohort,
   sub_screening)

backup <- ss

## Compliance outcome: at least one compliance in
## the time between subsequent regular inviations

# define subsequent events
ss1 <- ss %>% 
  group_by(id) %>%
  filter(regular=='Yes') %>% 
  mutate(invitation_n=1:n()) %>% 
  dplyr::select(id,id_prestazione,invitation_n) %>% 
  ungroup

ss2 <- left_join(ss,ss1,
                 by=c('id','id_prestazione'))

ss3 <- ss2 %>% group_by(id) %>% 
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>% 
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>% 
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>% 
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>% 
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n)) %>%
  mutate(invitation_n=ifelse(is.na(invitation_n),
                             lag(invitation_n,n=1),invitation_n))

ss <- backup <- ss3
# 334587x29

rm(ss1,ss2,ss3)

# define 'any-*' indicator variables, by id and invitation number
ss <- ss %>% 
  group_by(id,invitation_n) %>% 
  mutate(any_reminder=any(reminder=='Yes'),
         any_recent=any(recent=='Yes'),
         any_special=any(special=='Yes'),
         any_compliance=any(compliance=='Yes'),
         any_cvd=any(cvd=='Yes'),
         any_diabetes=any(diabetes=='Yes'),
         any_tumor=any(tumors=='Yes'),
         any_neuro=any(neuro=='Yes'),
         any_permanent=any(permanent=='Yes'),
         any_time=first(time_from_first),
         any_emigration=any(emigration=='InPeriod'),
         any_temporary=any(temporary=='Yes'))
# 334587x41

# compute the maximum number of invitations per individual
ss <- ss %>% 
  group_by(id) %>% 
  mutate(max_invitations=max(invitation_n)) %>% 
  ungroup
#     1     2     3     4     5     6     7     8     9    10    11 
# 24002 78132 77711 73748 50886 23458  5297  1034   242    66    11 

# create the lagged variables to use in the model
ss <- ss %>% 
  group_by(id) %>% 
  mutate(any_recent_lag1=lag(ifelse(any_recent,'Yes','No')),
         any_reminder_lag1=lag(ifelse(any_reminder,'Yes','No')),
         any_special_lag1=lag(ifelse(any_special,'Yes','No'))) %>%
  mutate(any_recent_lag1=ifelse(is.na(any_recent_lag1),'No',any_recent_lag1),
          any_reminder_lag1=ifelse(is.na(any_reminder_lag1),'No',any_reminder_lag1),
          any_special_lag1=ifelse(is.na(any_special_lag1),'No',any_special_lag1)) %>% 
  ungroup
# 334587x45

# clean first invitation after death

id_death_before <- ss %>% 
  filter(time_from_first==0) %>% 
  filter(data_mor<data_invito) %>% 
  dplyr::select(id) %>% 
  unlist %>% 
  unique

ss <- ss %>% 
  filter(id%notin%id_death_before)
# 334568x45

# update cohort
cc <- cc %>% 
  filter(id%in%(ss$id %>% unlist %>% unique))
# 79385x19

rm(backup,id_death_before)

