dat_munging <- dat_in %>% filter(SampleTypeCode == 'AQMH') #select multihabitat sampling

dat_in_sampling_info <- dat_in %>% 
  select(SIDNO, 
         LabRecordID, 
         FieldComponent, #primary or split
         ProvisionalData,
         ProjectLabel,
         SiteNumber, #maps on to spatial data
         CollectionDate,
         SampleTypeCode,
         SiteVisitSampleNumber,
         ProjectAssignedSampleLabel,
         SiteName,
         StudyReachName,
         TimeDatum,
         CollectionYear,
         CollectionMonth,
         CollectionDayOfYear,
         `NAWQA-SMCOD`,
         NAWQAStudyUnitCode,
         TotAreaSampled_cm2
         ) %>% distinct() %>%
  group_by_at(vars(-LabRecordID)) %>%
  summarize(LabRecordID_count = length(LabRecordID)) %>% 
  distinct() %>%
  filter(SampleTypeCode == 'AQMH')
  # filter(SampleTypeCode == 'APHY')
  
# no NAs for LabRecordID_count
dat_in_sampling_info$LabRecordID_count %>% is.na() %>% sum()

# number of lab records ranges from 0-100, mean is 44.8
dat_in_sampling_info$LabRecordID_count %>% max()
dat_in_sampling_info$LabRecordID_count %>% min()
dat_in_sampling_info$LabRecordID_count %>% mean()
dat_in_sampling_info$LabRecordID_count %>% hist()

# 207 distinct SiteNumbers
dat_in_sampling_info$SiteNumber %>% unique() %>% length()


dat_in_location_summary <- dat_in_sampling_info %>%
  group_by(SiteNumber) %>%
  summarize(n_times = CollectionDate %>% unique %>% length,
            n_years = CollectionYear %>% unique %>% length,
            n_months = CollectionMonth %>% unique %>% length,
            month_list = CollectionMonth %>% unique %>% paste(collapse = '_'),
            areas_sampled = TotAreaSampled_cm2 %>% unique %>% paste(collapse = '_'),
            mean_areas_sampled = TotAreaSampled_cm2 %>% mean(.,na.rm = TRUE))


