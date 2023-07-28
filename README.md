# movementspeechtempo_public


Data: 
- rhythm_dataset_for_stat_env8.csv contains all data with an acoustic envelope of 8 Hz (selected in JSHLR paper) and motion data
- rhythm_dataset_for_stat_env10.csv and rhythm_dataset_for_stat_env12.csv contain data with an acoustic envelope of 10 or 12 Hz and motion data

Controlled factors:
- tasks ("read", "spontaneous" speech, "motion only")
- workload (here called condition_m with "low", "moderate" workload and a baseline condition "speech only")

Dependent variables:
- Pedaling rate (mov_av_rhythm)
- Speech rates(env8_av_rhythm, env8_av_rhythmimf1, env8_av_rhythmimf2)
  
Script: 
Explorations_Susanne.R 
- all preprocessing steps
- final figures (saved in subfolder figures)
- model coefficients (saved in subfolder modelcoef)
- and descriptive stats (saved in subfolder means)
 
