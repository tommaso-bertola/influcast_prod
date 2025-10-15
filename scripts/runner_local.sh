times=(1 2 3 4 5 6 7 8 9 10)
maxiter=(70)
runs=(100)
swarmsize=(70)

if [ -z "$1" ]; then
    unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)
else
    unique_string=$1
fi
# unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)

if [ -z "$2" ]; then
    signal=("ILI") #"A" "B" "AB" "ILI"
else
    signal=$2
fi

if [ -z "$3" ]; then
    consolidation=("FALSE") #TRUE FALSE
else
    consolidation=$3
fi

# current_season=('2024-2025')
if [ -z "$4" ]; then
    current_season=$(cat uploading_predictions/current_season.txt)
else
    current_season=$4
fi

week=(NA)
if [ -z "$5" ]; then
    current_week=$(cat uploading_predictions/current_week.txt)
else
    current_week=$5
fi

epi_fit_age_groups=(
#  "SIR_multistrain_no_age-sum_multi_3-1"
#  "SIR_multistrain_5-sum_multi_2-4"
#  "SIR_multistrain_4-sum_multi_2-4"
#  "SIR_multistrain_3-sum_multi_2-4"
 "SIR_multistrain_no_age-per_thousand_nat_reg-1"
 )
# do not use spaces in the description
# desc="${unique_string}_test_national_regional_fit"
desc="${unique_string}_no_description_use_exp_3_no_arrange" 

LOGFILE="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S').txt"

# parallel --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: "$unique_string" ::: "$desc" ::: ${epi_fit_age_groups[@]} ::: ${times[@]} ::: ${week[@]} ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${season[@]}
# parallel --sshloginfile machines.txt --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: "$unique_string" ::: "$desc" ::: ${epi_fit_age_groups[@]} ::: ${times[@]} ::: ${week[@]} ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${season[@]} ::: ${signal[@]}
parallel --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: "$unique_string" ::: "$desc" ::: ${epi_fit_age_groups[@]} ::: ${times[@]} ::: ${week[@]} ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${current_season[@]} ::: ${signal[@]} ::: ${consolidation[@]} ::: ${current_week[@]}
