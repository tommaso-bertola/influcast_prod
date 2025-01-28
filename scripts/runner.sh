times=(1)
week=(50)
season=('2024-2025')
maxiter=(70)
runs=(10)
swarmsize=(70)
if [ -z "$1" ]; then
    unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)
else
    unique_string=$1
fi
# unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)

epi_fit_age_groups=(
#  "SIR_multistrain_no_age-sum_multi_3-1"
#  "SIR_multistrain_5-sum_multi_2-4"
#  "SIR_multistrain_4-sum_multi_2-4"
#  "SIR_multistrain_3-sum_multi_2-4"
 "SIR_multistrain_no_age-per_thousand_nat_reg-1"
 )
# do not use spaces in the description
desc="${unique_string}_test_national_regional_fit"

LOGFILE="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S').txt"

# parallel --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: "$unique_string" ::: "$desc" ::: ${epi_fit_age_groups[@]} ::: ${times[@]} ::: ${week[@]} ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${season[@]}
parallel --sshloginfile machines.txt --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: "$unique_string" ::: "$desc" ::: ${epi_fit_age_groups[@]} ::: ${times[@]} ::: ${week[@]} ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${season[@]}
