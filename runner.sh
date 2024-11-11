times=(
1
)
week=(
8
)

maxiter=(20 50)
runs=(50)
swarmsize=(30 50 70)
epidemicmodel=("SIR_multistrain_5" "SIR_multistrain_4" "SIR_multistrain_3")

unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)

# do not use spaces in the description
desc="${unique_string}_test_swarmsize_maxiter_epimodel"

LOGFILE="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S').txt"

parallel --sshloginfile machines.txt --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: ${times[@]} ::: ${week[@]} ::: "$desc" ::: ${maxiter[@]} ::: ${runs[@]} ::: ${swarmsize[@]} ::: ${epidemicmodel[@]} ::: "$unique_string"