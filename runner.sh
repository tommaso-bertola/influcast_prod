times=(
1
)
week=(
5
)

maxiter=(50)
runs=(200)

# do not use spaces in the description
desc="test_multiple_machines"

LOGFILE="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S').txt"

parallel --sshloginfile machines.txt --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast_prod Rscript source/main.R ::: ${times[@]} ::: ${week[@]} ::: "$desc" ::: ${maxiter[@]} ::: ${runs[@]}