times=(
1
)
week=(
52 5 10
)

maxiter=(15)
runs=(4)

# do not use spaces in the description
desc="test_codice_input_data_scorporato"

LOGFILE="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S').txt"

parallel --resume --jobs 1 --bar --joblog "$LOGFILE" --workdir /home/ubuntu/influcast Rscript source/main.R ::: ${times[@]} ::: ${week[@]} ::: "$desc" ::: ${maxiter[@]} ::: ${runs[@]}