#!/bin/bash
# msg="tommasobertola"
msg="comunelab.sandbox --channel influcast_status"

# add time stamp to the notifications and send over keybase
notify() {
    case "$2" in
    error)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - ERROR]: $1"
        keybase chat send $msg "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_red_square:]: $1"
        ;;
    warning)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - WARNING]: $1"
        keybase chat send $msg "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_orange_square:]: $1"
        ;;
    start)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - START]: $1"
        keybase chat send $msg "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_blue_square:] $1"
        ;;
    success)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - SUCCESS]: $1"
        keybase chat send $msg "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_green_square:] $1"
        ;;
    *)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - OTHER]: $1"
        keybase chat send $msg "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :information_source:] $1"
        ;;
    esac

}

notify "Run started" "start"

hostname=$(hostname)
if [ "$hostname" != "network-vm-bluesky" ]; then
    notify "Hostname is not 'influcast'. Exiting script." "error"
    exit 1
fi

LOGFILE_remote="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S')_download_remote.txt"
LOGFILE_local="joblog/job_$(date '+%Y-%m-%d_%H-%M-%S')_download_local_influcast0.txt"
# notify "Update local Influcast repository on remote machines" "info"
# parallel --sshloginfile machines.txt --resume --onall --joblog "$LOGFILE_remote" --workdir /home/ubuntu/influcast_prod/scripts ./download_new_data.sh ::: 1
# if [ $? -ne 0 ]; then
#     notify "Error in downloading data from remote machines. Exiting script." "error"
#     exit 1
# else
#     notify "Remote data downloaded successfully" "success"
# fi

# notify "Update local Influcast repository on local machine" "info"
# parallel --resume --jobs 1 --joblog "$LOGFILE_local" --workdir /home/ubuntu/influcast_prod/scripts ./download_new_data.sh ::: 1
# if [ $? -ne 0 ]; then
#     notify "Error in downloading data from local machine. Exiting script." "error"
#     exit 1
# else
#     notify "Local data downloaded successfully" "success"
# fi
# sleep 1

# notify "Get retrospective data" "info"
# # trick the script to run with old data for retrospective analysis
# Rscript source/get_retrospective_data.R >/dev/null 2>&1

notify "Starting the data processing on local machines" "info"
unique_string=$(date +"%Y%m%d%H%M%S%N" | sha256sum | awk '{print $1}' | cut -c1-5)
notify "Unique string is $unique_string" "info"
signal=$(cat uploading_predictions/current_signal.txt)
consolidation=$(cat uploading_predictions/consolidation.txt)
./scripts/runner_local.sh $unique_string $signal $consolidation
if [ $? -ne 0 ]; then
    notify "Error in computing the model estimates. Exiting script." "error"
    exit 1
else
    notify "Model estimates computed successfully" "success"
fi

# #gather results
# notify "Gathering results and deleting from remote machines" "info"
# ./scripts/copy_script.sh
# if [ $? -ne 0 ]; then
#     notify "Error in gathering results from rempote machines. Exiting script." "error"
#     exit 1
# else
#     notify "Results were gathered successfully and are now stored locally" "success"
# fi

notify "Unifying results" "info"
Rscript source/unify_results.R $unique_string >/dev/null 2>&1
if [ $? -ne 0 ]; then
    notify "Error in unifying results. Exiting script." "error"
    exit 1
else
    notify "Results were unified successfully" "success"
fi

notify "Preparing output estiamtes rds file"
Rscript source/prepare_output.R $unique_string >/dev/null 2>&1
if [ $? -ne 0 ]; then
    notify "Error in preparing output estimates. Exiting script." "error"
    exit 1
else
    notify "Output rds estimates were prepared successfully" "success"
fi

notify "Preparing output csv file" "info"
# echo "Rscript source/prepare_output.R"
Rscript source/output_parser.R $unique_string >/dev/null 2>&1
if [ $? -ne 0 ]; then
    notify "Error in preparing output csv estimates. Exiting script." "error"
    exit 1
else
    notify "Output csv estimates were prepared successfully" "success"
fi

FILE=$(cat uploading_predictions/current_week.txt)
SIGNAL=$(cat uploading_predictions/current_signal.txt)
FILE_CSV="$FILE\_$SIGNAL.csv"

if [ $consolidation == "TRUE" ]; then
    FILE="consolidated/$FILE"
else
    FILE="not_consolidated/$FILE"
fi

keybase chat upload $msg uploading_predictions/$FILE\_$SIGNAL\_regional.png
keybase chat upload $msg uploading_predictions/$FILE\_$SIGNAL\_national.png
keybase chat upload $msg uploading_predictions/$FILE\_$SIGNAL.csv
cp scripts/runner.sh uploading_predictions/runner.txt
keybase chat upload $msg uploading_predictions/runner.txt
keybase chat upload $msg uploading_predictions/consolidation.txt
keybase chat upload $msg fitness_methods/sum_fitness_multi_4_national_regional.R
if [ $? -ne 0 ]; then
    notify "Error in sending image on keybase. Exiting script." "error"
    exit 1
fi

notify "Estimates are not published to the website. Proceed manually" "info"
# notify "Publishing estimates to Influcast website (file $FILE_CSV)" "info"
# ./scripts/publish_estimates.sh $FILE_CSV
# if [ $? -ne 0 ]; then
#     notify "Error in publishing estimates. Exiting script." "error"
#     exit 1
# else
#     notify "Estimates were published successfully" "success"
# fi

notify "Script finished successfully" "success"
notify "Run finished at $(date +"%Y-%m-%d_%H-%M-%S") @tommasobertola" "success"
