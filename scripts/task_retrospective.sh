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


line_count=$(wc -l < /home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv)
echo "Number of rows: $line_count"

# while IFS= read -r line; do
#     echo "$line"
# done < /home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv
current_season=$(cat /home/ubuntu/influcast_prod/uploading_predictions/current_season.txt)

while [ $line_count -ne 0 ]; do
    echo "Processing line $line_count"
    notify "Get retrospective data" "info"
    Rscript /home/ubuntu/influcast_prod/source/get_retrospective_data.R "$current_season"
    if [ $? -ne 0 ]; then
        notify "Error in getting retrospective data. Exiting script." "error"
        exit 1
    else
        notify "Retrospective data retrieved successfully" "success"
    fi
    line_count=$(wc -l < /home/ubuntu/influcast_prod/retrospective/missing_rounds_comunipd-mobnetSI2R_working_copy.csv)
    ./scripts/task_to_execute_retrospective.sh "$current_season"
done