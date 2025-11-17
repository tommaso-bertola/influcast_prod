
signal=("A" "B" "ARI")
signal_type=("ARI")
consolidation="FALSE"
path_file="ari/not_consolidated/squared_weight"

for sig in "${signal[@]}"; do
    echo "Running for signal: $sig"
    echo $sig >uploading_predictions/current_signal.txt
    bash ./scripts/task_to_execute_local.sh $path_file
done
