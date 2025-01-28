host=$(hostname)

rsync -az --remove-source-files ubuntu@10.67.31.39:influcast_prod/sim_results/ sim_results/ > /dev/null 2>&1
# rsync -az --remove-source-files ubuntu@10.67.31.135:influcast_prod/sim_results/ sim_results/ > /dev/null 2>&1