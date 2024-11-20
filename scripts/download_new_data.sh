#!/bin/bash

host=$(hostname)
notify() {
    if [ -z "$2" ]; then
        echo "[$(date +"%Y-%m-%d_%H-%M-%S") - ok]: $1"
    else
        echo "[$(date +"%Y-%m-%d_%H-%M-%S") - $2]: $1"
    fi
}

cd /home/ubuntu/Influcast
notify "Pulling data from Influcast repository" $host
# echo "git checkout main"
# echo "gh repo sync tommaso-bertola/Influcast -b main"
# echo "git pull"
file="/home/ubuntu/influcast_prod/joblog/job_$(date '+%Y-%m-%d_%H-%M-%S')_git_pull_log_$host.txt"

git checkout main  >> file
gh repo sync tommaso-bertola/Influcast -b main >> file
git pull >> file
cd /home/ubuntu/influcast_prod