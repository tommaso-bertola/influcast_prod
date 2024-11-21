#!/bin/bash

host=$(hostname)

cd /home/ubuntu/Influcast
# echo "git checkout main"
# echo "gh repo sync tommaso-bertola/Influcast -b main"
# echo "git pull"
file="/home/ubuntu/influcast_prod/joblog/job_$(date '+%Y-%m-%d_%H-%M-%S')_git_pull_log_$host.txt"

git checkout main >>$file 2>&1
gh repo sync tommaso-bertola/Influcast -b main >>$file 2>&1
git pull >>$file 2>&1
cd /home/ubuntu/influcast_prod
