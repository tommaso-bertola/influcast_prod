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
git checkout main
gh repo sync tommaso-bertola/Influcast -b main
git pull
cd /home/ubuntu/influcast_prod