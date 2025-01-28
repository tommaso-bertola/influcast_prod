#!/bin/bash

# download new data to check if there were updates on the repository
./scripts/download_new_data.sh

if [ -z "$1" ]; then
    exit 1
else
    FILE_CSV=$1
fi

repo=/home/ubuntu/test-repo-2
# repo_influcast=/home/ubuntu/Influcast
# repo=$repo_influcast


# cd $repo
# submit_branch="testing_again_again_submit_branch_$(date +"%Y%m%d")"
# git checkout -b $submit_branch
# cp /home/ubuntu/influcast_prod/uploading_predictions/$FILE_CSV $repo/previsioni/comunipd-mobnetSI2R
# git add $repo/previsioni/comunipd-mobnetSI2R/$FILE_CSV
# git commit -m "uploading $FILE_CSV"
# gh pr create --title "comunipd-mobnetSI2R-$(date +"%Y%m%d")" --body "Invio settimanale"
# cd -

# git checkout -b <submit_branch>
# cp /home/ubuntu/influcast_prod/uploading_predictions/FILE_CSV /home/ubuntu/Influcast
# git add /home/ubuntu/Influcast/previsioni/comunipd-mobnetSI2R/$FILE_CSV
# git commit -m "uploading $FILE_CSV"
# gh pr create --title "comunipd-mobnetSI2R-$(date +"%Y%m%d")" --body "Invio settimanale"