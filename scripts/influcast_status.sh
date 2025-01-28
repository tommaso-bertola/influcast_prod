#!/bin/bash

# add time stamp to the notifications and send over keybase
notify() {
    case "$2" in
    error)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - ERROR]: $1"
        echo $(whoami)
        # keybase chat send comunelab --channel influcast_status "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_red_square:]: $1"
        keybase chat send tommasobertola "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_red_square:]: $1"
        ;;
    warning)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - WARNING]: $1"
        echo $(whoami)
        # keybase chat send comunelab --channel influcast_status  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_orange_square:]: $1"
        keybase chat send tommasobertola  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_orange_square:]: $1"
        ;;
    start)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - START]: $1"
        echo $(whoami)
        # keybase chat send comunelab --channel influcast_status  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_blue_square:] $1"
        keybase chat send tommasobertola "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_blue_square:] $1"
        ;;
    success)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - SUCCESS]: $1"
        echo $(whoami)
        # keybase chat send comunelab --channel influcast_status  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_green_square:] $1"
        keybase chat send tommasobertola  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :large_green_square:] $1"
        ;;
    *)
        echo "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - OTHER]: $1"
        echo $(whoami)
        # keybase chat send comunelab --channel influcast_status  "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :information_source:] $1"
        keybase chat send tommasobertola "[$(date +"%Y-%m-%d_%H-%M-%S.%3N") - :information_source:] $1"
        ;;
    esac

}


notify "Ping form influcast" "start"