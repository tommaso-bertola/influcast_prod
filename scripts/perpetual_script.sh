#!/bin/bash

# Set the target day (e.g., "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
TARGET_DAY="Sat"

# Set the target time (24-hour format HH:MM)
TARGET_TIME="10:30"

# Path to the script you want to execute
SCRIPT_TO_RUN="./task_to_run.sh"

# Log file for tracking execution
LOG_FILE="./ouput.log"

# Indefinite loop
while true; do
  # Get the current day (e.g., "Mon", "Tue")
  CURRENT_DAY=$(date +"%a")

  # Get the current time (24-hour format HH:MM)
  CURRENT_TIME=$(date +"%H:%M")

  # Check if both the day and time match the target
  if [ "$CURRENT_DAY" == "$TARGET_DAY" ] && [ "$CURRENT_TIME" == "$TARGET_TIME" ]; then
    echo "[$(date)]============================" >> "$LOG_FILE"
    bash "$SCRIPT_TO_RUN" >> "$LOG_FILE" 2>&1

    # Wait for a minute to avoid multiple executions within the same minute
    sleep 60
  fi

  # Sleep for a short period to reduce CPU usage
  sleep 50
done
