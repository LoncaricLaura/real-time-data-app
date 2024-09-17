 #!/bin/bash
RED_COLOR='\033[0;31m'
GREEN_COLOR='\033[0;32m'
NO_COLOR='\033[0;0m'
current_timestamp=$(date +"%Y-%m-%d_%H:%M:%S")
PATH_TO_PROJECT=$1
printf "${GREEN_COLOR}$current_timestamp: Starting script...${NO_COLOR}\n"
echo "Environment variables: $(env)"
cabal --version
ghc --version
cd $PATH_TO_PROJECT
. .env
mkdir -p web/charts
mkdir -p web/histograms
mkdir -p logs/
echo "Current directory: $(pwd)"
echo "Starting cabal project..."
cabal run >> ./logs/real-time-data-app_$current_timestamp.log
if [[ $? -eq 0 ]]; then
    printf "${GREEN_COLOR}Statistics saved successfully.${NO_COLOR}\n"
    echo "Copying on another machine..."
    scp -P $REMOTE_PORT -r $(pwd)"/web" $REMOTE_ADDRESS:$REMOTE_DIR_PATH
    if [[ $? -eq 0 ]]; then
        printf "${GREEN_COLOR}COPY: Copying success!\n"
        echo "$current_timestamp: Exiting script..."
        exit 0
    else
        printf "${RED_COLOR}COPY: Copy failed! Try again later!\n"
        echo "$current_timestamp: Failed to copy on another machine!"
        exit 1
    fi
else
    printf "${RED_COLOR}CABALRUN: Failed to save statistics. Try again later.\n"
    echo "$current_timestamp: Failed to save statistics!"
    exit 1
fi
