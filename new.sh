#!/bin/bash

help()
{
    echo ""
    echo "Usage $0 -p <aoc_year_folder> -n <project_name>"
    exit 1
}

while getopts "n:p:" opt
do
    case "$opt" in
        n ) PROJECT_NAME="$OPTARG" ;;
        p ) AOC_FOLDER="$OPTARG" ;;
        ? ) help ;;
    esac
done

if [ -z "$PROJECT_NAME" ] || [ -z "$AOC_FOLDER" ]
then
    echo "Some or all of the parameters are empty";
    help
    exit 1
fi

if [ ! -d "$AOC_FOLDER" ]
then
    mkdir "$AOC_FOLDER"
fi

NEW_PROJECT_PATH="$AOC_FOLDER"/"$PROJECT_NAME"

if [ ! -d "$NEW_PROJECT_PATH" ]
then
    cp -r template "$NEW_PROJECT_PATH"
fi

NEW_PROJECT_NAME=$(sed -r 's/(^|_)([a-z])/\U\2/g')

sed -i "s/Template/$NEW_PROJECT_NAME/" "$NEW_PROJECT_PATH"/template.gpr

mv "$NEW_PROJECT_PATH"/template.gpr "$NEW_PROJECT_PATH"/"$PROJECT_NAME".gpr