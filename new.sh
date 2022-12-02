#!/bin/bash

help()
{
    echo ""
    echo "Usage $0 -p <aoc_year_folder> -n <project_name>"
    exit 1
}

setup_vscode_project()
{
    if [ ! -d ".vscode" ]
    then
        mkdir .vscode
    fi

    cp template/settings_template.json .vscode/settings.json
    sed -i "s#gpr_file_path#$1#" .vscode/settings.json
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
    echo "Create new AOC directory..."
fi

NEW_PROJECT_PATH="$AOC_FOLDER"/"$PROJECT_NAME"
GPR_FILE_PATH="$NEW_PROJECT_PATH"/"$PROJECT_NAME".gpr

if [ ! -d "$NEW_PROJECT_PATH" ]
then
    cp -r template/project "$NEW_PROJECT_PATH"
else
    echo "Project $NEW_PROJECT_PATH already exists..."
    exit 1
fi


NEW_PROJECT_NAME=$(echo "$PROJECT_NAME" | sed -r 's/(^|_)([a-z])/\U_\2/g' | sed -r 's/^.//')
echo "Preparing the project ${NEW_PROJECT_NAME}..."

echo "Renaming GPR project..."
sed -i "s/Template/$NEW_PROJECT_NAME/" "$NEW_PROJECT_PATH"/template.gpr
mv "$NEW_PROJECT_PATH"/template.gpr "$GPR_FILE_PATH"

echo "Done!"

while true; 
do
    read -rp "Setup VSCode Ada extension for new project? [y/n] " yn
    case $yn in
        [Yy]* ) setup_vscode_project "$GPR_FILE_PATH"; break;;
        [Nn]* ) exit;;
        * ) echo "Please answer [y]es or [n]o.";;
    esac
done

echo "Ready to code now! =)"
