#!/bin/bash
read -s MESSAGE
echo $MESSAGE | gpg -e -r D4457E3A -o `date +"%Y-%m-%d-%H-%M-%S"`.gpg
