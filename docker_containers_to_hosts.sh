#!bin/bash
docker ps --format '{{.ID}}' | xargs docker inspect | jq -r '.[].NetworkSettings.Networks."kafka-compose_all" | .IPAddress + " " + ([.Aliases] | map(join(" ")))[0]' >>/etc/hosts

