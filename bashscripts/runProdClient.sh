fuser -k 9000/tcp || true
source /home/gitlab-runner/set-env-vars.txt
serve -s prod-client/dist/ -l 9000