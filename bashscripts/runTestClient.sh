fuser -k 9500/tcp || true
source /home/gitlab-runner/set-env-vars.txt
serve -s test-client/dist/ -l 9500