fuser -k 8999/tcp || true
source /home/gitlab-runner/set-env-vars-prod.txt
java -jar test-server/libs/server-0.0.1-SNAPSHOT.jar --server.port=8999
