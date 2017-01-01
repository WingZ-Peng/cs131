#!/bin/bash
tmux new -s '131project'
for server in Alford Bolden Hamilton Parker Welsh
do
  tmux new-window "python server.py $server"
  tmux rename-window "$server"
done
tmux new-window 'telnet localhost 8000'
tmux attach-session -t '131project'
