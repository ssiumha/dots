#!/bin/bash
# Start nginx and tmux, then tail to keep container running

# Start nginx in background
nginx

# Wait for nginx to be ready
sleep 1

# Start tmux session
tmux new-session -d -s main

# Keep container running
tail -f /dev/null
