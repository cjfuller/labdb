#!/bin/bash

OBJC_DISABLE_INITIALIZE_FORK_SAFETY=YES PORT=3001 bundle exec puma --config config/puma.rb
