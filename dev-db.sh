#!/bin/bash


docker run \
    -e POSTGRES_PASSWORD="" \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -v ~/labdb-data:/var/lib/postgresql/data \
    -p 5432:5432 \
    postgres:14

