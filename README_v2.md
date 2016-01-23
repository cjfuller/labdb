The new install!

1. Install docker, docker-compose, docker-machine, virtualbox
`brew install docker docker-compose docker-machine virtualbox`

2. Create a docker machine
`docker-machine create --driver virtualbox dev`

3. Start it up
`docker-machine start dev`
`eval "$(docker-machine env dev)"`

4. `docker-compose build`

5. `docker-compose up`

6. `docker-compose run web bash -c "psql -h db --username postgres"`

7. `CREATE ROLE cfuller WITH SUPERUSER LOGIN;`

8. `CREATE ROLE labdb WITH SUPERUSER LOGIN;`

9. (legacy: ) `ALTER ROLE labdb WITH LOGIN;`

9. `docker-compose run web bash -c "psql -h db --username postgres < /app/20150711_084336_labdb_backup.dump"`

10. `docker-machine ip dev` to get the IP, then visit :3000 in a browser.
