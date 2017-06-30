this project was initialized using the command `stack new yesod-postgres`.

to set up database in psql (first run `postgres -D /usr/local/var/postgres` if you don't have a daemon running):
```psql
create role "rhymebook_LOWER";
alter role "rhymebook_LOWER" with login;
create database "rhymebook_LOWER";
```

seed the database initially by running `stack run seed`

then running `stack exec yesod devel` will start the server.
