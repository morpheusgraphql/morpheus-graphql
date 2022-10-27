# External PubSub example

This example is meant to showcase the possibility to synchronise events between external
pub sub systems (PostgreSQL, Rabbitmq, Redis). The idea is to simply listen to the external
pub sub and forward the events into the morpheus's Graphql event system.

To test this, you should have a postgresql instance running. A docker-compose file is provided
to simplify the process. Once you got docker-compose you can run:

```
docker-compose up postgres
```

Once the postgres container is running (or another instance), you can start the example server:

```
stack run
```

Then you can launch a graphql subscription:

```graphql
# on ws://localhost:8888
subscription {
  listenToPostgres
}
```

Then start an sql prompt:

```
psql -h localhost -p 5432 -U morpheus_user_test -d morpheus_user_db
```

And then you can start a notify command in psql:

```
morpheus_user_db=# notify postgres_channel, 'And live a coward in thine own esteem letting I dare not wait upon I would like te poor cat in the adage';
NOTIFY
```

You should get the lady-macbeth excerpt in your graphql client as a subscription.
