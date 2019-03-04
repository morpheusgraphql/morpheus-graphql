# morpheus-graphql

## intsall

```
npm i
sls offline start

```

## request on api/graphql

# Request 1

variables:

```json
{
  "cord": {
    "latitude": "123",
    "longitude": "2413"
  }
}
```

Query:

```graphql
query GetUsers($cord: Coordinates) {
  user {
    name
    email
    address(coordinates: $cord) {
      ...AdressDetails
    }
    office(cityID: HH, zipCode: 123) {
      city
      houseNumber
      street
      owner {
        name
      }
    }
    home
  }
}

fragment AdressDetails on Address {
  houseNumber
  city
  street
}
```

# Simple Request

query without variables:

```graphql
query GetUsers {
  user {
    name
    email
    office(cityID: HH) {
      city
      houseNumber
      street
    }
  }
}
```
