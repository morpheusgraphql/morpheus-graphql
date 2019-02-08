# morpheus-graphql

```
npm i
sls offline start

```

# request on api/graphql

```graphql
query GetUsers {
  user {
    name
    email
    address(token: "324", cityID: "Hamburg") {
      ...AdressDetails
    }
    office(officeID: "hh 32 ") {
      city
      street
    }
    home {
      city
    }
  }
}

fragment User on User {
  address {
    city
  }
}

fragment AdressDetails on Address {
  houseNumber
  street
  city
  owner {
    ...User
  }
}
```
