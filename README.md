# morpheus-graphql

Build GraphQL APIs with your favourite functional language!

## intsall

```
npm i
sls offline start

```

## request on api/graphql

```graphql
query GetUsers($office: String) {
  user {
    name
    email
    address(latitude: "Hi Nicas", longitude: "office") {
      ...AdressDetails
    }
    office(zipCode: "4134", cityID: "cityID") {
      city
      street
    }
    home {
      city
    }
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

fragment User on User {
  address(latitude: "Hi Nicas", longitude: "Dublin") {
    city
  }
}
```
