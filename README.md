# morpheus-graphql

## intsall

```
npm i
sls offline start

```

## request on api/graphql

```graphql
query GetUsers {
  user {
    name
    email
    address(latitude: "Hi Nicas", longitude: "Dublin") {
      ...AdressDetails
    }
    office(zipcode: "4134") {
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
  address {
    city
  }
}
```
