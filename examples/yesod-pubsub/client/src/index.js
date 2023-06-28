import initGraphQL from './graphql.js';
import GqlClient from "graphql-client"

let endpoint = "127.0.0.1:3000/api"
let proxy = null

var gql = initGraphQL(`ws://${endpoint}`, proxy);
let client = GqlClient({
	url: `http://${endpoint}`,
})

gql.subscribe(
	"subscription { subCounter() }",
	(res) => {
		console.log(">>> Result", JSON.stringify(res));
	}
)

setTimeout(() => {
	client.query("mutation {updateCounter(updateCounter_newVal:5)}");
}, 1000)

