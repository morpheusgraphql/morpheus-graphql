
import * as url from "url"
import * as graphqlWs from "graphql-ws"
import WebSocket from "ws"
import HttpsProxyAgent from "https-proxy-agent"

let notifiers = [];
let proxyUrl = null;

class ProxyWebSocket extends WebSocket{
	constructor(address, protocols){
		if(proxyUrl != null){
			let agent = new HttpsProxyAgent(url.parse(proxyUrl))
			super(address, protocols, { agent: agent });
		}else{
			super(address, protocols);
		}
	}
}

class GraphQLSocket{

	constructor(){
		this.socket = null;
		this.__subs = []
	}

	connect(wss_url, wsToken){
		this.socket =
			graphqlWs.createClient({
				url: wss_url,
				webSocketImpl: ProxyWebSocket,
			});
		while(this.__subs.length > 0){
			let sub = this.__subs.pop();
			this.__subscribe(sub);
		}

	}

	subscribe(sub){
		// Add to the subscription queue if we haven't established a socket.
		if(this.socket == null) {
			this.__subs.push(sub);
		}else {
			this.__subscribe(sub);
		}
	}

	__subscribe(sub, onResult){
		let onStart = (data) => {
			console.log(">>> Start", JSON.stringify(data));
		}

		let onAbort = (data) => {
			// console.log(">>> Abort", JSON.stringify(data));
		}

		let onCancel = (data) => {
			// console.log(">>> Cancel", JSON.stringify(data));
		}

		let onError = (data) => {
			console.log(">>> Error", JSON.stringify(data));
		}

		if(onResult == null)
			onResult = (res) => {
				console.log(">>> Result", JSON.stringify(res));
			}

		this.socket.subscribe({ query: sub }, {
			next: onResult,
			error: onError,
			complete: onCancel,
		});
	}
}

export function init(wss_url, proxyUrl2){
	proxyUrl = proxyUrl2;
	let sock = new GraphQLSocket();
	sock.connect(wss_url);
	return sock;
}


export default init
