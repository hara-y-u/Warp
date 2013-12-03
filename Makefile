lib/warp.js: warp-client/client.coffee
	coffee -c warp-client/client.coffee

clean:
	rm -f warp-client/client.js
