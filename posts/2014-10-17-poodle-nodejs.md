---
title: POODLE and node.js
tags: poodle, ssl, nodejs, security
uuid: bfa447a6-0b2b-44e2-b693-b042f209d8cf
legacy: poodle-and-nodejs
feature-image: /images/posts/POODLE.png
---

![SSL 3 is broken](/images/posts/POODLE.png)

Earlier this week Google released details on a new security flaw that some of their researchers found in SSL version 3.0.

POODLE, the name given to this vulnerability, stands for _"Padding Oracle On Downgraded Legacy Encryption"_. Attackers using POODLE can gain access to plaintext in some areas in a SSL connection. This could be used to extract important information such as a session cookie, which would allow the attacker to hijack an account and pretend to be that user.

Unlike Heartbleed and Shellshock, which were two other major vulnerabilities found this year, POODLE is not something that you can protect yourself against with a single patch, at least when it comes to servers.

Google has already stated that they will be phasing-out support for SSL 3.0 in all their products, which includes Google Chrome. SSL 3.0 can also be manually disabled on most browsers right now.

For servers, guides have been published on how to disable the protocol on popular web server software such as Apache and Nginx. However, I haven't seen many articles on the web on how to fix this on Node.js.

Luckily, I stumbled upon this useful post by @3rd-Eden on GitHub:

[Protecting against POODLE in node.js](https://gist.github.com/3rd-Eden/715522f6950044da45d8)

Patching the vulnerability is very simple, but it does require modifying your code:

### Step 1: Check if the server allows SSL 3.0:

On Unix systems, you can use the openssl client to quickly check if a server is vulnerable:

```
openssl s_client -ssl3 -connect <hostname>:443
```

If the client is able to do a successful handshake, then it is very likely that the server is vulnerable to POODLE.

### Step 2: Disable SSL 3.0 on the server settings object

If your server is accepting SSL 3.0 connections, you will need to find where the `https.createServer()` function is being called.

Once you have located the file, you will need to load in the constants module:

```js
var constants = require('constants');
```

Then locate the `options` object provided as the first argument to `createServer()` and add the following lines to the options object:

```js
var options = {
	...
	secureProtocol: 'SSLv23_method',
	secureOptions: constants.SSL_OP_NO_SSLv3,
}

var server = https.createServer(options, app);
```

This configuration will disable SSL 3.0 on your node.js server. Please note that this might make your website incompatible with really old browsers.

### Step 3: Check that it worked

Relaunch your server and repeat step 1 to check that the client is unable to negotiate a connection successfully with SSL 3.0

### Step 4: Wait for an official patch:

There seems to be an ongoing discussion on whether to make this the default behavior for node.js in future versions:

[https://github.com/joyent/node/pull/8551](https://github.com/joyent/node/pull/8551)
