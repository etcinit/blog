---
title: Heapster and Deis: So hip!
tags: docker, heapster, coreos, influxdb
uuid: 09e80619-b671-47ec-b419-a57813d55ff2
legacy: heapster-and-deis-so-hip
---

![Metrics in Grafana](//i.imgur.com/uUnd1oq.png)

I've been playing with Deis, a Docker orchestration platform on AWS for the past
few days. In fact, I got some services running on it. I have found it to be very
useful since it automates a many processes that would actually be a pain to
setup manually, such as automatically building Docker images, setting up load
balancing across containers and servers, and making it super simple to scale
different parts of an application.

However, one key missing part is monitoring. Deis' documentation points you to
Cadvisor and Heapster. These are two applications built by Google in order to
collect and store statistics.

Cadvisor is very simple to setup. All you need is to load the service file using
`fleetctl`. Once its running you should be able to see stats about containers
running on each server. This is nice, but it does not really give you a
complete picture given that it only shows stats for the host it is running on.
If you have 5 servers in your cluster, you need to open 5 tabs to see the stats
of each server.

Heapster is the next step. It connects to instances of Cadvisor, collects
metrics, and then pushes them to a sink, such as an InfluxDB server, every 10
seconds.

Heapster is built with Kubernetes in mind, but luckily it also supports CoreOS
which is what Deis runs on. It uses `fleet` to discover other nodes in the
cluster and connect to their instance of the Cadvisor container.

In the past, I had managed to get this working but only for a few minutes.
There seemed to be a bug with Heapster that caused it to stop flushing data to
InfluxDB after a while. However, I recently tried using the newest version
(v0.10.0) and the problem seems fixed now.

Below I'm including the two unit files that I'm using per CoreOS cluster to get
this service working. With a few modifications, you should be able to get it
running on a Deis cluster or any generic CoreOS cluster.

## Units

_Note: The following services do not setup either InfluxDB, or Grafana. You will
need to install this on a server or set them up using containers, and create a
database for Heapster to write to. You can setup the Grafana dashboard by
importing this [template](https://github.com/GoogleCloudPlatform/heapster/blob/master/grafana/kubernetes-dashboard.json)_

### cAdvisor:

No modifications necessary.

``` {#cadvisor .ini .numberLines startFrom="0"}
[Unit]
Description=cAdvisor Service
After=docker.service
Requires=docker.service

[Service]
TimeoutStartSec=10m
Restart=always
ExecStartPre=-/usr/bin/docker kill cadvisor
ExecStartPre=-/usr/bin/docker rm -f cadvisor
ExecStartPre=/usr/bin/docker pull google/cadvisor
ExecStart=/usr/bin/docker run --volume=/:/rootfs:ro \
    --volume=/var/run:/var/run:rw \
    --volume=/sys:/sys:ro \
    --volume=/var/lib/docker/:/var/lib/docker:ro \
    --publish=4194:4194 --name=cadvisor \
    --net=host google/cadvisor:latest \
    --logtostderr \
    --port=4194
ExecStop=/usr/bin/docker stop -t 2 cadvisor

[X-Fleet]
Global=true
```

### Heapster:

Modify the environment variables to match your setup. Also, note that this is
not a global service. It is intended to run on only one server. Otherwise you
would get duplicate stats.

``` {#heapster .ini .numberLines startFrom="0"}
[Unit]
Description=Heapster
After=docker.service cadvisor.service
Requires=docker.service cadvisor.service

[Service]
Environment="INFLUXDB_HOST=YOUR-HOSTNAME-HERE:8086" \
    "INFLUXDB_USERNAME=YOUR-USERNAME-HERE" \
    "INFLUXDB_PASSWORD=YOUR-PASSWORD-HERE" \
    "INFLUXDB_NAME=k8s" \
    "CADVISOR_PORT=4194"
EnvironmentFile=/etc/environment

TimeoutStartSec=0
ExecStartPre=-/usr/bin/docker kill heapster1
ExecStartPre=-/usr/bin/docker rm heapster1
ExecStartPre=/usr/bin/docker pull kubernetes/heapster:v0.10.0
ExecStart=/usr/bin/docker run --name heapster1 kubernetes/heapster:v0.10.0 \
    /usr/bin/heapster \
    --sink influxdb \
    --sink_influxdb_host=${INFLUXDB_HOST} \
    --sink_influxdb_name=${INFLUXDB_NAME} \
    --sink_influxdb_username=${INFLUXDB_USERNAME} \
    --sink_influxdb_password=${INFLUXDB_PASSWORD} \
    --coreos \
    --fleet_endpoints=http://${COREOS_PRIVATE_IPV4}:4001 \
    --cadvisor_port=${CADVISOR_PORT}
ExecStop=/usr/bin/docker stop heapster1
```

## Additional resources:

- [cAdvisor](https://github.com/google/cadvisor)
- [Heapster on GitHub](https://github.com/GoogleCloudPlatform/heapster)
- [Official guide for CoreOS](https://github.com/GoogleCloudPlatform/heapster/blob/master/docs/coreos.md)
