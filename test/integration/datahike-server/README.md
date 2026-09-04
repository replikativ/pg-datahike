# Datahike Server release soak

This gate exercises pg-datahike through the packaging and lifecycle users are
expected to deploy: Datahike Server, not the adapter's development launcher.
It complements the in-process and driver suites by checking boundaries they do
not cover together:

- the standalone JAR contains the expected pg-datahike version;
- the server-owned catalog exposes a durable file database over PostgreSQL;
- password authentication is required and plaintext connections are refused;
- `sslmode=verify-full` succeeds against a generated test CA;
- repeated abrupt client exits do not strand the listener; and
- a graceful server restart preserves data and leaves PostgreSQL writable.

Build a version-pinned Datahike Server JAR, then run:

```bash
EXPECTED_DATAHIKE_VERSION=0.8.1870 \
EXPECTED_PG_DATAHIKE_VERSION=0.1.189 \
  test/integration/datahike-server/run-jar.sh \
  /path/to/datahike-http-server-VERSION-standalone.jar
```

The harness creates a short-lived PKCS#12 certificate and file stores beneath
`/tmp`, binds only to loopback, and removes its runtime data on exit. It needs
`curl`, `java`, `keytool`, `psql`, `sha256sum`, and `unzip`.

Run the same lifecycle through the non-root container image with:

```bash
EXPECTED_DATAHIKE_VERSION=0.8.1870 \
EXPECTED_PG_DATAHIKE_VERSION=0.1.189 \
DATAHIKE_CONTAINER_ENGINE=podman \
  test/integration/datahike-server/run-container.sh datahike-server:dev
```

The container gate copies the packaged JAR back out of the image and checks
its embedded versions. It additionally asserts the runtime user is
`10001:10001`, and persists the catalog/database through a stop/start cycle on
a named volume. Docker and Podman are both supported.
