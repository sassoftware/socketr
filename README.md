# socketR
<!-- badges: start -->
[![r-universe status](https://sassoftware.r-universe.dev/badges/:name)](https://sassoftware.r-universe.dev)
[![r-universe status](https://sassoftware.r-universe.dev/badges/:packages)](https://sassoftware.r-universe.dev)
[![r-universe status](https://sassoftware.r-universe.dev/socketr/badges/version)](https://sassoftware.r-universe.dev)
[![r-universe status](https://sassoftware.r-universe.dev/socketr/badges/checks)](https://sassoftware.r-universe.dev)
<!-- badges: end -->

`socketR` lets you work with network sockets directly from R on Linux. It
supports common TCP, UDP, and Unix-domain socket workflows, with both
function-based and object-oriented APIs.

Web documentation: https://sassoftware.github.io/socketr/

```r
## CRAN instructions will be added once published.

## install release from r-universe
install.packages('socketR', repos = c('https://sassoftware.r-universe.dev', 'https://cloud.r-project.org'))

## dev version
remotes::install_github("sassoftware/socketr")
```

## Example: TCP loopback

```r
library("socketR")

server <- socket_create("inet", "stream")
socket_reuse_address(server, TRUE)
socket_bind(server, "127.0.0.1", 0)
port <- socket_local_name(server)$port
socket_listen(server)

client <- socket_create("inet", "stream")
socket_connect(client, "127.0.0.1", port)
peer <- socket_accept(server)

socket_send(client, "hello")
rawToChar(socket_receive(peer, 5))

socket_close(peer); socket_close(client); socket_close(server)
```

## Datagram and Unix-domain sockets

Use `socket_create("inet", "dgram")` with `socket_send_to()` and
`socket_receive_from()` for TCP/UDP. Use `socket_create("unix", "stream")` and pass a
filesystem path as `address` to `socket_bind()` / `socket_connect()` for
Unix-domain sockets.

## Polling and options

```r
socket_poll(list(client, peer), events = "read", timeout_ms = 1000)
socket_set_blocking(client, FALSE)
socket_no_delay(client, TRUE)
socket_quick_ack(client, TRUE) # Linux when TCP_QUICKACK is available
socket_receive_timeout(client, 0.5)
```

Generic options are available through `socket_get_option()` and
`socket_set_option()` with typed values: `int`, `logical`, `timeval`, `linger`, or
`raw`.

Multiple options can be applied in order with `socket_set_options()`. If one
fails, the `socketr_option_batch_error` condition reports the failed option and
the options already applied.

## R6 API

```r
s <- Socket$new("inet", "stream")
s$bind("127.0.0.1", 0)$listen()
s$close()
```

## Contributing

We welcome contributions! 

Please read [CONTRIBUTING.md](CONTRIBUTING.md) 
for details on how to submit contributions to this project.

## License

This project is licensed under the Apache 2.0 License.

Direct and indirect dependencies are governed by their own licenses. See `DEP-LICENSES.md` file for details.