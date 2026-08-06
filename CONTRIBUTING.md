# Contributing to pg-datahike

## Licensing of contributions

pg-datahike is released under the [PostgreSQL License](LICENSE) — the same
license as PostgreSQL itself. Contributions are accepted under that license.

There is **no CLA**. Instead we use the [Developer Certificate of Origin][dco]
(DCO): sign off each commit to certify that you wrote the patch, or otherwise
have the right to submit it under the project's license.

    git commit -s -m "your message"

which appends

    Signed-off-by: Your Name <your@email>

Use your real name and a reachable address; `git config user.name` and
`user.email` should already be set to these.

[dco]: https://developercertificate.org/

### Why permissive, and why no CLA

A PostgreSQL-compatible server should not be harder to build on than
PostgreSQL. The PostgreSQL License means anyone can embed, fork, or ship
pg-datahike commercially, closed-source, without asking.

Because the inbound license is permissive, we do not need contributors to
sign rights away — every patch arrives already licensed for any use,
including in commercial products. The trade-off is deliberate and worth
stating: without a CLA the project cannot be relicensed later, and the
PostgreSQL License carries no express patent grant. Both are accepted
consequences of keeping contribution friction at zero.

### Note on borrowed code

Do not paste code from EPL-licensed sources — including datahike, konserve,
and the rest of the replikativ stack — into pg-datahike's own files. Those
files would have to keep their original license, which defeats the point of a
uniformly permissive tree. Depending on those libraries is fine; copying
their source into this repository is not.

Behavioral references to PostgreSQL are fine and encouraged: cite the
relevant `src/backend/...` file in a comment as a specification, and write an
original implementation. Do not transcribe PostgreSQL's C source.

## Development

See [README.md](README.md) for architecture and the module map, and
[doc/](doc/) for design notes.

    clojure -T:build compile-java   # required before anything loads the server
    clojure -M:test                 # unit tests
    clojure -M:format               # cljfmt check (CI-equivalent)
    clojure -M:ffix                 # cljfmt fix

Integration suites under `test/integration/` clone upstream drivers
(pgjdbc, asyncpg, node-postgres) and run their conformance tests against
pg-datahike; each has its own README.
