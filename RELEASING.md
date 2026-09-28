# Releasing

Pushing a tag `v<version>` runs the [release workflow](.github/workflows/release.yml): all CI
jobs, then [sbt-ci-release](https://github.com/sbt/sbt-ci-release) publishes one artifact per
Chisel group to Maven Central. The version is the tag without the `v`.

```bash
git tag v0.0.1-RC1
git push origin v0.0.1-RC1
```

Before tagging, `main` must be green. Versions follow early semver.

Without a tag, the version is a snapshot derived from git. To publish a fixed version locally:

```bash
sbt 'set ThisBuild / version := "0.0.1-SNAPSHOT"' publishLocal
```

## One-time setup

The repository needs these secrets (Settings, "Secrets and variables", "Actions"). They can be
the same as for [epoxy](https://github.com/tjarker/epoxy/blob/main/RELEASING.md), which explains
how to create them.

| Secret | Value |
|---|---|
| `SONATYPE_USERNAME` | Sonatype Central user token name |
| `SONATYPE_PASSWORD` | Sonatype Central user token password |
| `PGP_SECRET` | base64 of the exported secret signing key |
| `PGP_PASSPHRASE` | passphrase of the signing key |
