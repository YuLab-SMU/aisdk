# Network / SSL / Proxy — Detailed Playbook

## Symptom signatures

- `cannot open URL 'https://...'`
- `Couldn't resolve host`
- `SSL connect error` / `SSL certificate problem`
- `Operation timed out after N milliseconds`
- `download had nonzero exit status`
- `Error in install.packages : trying to use CRAN without setting a mirror`

## Step 1 — confirm what the live session has

`r_session_state` already returns most of this. Pull it and read the
`envvars`, `repos`, and `options` blocks. Specifically look at:

- `http_proxy` / `https_proxy` / `no_proxy` (and the uppercase variants).
- `CURL_CA_BUNDLE`, `SSL_CERT_FILE` — when set, override the system CA store.
- `getOption("repos")` — `@CRAN@` means no mirror chosen yet.
- `getOption("timeout")` — defaults to 60s; large packages on slow links exceed this.
- `getOption("download.file.method")` — "libcurl" (default) vs "wget" / "curl".

## Step 2 — verify connectivity from R

```r
# r_eval
curl::has_internet()
curl::nslookup("cran.r-project.org", error = FALSE)
# Actual fetch:
con <- url("https://cran.r-project.org/CRAN_mirrors.csv")
readLines(con, n = 3); close(con)
```

If `has_internet()` is TRUE but the mirror fetch fails, the problem is
proxy or cert, not connectivity.

## Step 3 — fix patterns

**No mirror set**:
```r
chooseCRANmirror(graphics = FALSE, ind = 1L)  # or pick a known-good mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))
```

**Timeout on large downloads**:
```r
options(timeout = max(600, getOption("timeout")))
```

**Behind a proxy** (set in `~/.Renviron`, restart R):
```
http_proxy=http://proxy.corp:8080
https_proxy=http://proxy.corp:8080
no_proxy=localhost,127.0.0.1,.corp
```

**SSL cert errors on corporate machine**:
```
# in ~/.Renviron, point R at the corp CA bundle
CURL_CA_BUNDLE=/etc/ssl/certs/ca-bundle.crt
```

**GitHub-rate-limited or auth-required `install_github`**:
```r
# r_eval to confirm token presence
nzchar(Sys.getenv("GITHUB_PAT"))
nzchar(Sys.getenv("GITHUB_TOKEN"))
```

Set a PAT via `usethis::create_github_token()` and store in `~/.Renviron`.

## Step 4 — the BiocFileCache / pkgcache angle

Some failures are stale caches, not network:

```r
# r_eval
if (requireNamespace("pkgcache", quietly = TRUE)) {
  pkgcache::meta_cache_summary()
  pkgcache::meta_cache_update()
}
```
