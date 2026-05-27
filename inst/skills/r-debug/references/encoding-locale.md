# Encoding & Locale — Detailed Playbook

R errors that mention `multibyte`, `invalid string`, `non-ASCII`, or that
print Chinese / Japanese / Korean characters as `<U+XXXX>` almost always
come from one of three causes: the session locale, the file's encoding,
or the connection (`readLines`, `read.csv`) using the wrong encoding.

## Step 1 — what does the live session report?

```r
# r_eval
Sys.getlocale()
Sys.getenv(c("LANG", "LC_ALL", "LC_CTYPE"))
l10n_info()
```

What you want to see on a modern system:

- `LC_CTYPE` set to a UTF-8 locale (`en_US.UTF-8`, `zh_CN.UTF-8`, etc.).
- `l10n_info()$`MBCS`` = TRUE and `l10n_info()$UTF-8` = TRUE.

What is a problem:

- `Sys.getlocale()` returns `"C"` → no multibyte support; `readLines` on
  a UTF-8 file will mangle bytes.
- On Windows pre-UCRT R: `Sys.getlocale("LC_CTYPE")` returns
  `"Chinese (Simplified)_People's Republic of China.936"` (GBK code page).
  Files saved as UTF-8 will then look garbled.

## Step 2 — what does the file actually say?

Never trust the file's extension. Read the first few bytes:

```r
# r_eval
con <- file(path, "rb"); on.exit(close(con))
head_bytes <- readBin(con, what = "raw", n = 4)
head_bytes
# 0xEF 0xBB 0xBF  -> UTF-8 with BOM
# 0xFF 0xFE       -> UTF-16 LE
# 0xFE 0xFF       -> UTF-16 BE
```

For text content, `readr::guess_encoding(path)` or
`stringi::stri_enc_detect(readBin(path, "raw", 4096))` give a confidence
score over candidate encodings.

From `bash`:

```bash
file -i path
head -c 8 path | xxd
```

## Step 3 — apply the fix at the call site

For `read.csv` / `read.table`:

```r
read.csv(path, fileEncoding = "UTF-8")      # or "GBK", "GB18030", "latin1"
```

For `readLines`:

```r
readLines(path, encoding = "UTF-8")
```

For `data.table::fread`: it auto-detects in most cases; if not, use
`encoding = "UTF-8"`.

For console printing of a string already in R memory:

```r
Encoding(x) <- "UTF-8"
# or, on Windows pre-UCRT:
enc2native(x)
```

## Step 4 — Windows non-UTF-8 sessions

If `l10n_info()$`UTF-8`` is FALSE on Windows, recommend upgrading to R
4.2+ which ships UCRT and uses UTF-8 by default. Until then, files
written by other tools as UTF-8 will need explicit `fileEncoding` on
every read.
