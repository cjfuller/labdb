#!/usr/bin/env nu

export def current_hash [] {
    git log -1 --format=%h | str trim
}

def prepare_js [] {
    rm -f public/_s/*.js
    npm install
    npm run-script compile
}

def js_file [version: string] {
    $"app_($version).js"
}

def gcs_path [version: string] {
    $"gs://labdb-static/(js_file $version)"
}

def upload_js [version: string] {
    let target = gcs_path $version
    gsutil cp public/_s/app_.js $target
    gsutil acl ch -u "AllUsers:R" $target
}

def main [] {
    prepare_js
    upload_js (current_hash)
    echo "-> OK" | ansi green
}
