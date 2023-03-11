#!/usr/bin/env nu

use deploy_frontend.nu current_hash

def build_container [] {
    let sha = current_hash
    (gcloud builds submit --region northamerica-northeast1
        --config cloudbuild.yml $"--substitutions=SHORT_SHA=($sha)")
}

def main [] {
    build_container
    echo "Now update the cloud run deployment in the GCP console to point to this new container."
}
