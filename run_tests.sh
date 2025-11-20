#!/bin/bash
set -e
PYTEST=${PYTEST:-pytest}
$PYTEST -q tests/run_all.py
