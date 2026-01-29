#!/bin/bash
set -e
# Source Intel oneAPI so ifx is on PATH (Docker image installs oneAPI here)
if [ -f /opt/intel/oneapi/setvars.sh ]; then
  source /opt/intel/oneapi/setvars.sh --force > /dev/null 2>&1 || true
fi
# Fallback: add common oneAPI compiler paths so ifx is found even if setvars layout differs
if ! command -v ifx > /dev/null 2>&1; then
  for d in /opt/intel/oneapi/compiler/latest/linux/bin /opt/intel/oneapi/compiler/*/linux/bin; do
    [ -x "$d/ifx" ] && export PATH="$d:$PATH" && break
  done
fi
exec java -jar /app/app.jar
