# Multi-stage build for Spring Boot application
FROM maven:3.9-eclipse-temurin-17 AS build

# Set working directory
WORKDIR /app

# Copy pom.xml first (for better layer caching)
# When building from project root: backend/pom.xml
# When building from backend dir: pom.xml
COPY backend/pom.xml pom.xml

# Download dependencies (this layer will be cached if pom.xml doesn't change)
# Retry up to 3 times in case of network issues (502, timeout, etc.)
RUN for i in 1 2 3; do \
        mvn dependency:go-offline -B && break || \
        (echo "Attempt $i failed, retrying..." && sleep 5); \
    done || echo "Warning: dependency:go-offline failed, will download during build"

# Copy source code
# When building from project root: backend/src
# When building from backend dir: src
COPY backend/src ./src

# Build the application
RUN mvn clean package -DskipTests

# Runtime stage: Ubuntu Jammy (Intel oneAPI available via APT; runs on Red Hat hosts and elsewhere)
FROM eclipse-temurin:17-jre-jammy

# Install Intel oneAPI Fortran (ifx) and runtime deps; gcc required by ifx. gfortran used as fallback when ifx fails (#10417).
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get install -y --no-install-recommends \
    wget \
    gnupg \
    ca-certificates \
    gcc \
    g++ \
    gfortran \
    python3 \
    python3-pip \
    python3-numpy \
    && wget -qO- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB | gpg --dearmor -o /usr/share/keyrings/oneapi-archive-keyring.gpg \
    && echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] https://apt.repos.intel.com/oneapi all main" > /etc/apt/sources.list.d/oneAPI.list \
    && apt-get update \
    && apt-get install -y --no-install-recommends intel-oneapi-compiler-fortran \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Python plotting (matplotlib)
RUN pip3 install --no-cache-dir matplotlib

# Compile Fortran with Intel ifx (not gfortran). setvars.sh in entrypoint adds ifx to PATH.
ENV COSMO_FORTRAN_COMPILER=ifx
ENV COSMO_FORTRAN_EXE_NAME=multifix

# Create app directory
WORKDIR /app

# Copy the JAR file from build stage
COPY --from=build /app/target/backend-0.0.1-SNAPSHOT.jar app.jar

# Entrypoint: create script in-image so it has Unix (LF) line endings (avoids "no such file or directory" when host has CRLF)
RUN set -e && printf '%s\n' \
    '#!/bin/bash' \
    'set -e' \
    '# Source Intel oneAPI so ifx is on PATH' \
    'if [ -f /opt/intel/oneapi/setvars.sh ]; then' \
    '  source /opt/intel/oneapi/setvars.sh --force > /dev/null 2>&1 || true' \
    'fi' \
    'if ! command -v ifx > /dev/null 2>&1; then' \
    '  for d in /opt/intel/oneapi/compiler/latest/linux/bin /opt/intel/oneapi/compiler/*/linux/bin; do' \
    '    [ -x "$d/ifx" ] && export PATH="$d:$PATH" && break' \
    '  done' \
    'fi' \
    'exec java -jar /app/app.jar' \
    > /app/docker-entrypoint.sh && chmod +x /app/docker-entrypoint.sh

# Copy fortran directory (when building from project root)
# For docker-compose: This is overridden by volume mount, so it's safe to include
COPY fortran /app/fortran

# Expose port
EXPOSE 8080

# Run via script so oneAPI environment (ifx on PATH) is set
ENTRYPOINT ["/app/docker-entrypoint.sh"]
