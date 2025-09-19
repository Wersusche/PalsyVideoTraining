# syntax=docker/dockerfile:1
FROM node:20-alpine AS frontend-build
WORKDIR /app
COPY frontend/package*.json ./
RUN npm ci
COPY frontend ./
RUN npm run build

FROM caddy:2-alpine
WORKDIR /srv
COPY --from=frontend-build /app/dist .
COPY deploy/caddy/Caddyfile /etc/caddy/Caddyfile
RUN mkdir -p /media
VOLUME ["/media"]
