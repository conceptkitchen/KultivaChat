# Gunicorn configuration file
bind = "0.0.0.0:8081"
workers = 2
timeout = 60
worker_class = "sync"
worker_connections = 1000
max_requests = 1000
max_requests_jitter = 100
preload_app = True
keepalive = 5
max_worker_memory = 300