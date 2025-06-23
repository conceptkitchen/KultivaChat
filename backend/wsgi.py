#!/usr/bin/env python3
"""
WSGI entry point for Gunicorn deployment
"""
from app import app

if __name__ == "__main__":
    app.run()