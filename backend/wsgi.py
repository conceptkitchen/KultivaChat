"""
WSGI entry point for production deployment
"""
from main_2 import app

if __name__ == "__main__":
    app.run(host='0.0.0.0', port=8081)