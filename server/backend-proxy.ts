import { spawn, ChildProcess } from 'child_process';
import { Express } from 'express';

export class BackendManager {
  private backendProcess: ChildProcess | null = null;
  private isStarting = false;
  private startPromise: Promise<void> | null = null;

  async ensureBackendRunning(): Promise<void> {
    if (this.isStarting && this.startPromise) {
      return this.startPromise;
    }

    if (this.backendProcess && !this.backendProcess.killed) {
      return Promise.resolve();
    }

    this.isStarting = true;
    this.startPromise = this.startBackend();
    
    try {
      await this.startPromise;
    } finally {
      this.isStarting = false;
      this.startPromise = null;
    }
  }

  private startBackend(): Promise<void> {
    return new Promise((resolve, reject) => {
      console.log('Starting Python backend process...');
      
      this.backendProcess = spawn('python', ['main_2.py'], {
        cwd: './backend',
        stdio: ['pipe', 'pipe', 'pipe'],
        detached: false
      });

      let resolved = false;
      const timeout = setTimeout(() => {
        if (!resolved) {
          resolved = true;
          reject(new Error('Backend startup timeout'));
        }
      }, 30000);

      this.backendProcess.stdout?.on('data', (data) => {
        const output = data.toString();
        console.log('[BACKEND]', output.trim());
        
        if (output.includes('Running on http://127.0.0.1:8081') && !resolved) {
          resolved = true;
          clearTimeout(timeout);
          console.log('Backend server ready on port 8081');
          resolve();
        }
      });

      this.backendProcess.stderr?.on('data', (data) => {
        console.error('[BACKEND ERROR]', data.toString().trim());
      });

      this.backendProcess.on('exit', (code) => {
        console.log(`Backend process exited with code ${code}`);
        this.backendProcess = null;
        if (!resolved) {
          resolved = true;
          clearTimeout(timeout);
          reject(new Error(`Backend process exited with code ${code}`));
        }
      });

      this.backendProcess.on('error', (error) => {
        console.error('Backend process error:', error);
        if (!resolved) {
          resolved = true;
          clearTimeout(timeout);
          reject(error);
        }
      });
    });
  }

  setupRoutes(app: Express) {
    // Middleware to ensure backend is running before proxying
    app.use('/api', async (req, res, next) => {
      try {
        await this.ensureBackendRunning();
        next();
      } catch (error) {
        console.error('Failed to start backend:', error);
        res.status(503).json({ 
          error: 'Backend service unavailable',
          message: 'Failed to start backend server'
        });
      }
    });
  }

  async stop() {
    if (this.backendProcess && !this.backendProcess.killed) {
      console.log('Stopping backend process...');
      this.backendProcess.kill('SIGTERM');
      
      // Force kill after 5 seconds
      setTimeout(() => {
        if (this.backendProcess && !this.backendProcess.killed) {
          this.backendProcess.kill('SIGKILL');
        }
      }, 5000);
    }
  }
}