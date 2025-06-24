import { spawn, ChildProcess } from 'child_process';
import { Request, Response, NextFunction } from 'express';

class PythonBackendService {
  private process: ChildProcess | null = null;
  private isReady = false;
  private startupPromise: Promise<void> | null = null;

  async start(): Promise<void> {
    if (this.startupPromise) {
      return this.startupPromise;
    }

    this.startupPromise = new Promise((resolve, reject) => {
      console.log('Starting Python backend...');
      
      this.process = spawn('python', ['main_2.py'], {
        cwd: './backend',
        stdio: ['pipe', 'pipe', 'pipe'],
        env: { ...process.env }
      });

      let resolved = false;
      const timeout = setTimeout(() => {
        if (!resolved) {
          resolved = true;
          reject(new Error('Backend startup timeout after 30s'));
        }
      }, 30000);

      this.process.stdout?.on('data', (data) => {
        const output = data.toString();
        console.log('[BACKEND]', output.trim());
        
        if (output.includes('Running on http://127.0.0.1:8081') && !resolved) {
          resolved = true;
          clearTimeout(timeout);
          this.isReady = true;
          console.log('Python backend ready on port 8081');
          resolve();
        }
      });

      this.process.stderr?.on('data', (data) => {
        console.error('[BACKEND ERROR]', data.toString().trim());
      });

      this.process.on('exit', (code) => {
        console.log(`Backend process exited with code ${code}`);
        this.isReady = false;
        this.process = null;
        if (!resolved) {
          resolved = true;
          clearTimeout(timeout);
          reject(new Error(`Backend exited with code ${code}`));
        }
      });

      this.process.on('error', (error) => {
        console.error('Backend process error:', error);
        this.isReady = false;
        if (!resolved) {
          resolved = true;
          clearTimeout(timeout);
          reject(error);
        }
      });
    });

    return this.startupPromise;
  }

  middleware() {
    return async (req: Request, res: Response, next: NextFunction) => {
      if (!this.isReady) {
        try {
          await this.start();
        } catch (error) {
          console.error('Failed to start backend:', error);
          return res.status(503).json({
            error: 'Backend service unavailable',
            message: 'Python backend failed to start'
          });
        }
      }
      next();
    };
  }

  stop() {
    if (this.process && !this.process.killed) {
      console.log('Stopping Python backend...');
      this.process.kill('SIGTERM');
      setTimeout(() => {
        if (this.process && !this.process.killed) {
          this.process.kill('SIGKILL');
        }
      }, 5000);
    }
    this.isReady = false;
    this.startupPromise = null;
  }
}

export const pythonBackend = new PythonBackendService();