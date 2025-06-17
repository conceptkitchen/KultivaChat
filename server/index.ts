// Temporary redirect to production startup script
import { exec } from 'child_process';
import path from 'path';

console.log('Redirecting to production startup script...');

const productionScript = path.join(process.cwd(), 'production-start.sh');
const child = exec(`bash ${productionScript}`, (error, stdout, stderr) => {
  if (error) {
    console.error(`Error: ${error}`);
    return;
  }
  console.log(stdout);
  if (stderr) {
    console.error(stderr);
  }
});

child.stdout?.pipe(process.stdout);
child.stderr?.pipe(process.stderr);

// Keep the process alive
child.on('exit', (code) => {
  process.exit(code);
});