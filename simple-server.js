const express = require('express');
const app = express();

app.get('/', (req, res) => {
  res.send('<h1>Simple Server Working</h1><p>This confirms basic Express functionality</p>');
});

app.get('/health', (req, res) => {
  res.json({ status: 'ok', time: new Date().toISOString() });
});

const PORT = 5000;
app.listen(PORT, '0.0.0.0', () => {
  console.log(`Simple server running on port ${PORT}`);
});