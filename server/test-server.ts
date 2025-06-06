import express from "express";
import { createServer } from "http";

const app = express();
const server = createServer(app);

app.get("/", (req, res) => {
  res.send("Test server is working");
});

app.get("/api/test", (req, res) => {
  res.json({ message: "API is working" });
});

const PORT = 5001;
server.listen(PORT, "0.0.0.0", () => {
  console.log(`Test server running on port ${PORT}`);
});