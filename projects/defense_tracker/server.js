const express = require('express');
const { MongoClient } = require('mongodb');

const app = express();
const port = 3000;

app.use(express.static('public'));

const client = new MongoClient("mongodb+srv://gtsoftball:Jackets2023@gtsoftballdata.gyxmwrc.mongodb.net/?retryWrites=true&w=majority", { useNewUrlParser: true, useUnifiedTopology: true });

app.post('/upload', async (req, res) => {
  const data = req.body;

  try {
    await client.connect();

    const database = client.db('gtSoftballData');
    const collection = database.collection('plusDefense');

    const result = await collection.insertMany(data);

    res.json({ message: `${result.insertedCount} rows uploaded`});
  } catch (error) {
    console.error('Error:', error);
    res.status(500).json({ message: 'Internal Server Error' });
  }
});

app.get('/retrieve_stats', async (req, res) => {
    try {
      await client.connect();
  
      const database = client.db('gtSoftballData');
      const collection = database.collection('plusDefense');
  
      // Retrieve data from MongoDB
      const data = await collection.find().toArray();
  
      res.json(data);
    } catch (error) {
      console.error('Error:', error);
      res.status(500).json({ message: 'Internal Server Error' });
    }
  });

app.get('/defense_tracker', (req, res) => {
    res.sendFile(__dirname + '/defense_tracker_app.html');
});

app.get('/defense_stats', (req, res) => {
    res.sendFile(__dirname + '/defense_stats.html');
});

app.listen(port, () => {
  console.log(`Server is running on port ${port}`);
});
