
// Get form values
const dateForm = document.getElementById('dateForm');
const statsForm = document.getElementById('statsForm');
const popup = document.querySelector('.popup');
const tableContainer = document.getElementById('tableContainer');
const tableBody = document.querySelector('#statsTable tbody');
const uploadDataButton = document.getElementById('uploadDataButton');  

let csv_filename = '';

dateForm.addEventListener('submit', function(e) {
    e.preventDefault();
    const date = document.getElementById('date').value;
    const type = document.getElementById('type').value;

    csv_filename = type + '_' + date + '.csv';

    typeForm.style.display = 'none';
    dateForm.style.display = 'none';
    popup.style.display = 'none';
    tableContainer.style.display = 'block';
});
    
let tableData = [['player', 'type', 'converted', 'type', 'date']];
      
// Event listener for form submission
document.getElementById('statsForm').addEventListener('submit', function(e) {
    e.preventDefault();

    const playerName = document.getElementById('playerName').value;
    const playType = document.getElementById('playType').value;
    const converted = document.getElementById('converted').value;
    const date = document.getElementById('date').value;
    const type = document.getElementById('type').value;

    // Create new table row
    const newRow = document.createElement('tr');
    const playerNameCell = document.createElement('td');
    playerNameCell.textContent = playerName;
    const playTypeCell = document.createElement('td');
    playTypeCell.textContent = playType;
    const convertedCell = document.createElement('td');
    convertedCell.textContent = converted;
    const typeCell = document.createElement('td');
    typeCell.textContent = type;
    const dateCell = document.createElement('td');
    dateCell.textContent = date;

    newRow.appendChild(playerNameCell);
    newRow.appendChild(playTypeCell);
    newRow.appendChild(convertedCell);
    newRow.appendChild(typeCell);
    newRow.appendChild(dateCell);

    const deleteButton = document.createElement('button');
    deleteButton.textContent = 'Delete';
    deleteButton.addEventListener('click', function() {
        // Remove row from table
        newRow.remove();
        // Remove row from tableData array
        const index = tableData.findIndex(row => row[0] === playerName && row[1] === playType && row[2] === converted && row[3] === type && row[4] === date);
        if (index !== -1) {
          tableData.splice(index, 1);
        }
        // Hide export button if table is empty
        if (tableData.length === 0) {
          uploadDataButton.style.display = 'none';
        }
      });
    newRow.appendChild(deleteButton);

     // Append new row to the table body
    tableBody.appendChild(newRow);

    // Add data to tableData array
    tableData.push([playerName, playType, converted, type, date]);

    // Show export button if table has data
    if (tableData.length > 0) {
        uploadDataButton.style.display = 'block';
    }
        
});

window.uploadData = async () => {
    try {
      // Get table data
      const tableRows = document.querySelectorAll('#statsTable tbody tr');
      const data = Array.from(tableRows).map(row => {
        const cells = row.querySelectorAll('td');
        return {
          playerName: cells[0].textContent,
          playType: cells[1].textContent,
          converted: cells[2].textContent,
          type: cells[3].textContent,
          date: cells[4].textContent,
        };
      });
  
      // Make a POST request to the server to handle data upload
      const uploadResponse = await fetch('/upload', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify(data),
      });
  
      const result = await uploadResponse.json();
  
      // Display status message
      document.getElementById('status').innerText = result.message;
    } catch (error) {
      console.error('Error:', error);
    }
  };